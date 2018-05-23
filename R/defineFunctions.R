# Define R functions ------------------

# Function definitions

#' @title Create production unit matrix
#' 
#' @description Internal function to generalize creation of 
#' the PU_n data structure for different PUs and fish sexes.
#' Not intended to be called directly, but visible 
#' for model transparency.
#' 
#' @return Matrix of fish numbers by ages in each PU following
#' execution of the upstream migration model in source code.
#' 
#' @export
#' 
# JMS Dec 2017    
createPUMatrix <- function(isFemale, pu, isEgg=FALSE){ 
  
  # dynamically identify the x_n dataframe, for PU n
  fishCount <- get(paste0('x_',pu))
  
  # dynamically identify the sp_n dataframe, for PU n
  spawnProb <- get(paste0('sp_',pu))
  
  if (!isEgg) {
    # for males and females
    d_pu  <- ddply(spawnProb[spawnProb$female == isFemale, ],
                   .(fishAges, pus),
                   summarise,
                   gender = sum(surv)) 
  } else {
    # for eggs
    d_pu <- ddply(spawnProb,
                  .(fishAges, pus),
                  summarise,
                  gender = sum(fecundity * surv)) 
  }
  
  if (nrow(d_pu) == 0) {
    d_pu <- matrix(NA, 1, 3,
                   dimnames = list(c(NULL), 
                                   c('fishAges', 'pus', 'gender')))
  }
  
  PUS = merge(fishCount,
              d_pu,
              by = c('fishAges', 'pus'),
              all.x = T)
  PUS$pus = as.character(PUS$pus)
  
  return(PUS)
}                    


#' @title Processing for egg calculations
#' 
#' @description Internal function used to sum number of eggs in each 
#' production unit from each of the four migration
#' routes, and apply carrying capacity based on k_pus. In addition
#' to cohort processing routines (hence the name). Not
#' intended to be called directly. Visible for the sake of
#' model transparency.
#' 
#' @return List of number of eggs spawned in each production unit
#' by simulated females, or the carrying capacity: whichever
#' is smaller.
#' 
#' @export
#' 
additionalEggsProcessing <- function(fec) {
  
  # Calculate total number of eggs in each PU
  fec2 = vector(mode = 'list', length = length(fec))
  for (i in 1:length(fec)) {
    fec2[[i]] = mapply(sum, fec[[i]])
  }
  # Now sum all eggs from each of the shared PUs for all routes.  Put all
  # of the eggs from shared PUs into fec2[[min]] that shares the PU,
  # set all others to zero
  fec2[[1]][1] = fec2[[1]][1] + fec2[[2]][1] + fec2[[3]][1]  +
    fec2[[4]][1]
  fec2[[1]][2] = fec2[[1]][2] + fec2[[2]][2]
  fec2[[1]][3] = fec2[[1]][3] + fec2[[2]][3] + fec2[[3]][4] + fec2[[4]][4]
  fec2[[1]][c(4:7)] = fec2[[1]][c(4:7)] + fec2[[3]][c(5:8)]
  fec2[[2]][c(4, 5)] = fec2[[2]][c(4, 5)] + fec2[[4]][c(5, 6)]
  fec2[[3]][c(3)] = fec2[[3]][3] + fec2[[4]][3]
  fec2[[2]][c(1:3)] = 0
  fec2[[3]][c(1:2, 4:8)] = 0
  fec2[[4]][1:6] = 0
  
  # Apply carrying capacity limitation to each production unit based
  # on habitat availability
  fec_Max = vector(mode = 'list', length = length(fec))
  for (i in 1:length(fec2)) {
    for (j in 1:length(fec2[[i]])) {
      if (fec2[[i]][j] > k_pus[[i]][j]) {
        fec_Max[[i]][j] = k_pus[[i]][j]
      } else {
        fec_Max[[i]][j] = fec2[[i]][j]
      }
    }
  }
  return(fec_Max)
  #toc()
}  

#' @title Process population
#' 
#' @description A function that uses \code{\link{createPUMatrix}},
#' \code{\link{assignFishToRoutes}}, and 
#' \code{\link{additionalEggsProcessing}} to return the 
#' number of fish in each cohort in each PU prior to 
#' application of annual mortality rates and downstream
#' migration. Not intended to be called directly.
#'  Visible for the sake of model transparency.
#'  
# JMS Dec 2017
processPopulation <- function(isFemale, isEgg = FALSE) {
  environment(createPUMatrix) <- .shadia
  environment(assignFishToRoutes) <- .shadia
  environment(additionalEggsProcessing) <- .shadia
  
  # uses generalized function for creating PU matrix
  # of males, females, or eggs
  PUS_1 <-  createPUMatrix(isFemale, 1, isEgg)
  PUS_2 <-  createPUMatrix(isFemale, 2, isEgg)
  PUS_3 <-  createPUMatrix(isFemale, 3, isEgg)
  PUS_4 <-  createPUMatrix(isFemale, 4, isEgg)
  
  # Collect age-structured male, female, or egg population
  # in each PU.
  
    # Pre-allocate a list to hold the info
    population = vector(mode = 'list', length = nRoutes)
    population[[1]] = vector(mode = 'list', length = (nPU[[1]])) # main-to-pisc
    population[[2]] = vector(mode = 'list', length = (nPU[[2]])) # main-to-main
    population[[3]] = vector(mode = 'list', length = (nPU[[3]])) # still-to-pisc
    population[[4]] = vector(mode = 'list', length = (nPU[[4]])) # still-to-main
    
    # Assign the fish or eggs to routes
    population[[1]] <- assignFishToRoutes(1, PUS_1)
    population[[2]] <- assignFishToRoutes(2, PUS_2)
    population[[3]] <- assignFishToRoutes(3, PUS_3)
    population[[4]] <- assignFishToRoutes(4, PUS_4)
    
    # Remove NA values and replace with zeroes because
    # that's what they are.
    population = rapply(
      population,
      f = function(x)
        ifelse(is.na(x), 0, x),
      how = 'replace'
    )
    if (!isEgg) {
      return(population)
    } else{
      fec_Max <- additionalEggsProcessing(population)
      return(fec_Max)
    }
}    

#' @title Assign fish to routes
#' 
#' @description Generically generate individual x_PUS_n,
#' for fish gender and PU number. Not intended to be called
#' directly. Visible for the sake of model transparency.
#' 
#' @export
#' 
# JMS Dec 2017
assignFishToRoutes <- function(puNum, PUS) {
  
  # number of km segments in the PU
  iLen <- length(puNames[[puNum]])
  
  fishPU = vector(mode = 'list', length = (iLen)) 
  
  # Now collect the number of fish in the current PU in each route from the PUS
  # dataframe and add them to the correct elements of the empty list
  
  for (i in 1:iLen) { # number of km segments in the PU
    for (t in 1:maxAge) { # fish ages in years
      
      conditionA <- PUS$pus == unique(puNames[[puNum]])[i]
      conditionB <- PUS$fishAges == unique(PUS$fishAges)[t]          
      
      fishPU[[i]][t] = PUS$gender[conditionA & conditionB]
    }
  }
  return(fishPU)      
}   

#' @title Calculate 95 percent confidence intervals
#' 
#' @description A helper function to calculate 95% confidence
#' intervals on an object.
#' 
#' @param x A numeric vector.
#' 
#' @return Returns the 2.5th and 97.5th quantiles of
#' an object.
#' 
#' @export
#' 
CI <- function(x) {
  quantile(x, probs = c(0.025, 0.975))
}


#' @title Substring right
#' 
#' @description Collect characters from the right side of a text string.
#' 
#' @param x A character string or vector of
#' character strings.
#' 
#' @param n The number of characters from the right
#' to collect.
#' 
#' @return A character string of n elements from
#' the right of \code{x}.
#' 
#' @export
#' 
substrRight <- function(x, n) {
  substr(x, nchar(x) - n + 1, nchar(x))
}

#' @title Inverse logit
#' 
#' @description A function for back-transformation 
#' of variables from the logit scale.
#' 
#' @param x A numeric vector.
#' 
#' @return A numeric vector on the probability scale [0, 1].
#' 
#' @export
#' 
invlogit = function(x) {
  exp(x) / (1 + exp(x))
}

#' @title Add stochasticity to a list
#'
#' @description Add stochastic noise to a list variable.
#' 
#' @param x A list of numeric vectors.
#' 
#' @param stoch A numeric vector of length 1.
#' 
#' @return The element-wise product of x and stoch.
#' 
#' @details This function was created to multiply multiple
#' elements of a list by a stochastic variable that was
#' drawn from a random number generator. But, more generally
#' it is just list multiplication with a different name.
#' 
#' @export
#' 
addStochList <- function(x, stoch){
  mapply("*", x, stoch)
  return(x)
}
