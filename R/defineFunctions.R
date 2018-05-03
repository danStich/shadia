# Define R functions ------------------

# createPUMatrix()
# generalize creation of the PU_n data 
# structure for different PUs, fish sexes.
# Return PUS for the PU n
# inputs: isFemale (0/1); pu n (1..4); isEgg (default FALSE)
# dynamically determine spawnProb (sp_n for PU n); fishCount (x_n for PU n)
# JMS Dec 2017
  createPUMatrix <- function(isFemale, pu, isEgg=FALSE){
  
    # dynamically identify the x_n dataframe, for PU n
    fishCount <- get(paste0('x_',pu))
    names(fishCount) <- c('pus', 'fishAges')
  
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

# additionalEggsProcessing()
# additional processing performed for all egg calculations
# pulled into function, JMS Dec 2017
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

# processPopulation()
# JMS Dec 2017
processPopulation <- function(isFemale, isEgg = FALSE) {
  # uses generalized function for creating PU matrix of males, females, or eggs
  PUS_1 <<-  createPUMatrix(isFemale, 1, isEgg)
  PUS_2 <<-  createPUMatrix(isFemale, 2, isEgg)
  PUS_3 <<-  createPUMatrix(isFemale, 3, isEgg)
  PUS_4 <<-  createPUMatrix(isFemale, 4, isEgg)

  # Collect age-structured male, female, or egg population in each PU
  # Pre-allocate a list to hold the info
  population <<- list(
    # Assign the fish or eggs
    assignFishToRoutes(1, PUS_1),
    assignFishToRoutes(2, PUS_2),
    assignFishToRoutes(3, PUS_3),
    assignFishToRoutes(4, PUS_4)
  )
  
  # Remove NA values and replace with zeroes b/c that's what they are
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

# assignFishToRoutes()
# Generically generate individual x_PUS_n, for fish gender and
#   PU number. JMS Dec 2017
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


# writeSimData()
# JMS
writeSimData <- function(filename) {
  # Write the inputs and outputs to a text file that can be read into R
  if (!file.exists(paste(
    filename,
    'Sims',
    format(Sys.time(), '%m%d%Y%H%M%S'),
    '.txt',
    sep = ''
  ))) {
    write.table(
      res,
      paste(
        filename,
        'Sims',
        format(Sys.time(), '%m%d%Y%H%M%S'),
        '.txt',
        sep = ''
      ),
      sep = ',',
      row.names = FALSE,
      quote = FALSE,
      append = FALSE
    )
  } else {
    Sys.sleep(runif(1, 1, 3))
    write.table(
      res,
      #paste('dat/pnrSims', format(Sys.time(), '%m%d%Y%H%M%S'),
      paste(filename, 'Sims', format(Sys.time(), '%m%d%Y%H%M%S'),
            sep = ''),
      sep = ',',
      row.names = FALSE,
      quote = FALSE
    )
  }
}

# writeSenData()
# JMS
writeSenData <- function(filename) {
  # Write the sensitivity analysis to a file
  if (!file.exists(paste(
    filename,
    'Sen',
    format(Sys.time(), '%m%d%Y%H%M%S'),
    '.txt',
    sep = ''
  ))) {
    write.table(
      sens,
      paste(
        filename,
        'Sen',
        format(Sys.time(), '%m%d%Y%H%M%S'),
        '.txt',
        sep = ''
      ),
      sep = ',',
      row.names = FALSE,
      quote = FALSE,
      append = FALSE
    )
  } else {
    Sys.sleep(runif(1, 1, 3))
    write.table(
      sens,
      paste(filename, 'Sen', format(Sys.time(), '%m%d%Y%H%M%S'),
            sep = ''),
      sep = ',',
      row.names = FALSE,
      quote = FALSE
    )
  }
}

# writeData()
writeData <- function(filename) {
  writeSimData(filename)
  writeSenData(filename)
}

# CI()
# Get confidence intervals
CI <- function(x) {
  quantile(x, probs = c(0.025, 0.975))
}

# tz()
# Functions from lubridate
tz <- function (x) {
  if (is.null(attr(x, "tzone")) && !is.POSIXt(x))
    return("UTC")
  tzs <- attr(as.POSIXlt(x), "tzone")
  tzs[1]
}

# yday()
yday <- function (x) {
  as.POSIXlt(x, tz = tz(x))$yday + 1
}

# year()
year <- function (x) {
  as.POSIXlt(x, tz = tz(x))$year + 1900
}

# substrRight()
# Collect chars from the right side of a text string
substrRight <- function(x, n) {
  substr(x, nchar(x) - n + 1, nchar(x))
}

# invlogit()
# Make function for back-transformation from logit scale
invlogit <- function(x) {
  exp(x) / (1 + exp(x))
}
