#' @title Process population
#' 
#' @description A function that uses \code{\link{createPUMatrix}},
#' \code{\link{assignFishToRoutes}}, and 
#' \code{\link{additionalEggsProcessing}} to return the 
#' number of fish in each cohort in each PU prior to 
#' application of annual mortality rates and downstream
#' migration.  Called by \code{\link{processCohorts}}.
#' Not intended to be called directly. Visible for
#' model transparency.
#'  
#' @export
#' 
processPopulation <- function(isFemale, isEgg = FALSE) {
  environment(createPUMatrix) <- .shadia
  environment(assignFishToRoutes) <- .shadia
  environment(additionalEggsProcessing) <- .shadia
  
    # uses generalized function for creating PU matrix
    # of males, females, or eggs
    for(i in 1:nRoutes){
      assign(paste0('PUS_', i), createPUMatrix(isFemale, i, isEgg), envir = .shadia)
    }

    # Collect age-structured male, female, or egg population
    # in each PU.
    population <- vector(mode = 'list', length = nRoutes)
    
    for(i in 1:nRoutes){
      population[[i]] <- vector(mode = 'list', length = (nPU[[i]]))
      population[[i]] <- assignFishToRoutes(i, get(paste0("PUS_",i), envir = .shadia))
    }

      # Remove NA values and replace with zeroes because
      # that's what they are.
      population <- rapply(
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
