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
  fishCount <- get(paste0('x_',pu), envir = .shadia)
  
  # dynamically identify the sp_n dataframe, for PU n
  spawnProb <- get(paste0('sp_',pu), envir = .shadia)
  
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
  
  PUS <- merge(fishCount,
              d_pu,
              by = c('fishAges', 'pus'),
              all.x = T)
  PUS$pus <- as.character(PUS$pus)
  
  return(PUS)
}                    
