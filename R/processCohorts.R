#' @title Process numbers of fish by age, sex, PU, and migration route
#' 
#' @description Internal function used to calculate the
#' numbers of males, females, and eggs spawned in each 
#' age class in each production unit of each of four
#' migration routes.
#' 
#' Not intended to be called directly, but visible
#' for the sake of model transparency.
#' 
#' @return A list of lists, each holding the numbers of 
#' males, females, or eggs in each age class in each 
#' production unit of each of four migration routes 
#' prior to the application of post spawning mortality
#' or carrying capacity.
#' 
#' @export 
#' 
processCohorts <- function(){
  environment(processPopulation) <- .shadia

  males <- eval(processPopulation(isFemale=FALSE), envir = .shadia)
  males2res <- males

  females <- eval(processPopulation(isFemale=TRUE), envir = .shadia)
  females2res <- females  

  fec_Max <- eval(processPopulation(isFemale=FALSE, isEgg=TRUE), envir = .shadia)

  return(list(  
    males = males,
    females = females,
    fec_Max = fec_Max,
    males2res = males2res,
    females2res = females2res
  ))   
  
}