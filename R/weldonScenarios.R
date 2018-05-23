#' @title Weldon Dam relicensing scenarios
#' 
#' @description Special, internal function to replace Weldon passage
#' depending on when passage is implemented. This is a relic from
#' specific management strategy evaluation scenarios related to
#' hydropower relicensing, which is not implemented in the 
#' current release. Retained as a reminder of flexibility, and for
#' potential generalized usage in future releases.
#' 
#' Not intended to be called directly, but visible 
#' for the sake of model transparency.
#' 
#' @return A list containing the initial population size,
#' spawner-recruitment and repeat-spawning probabilities,
#' and spawning/recruitment pools.
#' 
#' @export
#' 
weldonScenarios <- function(){

  if (n < scenario) {
    upEffs[[2]][5] = 0
    upEffs[[4]][6] = 0
  } else {
    upEffs[[2]][5] = MattaceunkUp
    upEffs[[4]][6] = MattaceunkUp
  }
  
  return(list(upEffs=upEffs))
  
}