#' Annual upstream migration parameters
#' 
#' A helper function used to define upstream passage efficiencies 
#' and routes based on stochastic variables and user-defined
#' inputs to \code{\link{penobscotRiverModel}}, as well as
#' the probability of using the mainstem Penobscot River
#' during downstream migration. Not intended to
#' be called directly, but visible for model transparency.
#' 
#' @param NULL
#' 
#' @return A named list of objects used for the annual 
#' upstream migration model, including 1) upstream dam 
#' passage efficiencies for each of the four possible 
#' migration routes (list), 2) probability of using the 
#' mainstem Penobscot River around Marsh Island (vector),
#' 3) the probability of using the mainstem Penobscot 
#' River during downstream migration (vector), and
#' 4) probability of using the mainstem Penobscot River
#' at the confluence with the Piscataquis River (vector).
#' 
#' @export

annualUpstream <- function(){
  
if(river=='penobscot'){
  # Assign efficiencies to the upstream passage groups (pisc or main)
  upEffs <- vector(mode = 'list', length = nRoutes)
  upEffs[[1]] <- vector(mode = 'numeric', length = length(damRkms[[1]]))
  upEffs[[2]] <- vector(mode = 'numeric', length = length(damRkms[[2]]))
  upEffs[[3]] <- vector(mode = 'numeric', length = length(damRkms[[3]]))
  upEffs[[4]] <- vector(mode = 'numeric', length = length(damRkms[[4]]))

  # Route 1- Mainstem to piscataquis
  upEffs[[1]][1] <- Open
  upEffs[[1]][2] <- Confluence
  upEffs[[1]][3] <- MilfordUp
  upEffs[[1]][4] <- HowlandUp
  upEffs[[1]][5] <- BrownsMillUp
  upEffs[[1]][6] <- MooseheadUp
  upEffs[[1]][7] <- GuilfordUp
  # Route 2- Main-stem to main-stem
  upEffs[[2]][1] <- Open
  upEffs[[2]][2] <- Confluence
  upEffs[[2]][3] <- MilfordUp
  upEffs[[2]][4] <- WestEnfieldUp
  upEffs[[2]][5] <- MattaceunkUp
  # Route 1- Stillwater to piscataquis
  upEffs[[3]][1] <- Open
  upEffs[[3]][2] <- OronoUp
  upEffs[[3]][3] <- StillwaterUp
  upEffs[[3]][4] <- GilmanUp
  upEffs[[3]][5] <- HowlandUp
  upEffs[[3]][6] <- BrownsMillUp
  upEffs[[3]][7] <- MooseheadUp
  upEffs[[3]][8] <- GuilfordUp
  # Route 1- Stillwater to main-stem
  upEffs[[4]][1] <- Open
  upEffs[[4]][2] <- OronoUp
  upEffs[[4]][3] <- StillwaterUp
  upEffs[[4]][4] <- GilmanUp
  upEffs[[4]][5] <- WestEnfieldUp
  upEffs[[4]][6] <- MattaceunkUp

  # JMS: Draw probability of using the Stillwater Branch moved to outer-loop-sampling.R
  pMainUp <- 1 - pStillwaterUp
  pMainD <- 1 - pStillwaterD

  # # Draw probability of using the Piscataquis for upstream migration. NOTE: NEED
  # # TO MAKE THIS CONDITIONAL ON FLOW.
  # JMS: Draw of pPiscUp moved to outer-loop-sampling.R
  pMainstemUp <- 1 - pPiscUp  # Probability of using mainstem in upper river

return(list(
  upEffs = upEffs,
  pMainUp = pMainUp,
  pMainD = pMainD,
  pMainstemUp = pMainstemUp
))  
}
  
if(river=='merrimack'){
  # Assign efficiencies to the upstream passage groups (pisc or main)
  upEffs <- vector(mode = 'list', length = nRoutes)

  # Route 1- Mainstem to piscataquis
  upEffs[[1]][1] <- EssexUp
  upEffs[[1]][2] <- PawtucketUp
  upEffs[[1]][3] <- AmoskeagUp
  upEffs[[1]][4] <- HooksetUp

  return(list(
    upEffs = upEffs
  ))  
}  
  
if(river=='connecticut'){
  # Assign efficiencies to the upstream passage groups (pisc or main)
  upEffs <- vector(mode = 'list', length = nRoutes)

  # Route 1- Mainstem to piscataquis
  upEffs[[1]][1] <- HolyokeUp
  upEffs[[1]][2] <- TurnersUp
  upEffs[[1]][3] <- CabotUp
  upEffs[[1]][4] <- VernonUp
  upEffs[[1]][5] <- BellowsUp

  return(list(
    upEffs = upEffs
  ))  
}    
  
  
  
}  
  