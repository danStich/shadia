#' @title Production potential definition
#'
#' @description Internal function used to
#' assign initial values of production potential
#' for American shad in production units of Rivers
#' 
#' Not intended to be called directly, but visible 
#' for model transparency.
#' 
#' @return A list of habitat production potential
#' for production units in the Penobscot River, Maine, USA.
#' 
#' @references Maine Department of Marine Resources (MDMR). 2009. 
#' Operational plan for the restoration of diadromous fishes 
#' to the Penobscot River. Final Report, MDMR, 
#' Maine Department of Inland Fish and Wildlife, 
#' Augusta, ME. 
#' 
#' Technical Committee for Anadromous Fishery Management
#' of the Merrimack River Basin. 2010. A plan for the 
#' restoration of American shad Merrmimack River watershed. 
#' 
#' @export
#' 
defineHabitat <- function(){

if(river=='penobscot'){
habitat <- vector(mode = 'list', length = nRoutes)

  OronoHabitat <- 1000
  StillwaterHabitat <- 10000
  habitat[[1]] <- c(
    (22344 + 34868),
    (14339 + 34868),
    (400560 + 26285 + 12746),
    (153461 + 37769 + 15257),
    1053,
    22591,
    14922
  )
  habitat[[2]] <- c((22344 + 34868),
                   (14339 + 34868),
                   (400560 + 26285 + 12746),
                   (333196 + 205744),
                   (204336 + 25773)
  )
  habitat[[3]] <- c(
    (22344 + 34868),
    (OronoHabitat),
    (StillwaterHabitat),
    (400560 + 26285 + 12746),
    (153461 + 37769 + 15257),
    1053,
    22591,
    14922
  )
  habitat[[4]] <- c((22344 + 34868),
                   (OronoHabitat),
                   (StillwaterHabitat),
                   (400560 + 26285 + 12746),
                   (333196 + 205744),
                   (204336 + 25773)
  )
  
return(list(
  OronoHabitat=OronoHabitat,
  StillwaterHabitat=StillwaterHabitat,
  habitat=habitat
  ))  
}
  
if(river=='merrimack'){
  
  habitat <- vector(mode='list', length=nRoutes)
  
  habitat[[1]] <- c(202782,
                    92910,
                    220532,
                    48000,
                    182598
  )
  
  return(list(
    habitat=habitat
  ))
  
}  
  
  
  
  
}  