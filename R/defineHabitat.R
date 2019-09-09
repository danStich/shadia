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
  habitat[[2]] <- c(
    (22344 + 34868),
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
  habitat[[4]] <- c(
    (22344 + 34868),
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
  
  habitat[[1]] <- c(
    202782,
    92910,
    220532,
    48000,
    182598
  )
  habitat[[2]] <- c(
    202782,
    92910,
    220532,
    48000,
    182598
  )
  
  return(list(
    habitat=habitat
  ))
  
}  
  

if(river=='connecticut'){
  # Connecticut River relies on habitat estimated
  # and production potential of 203 spawners/ha
  # as identified in the Connecticut River
  # Shad Management Plan (2017) for inputs  
    habitat <- vector(mode = 'list', length = nRoutes)
    
    habitat[[1]] <- c(4825, 1369, 0, 762, 1042)*203
    habitat[[2]] <- c(4825, 1369, 0, 762, 1042)*203
    
    return(list(
      habitat=habitat
    ))
}  
  
  
if(river=='susquehanna'){
  
  # Susquehanna River relies on ha, using same numbers
  # as Connecticut River for the sake of consistency
  habitat <- vector(mode = 'list', length = nRoutes)
  
  habitat[[1]] <- c(0, 0, 0, 1833, 11057, 2018)*203
  habitat[[2]] <- c(0, 0, 0, 1833, 11057, 1489, 1489, 0)*203
  habitat[[3]] <- c(0, 0, 0, 1833, 11057, 6664, 104, 0)*203
  habitat[[4]] <- c(0, 0, 0, 1833, 11057, 6664, 1142, 1147, 0, 0)*203  
  
  return(list(
    habitat=habitat
  ))
}    
  

if(river=='saco'){
  
  # Saco River relies on ha, using same numbers as others
  # for sake of consistency  
  habitat <- vector(mode='list', length=nRoutes)
  
  habitat[[1]] <- c(268, 5, 178, 97, 57, 238, 157)*203
  
  return(list(
    habitat=habitat
  ))
  
} 
  
if(river=='kennebec'){
  
  habitat <- vector(mode='list', length=nRoutes)
  
  habitat[[1]] <- c(
    686270,
    0,
    26150,
    63276,
    95287
  )
  habitat[[2]] <- c(
    686270,
    21536,
    23746
  )
  
  return(list(
    habitat=habitat
  ))
  
}    
  
  
}  