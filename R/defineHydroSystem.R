#' @title Definition of hydro system
#' 
#' @description Internal function used to define the
#' configuration of the hydrosystem and catchment. Not
#' intended to be called directly, but visible for the
#' sake of model transparency. Relies on river ID (chr)
#' defined in the main file for each system (e.g. penobscot,
#' merrimack, etc.)
#' 
#' @return A list of values related to the hydro system, 
#' including 1) the number of migration routes, 2) number of 
#' dams, 3) number of production units delineated by dams,
#' 4) the upstream extent to habitat in river kilometers (rkm), 
#' and 5) the locations of dams in rkm.
#'
#' @export
#'
defineHydroSystem <- function(){
  
  
if(river=='penobscot'){  
# Penobscot River -----------------------------------
# For the Penobscot, there are two potential
# migration routes, so this needs to be accommodated.
# FOR ALL OF THE HABITAT VARIABLES AND THE PASSAGE ROUTE
# VARIABLES, GROUP 1 CORRESPONDS TO THE PISCATAQUIS ROUTE 
# AND GROUP 2 TO THE UPPER MAINSTEM PENOBSCOT ROUTE.
nRoutes <- 4

# Need separate habitat values for each migration route
# Number of dams- In the Penobscot River we are treating the confluence of the
# Stillwater Branch and the Penobscot River as a dam because it has the
# Potential to cause delay to fish that must migrate up the mainstem of the
# river. The passage rate at that dam will be derived based on delay.
# Group 1: Mainstem to Piscataquis
# Group 2: Mainstem to mainstem
# Group 3: Stillwater to Piscataquis
# Group 4: Stillwater to mainstem
nDams <- c(6, 4, 7 , 5)

# Define number of production units- dams plus two in this system
nPU <- nDams + 1

# Define maximum rkm for the system. In the Piscataquis, Guilford Dam is fine,
# and for the mainstem use Mattaceunk
maxrkm <- c(182, 165, 182, 165)

# Define rkms for each of the dams for each migration group
damRkms <- vector(mode = 'list', length = nRoutes)
damRkms[[1]] <- c(40, 52, 60, 99, 165, 166, 179)     # Dam rkms for group 1
damRkms[[2]] <- c(40, 52, 60, 100, 150)              # Dam rkms for group 2
damRkms[[3]] <- c(40, 52, 56, 62, 99, 165, 166, 179) # Dam rkms for group 3
damRkms[[4]] <- c(40, 52, 56, 62, 100, 150)          # Dam rkms for group 4

return(list(
nRoutes=nRoutes,
nDams=nDams,
nPU=nPU,
maxrkm=maxrkm,
damRkms=damRkms
))
}
  
if(river=='merrimack'){
nRoutes <- 1
nDams <- 4

# Define number of production units- dams plus two in this system
nPU <- nDams + 1

# Define maximum rkm for the system.
maxrkm <- 204

# Define rkms for each of the dams for each migration group
damRkms <- vector(mode = 'list', length = nRoutes)
damRkms[[1]] <- c(45, 65, 120, 135)     # Dam rkms for group 1

return(list(
nRoutes=nRoutes,
nDams=nDams,
nPU=nPU,
maxrkm=maxrkm,
damRkms=damRkms
))  
  
  
  
  
  
  
}  






}

