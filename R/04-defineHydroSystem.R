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
defineHydroSystem <- function(river) {
  if (river == "penobscot") {
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
    nDams <- c(6, 4, 7, 5)

    # Define number of production units- dams plus two in this system
    nPU <- nDams + 1

    # Define maximum rkm for the system. In the Piscataquis, Guilford Dam is fine,
    # and for the mainstem use Mattaceunk
    maxrkm <- c(182, 165, 182, 165)

    # Define rkms for each of the dams for each migration group
    damRkms <- vector(mode = "list", length = nRoutes)
    damRkms[[1]] <- c(40, 52, 60, 99, 165, 166, 179) # Dam rkms for group 1
    damRkms[[2]] <- c(40, 52, 60, 100, 150) # Dam rkms for group 2
    damRkms[[3]] <- c(40, 52, 56, 62, 99, 165, 166, 179) # Dam rkms for group 3
    damRkms[[4]] <- c(40, 52, 56, 62, 100, 150) # Dam rkms for group 4

    return(list(
      nRoutes = nRoutes,
      nDams = nDams,
      nPU = nPU,
      maxrkm = maxrkm,
      damRkms = damRkms
    ))
  }


  if (river == "merrimack") {
    nRoutes <- 2
    nDams <- c(4, 4)

    # Define number of production units- dams plus two in this system
    nPU <- nDams + 1

    # Define maximum rkm for the system.
    maxrkm <- c(204, 204)

    # Define rkms for each of the dams for each migration group
    damRkms <- vector(mode = "list", length = nRoutes)
    damRkms[[1]] <- c(45, 64, 120, 135) # Dam rkms for group 1
    damRkms[[2]] <- c(45, 65, 120, 135) # Dam rkms for group 1

    # Return the list
    return(list(
      nRoutes = nRoutes,
      nDams = nDams,
      nPU = nPU,
      maxrkm = maxrkm,
      damRkms = damRkms
    ))
  }


  if (river == "connecticut") {
    # Number of routes
    #  Route 1 is spillway/gatehouse
    #  Route 2 is Cabot/canal/gatehouse
    nRoutes <- 2
    nDams <- c(4, 4)

    # Define number of production units- dams plus one in this system
    nPU <- nDams + 1

    # Define maximum rkm for the system.
    maxrkm <- c(281, 281)

    # Define rkms for each of the dams for each migration route
    damRkms <- vector(mode = "list", length = nRoutes)
    damRkms[[1]] <- c(139, 198, 199, 229) # Route 1
    damRkms[[2]] <- c(139, 194, 199, 229) # Route 2

    return(list(
      nRoutes = nRoutes,
      nDams = nDams,
      nPU = nPU,
      maxrkm = maxrkm,
      damRkms = damRkms
    ))
  }


  if (river == "susquehanna") {
    # Susquehanna River
    # Four passage routes in Susquehanna River
    nRoutes <- 4

    # Need separate habitat values for each migration route
    # Group 1: Juniata River spawners
    # Group 2: West Branch spawners
    # Group 3: Chemung River spawners
    # Group 4: North Branch spawners
    nDams <- c(5, 7, 7, 9)

    # Define number of production units- dams plus one in this system
    nPU <- nDams + 1

    # Define maximum rkm for the system in each route
    maxrkm <- c(265, 474, 538, 715)

    # Define rkms for each of the dams for each migration group
    damRkms <- vector(mode = "list", length = nRoutes)
    damRkms[[1]] <- c(16, 39, 50, 90, 141)
    damRkms[[2]] <- c(16, 39, 50, 90, 204, 275, 326)
    damRkms[[3]] <- c(16, 39, 50, 90, 204, 472, 511)
    damRkms[[4]] <- c(16, 39, 50, 90, 204, 472, 545, 645, 692)

    hydro_out <- list(
      nRoutes = nRoutes,
      nDams = nDams,
      nPU = nPU,
      maxrkm = maxrkm,
      damRkms = damRkms
    )

    return(hydro_out)
  }


  if (river == "saco") {
    nRoutes <- 1
    nDams <- 6

    # Define number of production units- dams plus two in this system
    nPU <- nDams + 1

    # Define maximum rkm for the system.
    maxrkm <- 54

    # Define rkms for each of the dams for each migration group
    damRkms <- vector(mode = "list", length = nRoutes)
    damRkms[[1]] <- c(9, 11, 27, 32, 42, 44) # Dam rkms

    # Return the list
    return(list(
      nRoutes = nRoutes,
      nDams = nDams,
      nPU = nPU,
      maxrkm = maxrkm,
      damRkms = damRkms
    ))
  }


  if (river == "kennebec") {
    # Number of routes
    # Route 1 is mainstem
    # Route 2 is Sebasticook
    nRoutes <- 2
    nDams <- c(4, 2)

    # Define number of production units- dams plus one in this system
    nPU <- nDams + 1

    # Define maximum rkm for the system.
    maxrkm <- c(228, 151)

    # Define rkms for each of the dams for each migration route
    damRkms <- vector(mode = "list", length = nRoutes)
    damRkms[[1]] <- c(101, 103, 113, 134) # Route 1
    damRkms[[2]] <- c(109, 135) # Route 2

    return(list(
      nRoutes = nRoutes,
      nDams = nDams,
      nPU = nPU,
      maxrkm = maxrkm,
      damRkms = damRkms
    ))
  }

  
  if (river == "hudson") {
    # Number of routes
    # Route 1 is upper hudson
    # Route 2 is mohawk
    nRoutes <- 2
    nDams <- c(7, 20)

    # Define number of production units- dams plus one in this system
    nPU <- nDams + 1

    # Define maximum rkm for the system.
    maxrkm <- c(305, 411)

    # Define rkms for each of the dams for each migration route
    damRkms <- vector(mode = "list", length = nRoutes)
    damRkms[[1]] <- c(246, 255, 261, 265, 268, 293, 298) # Route 1
    damRkms[[2]] <- c(
      246, 247, 248, 249, 250, 251, 252, 270,
      287, 295, 305, 312, 319, 335, 340, 362,
      369, 374, 393, 410
    ) # Route 2

    return(list(
      nRoutes = nRoutes,
      nDams = nDams,
      nPU = nPU,
      maxrkm = maxrkm,
      damRkms = damRkms
    ))
  }
  
  
  if (river == "androscoggin") {
      # Number of routes
      # Route 1 is andro/little andro
      # Route 2 is sabattus
      nRoutes <- 2
      nDams <- c(10, 5)
  
      # Define number of production units- dams plus one in this system
      nPU <- nDams + 1
  
      # Define maximum rkm for the system.
      # Each of these has +1 RKM bc Brunswick is at rkm 0
      # Arbitrarily extended max RKM to prevent deserialize
      maxrkm <- c(300, 200)
  
      # Define rkms for each of the dams for each migration route
      # Each of these has +1 RKM bc Brunswick is at rkm 0
      # Each of the RKMs are adjusted for dist to Sabattus River
      damRkms <- vector(mode = "list", length = nRoutes)
      damRkms[[1]] <- c(1, 9, 14, 38, 39, 44, 53, 61, 71, 89) # L. Andro
      damRkms[[2]] <- c(1, 9, 14, 20, 33) # Sabattus
  
      return(list(
        nRoutes = nRoutes,
        nDams = nDams,
        nPU = nPU,
        maxrkm = maxrkm,
        damRkms = damRkms
      ))
    }  
}
