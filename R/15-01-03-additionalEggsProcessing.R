#' @title Processing for egg calculations
#'
#' @description Internal function used to sum number of eggs in each
#' production unit from each of the four migration
#' routes, and apply carrying capacity based on k_pus. In addition
#' to cohort processing routines (hence the name). Not
#' intended to be called directly. Visible for the sake of
#' model transparency.
#'
#' @return List of number of eggs spawned in each production unit
#' by simulated females, or the carrying capacity: whichever
#' is smaller.
#'
#' @export
#'
additionalEggsProcessing <- function(fec) {
  if (river == "penobscot") {
    # Group 1: Mainstem to Piscataquis
    # Group 2: Mainstem to mainstem
    # Group 3: Stillwater to Piscataquis
    # Group 4: Stillwater to mainstem
    
    # Calculate total number of eggs in each PU
    fecp <- vector(mode = "list", length = length(fec))
    for (i in 1:length(fec)) {
      fecp[[i]] <- mapply(sum, na.rm = TRUE, fec[[i]])
    }
    
    fec2 <- fecp
    
    # Now sum all eggs from each of the shared PUs for all routes.
    # Fish using Stillwater Branch automatically passed to Milford Headpond
    fec2[[1]][1] <- (fecp[[1]][1] + fecp[[2]][1] + fecp[[3]][1] +fecp[[4]][1])
    fec2[[1]][2] <- (fecp[[1]][2] + fecp[[2]][2])
    fec2[[1]][3] <- (fecp[[1]][3] + fecp[[2]][3] + fecp[[3]][2] + fecp[[3]][3] + fecp[[4]][2] + fecp[[4]][3] + fecp[[3]][4] + fecp[[4]][4])
    fec2[[1]][4] <- (fecp[[1]][4] + fecp[[3]][5])
    fec2[[1]][5] <- (fecp[[1]][5] + fecp[[3]][6])
    fec2[[1]][6] <- (fecp[[1]][6] + fecp[[3]][7])
    fec2[[1]][7] <- (fecp[[1]][7] + fecp[[3]][8])

    fec2[[2]][1] <- 0
    fec2[[2]][2] <- 0
    fec2[[2]][3] <- 0
    fec2[[2]][4] <- fecp[[2]][4] + fecp[[4]][5]
    fec2[[2]][5] <- fecp[[2]][5] + fecp[[4]][6]
    
    fec2[[3]][1] <- 0
    fec2[[3]][2] <- 0
    fec2[[3]][3] <- 0
    fec2[[3]][4] <- 0
    fec2[[3]][5] <- 0
    fec2[[3]][6] <- 0
    fec2[[3]][7] <- 0
    fec2[[3]][8] <- 0
    
    fec2[[4]][1] <- 0
    fec2[[4]][2] <- 0
    fec2[[4]][3] <- 0
    fec2[[4]][4] <- 0
    fec2[[4]][5] <- 0
    fec2[[4]][6] <- 0

    # Apply carrying capacity limitation to each production unit based
    # on habitat availability
    if (k_method == "discrete") {
      fec_Max <- vector(mode = "list", length = length(fec2))
      for (i in 1:length(fec2)) {
        for (j in 1:length(fec2[[i]])) {
          if (fec2[[i]][j] > k_pus[[i]][j]) {
            fec_Max[[i]][j] <- k_pus[[i]][j]
          } else {
            fec_Max[[i]][j] <- fec2[[i]][j]
          }
        }
      }
    }

    if (k_method == "cumulative") {

      fec_Max <- fec2

      sum_fec <- lapply(fec2, cumsum)

      # NEW CHUNK
      for (i in 1:length(fec2)) {

        for (j in 1:length(fec2[[i]])) {

          if (sum_fec[[i]][j] > k_pus[[i]][j]) {
            fec_Max[[i]][j] <- fec2[[i]][j]*((k_pus[[i]][j])/(sum_fec[[i]][j]))
          } else {
            fec_Max[[i]][j] <- fec2[[i]][j]
          }

        }
        
      }
      
    } 
    return(fec_Max)
    # toc()
  }


  if (river == "merrimack") {
    # Calculate total number of eggs in each PU
    fecp <- vector(mode = "list", length = length(fec))
    for (i in 1:length(fec)) {
      fecp[[i]] <- mapply(sum, fec[[i]])
    }
    
    fec2 <- fecp
    
    fec2[[1]][1] <- (fecp[[1]][1] + fec2[[2]][1])
    fec2[[1]][2] <- (fecp[[1]][2] + fec2[[2]][2])
    fec2[[1]][3] <- (fecp[[1]][3] + fec2[[2]][3])
    fec2[[1]][4] <- (fecp[[1]][4] + fec2[[2]][4])
    fec2[[1]][5] <- (fecp[[1]][5] + fec2[[2]][5])

    fec2[[2]][1] <- 0
    fec2[[2]][2] <- 0
    fec2[[2]][3] <- 0
    fec2[[2]][4] <- 0
    fec2[[2]][5] <- 0
    
    # Apply carrying capacity limitation to each production unit based
    # on habitat availability
    # Apply carrying capacity limitation to each production unit based
    # on habitat availability
    if (k_method == "discrete") {
      fec_Max <- vector(mode = "list", length = length(fec2))
      for (i in 1:length(fec2)) {
        for (j in 1:length(fec2[[i]])) {
          if (fec2[[i]][j] > k_pus[[i]][j]) {
            fec_Max[[i]][j] <- k_pus[[i]][j]
          } else {
            fec_Max[[i]][j] <- fec2[[i]][j]
          }
        }
      }
    }

    if (k_method == "cumulative") {

      fec_Max <- fec2

      sum_fec <- lapply(fec2, cumsum)

      # NEW CHUNK
      for (i in 1:length(fec2)) {

        for (j in 1:length(fec2[[i]])) {

          if (sum_fec[[i]][j] > k_pus[[i]][j]) {
            fec_Max[[i]][j] <- fec2[[i]][j]*((k_pus[[i]][j])/(sum_fec[[i]][j]))
          } else {
            fec_Max[[i]][j] <- fec2[[i]][j]
          }

        }
        
      }
      
    } 
    return(fec_Max)
    # toc()
  }


  if (river == "connecticut") {
    # Calculate total number of eggs in each PU
    fecp <- vector(mode = "list", length = length(fec))
    for (i in 1:length(fec)) {
      fecp[[i]] <- mapply(sum, fec[[i]])
    }
    
    fec2 <- fecp
    
    # Now sum all eggs from each of the shared PUs for all routes.
    # Spillway route
    fec2[[1]][1] <- (fecp[[1]][1] + fecp[[2]][1]) * pSpillway
    fec2[[1]][2] <- (fecp[[1]][2] + fecp[[2]][2]) * pSpillway
    fec2[[1]][3] <- 0 # No spawning in the fishway
    fec2[[1]][4] <- (fecp[[1]][4] + fecp[[2]][4]) * pSpillway
    fec2[[1]][5] <- (fecp[[1]][5] + fecp[[2]][5]) * pSpillway
    
    # Canal route
    fec2[[2]][1] <- (fecp[[1]][1] + fecp[[2]][1]) * (1 - pSpillway)
    fec2[[2]][2] <- (fecp[[1]][2] + fecp[[2]][2]) * (1 - pSpillway)
    fec2[[2]][3] <- 0 # No juv survival allowed in the canal
    fec2[[2]][4] <- (fecp[[1]][4] + fecp[[2]][4]) * (1 - pSpillway)
    fec2[[2]][5] <- (fecp[[1]][5] + fecp[[2]][5]) * (1 - pSpillway)

    # Apply carrying capacity limitation to each production unit based
    # on habitat availability
    if (k_method == "discrete") {
      fec_Max <- vector(mode = "list", length = length(fec))
      for (i in 1:length(fec2)) {
        for (j in 1:length(fec2[[i]])) {
          if (fec2[[i]][j] > k_pus[[i]][j]) {
            fec_Max[[i]][j] <- k_pus[[i]][j]
          } else {
            fec_Max[[i]][j] <- fec2[[i]][j]
          }
        }
      }
    }

    if (k_method == "cumulative") {

      fec_Max <- fec

      sum_fec <- lapply(fec2, cumsum)

      # NEW CHUNK
      for (i in 1:length(fec2)) {

        for (j in 1:length(fec2[[i]])) {

          if (sum_fec[[i]][j] > k_pus[[i]][j]) {
            fec_Max[[i]][j] <- fec2[[i]][j]*((k_pus[[i]][j])/(sum_fec[[i]][j]))
          } else {
            fec_Max[[i]][j] <- fec2[[i]][j]
          }

        }
        
      }
      
    } 
    return(fec_Max)
    # toc()
  }

  
  if (river == "susquehanna") {
    # Calculate total number of eggs in each PU
    fecp <- vector(mode = "list", length = length(fec))
    for (i in 1:length(fec)) {
      fecp[[i]] <- mapply(sum, fec[[i]])
    }
    
    fec2 <- fecp
    
    # Now sum all eggs from each of the shared PUs for all routes.  Put all
    # of the eggs from shared PUs into fec2[[min]] that shares the PU,
    # set all others to zero
    fec2[[1]][1] <- 0
    fec2[[1]][2] <- 0
    fec2[[1]][3] <- 0
    fec2[[1]][4] <- (fecp[[1]][4] + fecp[[2]][4] + fecp[[3]][4] + fecp[[4]][4])*p_JuniataUp
    fec2[[1]][5] <- (fecp[[1]][5] + fecp[[2]][5] + fecp[[3]][5] + fecp[[4]][5])*p_JuniataUp

    fec2[[2]][1] <- 0
    fec2[[2]][2] <- 0
    fec2[[2]][3] <- 0
    fec2[[2]][4] <- (fecp[[1]][4] + fecp[[2]][4] + fecp[[3]][4] + fecp[[4]][4])*p_WestBranchUp/(p_WestBranchUp + p_ChemungUp + p_NorthBranchUp)
    fec2[[2]][5] <- (fecp[[1]][5] + fecp[[2]][5] + fecp[[3]][5] + fecp[[4]][5])*p_WestBranchUp/(p_WestBranchUp + p_ChemungUp + p_NorthBranchUp)

    fec2[[3]][1] <- 0
    fec2[[3]][2] <- 0
    fec2[[3]][3] <- 0
    fec2[[3]][4] <- (fecp[[1]][4] + fecp[[2]][4] + fecp[[3]][4] + fecp[[4]][4])*p_ChemungUp/(p_WestBranchUp + p_ChemungUp + p_NorthBranchUp)
    fec2[[3]][5] <- (fecp[[1]][5] + fecp[[2]][5] + fecp[[3]][5] + fecp[[4]][5])*p_ChemungUp/(p_WestBranchUp + p_ChemungUp + p_NorthBranchUp)    
    
    fec2[[3]][6] <- (fecp[[3]][6] + fecp[[4]][6])*p_ChemungUp/(p_ChemungUp + p_NorthBranchUp)

    fec2[[4]][1] <- 0
    fec2[[4]][2] <- 0
    fec2[[4]][3] <- 0
    fec2[[4]][4] <- (fecp[[1]][4] + fecp[[2]][4] + fecp[[3]][4] + fecp[[4]][4])*p_NorthBranchUp/(p_WestBranchUp + p_ChemungUp + p_NorthBranchUp)
    fec2[[4]][5] <- (fecp[[1]][5] + fecp[[2]][5] + fecp[[3]][5] + fecp[[4]][5])*p_NorthBranchUp/(p_WestBranchUp + p_ChemungUp + p_NorthBranchUp)      
    
    fec2[[4]][6] <- (fecp[[3]][6] + fecp[[4]][6])*p_NorthBranchUp/(p_ChemungUp + p_NorthBranchUp)

    # Apply carrying capacity limitation to each production unit based
    # on habitat availability
    if (k_method == "discrete") {
      fec_Max <- vector(mode = "list", length = length(fec))
      for (i in 1:length(fec2)) {
        for (j in 1:length(fec2[[i]])) {
          if (fec2[[i]][j] > k_pus[[i]][j]) {
            fec_Max[[i]][j] <- k_pus[[i]][j]
          } else {
            fec_Max[[i]][j] <- fec2[[i]][j]
          }
        }
      }
    }

    if (k_method == "cumulative") {

      fec_Max <- fec

      sum_fec <- lapply(fec2, cumsum)
      
      # NEW CHUNK
      for (i in 1:length(fec2)) {

        for (j in 1:length(fec2[[i]])) {

          if (sum_fec[[i]][j] > k_pus[[i]][j]) {
            fec_Max[[i]][j] <- fec2[[i]][j]*((k_pus[[i]][j])/(sum_fec[[i]][j]))
          } else {
            fec_Max[[i]][j] <- fec2[[i]][j]
          }

        }
        
      }
      
    }  
    return(fec_Max)
    # toc()
  }


  if (river == "saco") {
    # Calculate total number of eggs in each PU
    fec2 <- vector(mode = "list", length = length(fec))
    for (i in 1:length(fec)) {
      fec2[[i]] <- mapply(sum, na.rm = TRUE, fec[[i]])
    }

    # Apply carrying capacity limitation to each production unit based
    # on habitat availability
    if (k_method == "discrete") {
      fec_Max <- vector(mode = "list", length = length(fec))
      for (i in 1:length(fec2)) {
        for (j in 1:length(fec2[[i]])) {
          if (fec2[[i]][j] > k_pus[[i]][j]) {
            fec_Max[[i]][j] <- k_pus[[i]][j]
          } else {
            fec_Max[[i]][j] <- fec2[[i]][j]
          }
        }
      }
    }

    if (k_method == "cumulative") {

      fec_Max <- fec

      sum_fec <- lapply(fec2, cumsum)

      # NEW CHUNK
      for (i in 1:length(fec2)) {

        for (j in 1:length(fec2[[i]])) {

          if (sum_fec[[i]][j] > k_pus[[i]][j]) {
            fec_Max[[i]][j] <- fec2[[i]][j]*((k_pus[[i]][j])/(sum_fec[[i]][j]))
          } else {
            fec_Max[[i]][j] <- fec2[[i]][j]
          }

        }
        
      }
      
    } 
    return(fec_Max)
    # toc()
  }


  if (river == "kennebec") {
    # Calculate total number of eggs in each PU
    # Calculate total number of eggs in each PU
    fecp <- vector(mode = "list", length = length(fec))
    for (i in 1:length(fec)) {
      fecp[[i]] <- mapply(sum, fec[[i]])
    }
    
    fec2 <- fecp

    fec2[[1]][1] <- (fecp[[1]][1] + fecp[[2]][1]) * (1 - p_sebasticook)
    fec2[[2]][1] <- (fecp[[1]][1] + fecp[[2]][1]) * (p_sebasticook)

    # Apply carrying capacity limitation to each production unit based
    # on habitat availability
    if (k_method == "discrete") {
      fec_Max <- vector(mode = "list", length = length(fec))
      for (i in 1:length(fec2)) {
        for (j in 1:length(fec2[[i]])) {
          if (fec2[[i]][j] > k_pus[[i]][j]) {
            fec_Max[[i]][j] <- k_pus[[i]][j]
          } else {
            fec_Max[[i]][j] <- fec2[[i]][j]
          }
        }
      }
    }

    if (k_method == "cumulative") {

      fec_Max <- fec

      sum_fec <- lapply(fec2, cumsum)

      # NEW CHUNK
      for (i in 1:length(fec2)) {

        for (j in 1:length(fec2[[i]])) {

          if (sum_fec[[i]][j] > k_pus[[i]][j]) {
            fec_Max[[i]][j] <- fec2[[i]][j]*((k_pus[[i]][j])/(sum_fec[[i]][j]))
          } else {
            fec_Max[[i]][j] <- fec2[[i]][j]
          }

        }
        
      }
      
    } 
    return(fec_Max)
    # toc()
  }


  if (river == "hudson") {
    # Calculate total number of eggs in each PU
    fecp <- vector(mode = "list", length = length(fec))
    for (i in 1:length(fec)) {
      fecp[[i]] <- mapply(sum, na.rm = TRUE, fec[[i]])
    }
    
    fec2 <- fecp
    fec2[[1]][1] <- fec2[[1]][1] + fec2[[2]][1]
    fec2[[2]][1] <- 0

    # Apply carrying capacity limitation to each production unit based
    # on habitat availability
    if (k_method == "discrete") {
      fec_Max <- vector(mode = "list", length = length(fec2))
      for (i in 1:length(fec2)) {
        for (j in 1:length(fec2[[i]])) {
          if (fec2[[i]][j] > k_pus[[i]][j]) {
            fec_Max[[i]][j] <- k_pus[[i]][j]
          } else {
            fec_Max[[i]][j] <- fec2[[i]][j]
          }
        }
      }
    }

    if (k_method == "cumulative") {

      fec_Max <- fec2

      sum_fec <- lapply(fec2, cumsum)

      # NEW CHUNK
      for (i in 1:length(fec2)) {

        for (j in 1:length(fec2[[i]])) {

          if (sum_fec[[i]][j] > k_pus[[i]][j]) {
            fec_Max[[i]][j] <- fec2[[i]][j]*((k_pus[[i]][j])/(sum_fec[[i]][j]))
          } else {
            fec_Max[[i]][j] <- fec2[[i]][j]
          }

        }
        
      }
      
    } 
    return(fec_Max)
    # toc()
  }

  
  if (river == "androscoggin") {
    # Calculate total number of eggs in each PU
    fecp <- vector(mode = "list", length = length(fec))
    for (i in 1:length(fec)) {
      fecp[[i]] <- mapply(sum, na.rm = TRUE, fec[[i]])
    }
    
    fec2 <- fecp
    
    # No reproduction downstream of Brunswick
    fec2[[1]][1] <- 0
    fec2[[2]][1] <- 0    
    
    # Shared PUs: sum across route and store in main
    fec2[[1]][2] <- (fecp[[1]][2] + fecp[[2]][2]) * (1 - p_sabattus)
    fec2[[1]][3] <- (fecp[[1]][3] + fecp[[2]][3]) * (1 - p_sabattus)
    fec2[[1]][4] <- (fecp[[1]][4] + fecp[[2]][4]) * (1 - p_sabattus)
    
    # Shared PUs for Sabattus
    fec2[[2]][2] <- (fecp[[1]][2] + fecp[[2]][2]) * p_sabattus
    fec2[[2]][3] <- (fecp[[1]][3] + fecp[[2]][3]) * p_sabattus
    fec2[[2]][4] <- (fecp[[1]][4] + fecp[[2]][4]) * p_sabattus 

    # Apply carrying capacity limitation to each production unit based
    # on habitat availability
    if (k_method == "discrete") {
      fec_Max <- vector(mode = "list", length = length(fec))
      for (i in 1:length(fec2)) {
        for (j in 1:length(fec2[[i]])) {
          if (fec2[[i]][j] > k_pus[[i]][j]) {
            fec_Max[[i]][j] <- k_pus[[i]][j]
          } else {
            fec_Max[[i]][j] <- fec2[[i]][j]
          }
        }
      }
    }

    if (k_method == "cumulative") {

      fec_Max <- fec

      sum_fec <- lapply(fec2, cumsum)

      # NEW CHUNK
      for (i in 1:length(fec2)) {

        for (j in 1:length(fec2[[i]])) {

          if (sum_fec[[i]][j] > k_pus[[i]][j]) {
            fec_Max[[i]][j] <- fec2[[i]][j]*((k_pus[[i]][j])/(sum_fec[[i]][j]))
          } else {
            fec_Max[[i]][j] <- fec2[[i]][j]
          }

        }
        
      }
      
    }  
    return(fec_Max)
    # toc()
  }
    
}
