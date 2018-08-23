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
additionalEggsProcessing <- function(fec){
  
  if(river=='penobscot'){
    # Calculate total number of eggs in each PU
    fec2 <- vector(mode = 'list', length = length(fec))
    for (i in 1:length(fec)) {
      fec2[[i]] <- mapply(sum, fec[[i]])
    }
    # Now sum all eggs from each of the shared PUs for all routes.  Put all
    # of the eggs from shared PUs into fec2[[min]] that shares the PU,
    # set all others to zero
    fec2[[1]][1] <- fec2[[1]][1] + fec2[[2]][1] + fec2[[3]][1]  +
      fec2[[4]][1]
    fec2[[1]][2] <- fec2[[1]][2] + fec2[[2]][2]
    fec2[[1]][3] <- fec2[[1]][3] + fec2[[2]][3] + fec2[[3]][4] + fec2[[4]][4]
    fec2[[1]][c(4:7)] <- fec2[[1]][c(4:7)] + fec2[[3]][c(5:8)]
    fec2[[2]][c(4, 5)] <- fec2[[2]][c(4, 5)] + fec2[[4]][c(5, 6)]
    fec2[[3]][c(3)] <- fec2[[3]][3] + fec2[[4]][3]
    fec2[[2]][c(1:3)] <- 0
    fec2[[3]][c(1:2, 4:8)] <- 0
    fec2[[4]][1:6] <- 0
    
    # Apply carrying capacity limitation to each production unit based
    # on habitat availability
    fec_Max <- vector(mode = 'list', length = length(fec))
    for (i in 1:length(fec2)) {
      for (j in 1:length(fec2[[i]])) {
        if (fec2[[i]][j] > k_pus[[i]][j]) {
          fec_Max[[i]][j] = k_pus[[i]][j]
        } else {
          fec_Max[[i]][j] = fec2[[i]][j]
        }
      }
    }
    return(fec_Max)
    #toc()
  }  
  
  if(river=='merrimack'){
    # Calculate total number of eggs in each PU
    fec2 <- vector(mode = 'list', length = length(fec))
    for (i in 1:length(fec)) {
      fec2[[i]] <- mapply(sum, na.rm=TRUE, fec[[i]])
    }

    fec2[[1]][1] <- fec2[[1]][1] + fec2[[2]][1]
    fec2[[1]][2] <- fec2[[1]][2] + fec2[[2]][2]
    fec2[[1]][3] <- fec2[[1]][3] + fec2[[2]][3]
    fec2[[1]][4] <- fec2[[1]][4] + fec2[[2]][4]
    fec2[[2]][1:4] <- 0    

    # Apply carrying capacity limitation to each production unit based
    # on habitat availability
    fec_Max <- vector(mode = 'list', length = length(fec))
    for (i in 1:length(fec2)) {
      for (j in 1:length(fec2[[i]])) {
        if (fec2[[i]][j] > k_pus[[i]][j]) {
          fec_Max[[i]][j] = k_pus[[i]][j]
        } else {
          fec_Max[[i]][j] = fec2[[i]][j]
        }
      }
    }
    return(fec_Max)
    #toc()
  }  
  
  if(river=='connecticut'){
    # Calculate total number of eggs in each PU
    fec2 <- vector(mode = 'list', length = length(fec))
    for (i in 1:length(fec)) {
      fec2[[i]] <- mapply(sum, fec[[i]])
    }

    # Now sum all eggs from each of the shared PUs for all routes.  Put all
    # of the eggs from shared PUs into fec2[[min]] that shares the PU,
    # set all others to zero
    fec2[[1]][1] <- fec2[[1]][1] + fec2[[2]][1]
    fec2[[1]][2] <- fec2[[1]][2] + fec2[[2]][2]
    fec2[[1]][3] <- 0 # No juv survival allowed in the canal!!
    fec2[[1]][4] <- fec2[[1]][4] + fec2[[2]][4]
    fec2[[1]][5] <- fec2[[1]][5] + fec2[[2]][5]
    fec2[[2]][1:5] <- 0
    
    # Apply carrying capacity limitation to each production unit based
    # on habitat availability
    fec_Max <- vector(mode = 'list', length = length(fec))
    for (i in 1:length(fec2)) {
      for (j in 1:length(fec2[[i]])) {
        if (fec2[[i]][j] > k_pus[[i]][j]) {
          fec_Max[[i]][j] = k_pus[[i]][j]
        } else {
          fec_Max[[i]][j] = fec2[[i]][j]
        }
      }
    }
    return(fec_Max)
    #toc()
  }  

  if(river=='susquehanna'){
    # Calculate total number of eggs in each PU
    fec2 <- vector(mode = 'list', length = length(fec))
    for (i in 1:length(fec)) {
      fec2[[i]] <- mapply(sum, fec[[i]])
    }
    # Now sum all eggs from each of the shared PUs for all routes.  Put all
    # of the eggs from shared PUs into fec2[[min]] that shares the PU,
    # set all others to zero
    fec2[[1]][1] <- fec2[[1]][1]+fec2[[2]][1]+fec2[[3]][1]+fec2[[4]][1]
    fec2[[1]][2] <- fec2[[1]][2]+fec2[[2]][2]+fec2[[3]][2]+fec2[[4]][2]
    fec2[[1]][3] <- fec2[[1]][3]+fec2[[2]][3]+fec2[[3]][3]+fec2[[4]][3]
    fec2[[1]][4] <- fec2[[1]][4]+fec2[[2]][4]+fec2[[3]][4]+fec2[[4]][4]
    fec2[[1]][5] <- fec2[[1]][5]+fec2[[2]][5]+fec2[[3]][5]+fec2[[4]][5]
    
    fec2[[2]][1:5] <- 0
    
    fec2[[3]][1:5] <- 0
    fec2[[3]][6] <- fec2[[3]][6] + fec2[[4]][6]
    
    fec2[[4]][1:6] <- 0
    
    # Apply carrying capacity limitation to each production unit based
    # on habitat availability
    fec_Max <- vector(mode = 'list', length = length(fec))
    for (i in 1:length(fec2)) {
      for (j in 1:length(fec2[[i]])) {
        if (fec2[[i]][j] > k_pus[[i]][j]) {
          fec_Max[[i]][j] = k_pus[[i]][j]
        } else {
          fec_Max[[i]][j] = fec2[[i]][j]
        }
      }
    }
    return(fec_Max)
    #toc()
  }    
  
}

