#' @title Assign freshwater fishing mortality

#' @description Internal function used to assign 
#' freshwater fishing mortality for each PU
#' in each migration route as an annual rate.
#' 
#' Not intended to be called directly, but visible
#' for the sake of model transparency.
#' 
#' @export
#' 
fwFishingMort <- function(inRiverF, river){

  if(river=='penobscot'){  
    inriv <- vector(mode = 'list', length = nRoutes)
    inriv[[1]] <- c(rep(inRiverF, 3), rep(0, 4))
    inriv[[2]] <- c(rep(inRiverF, 4), 0)
    inriv[[3]] <- c(rep(inRiverF, 4), rep(0, 4))
    inriv[[4]] <- c(rep(inRiverF, 5), 0)
   
    return(list(inriv=inriv))
  }
    
  if(river=='merrimack'){  
    inriv <- vector(mode = 'list', length = nRoutes)
    inriv[[1]] <- rep(inRiverF, 5)
    inriv[[2]] <- rep(inRiverF, 5)
    return(list(inriv=inriv))
  }  
  
  if(river=='connecticut'){  
    inriv <- vector(mode = 'list', length = nRoutes)
    inriv[[1]] <- rep(inRiverF, 5)
    inriv[[2]] <- rep(inRiverF, 5)
   
    return(list(inriv=inriv))
  }
  
  if(river=='susquehanna'){  
    inriv <- vector(mode = 'list', length = nRoutes)
    inriv[[1]] <- rep(inRiverF, 6)
    inriv[[2]] <- rep(inRiverF, 8)
    inriv[[3]] <- rep(inRiverF, 8)
    inriv[[4]] <- rep(inRiverF, 10)    
    
    return(inriv=inriv)
  }  
  
  if(river=='saco'){  
    inriv <- vector(mode = 'list', length = nRoutes)
    inriv[[1]] <- rep(inRiverF, 7)
   
    return(list(inriv=inriv))
  }  
  
  if(river=='kennebec'){  
    inriv <- vector(mode = 'list', length = nRoutes)
    inriv[[1]] <- rep(inRiverF, 5)
    inriv[[2]] <- rep(inRiverF, 3)
   
    return(list(inriv=inriv))
  }  
  
    if(river=='hudson'){  
    inriv <- vector(mode = 'list', length = nRoutes)
    inriv[[1]] <- rep(inRiverF, nPU[1])
    inriv[[2]] <- rep(inRiverF, nPU[2])
   
    return(inriv)
  }  
  
}