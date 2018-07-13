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
fwFishingMort <- function(){

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
    inriv[[1]] <- c(rep(inRiverF, 5))
    return(list(inriv=inriv))
  }  
  
}