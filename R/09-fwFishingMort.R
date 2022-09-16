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
fwFishingMort <- function(inRiverF, river, nRoutes) {
  if (river == "penobscot") {
    inriv <- vector(mode = "list", length = nRoutes)
    inriv[[1]] <- rep(inRiverF, 7)
    inriv[[2]] <- rep(inRiverF, 5)
    inriv[[3]] <- rep(inRiverF, 8)
    inriv[[4]] <- rep(inRiverF, 6)
  }

  if (river == "merrimack") {
    inriv <- vector(mode = "list", length = nRoutes)
    inriv[[1]] <- rep(inRiverF, 5)
    inriv[[2]] <- rep(inRiverF, 5)
  }

  if (river == "connecticut") {
    inriv <- vector(mode = "list", length = nRoutes)
    inriv[[1]] <- rep(inRiverF, 5)
    inriv[[2]] <- rep(inRiverF, 5)
  }

  if (river == "susquehanna") {
    inriv <- vector(mode = "list", length = nRoutes)
    inriv[[1]] <- rep(inRiverF, 6)
    inriv[[2]] <- rep(inRiverF, 8)
    inriv[[3]] <- rep(inRiverF, 8)
    inriv[[4]] <- rep(inRiverF, 10)
  }

  if (river == "saco") {
    inriv <- vector(mode = "list", length = nRoutes)
    inriv[[1]] <- rep(inRiverF, 7)
  }

  if (river == "kennebec") {
    inriv <- vector(mode = "list", length = nRoutes)
    inriv[[1]] <- rep(inRiverF, 5)
    inriv[[2]] <- rep(inRiverF, 3)
  }

  if (river == "hudson") {
    inriv <- vector(mode = "list", length = nRoutes)
    inriv[[1]] <- rep(inRiverF, 8)
    inriv[[2]] <- rep(inRiverF, 21)
  }

  if (river == "androscoggin") {
    inriv <- vector(mode = "list", length = nRoutes)
    inriv[[1]] <- rep(inRiverF, 11)
    inriv[[2]] <- rep(inRiverF, 6)
  }  
  
  return(inriv)
}
