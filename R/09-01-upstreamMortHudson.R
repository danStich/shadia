#' @title Assign upstream mortality during upstream
#' passage of dams.

#' @description Internal function used to assign
#' cumulative upstream mortality incurred during passage
#' of locks.
#' 
#' Only implemented in \code{mohawkHudsonRiverModel()}
#' A single value is applied to all dams in the 
#' watershed for now.
#'
#' Not intended to be called directly, but visible
#' for the sake of model transparency.
#'
#' @export
#'
upstreamMort <- function(upstreamMortality, river, nRoutes) {
#   if (river == "penobscot") {
#     up_mort <- vector(mode = "list", length = nRoutes)
#     up_mort[[1]] <- c(rep(upstreamMortality, 3), rep(0, 4))
#     up_mort[[2]] <- c(rep(upstreamMortality, 4), 0)
#     up_mort[[3]] <- c(rep(upstreamMortality, 4), rep(0, 4))
#     up_mort[[4]] <- c(rep(upstreamMortality, 5), 0)
#   }
# 
#   if (river == "merrimack") {
#     up_mort <- vector(mode = "list", length = nRoutes)
#     up_mort[[1]] <- rep(upstreamMortality, 5)
#     up_mort[[2]] <- rep(upstreamMortality, 5)
#   }
# 
#   if (river == "connecticut") {
#     up_mort <- vector(mode = "list", length = nRoutes)
#     up_mort[[1]] <- rep(upstreamMortality, 5)
#     up_mort[[2]] <- rep(upstreamMortality, 5)
#   }
# 
#   if (river == "susquehanna") {
#     up_mort <- vector(mode = "list", length = nRoutes)
#     up_mort[[1]] <- rep(upstreamMortality, 6)
#     up_mort[[2]] <- rep(upstreamMortality, 8)
#     up_mort[[3]] <- rep(upstreamMortality, 8)
#     up_mort[[4]] <- rep(upstreamMortality, 10)
#   }
# 
#   if (river == "saco") {
#     up_mort <- vector(mode = "list", length = nRoutes)
#     up_mort[[1]] <- rep(upstreamMortality, 7)
#   }
# 
#   if (river == "kennebec") {
#     up_mort <- vector(mode = "list", length = nRoutes)
#     up_mort[[1]] <- rep(upstreamMortality, 5)
#     up_mort[[2]] <- rep(upstreamMortality, 3)
#   }

  if (river == "hudson") {
    up_mort <- vector(mode = "list", length = nRoutes)
    up_mort[[1]] <- 1 - cumprod(rep(1 - upstreamMortality, 8))
    up_mort[[2]] <- 1 - cumprod(rep(1 - upstreamMortality, 21))
    
    return(up_mort)    
  }

}
