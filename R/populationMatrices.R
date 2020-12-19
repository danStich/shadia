#' @title Define in-river population matrices
#'
#' @description Internal function used to create combinations
#' of production units and fish ages in each of the four
#' migration routes. Generalized to apply to eggs by females
#' at each age, too. Later filled in \code{\link{createPUMatrix}}
#'
#' Not intended to be called directly, but visible
#' for the sake of model transparency.
#'
#' @export
#'
populationMatrices <- function() {

  # Get combos of puNames and age classes in all PUs
  # and assign them to an object
  for (i in 1:nRoutes) {
    assign(paste0("x_", i), expand.grid(.shadia$puNames[[i]], seq(1, maxAge, 1)))
  }

  # Assign dynamically created objects to list
  outlist <- mget(paste0("x_", 1:nRoutes))
  outlist <- lapply(outlist, setNames, c("pus", "fishAges"))

  # Return statement
  return(
    outlist
  )
}
