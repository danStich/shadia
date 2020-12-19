#' @title Set population scalar
#'
#' @description Internal function for setting the
#' value of the population scalar that is used to
#' dynamically adjust population size to avoid
#' computational overflow.
#'
#' Not intended to be called directly, but visible
#' for transparency.
#'
#' @export
#'
# Function to set population scalar
setScalar <- function(spawningPool) {
  # if (sum(spawningPool) < 1e3) {
  #   scalar = 1
  # }
  if (sum(spawningPool) < 1e4) {
    scalar <- 1
  }
  if (sum(spawningPool) >= 1e4) {
    scalar <- 10
  }
  if (sum(spawningPool) >= 1e5) {
    scalar <- 1000
  }
  if (sum(spawningPool) >= 1e6) {
    scalar <- 10000
  }

  return(scalar)
}
