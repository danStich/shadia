#' @title Set population scalar for small rivers
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
  if (sum(spawningPool) < 1e3) {
    scalar = 1
  }
  if (sum(spawningPool) >= 1e3) {
    scalar <- 10
  }
  if (sum(spawningPool) >= 1e4) {
    scalar <- 100
  }
  if (sum(spawningPool) >= 1e5) {
    scalar <- 1000
  }
  if (sum(spawningPool) >= 1e6) {
    scalar <- 10000
  }
  if (sum(spawningPool) >= 2e6) {
    scalar <- 20000
  }
  if (sum(spawningPool) >= 3e6) {
    scalar <- 30000
  }
  if (sum(spawningPool) >= 4e6) {
    scalar <- 40000
  }
  if (sum(spawningPool) >= 5e6) {
    scalar <- 50000
  }
  if (sum(spawningPool) >= 6e6) {
    scalar <- 60000
  }
  if (sum(spawningPool) >= 7e6) {
    scalar <- 70000
  }
  if (sum(spawningPool) >= 8e6) {
    scalar <- 80000
  }
  if (sum(spawningPool) >= 9e6) {
    scalar <- 90000
  }
  if (sum(spawningPool) >= 1e7) {
    scalar <- 100000
  }
  if (sum(spawningPool) >= 2e7) {
    scalar <- 200000
  }

  return(scalar)
}
