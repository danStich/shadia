#' @title Scale population to speed computations
#'
#' @description Internal function for rescaling components of the population
#' based on scalar variable in \code{\link{setScalar}}.
#'
#' Not intended to be called directly, but visible for
#' transparency.
#'
#' @return A list of vectors containing the 1) population size,
#' 2) spawning pool, and 3) recruitment pool, all scaled
#' according to rules in \code{\link{setScalar}}.
#'
#' @export
#'
scalePop <- function(pop, spawningPool, recruitmentPool, scalar) {
  pop <- unlist(pop) / scalar
  spawningPool <- spawningPool / scalar
  recruitmentPool <- recruitmentPool / scalar

  return(list(
    pop,
    spawningPool,
    recruitmentPool
  ))
}
