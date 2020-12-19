#' @title Create starting population
#'
#' @description Internal function for creating a starting
#' population of fish based on a draw from a negative binomial
#' calibrated to adult spawning numbers. Number of spawners
#' is not exact match to `adults`.
#'
#' @return A list containing the initial population size,
#' number of spawners and number of non-spawning individuals
#' for the first year of simulation.
#'
#' @examples
#' test <- simStartingPop(1e4, 13, 0.45)
#' sum(test$spawningPool)
#' plot(test$spawningPool, type = "l")
#' @export
#'
simStartingPop <- function(adults, maxAge, oceanSurvival, spawnRecruit) {
  n_init <- MASS::rnegbin(adults * 10, 1 / oceanSurvival[1], 1)
  n_init[n_init == 0] <- 1
  n_init <- n_init[n_init < maxAge]

  age_tab <- table(n_init)

  pop <- data.frame(age_tab)$Freq
  spawningPool <- pop * spawnRecruit[1:length(pop)]
  recruitmentPool <- pop - spawningPool

  return(list(
    pop = pop,
    spawningPool = spawningPool,
    recruitmentPool = recruitmentPool
  ))
}
