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
  
  n_init <- MASS::rnegbin(adults, maxAge, oceanSurvival[1])#rpois(adults, maxAge/2) 
  n_init[n_init == 0] <- 1
  n_init <- n_init[n_init <= maxAge]

  age_tab <- table(n_init)*5  

  m_age <- vector(mode = "numeric", length = floor(maxAge/2-1))
  for(i in 1:floor(maxAge/2-1)){
    m_age[i] <- min(cumprod(oceanSurvival[i:floor(maxAge/2-1)]))
  }
  age_tab[1:floor(maxAge/2 - 1)] <- age_tab[floor(maxAge/2-1)]/m_age

  ages <- data.frame(age_tab)$Freq
  spawningPool <- ages * spawnRecruit
  pop <- ages + (spawningPool*(1-spawnRecruit))
  recruitmentPool <- pop - spawningPool
  
  return(list(
    pop = pop,
    spawningPool = spawningPool,
    recruitmentPool = recruitmentPool
  ))
}
