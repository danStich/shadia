#' @title Project population to the next year
#'
#' @description Internal function to perform
#' cohort-based projections of the population
#' into the next year of the simulation based
#' on marine survival, commercial fishery harvest,
#' and fishery bycatch (all annual rates).
#'
#' Not intended to be called directly, but
#' visible for the sake of transparency.
#'
#' @export
#'
nextGeneration <- function() {

  # Define a vector containing total number of spawners returning
  # to the ocean. This number does not include Age 1 individuals. Then
  # apply the marine survival rate and tally the total number of repeat
  # spawners in the population during the next time-step
  # Sum male and female out migrants
  for (i in 1:length(outMigrants)) {
    assign(
      paste("repeatSpawners_", i, sep = ""),
      outMigrants[i] *
        marineS[i] *
        (1 - commercialF[i]) *
        (1 - bycatchF[i])
    )
  }

  # Now graduate each cohort to the next age
  repeats <- append(c(0, 0), unlist(mget(ls(pat = "^repeatSpawners_")))[3:(maxAge)])
  # Assign names for each age class
  names(repeats) <- names(femalesOut)

  # Calculate numbers in the recruitment pool for next year
  # Apply marine survival rate to fish waiting in the ocean
  nextRecruits <- c(recruitsOut, recruitmentPool[1:(maxAge - 1)]) *
    marineS * (1 - commercialF) * (1 - bycatchF)
  # Assign names for each age class
  names(nextRecruits) <- names(recruitmentPool)

  # Calculate proportion of repeat spawners in each age class
  pRepeat <- repeats / (nextRecruits + repeats)

  # Combine repeat spawners with new recruits
  spawningPool <- nextRecruits * spawnRecruit + repeats
  recruitmentPool <- nextRecruits - nextRecruits * spawnRecruit

  # Record new population size for the start of the inner loop
  pop <- sum(spawningPool + recruitmentPool)

  pop <- pop * scalar
  spawningPool <- spawningPool * scalar
  recruitmentPool <- recruitmentPool * scalar

  return(list(
    repeats = repeats,
    nextRecruits = nextRecruits,
    pRepeat = pRepeat,
    spawningPool = spawningPool,
    recruitmentPool = recruitmentPool,
    pop = pop
  ))
}
