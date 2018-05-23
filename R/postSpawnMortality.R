#' @title Define in-river population matrices
#' 
#' @description Internal function used to apply 
#' post-spawning mortality to male and female 
#' spawners, and juvenile mortality rates for 
#' new recruits
#' 
#' Not intended to be called directly, but visible
#' for the sake of model transparency.
#' 
#' @return A list of lists, each holding the numbers of 
#' males, females, or eggs in each age class in each 
#' production unit of each of four migration routes 
#' following the application of post spawning mortality.
#' 
#' @export 
#' 
postSpawnMortality <- function(){
  
  # Males
  males[[1]] <- Map("*", males[[1]], post_spawn_survival_males)
  males[[2]] <- Map("*", males[[2]], post_spawn_survival_males)
  males[[3]] <- Map("*", males[[3]], post_spawn_survival_males)
  males[[4]] <- Map("*", males[[4]], post_spawn_survival_males)
  
  # Females
  females[[1]] <- Map("*", females[[1]], post_spawn_survival_females)
  females[[2]] <- Map("*", females[[2]], post_spawn_survival_females)
  females[[3]] <- Map("*", females[[3]], post_spawn_survival_females)
  females[[4]] <- Map("*", females[[4]], post_spawn_survival_females)
  
  # Apply juvenile mortality up to outmigration. This will be a list object with
  # Piscataquis recruits in the first element and Mainstem recruits in second.
  recruits = vector(mode = 'list', length = length(fec_Max))
  recruits[[1]] <- Map("*", fec_Max[[1]], juvenile_survival)
  recruits[[2]] <- Map("*", fec_Max[[2]], juvenile_survival)
  recruits[[3]] <- Map("*", fec_Max[[3]], juvenile_survival)
  recruits[[4]] <- Map("*", fec_Max[[4]], juvenile_survival)  
  
  return(list(
    males=males,
    females=females,
    recruits=recruits
  ))
  
}