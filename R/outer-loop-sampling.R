#' @title Stochastic sampling of simulation-specific variables
#' 
#' @description Internal function used to perform all
#' iteration-based stochastic sampling, including
#' management decisions (passage efficiencies,
#' timing, etc.).
#' 
#' Not intended to be called directly, but visible
#' for the sake of model transparency.
#' 
#' @export 
#' 
outerLoopSampling <- function(){

if(river=='penobscot'){
  # Assign the starting population based on a seed of
  # age-1 fish and application of an ocean survival curve
  # The population size is scaled to make the models
  # run faster. Output is re-scaled
  Age1 <- rpois(1, 2e5)
  
  # Sustenance harvest by PIN for penobscotRiverModel
  pinHarvest <- pinHarvest                      
  
  # Draw timing of Weldon passage implementation
  weldon <- 0#c(0, 10, 20)
  #     if(up[1] != 0.75){scenario = sample(weldon[2:3], 1, replace = TRUE)
  #       } else {
  scenario <- sample(weldon, 1, replace = TRUE)
  # }
  
  # Draw probability of using the Stillwater Branch. 
  pStillwaterUp <- rbeta(1, 15, 120) # During upstream passage
  pStillwaterD <- rbeta(1, 15, 120)  # During downstream passage
  
  # Draw probability of using the Piscataquis for upstream migration. NOTE: NEED
  # TO MAKE THIS CONDITIONAL ON FLOW.
  pPiscUp <- rbeta(1, 25, 75) # During upstream passage
  
  return(list(
    up = up,
    timely = timely,
    fB = fB,
    d = d,
    indirect = indirect,
    latent = latent,
    delay = delay,
    ildProduct = ildProduct,
    jReduction = jReduction,
    weldon = weldon,
    scenario = scenario,
    pStillwaterUp = pStillwaterUp,
    pStillwaterD = pStillwaterD,
    pPiscUp = pPiscUp,
    downstreamS = downstreamS,
    oceanSurvival = oceanSurvival,
    pinHarvest = pinHarvest,
    inRiverF = inRiverF,
    commercialF = commercialF,
    bycatchF = bycatchF,
    Age1 = Age1
  ))
}


if(river=='merrimack'){
  
  # Assign the starting population based on a seed of
  # age-1 fish and application of an ocean survival curve
  # The population size is scaled to make the models
  # run faster. Output is re-scaled  
  Age1 <- rpois(1, 2e5)
  
  return(list(
    up = up,
    timely = timely,
    fB = fB,
    d = d,
    indirect = indirect,
    latent = latent,
    delay = delay,
    ildProduct = ildProduct,
    jReduction = jReduction,
    downstreamS = downstreamS,
    oceanSurvival = oceanSurvival,
    inRiverF = inRiverF,
    commercialF = commercialF,
    bycatchF = bycatchF,
    Age1 = Age1
  ))
}


if(river=='connecticut'){
  
  # Assign the starting population based on a seed of
  # age-1 fish and application of an ocean survival curve
  # The population size is scaled to make the models
  # run faster. Output is re-scaled  
  Age1 <- rpois(1, 2e6)
  
  return(list(
    up = up,
    timely = timely,
    fB = fB,
    d = d,
    indirect = indirect,
    latent = latent,
    delay = delay,
    ildProduct = ildProduct,
    jReduction = jReduction,
    downstreamS = downstreamS,
    oceanSurvival = oceanSurvival,
    inRiverF = inRiverF,
    commercialF = commercialF,
    bycatchF = bycatchF,
    Age1 = Age1
  ))
}


if(river=='saco'){
  
  # Assign the starting population based on a seed of
  # age-1 fish and application of an ocean survival curve
  # The population size is scaled to make the models
  # run faster. Output is re-scaled  
  Age1 <- rpois(1, 2e5)
  
  return(list(
    up = up,
    timely = timely,
    fB = fB,
    d = d,
    indirect = indirect,
    latent = latent,
    delay = delay,
    ildProduct = ildProduct,
    jReduction = jReduction,
    downstreamS = downstreamS,
    oceanSurvival = oceanSurvival,
    inRiverF = inRiverF,
    commercialF = commercialF,
    bycatchF = bycatchF,
    Age1 = Age1
  ))
}


if(river=='kennebec'){
  
  # Assign the starting population based on a seed of
  # age-1 fish and application of an ocean survival curve
  # The population size is scaled to make the models
  # run faster. Output is re-scaled  
  Age1 <- rpois(1, 2e5)
  
  return(list(
    up = up,
    timely = timely,
    fB = fB,
    d = d,
    indirect = indirect,
    latent = latent,
    delay = delay,
    ildProduct = ildProduct,
    jReduction = jReduction,
    downstreamS = downstreamS,
    oceanSurvival = oceanSurvival,
    inRiverF = inRiverF,
    commercialF = commercialF,
    bycatchF = bycatchF,
    Age1 = Age1
  ))
}


if(river=='hudson'){
  
  # Assign the starting population based on a seed of
  # age-1 fish and application of an ocean survival curve
  # The population size is scaled to make the models
  # run faster. Output is re-scaled  
  Age1 <- rpois(1, 2e5)
  
  return(list(
    up = up,
    timely = timely,
    fB = fB,
    d = d,
    indirect = indirect,
    latent = latent,
    delay = delay,
    ildProduct = ildProduct,
    jReduction = jReduction,
    downstreamS = downstreamS,
    oceanSurvival = oceanSurvival,
    inRiverF = inRiverF,
    commercialF = commercialF,
    bycatchF = bycatchF,
    Age1 = Age1
  ))
}
}