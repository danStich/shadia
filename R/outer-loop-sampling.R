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

# Attempt to perform all draws needed for outer loop,
# then save them for re-use and reproducible runs.

# Maximum age for fish in this population
if (!exists("maxAge")) list2env(setUpData(), envir = .GlobalEnv)

# Draw passage rates for all dams in this system from a set of pre-defined
# values
up <- as.vector(mapply(sample, pDraws, 1))
#up = rep(1, 7) # Uncomment for perfect passage

# Define the variable 'timely' to determine over what period the time will be
# expressed as a rate. A value of '1' results in a 24-hr passage rate
timely <- vector(mode='list', length=length(timing))
for(i in 1:length(timing)){
  timely[[i]] <- sample(timing[[i]], 1)
}

# Define fall back
fB <- 1.00

# ---

# Downstream passage efficiencies
# Define downstream passage efficiencies
d <- as.vector(mapply(sample, dDraws, 1))

# Define indirect mortality in freshwater resulting from dam passage
#indirect = 1#sample(c(0.90, 0.95, 0.99, 1.00), 1, replace = TRUE)

# Define latent mortality in estuary from dam passage.
#latent = 1#sample(c(0.90, 0.95, 0.99, 1.00), 1, replace = TRUE)

# Survival reduction due to delay in project head ponds
delay <- 1 #rbeta(1, 4e3, 10)

ildProduct <- indirect * latent * delay

jReduction <- 1 #sample(c(.50,.60,.70,.80,.90,1.00), 1, replace =TRUE)

# Survival rates for various life-history stages
# Define ocean survival for each age (1-M from Hoenig 1983 in ASMFC 2007
# stock assessment). ALTERNATIVE: Could use age-variant M.
downstreamS <- 1                              # Survival per km (natural)
oceanSurvival <- rep(rbeta(1, 12, 8), maxAge) # Ocean survival rate

# Fishing mortality rates
inRiverF <- inRiverF                          # Recreational MORTALITY RATE
commercialF <- rep(commercialF, maxAge)       # Commercial MORTALITY RATE
bycatchF <- rep(bycatchF, maxAge)             # Bycatch MORTALITY RATE


if(river=='penobscot'){
  # Assign the starting population based on a seed of
  # age-1 fish and application of an ocean survival curve
  # The population size is scaled to make the models
  # run faster. Output is re-scaled
  Age1 <- rpois(1, 1e4)
  
  # Sustenance harvest by PIN for penobscotRiverModel
  pinHarvest <- pinHarvest                      
  
  # Draw timing of Weldon passage implementation
  weldon <- 0#c(0, 10, 20)
  #     if(up[1] != 0.75){scenario = sample(weldon[2:3], 1, replace = TRUE)
  #       } else {
  scenario <- sample(weldon, 1, replace = TRUE)
  # }
  
  # Draw probability of using the Stillwater Branch. NOTE: NEED TO MAKE THIS
  # CONDITIONAL ON FLOW.
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


if(river=='susquehanna'){
  # Assign the starting population based on a seed of
  # age-1 fish and application of an ocean survival curve
  # The population size is scaled to make the models
  # run faster. Output is re-scaled
  Age1 <- rpois(1, 1e4)
  
  # Draw probability of using each passage route, conditional on flow
  p_JuniataUp <- 0.206262
  p_WestBranchUp <- 0.2195443
  p_ChemungUp <- 0.2719814
  p_NorthBranchUp <- 0.3022123

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
    p_JuniataUp = p_JuniataUp,
    p_WestBranchUp = p_WestBranchUp,
    p_ChemungUp = p_ChemungUp,
    p_NorthBranchUp = p_NorthBranchUp,
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

}