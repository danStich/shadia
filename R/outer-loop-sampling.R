# outer-loop-sampling.R
#

# Attempt to perform all draws needed for outer loop,
# then save them for re-use and reproducible runs.

outerLoopSampling <- function(){
# Maximum age for fish in this population
  if (!exists("shad$maxAge")) setUpData()

# Draw passage rates for all dams in this system from a set of pre-defined
# values
shad$up <- mapply(sample, shad$pDraws, 1)

# Define the variable 'timely' to determine over what period the time will be
# expressed as a rate. A value of '1' results in a 24-hr passage rate
shad$timely <- sample(shad$timing, 1, replace = TRUE)

# Define fall back
shad$fB <- 1.00

# ---

# Downstream passage efficiencies
# Define downstream passage efficiencies
shad$d <- mapply(sample, shad$dDraws, 1)

# Define indirect mortality in freshwater resulting from dam passage
shad$indirect <- 1#sample(c(0.90, 0.95, 0.99, 1.00), 1, replace = TRUE)

# Define latent mortality in estuary from dam passage.
shad$latent <- 1#sample(c(0.90, 0.95, 0.99, 1.00), 1, replace = TRUE)

# Survival reduction due to delay in project head ponds
shad$delay <- 1#rbeta(1, 4e3, 10)

shad$ildProduct <- shad$indirect * shad$latent * shad$delay

# ---

shad$jReduction <- 1#sample(c(.50,.60,.70,.80,.90,1.00), 1, replace =TRUE)

# ---

# Draw timing of Weldon passage implementation
shad$weldon <- 0#c(0, 10, 20)
#     if(up[1] != 0.75){scenario = sample(weldon[2:3], 1, replace = TRUE)
#       } else {
shad$scenario <- sample(shad$weldon, 1, replace = TRUE)
# }

# ---

# Draw probability of using the Stillwater Branch. NOTE: NEED TO MAKE THIS
# CONDITIONAL ON FLOW.
shad$pStillwaterUp <- rbeta(1, 15, 120) # During upstream passage
shad$pStillwaterD <- rbeta(1, 15, 120)  # During downstream passage

# ---

# Draw probability of using the Piscataquis for upstream migration. NOTE: NEED
# TO MAKE THIS CONDITIONAL ON FLOW.
shad$pPiscUp <- rbeta(1, 25, 75) # During upstream passage

# ---

# STARTING POPULATION STRUCTURE FOR EACH SIMULATION -----------------------
# moved to setupData.R
# ---

# Survival rates for various life-history stages
# Define ocean survival for each age (1-M from Hoenig 1983 in ASMFC 2007
# stock assessment). ALTERNATIVE: Could use age-variant M
shad$downstreamS <- 1#rep(rbeta(1, 1e4, 10))     # Survival per km
shad$oceanSurvival <- rep(rbeta(1, 12, 8), shad$maxAge) # Ocean survival rate
shad$pinHarvest <- 0#sample(c(0, .05, .10), 1, replace=TRUE)
shad$inRiverF <- shad$pinHarvest                        # Recreational MORTALITY RATE
shad$commercialF <- rep(0, shad$maxAge)                 # Commercial MORTALITY RATE
shad$bycatchF <- rep(0, shad$maxAge)                    # Bycatch MORTALITY RATE

# ---

# Assign the starting population based 
# on a seed of age-1 fish and application
# of an ocean survival curve
# The population size is scaled to 
# make the models run faster. 
# Output is re-scaled

# Original number of Age 1 individuals in the population
shad$Age1 <- rpois(1, 1e4)#/scalar

# To save only outer loop sampling variables: uncomment
# filename can be adjusted in setParameters.R
# JMS
# rm(additionalEggsProcessing, assignFishToRoutes, b.l, Bmod, buck.lw, bucks, b.w, calMod, 
#    CI, cpue, createPUMatrix, ctr, dat, daylength, fish, fishw, growth, hmu, i, invlogit, 
#    ldat, mu, newDay, outer.lim, pnr, processPopulation, res.B, res.R, r.l, Rmod, roe.lw, 
#    roes, r.w, substrRight, t, tempD, tempData, test, tz, writeData, writeSenData, 
#    writeSimData, yday, year)
#save.image(paste(baseDirectory, outerLoopSamplingRData, sep='/'))

}