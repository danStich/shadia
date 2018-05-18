# outer-loop-sampling.R

outerLoopSampling <- function(){

# Attempt to perform all draws needed for outer loop,
# then save them for re-use and reproducible runs.

# Maximum age for fish in this population
if (!exists("maxAge")) list2env(setUpData(), envir = .GlobalEnv)

# Draw passage rates for all dams in this system from a set of pre-defined
# values
up = as.vector(mapply(sample, pDraws, 1))
#up = rep(1, 7) # Uncomment for perfect passage

# Define the variable 'timely' to determine over what period the time will be
# expressed as a rate. A value of '1' results in a 24-hr passage rate
timely = sample(timing, 1)

# Define fall back
fB = 1.00

# ---

# Downstream passage efficiencies
# Define downstream passage efficiencies
d = as.vector(mapply(sample, dDraws, 1))
#d = rep(1, 9) # Uncomment for perfect passage

# Define indirect mortality in freshwater resulting from dam passage
#indirect = 1#sample(c(0.90, 0.95, 0.99, 1.00), 1, replace = TRUE)

# Define latent mortality in estuary from dam passage.
#latent = 1#sample(c(0.90, 0.95, 0.99, 1.00), 1, replace = TRUE)

# Survival reduction due to delay in project head ponds
delay = 1#rbeta(1, 4e3, 10)

ildProduct <- indirect * latent * delay

# ---

#jReduction = 1#sample(c(.50,.60,.70,.80,.90,1.00), 1, replace =TRUE)

# ---

# Draw timing of Weldon passage implementation
weldon = 0#c(0, 10, 20)
#     if(up[1] != 0.75){scenario = sample(weldon[2:3], 1, replace = TRUE)
#       } else {
scenario = sample(weldon, 1, replace = TRUE)
# }

# ---

# Draw probability of using the Stillwater Branch. NOTE: NEED TO MAKE THIS
# CONDITIONAL ON FLOW.
pStillwaterUp = rbeta(1, 15, 120) # During upstream passage
pStillwaterD = rbeta(1, 15, 120)  # During downstream passage

# ---

# Draw probability of using the Piscataquis for upstream migration. NOTE: NEED
# TO MAKE THIS CONDITIONAL ON FLOW.
pPiscUp = rbeta(1, 25, 75) # During upstream passage

# ---

# STARTING POPULATION STRUCTURE FOR EACH SIMULATION -----------------------
# moved to setupData.R
# ---

# Survival rates for various life-history stages
# Define ocean survival for each age (1-M from Hoenig 1983 in ASMFC 2007
# stock assessment). ALTERNATIVE: Could use age-variant M
downstreamS   = 1#rep(rbeta(1, 1e4, 10))     # Survival per km
oceanSurvival = rep(rbeta(1, 12, 8), maxAge) # Ocean survival rate
pinHarvest = pinHarvest#sample(c(0, .05, .10), 1, replace=TRUE)
inRiverF = inRiverF                        # Recreational MORTALITY RATE
commercialF = rep(commercialF, maxAge)                # Commercial MORTALITY RATE
bycatchF = rep(bycatchF, maxAge)                    # Bycatch MORTALITY RATE

# ---

# Assign the starting population based on a seed of age-1 fish and application
# of an ocean survival curve
# The population size is scaled to make the models run faster. Output is re-
# scaled

# Original number of Age 1 individuals in the population
Age1 = rpois(1, 1e4)#/scalar

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

# To save only outer loop sampling variables: uncomment
# filename can be adjusted in setParameters.R
# JMS
# rm(additionalEggsProcessing, assignFishToRoutes, b.l, Bmod, buck.lw, bucks, b.w, calMod, 
#    CI, cpue, createPUMatrix, ctr, dat, daylength, fish, fishw, growth, hmu, i, invlogit, 
#    ldat, mu, newDay, outer.lim, pnr, processPopulation, res.B, res.R, r.l, Rmod, roe.lw, 
#    roes, r.w, substrRight, t, tempD, tempData, test, tz, writeData, writeSenData, 
#    writeSimData, yday, year)
#save.image(paste(baseDirectory, outerLoopSamplingRData, sep='/'))
