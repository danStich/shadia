# FRONT-END CODE ---------------------------------------------------------------
# PenobscotRiverModel.R
# optimizations Dec 2017


# Create a function out of the model
penobscotRiverModel <- function(){

# DSS: commented out because funs should be loaded
# automatically with other files in `R/` with 
# package implementation.
# TODO: set full path to the current directory, here, e.g.:
# Then remove the stop() line below.
# setwd("~/projects/NOAA-stich/code")
#stop('set the working directory in PenobscotRiverModel.R')
#setwd("C:/Users/STICHDS/Desktop/projects/shadmodel/penobscotProject/penobscot")


# DSS: commented out because funs should be loaded
# automatically with other files in `R/` with 
# package implementation.
# source('setParameters.R')
# source('defineFunctions.R') 
# All functions in defineFunctions.R should
# be loaded with package load

setParameters()

# ---------

# Define output vectors for simulation and hydrosystem configuration
if (useTictoc) tic("Running data load...")
defineOutputVectors()
defineHydrosystem()
if (useTictoc) toc()

# ---------

# TIMERS
# Start the timer for the simulation
ptmSim  <- proc.time()

# Progress meter
if (useProgress) {
  pb <-
    txtProgressBar(
      min = 0,
      max = nRuns * nYears,
      style = 3,
      char = '+'
    )
}

if (useTictoc) tic("total time")

# ---------

# SIMULATION SETTINGS FOR OUTER LOOP --------------------------------------
# Outer loop for number of simulations- this is how many runs it will do
for (k in 1:nRuns) {
  if (useTictoc) tic(paste("OUTER loop", k))

  # JMS: load the saved sampled variables for repeatability:
  # JMS: Perform sampling and save variables for the outer-loop ONE TIME, for repeatability
  # otherwise, load previously saved variables
  if (doOuterSampling) {
    #if (useTictoc) tic("Running outer loop sampling...")
    source(outerLoopSamplingSource)
    #if (useTictoc) toc()
  } else {
    #if (useTictoc) tic("LOADING outer loop sampling...")
    load(outerLoopSamplingRData)
    #if (useTictoc) toc()
  }


  # DAM PASSAGE EFFICIENCIES ------------------------------------------------
  # # Draw passage rates for all dams in this system from a set of pre-defined
  # # values
  # JMS: draws are moved to outer-loop-sampling.R

  # Define passage rates
  Open = 1.00
  Confluence = 1.00 * fB
  OronoUp = 1 * fB          # Orono upstream passage
  StillwaterUp = 1        # Stillwater upstream passage
  GilmanUp = 1            # Gilman Falls upstream passage
  MilfordUp = up[1] * fB       # Milford upstream passage
  HowlandUp = up[2] * fB       # Howland upstream passage
  WestEnfieldUp = up[3] * fB   # West Enfield upstream passage
  BrownsMillUp = up[4] * fB    # Browns Mill upstream passage
  MooseheadUp = up[5] * fB     # Moosehead (Dover) passage
  GuilfordUp = up[6] * fB      # Guilford Passage
  MattaceunkUp = up[7] * fB    # Mattaceunk (Weldon) passage

  # Downstream passage efficiencies
  # JMS: sampling moved to outer-loop-sampling.R

  # Define downstream passage efficiencies at each of the dams
  OpenD = 1.00 * delay                          # Perfect passage open reaches
  GilmanD = 1.00 * delay                        # Gilman passage
  StillwaterD = d[1] * indirect * latent * delay    # Stillwater passage
  OronoD = d[2] * indirect * latent * delay         # Orono passage
  MilfordD = d[3] * indirect * latent * delay       # Milford passage
  HowlandD = d[4] * indirect * latent * delay       # Howland passage
  WestEnfieldD = d[5] * indirect * latent * delay   # West Enfield passage
  BrownsMillD = d[6] * indirect * latent * delay    # Browns Mill passage
  MooseheadD = d[7] * indirect * latent * delay     # Moosehead (Dover) passage
  GuilfordD = d[8] * indirect * latent * delay      # Guilford Passage
  MattaceunkD = d[9] * indirect * latent * delay    # Mattaceunk (Weldon) passage

  # Make downstream survival probabilities for juveniles
  # JMS: Juvenile reduction factor draws are moved to outer-loop-sampling.R
  GilmanDj = 1.00                            # Gilman passage
  StillwaterDj = StillwaterD * jReduction    # Stillwater passage
  OronoDj = OronoD * jReduction              # Orono passage
  MilfordDj = MilfordD * jReduction          # Milford passage
  HowlandDj = HowlandD * jReduction          # Howland passage
  WestEnfieldDj = WestEnfieldD * jReduction  # West Enfield passage
  BrownsMillDj = BrownsMillD * jReduction    # Browns Mill passage
  MooseheadDj = MooseheadD * jReduction      # Moosehead (Dover) passage
  GuilfordDj = GuilfordD * jReduction        # Guilford Passage
  MattaceunkDj = MattaceunkD * jReduction    # Mattaceunk (Weldon) passage

  # JMS: Draw timing of Weldon passage implementation moved to outer-loop-sampling.R

  # HABITAT DEFINITION ------------------------------------------------------
  # How much habitat is in each of the newly created production units?
  # This will be used to draw carrying capacity later on.
  habitat = vector(mode = 'list', length = nRoutes)

  # NEED TO RE-COMMENT ALL OF THIS!
  OronoHabitat = 1000
  StillwaterHabitat = 10000
  habitat[[1]] = c(
    (22344 + 34868),
    (14339 + 34868),
    (400560 + 26285 + 12746),
    (153461 + 37769 + 15257),
    1053,
    22591,
    14922
  )
  habitat[[2]] = c((22344 + 34868),
                   (14339 + 34868),
                   (400560 + 26285 + 12746),
                   (333196 + 205744),
                   (204336 + 25773)
  )
  habitat[[3]] = c(
    (22344 + 34868),
    (OronoHabitat),
    (StillwaterHabitat),
    (400560 + 26285 + 12746),
    (153461 + 37769 + 15257),
    1053,
    22591,
    14922
  )
  habitat[[4]] = c((22344 + 34868),
                   (OronoHabitat),
                   (StillwaterHabitat),
                   (400560 + 26285 + 12746),
                   (333196 + 205744),
                   (204336 + 25773)
  )

  # UPSTREAM PASSAGE EFFICIENCIES AND MIGRATION ROUTE ----------------------------
  # NOTE: This section is special for the PNR because of multiple routes with
  # unequal numbers of dams and unequal reach lengths

  # Assign efficiencies to the upstream passage groups (pisc or main)
  upEffs = vector(mode = 'list', length = nRoutes)
  upEffs[[1]] = vector(mode = 'numeric', length = length(damRkms[[1]]))
  upEffs[[2]] = vector(mode = 'numeric', length = length(damRkms[[2]]))
  upEffs[[3]] = vector(mode = 'numeric', length = length(damRkms[[3]]))
  upEffs[[4]] = vector(mode = 'numeric', length = length(damRkms[[4]]))

  # Route 1- Mainstem to piscataquis
  upEffs[[1]][1] = Open
  upEffs[[1]][2] = Confluence
  upEffs[[1]][3] = MilfordUp
  upEffs[[1]][4] = HowlandUp
  upEffs[[1]][5] = BrownsMillUp
  upEffs[[1]][6] = MooseheadUp
  upEffs[[1]][7] = GuilfordUp
  # Route 2- Main-stem to main-stem
  upEffs[[2]][1] = Open
  upEffs[[2]][2] = Confluence
  upEffs[[2]][3] = MilfordUp
  upEffs[[2]][4] = WestEnfieldUp
  upEffs[[2]][5] = MattaceunkUp
  # Route 1- Stillwater to piscataquis
  upEffs[[3]][1] = Open
  upEffs[[3]][2] = OronoUp
  upEffs[[3]][3] = StillwaterUp
  upEffs[[3]][4] = GilmanUp
  upEffs[[3]][5] = HowlandUp
  upEffs[[3]][6] = BrownsMillUp
  upEffs[[3]][7] = MooseheadUp
  upEffs[[3]][8] = GuilfordUp
  # Route 1- Stillwater to main-stem
  upEffs[[4]][1] = Open
  upEffs[[4]][2] = OronoUp
  upEffs[[4]][3] = StillwaterUp
  upEffs[[4]][4] = GilmanUp
  upEffs[[4]][5] = WestEnfieldUp
  upEffs[[4]][6] = MattaceunkUp

  # JMS: Draw probability of using the Stillwater Branch moved to outer-loop-sampling.R
  pMainUp = 1 - pStillwaterUp
  pMainD = 1 - pStillwaterD

  # # Draw probability of using the Piscataquis for upstream migration. NOTE: NEED
  # # TO MAKE THIS CONDITIONAL ON FLOW.
  # JMS: Draw of pPiscUp moved to outer-loop-sampling.R
  pMainstemUp = 1 - pPiscUp  # Probability of using mainstem in upper river

  # # STARTING POPULATION STRUCTURE FOR EACH SIMULATION -----------------------
  # JMS: maxAge moved to setupData.R
  # JMS: Survival rates for life-history stages moved to outer-loop-sampling.R

  # Calculate in-river fishing mortalities for each PU in each of the four
  # possible migration routes
  inriv = vector(mode = 'list', length = 4)
  inriv[[1]] = c(rep(inRiverF, 3), rep(0, 4))
  inriv[[2]] = c(rep(inRiverF, 4), 0)
  inriv[[3]] = c(rep(inRiverF, 4), rep(0, 4))
  inriv[[4]] = c(rep(inRiverF, 5), 0)

  # Assign the starting population based on a seed of age-1 fish and application
  # of an ocean survival curve
  # The population size is scaled to make the models run faster. Output is re-
  # scaled
  #scalar = 100 # Done below dynamically now

  # Original number of Age 1 individuals in the population
  # JMS: moved to outer-loop-sampling.R

  # Original number of individuals (calculated from ocean survival) per age
  for (i in 2:maxAge) {
    assign(paste('Age', i, sep = ''),
           Age1 * (cumprod(oceanSurvival[2:i - 1]))[i - 1])
  }

  # Collect age classes in a vector
  pop = mget(ls(pat = "^Age"))

  # Define probability of recruitment to spawn- based on proportion of spawners
  # in each age class (Bailey and Zydlewski 2013)
  spawnRecruit = c(0, 0, 0, 0.01, .33, .84, .97, .99, 1.00)

  # Initial probalities of repeat spawning- will be derived in annual loop
  pRepeat = c(0, 0, 0, 0.004, 0.094286, 0.375714, 0.722286, 1.00, 1.00)

  # Define spawning population and recruitment pool
  spawningPool = unlist(pop) * spawnRecruit
  recruitmentPool = unlist(pop) - unlist(pop) * spawnRecruit

  # SIMULATION SETTINGS FOR INNER LOOP --------------------------------------
  # Run sim for nYears
  for (n in 1:nYears) {
    #if (useTictoc) tic(paste("inner loop", n))

    # Remove dynamically named objects from the work space so there are no
    # legacy effects in naming new objects- this could lead to negative
    # population sizes and the like
    rm(list = ls()[grep(ls(), pat = '_')])

    # Set passage efficiency at Weldon (MattaceunkUp, upEffs[[2]][5] & [[4]][6])
    # This code takes the timing scenario, and says "if the year is less than the
    # minimum year of passage implementation at Weldon, set it to zero"
    if (n < scenario) {
      upEffs[[2]][5] = 0
      upEffs[[4]][6] = 0
    } else {
      upEffs[[2]][5] = MattaceunkUp
      upEffs[[4]][6] = MattaceunkUp
    }

    # Reset the scalar based on population size
    if (sum(spawningPool) < 1e3) {
      scalar = 10
    }
    if (sum(spawningPool) >= 1e3) {
      scalar = 100
    }
    if (sum(spawningPool) >= 1e4) {
      scalar = 1000
    }
    if (sum(spawningPool) >= 1e5) {
      scalar = 10000
    }
    if (sum(spawningPool) >= 1e6) {
      scalar = 100000
    }

    pop = unlist(pop) / scalar
    spawningPool = spawningPool / scalar
    recruitmentPool = recruitmentPool / scalar

    # if you need to load/reuse inner loop sampling, uncomment/use this stop, then call
    # the inner loop sampling code.
    #stop('halt here for testing')

    # JMS: Perform sampling and save variables for the inner-loop ONE TIME, for repeatability
    # otherwise load the variables we saved...
    if (doInnerSampling) {
      #if (useTictoc) tic("Running inner loop sampling...")
      source(innerLoopSamplingSource)
      #if (useTictoc) toc()
    } else {
      #if (useTictoc) tic("LOADING inner loop sampling...")
      load(innerLoopSamplingRData)
      #Rcpp::sourceCpp('datasets/pnrCppFuns.cpp')        # source C++ functions used in model
      #if (useTictoc) toc()
    }

    # Calculate numbers of males, females, and eggs in each PU
    # Get all combinations of ages and pus
    #if (useTictoc) tic("calculate counts in each PU")

    # Main-to-piscataquis spawners
    x_1 = expand.grid(puNames[[1]], seq(1, maxAge, 1))
    names(x_1) = c('pus', 'fishAges')
    # Main-to-mainstem spawners
    x_2 = expand.grid(puNames[[2]], seq(1, maxAge, 1))
    names(x_2) = c('pus', 'fishAges')
    # Stillwater-to-piscataquis spawners
    x_3 = expand.grid(puNames[[3]], seq(1, maxAge, 1))
    names(x_3) = c('pus', 'fishAges')
    # Stillwater-to-mainstem spawners
    x_4 = expand.grid(puNames[[4]], seq(1, maxAge, 1))
    names(x_4) = c('pus', 'fishAges')
    # if (useTictoc) toc() # counts in each PU

    # ---- PROCESS fish and eggs ----

    # Processing of populations generalized and moved into functions. See defineFunctions.R
    # JMS Dec 2017

    #if (useTictoc) tic("process Populations")
    #if (useTictoc) tic("process MALES NEW")
    males <- processPopulation(isFemale=FALSE)
    # if (useTictoc) toc()
    #if (useTictoc) tic("process FEMALES NEW")
    females <- processPopulation(isFemale=TRUE)
    #if (useTictoc) toc()
    #if (useTictoc) tic("process EGGS NEW")
    fec_Max <- processPopulation(isFemale=FALSE, isEgg=TRUE)
    #if (useTictoc) toc()
    #if (useTictoc) toc() # process populations

    # Apply post-spawning mortality for adults
    # Males

    #if (useTictoc) tic("post-spawning mortality, DOWNSTREAM MIGRATION")

    males[[1]] = Map("*", males[[1]], post_spawn_survival_males)
    males[[2]] = Map("*", males[[2]], post_spawn_survival_males)
    males[[3]] = Map("*", males[[3]], post_spawn_survival_males)
    males[[4]] = Map("*", males[[4]], post_spawn_survival_males)
    # Females
    females[[1]] = Map("*", females[[1]], post_spawn_survival_females)
    females[[2]] = Map("*", females[[2]], post_spawn_survival_females)
    females[[3]] = Map("*", females[[3]], post_spawn_survival_females)
    females[[4]] = Map("*", females[[4]], post_spawn_survival_females)

    # Apply juvenile mortality up to outmigration. This will be a list object with
    # Piscataquis recruits in the first element and Mainstem recruits in second.
    recruits = vector(mode = 'list', length = length(fec_Max))
    recruits[[1]] = Map("*", fec_Max[[1]], juvenile_survival)
    recruits[[2]] = Map("*", fec_Max[[2]], juvenile_survival)
    recruits[[3]] = Map("*", fec_Max[[3]], juvenile_survival)
    recruits[[4]] = Map("*", fec_Max[[4]], juvenile_survival)

    # DOWNSTREAM MIGRATION ----------------------------------------------------
    # Derive downstream passage efficiencies for each group of spawners in each PU.
    # This uses the starting position of the fish, and then incorporates cumulative
    # dam passage efficiencies from the starting PU to the Bay- including the
    # adjustment for proportional passage through the Stillwater Branch that was
    # described in Holbrook et al. (2011) and Stich et al. (2015).
    # Piscataquis out-migrants that used the main-stem around Marsh Island
    sPU_Pisc = c()
    sPU_Pisc[[1]] = OpenD * (downstreamS ^ puRkm[[1]][1])
    sPU_Pisc[[2]] = (OpenD ^ 2) * (downstreamS ^ puRkm[[1]][2])
    sPU_Pisc[[3]] = sPU_Pisc[[2]] *
      (((1 - pStillwaterD) * MilfordD) + (pStillwaterD * StillwaterD * OronoD *
                                            GilmanD)) *
      (downstreamS ^ puRkm[[1]][3])
    sPU_Pisc[[4]] = sPU_Pisc[[3]]	* HowlandD * (downstreamS ^ puRkm[[1]][4])
    sPU_Pisc[[5]] = sPU_Pisc[[4]] * BrownsMillD * (downstreamS ^ puRkm[[1]][5])
    sPU_Pisc[[6]] = sPU_Pisc[[5]]	* MooseheadD * (downstreamS ^ puRkm[[1]][6])
    sPU_Pisc[[7]] = sPU_Pisc[[6]] * GuilfordD * (downstreamS ^ puRkm[[1]][7])
    # Mainstem out-migrants	that used the main-stem around Marsh Island
    sPU_Main = c()
    sPU_Main[[1]] = OpenD * (downstreamS ^ puRkm[[2]][1])
    sPU_Main[[2]] = (OpenD ^ 2) * (downstreamS ^ puRkm[[2]][2])
    sPU_Main[[3]] = sPU_Pisc[[2]] * (((1 - pStillwaterD) * MilfordD) +
                                       (pStillwaterD * StillwaterD * OronoD * GilmanD)) * (downstreamS ^
                                                                                             puRkm[[2]][3])
    sPU_Main[[4]] = sPU_Main[[3]] * WestEnfieldD * (downstreamS ^ puRkm[[2]][4])
    sPU_Main[[5]] = sPU_Main[[4]] * MattaceunkD * (downstreamS ^ puRkm[[2]][5])
    # Piscataquis out-migrants that used the Stillwater Branch around Marsh Island
    sPU_PiscStill = c()
    sPU_PiscStill[[1]] = OpenD * (downstreamS ^ puRkm[[3]][1])
    sPU_PiscStill[[2]] = (OpenD ^ 2) * (downstreamS ^ puRkm[[3]][2])
    sPU_PiscStill[[3]] = sPU_PiscStill[[2]] *
      (((1 - pStillwaterD) * MilfordD) + (pStillwaterD * StillwaterD * OronoD)) *
      (downstreamS ^ puRkm[[3]][3])
    sPU_PiscStill[[4]] = sPU_PiscStill[[3]] * (((1 - pStillwaterD) * 1.00) +
                                                 (pStillwaterD * GilmanD)) * (downstreamS ^ puRkm[[3]][4])
    sPU_PiscStill[[5]] = sPU_PiscStill[[4]]	* HowlandD *
      (downstreamS ^ puRkm[[3]][5])
    sPU_PiscStill[[6]] = sPU_PiscStill[[5]] * BrownsMillD *
      (downstreamS ^ puRkm[[3]][6])
    sPU_PiscStill[[7]] = sPU_PiscStill[[6]]	* MooseheadD *
      (downstreamS ^ puRkm[[3]][7])
    sPU_PiscStill[[8]] = sPU_PiscStill[[7]] * GuilfordD *
      (downstreamS ^ puRkm[[3]][8])
    # Mainstem out-migrants that used the Stillwater Branch around Marsh Island
    sPU_MainStill = c()
    sPU_MainStill[[1]] = OpenD * (downstreamS ^ puRkm[[4]][1])
    sPU_MainStill[[2]] = (OpenD ^ 2) * (downstreamS ^ puRkm[[4]][2])
    sPU_MainStill[[3]] = sPU_MainStill[[2]] *
      (((1 - pStillwaterD) * MilfordD) + (pStillwaterD * StillwaterD * OronoD)) *
      (downstreamS ^ puRkm[[4]][3])
    sPU_MainStill[[4]] = sPU_MainStill[[3]] * (((1 - pStillwaterD) * 1.00) +
                                                 (pStillwaterD * GilmanD)) * (downstreamS ^ puRkm[[4]][4])
    sPU_MainStill[[5]] = sPU_MainStill[[4]]  * WestEnfieldD *
      (downstreamS ^ puRkm[[4]][5])
    sPU_MainStill[[6]] = sPU_MainStill[[5]] * MattaceunkD *
      (downstreamS ^ puRkm[[4]][6])

    # Derive downstream passage efficiencies for each group of juveniles in each PU.
    # This uses the starting position of the fish, and then incorporates cumulative
    # dam passage efficiencies from the starting PU to the Bay- including the
    # adjustment for proportional passage through the Stillwater Branch that was
    # described in Holbrook et al. (2011) and Stich et al. (2015).
    # Piscataquis out-migrants that used the main-stem around Marsh Island
    sPUj_Pisc = c()
    sPUj_Pisc[[1]] = OpenD * (downstreamS ^ puRkm[[1]][1])
    sPUj_Pisc[[2]] = (OpenD ^ 2) * (downstreamS ^ puRkm[[1]][2])
    sPUj_Pisc[[3]] = sPUj_Pisc[[2]] *
      (((1 - pStillwaterD) * MilfordDj) +
         (pStillwaterD * StillwaterD * OronoDj * GilmanDj)) * (downstreamS ^
                                                                 puRkm[[1]][3])
    sPUj_Pisc[[4]] = sPUj_Pisc[[3]]	* HowlandDj * (downstreamS ^ puRkm[[1]][4])
    sPUj_Pisc[[5]] = sPUj_Pisc[[4]] * BrownsMillDj * (downstreamS ^ puRkm[[1]][5])
    sPUj_Pisc[[6]] = sPUj_Pisc[[5]]	* MooseheadDj * (downstreamS ^ puRkm[[1]][6])
    sPUj_Pisc[[7]] = sPUj_Pisc[[6]] * GuilfordDj * (downstreamS ^ puRkm[[1]][7])
    # Mainstem out-migrants	that used the main-stem around Marsh Island
    sPUj_Main = c()
    sPUj_Main[[1]] = OpenD * (downstreamS ^ puRkm[[2]][1])
    sPUj_Main[[2]] = (OpenD ^ 2) * (downstreamS ^ puRkm[[2]][2])
    sPUj_Main[[3]] = sPUj_Pisc[[2]] * (((1 - pStillwaterD) * MilfordDj) +
                                         (pStillwaterD * StillwaterD * OronoDj * GilmanDj)) * (downstreamS ^
                                                                                                 puRkm[[2]][3])
    sPUj_Main[[4]] = sPUj_Main[[3]] * WestEnfieldDj * (downstreamS ^ puRkm[[2]][4])
    sPUj_Main[[5]] = sPUj_Main[[4]] * MattaceunkDj * (downstreamS ^ puRkm[[2]][5])
    # Piscataquis out-migrants that used the Stillwater Branch around Marsh Island
    sPUj_PiscStill = c()
    sPUj_PiscStill[[1]] = OpenD * (downstreamS ^ puRkm[[3]][1])
    sPUj_PiscStill[[2]] = (OpenD ^ 2) * (downstreamS ^ puRkm[[3]][2])
    sPUj_PiscStill[[3]] = sPUj_PiscStill[[2]] *
      (((1 - pStillwaterD) * MilfordDj) + (pStillwaterD * StillwaterDj * OronoDj)) *
      (downstreamS ^ puRkm[[3]][3])
    sPUj_PiscStill[[4]] = sPUj_PiscStill[[3]] * (((1 - pStillwaterD) * 1.00) +
                                                   (pStillwaterD * GilmanDj)) * (downstreamS ^ puRkm[[3]][4])
    sPUj_PiscStill[[5]] = sPUj_PiscStill[[4]]	* HowlandDj *
      (downstreamS ^ puRkm[[3]][5])
    sPUj_PiscStill[[6]] = sPUj_PiscStill[[5]] * BrownsMillDj *
      (downstreamS ^ puRkm[[3]][6])
    sPUj_PiscStill[[7]] = sPUj_PiscStill[[6]]	* MooseheadDj *
      (downstreamS ^ puRkm[[3]][7])
    sPUj_PiscStill[[8]] = sPUj_PiscStill[[7]] * GuilfordDj *
      (downstreamS ^ puRkm[[3]][8])
    # Mainstem out-migrants that used the Stillwater Branch around Marsh Island
    sPUj_MainStill = c()
    sPUj_MainStill[[1]] = OpenD * (downstreamS ^ puRkm[[4]][1])
    sPUj_MainStill[[2]] = (OpenD ^ 2) * (downstreamS ^ puRkm[[4]][2])
    sPUj_MainStill[[3]] = sPUj_MainStill[[2]] *
      (((1 - pStillwaterD) * MilfordDj) + (pStillwaterD * StillwaterDj * OronoDj)) *
      (downstreamS ^ puRkm[[4]][3])
    sPUj_MainStill[[4]] = sPUj_MainStill[[3]] * (((1 - pStillwaterD) * 1.00) +
                                                   (pStillwaterD * GilmanDj)) * (downstreamS ^ puRkm[[4]][4])
    sPUj_MainStill[[5]] = sPUj_MainStill[[4]]  * WestEnfieldDj *
      (downstreamS ^ puRkm[[4]][5])
    sPUj_MainStill[[6]] = sPUj_MainStill[[5]] * MattaceunkDj *
      (downstreamS ^ puRkm[[4]][6])

    # Calculate number of males reaching the mouth of the river after spawn from
    # each PU from each upstream migration route
    malesOut = vector(mode = 'list', length = length(males))
    # Mainstem-to-piscataquis spawners
    for (i in 1:length(sPU_Pisc)) {
      malesOut[[1]][[i]] = mapply("*", males[[1]], sPU_Pisc)[, i]
    }
    # Mainstem-to-mainstem spawners
    for (i in 1:length(sPU_Main)) {
      malesOut[[2]][[i]] = mapply("*", males[[2]], sPU_Main)[, i]
    }
    # Stillwater-to-piscataquis spawners
    for (i in 1:length(sPU_PiscStill)) {
      malesOut[[3]][[i]] = mapply("*", males[[3]], sPU_PiscStill)[, i]
    }
    # Stillwater-to-mainstem spawners
    for (i in 1:length(sPU_MainStill)) {
      malesOut[[4]][[i]] = mapply("*", males[[4]], sPU_MainStill)[, i]
    }

    # Sum number of males in each age from all PUs reaching river mouth
    malesOut = apply(data.frame(malesOut[[1]]), 1, sum) +
      apply(data.frame(malesOut[[2]]), 1, sum) +
      apply(data.frame(malesOut[[3]]), 1, sum) +
      apply(data.frame(malesOut[[4]]), 1, sum)

    # Calculate number of females reaching the mouth of the river after spawn from
    # each PU in each route
    femalesOut = vector(mode = 'list', length = length(females))
    # Mainstem-to-piscataquis spawners
    for (i in 1:length(sPU_Pisc)) {
      femalesOut[[1]][[i]] = mapply("*", females[[1]], sPU_Pisc)[, i]
    }
    # Mainstem-to-mainstem spawners
    for (i in 1:length(sPU_Main)) {
      femalesOut[[2]][[i]] = mapply("*", females[[2]], sPU_Main)[, i]
    }
    # Stillwater-to-piscataquis spawners
    for (i in 1:length(sPU_PiscStill)) {
      femalesOut[[3]][[i]] = mapply("*", females[[3]], sPU_PiscStill)[, i]
    }
    # Stillwater-to-mainstem spawners
    for (i in 1:length(sPU_MainStill)) {
      femalesOut[[4]][[i]] = mapply("*", females[[4]], sPU_MainStill)[, i]
    }

    # Sum number of females in each age from all PUs reaching river mouth
    femalesOut = apply(data.frame(femalesOut[[1]]), 1, sum) +
      apply(data.frame(femalesOut[[2]]), 1, sum) +
      apply(data.frame(femalesOut[[3]]), 1, sum) +
      apply(data.frame(femalesOut[[4]]), 1, sum)

    # Calculate the number of recruits reaching the ocean
    # DOES NOT INCLUDE ELEVATED MORTALITY FOR JUVENILES YET, ALTHOUGH I
    # THINK THOSE DATA ARE AVAILABLE
    recruitsOut = vector(mode = 'list', length = length(recruits))
    # Mainstem-to-piscataquis spawners
    for (i in 1:length(sPUj_Pisc)) {
      recruitsOut[[1]][[i]] = mapply("*", recruits[[1]][[i]], sPUj_Pisc[i])
    }
    # Mainstem-to-mainstem spawners
    for (i in 1:length(sPUj_Main)) {
      recruitsOut[[2]][[i]] = mapply("*", recruits[[2]][[i]], sPUj_Main[i])
    }
    # Stillwater-to-piscataquis spawners
    for (i in 1:length(sPUj_PiscStill)) {
      recruitsOut[[3]][[i]] = mapply("*", recruits[[3]][[i]], sPUj_PiscStill[i])
    }
    # Stillwater-to-mainstem spawners
    for (i in 1:length(sPUj_MainStill)) {
      recruitsOut[[4]][[i]] = mapply("*", recruits[[4]][[i]], sPUj_MainStill[i])
    }

    # Sum number of recruits in each age from all PUs reaching river mouth
    recruitsOut = sum(unlist(recruitsOut))

    #if (useTictoc) toc() # post-spawning mortality, downstream migration

    # THE NEXT GENERATION ----------------------------------------------------------
    # next year (after applying ocean survival)
    # Define a vector containing total number of spawners returning
    # to the ocean. This number does not include Age 1 individuals. Then
    # apply the marine survival rate and tally the total number of repeat
    # spawners in the population during the next time-step
    # Sum male and female out migrants
    #if (useTictoc) tic("NEXT GENERATION")

    outMigrants = femalesOut + malesOut
    # Apply Ocean survival rate
    for (i in 1:length(outMigrants)) {
      assign(
        paste('repeatSpawners_', i, sep = ''),
        outMigrants[i] * oceanSurvival[i] * (1 - commercialF[i]) * (1 -
                                                                      bycatchF[i])
      )
    }
    # Now graduate each cohort to the next age
    repeats = append(0, unlist(mget(ls(pat = '^repeatSpawners_')))[2:(maxAge)])
    # Assign names for each age class
    names(repeats) = names(femalesOut)

    # Calculate numbers in the recruitment pool for next year, this number includes
    # the
    # Apply marine survival rate to fish waiting in the ocean
    nextRecruits = c(recruitsOut,
                     recruitmentPool[1:(length(recruitmentPool) - 1)]) *
      oceanSurvival * (1 - commercialF[i]) * (1 - bycatchF[i])
    # Assign names for each age class
    names(nextRecruits) = names (recruitmentPool)

    # Calculate proportion of repeat spawners in each age class
    pRepeat = repeats / (nextRecruits + repeats)

    # Combine repeat spawners with new recruits
    spawningPool = nextRecruits * spawnRecruit + repeats
    recruitmentPool = nextRecruits - nextRecruits * spawnRecruit

    # Record new population size for the start of the inner loop
    pop = sum(spawningPool + recruitmentPool)

    pop = pop * scalar
    spawningPool = spawningPool * scalar
    recruitmentPool = recruitmentPool * scalar
    # if (useTictoc) toc()

    # STORE OUTPUT IN PRE-ALLOCATED CONTAINERS --------------------------------
    #if (useTictoc) tic("store output")
    # Population size
    populationSize[(n + nYears * (k - 1))] = sum(spawningPool)

    # Year, fillling pre-allocated vector with this year
    years[(n + nYears * (k - 1))] = n

    # Scenario for passage timing at Weldon
    scen[(n + nYears * (k - 1))] = scenario

    # Upstream passage efficiencies, fillling pre-allocated vectors
    OrUp[(n + nYears * (k - 1))] = OronoUp
    StUp[(n + nYears * (k - 1))] = StillwaterUp
    GilmUp[(n + nYears * (k - 1))] = GilmanUp
    MdUp[(n + nYears * (k - 1))] = up[1]
    HdUp[(n + nYears * (k - 1))] = up[2]
    WEnfUp[(n + nYears * (k - 1))] = up[3]
    BMillUp[(n + nYears * (k - 1))] = up[4]
    MooseUp[(n + nYears * (k - 1))] = up[5]
    GuilfUp[(n + nYears * (k - 1))] = up[6]
    MattUp[(n + nYears * (k - 1))] =  upEffs[[2]][5]#up[7]

    # Downstream passage efficiencies, fillling pre-allocated vectors
    OrD[(n + nYears * (k - 1))] = d[2]
    StD[(n + nYears * (k - 1))] = d[1]
    GilmD[(n + nYears * (k - 1))] = 1
    MdD[(n + nYears * (k - 1))] = d[3]
    HdD[(n + nYears * (k - 1))] = d[4]
    WEnfD[(n + nYears * (k - 1))] = d[5]
    BMillD[(n + nYears * (k - 1))] = d[6]
    MooseD[(n + nYears * (k - 1))] = d[7]
    GuilfD[(n + nYears * (k - 1))] = d[8]
    MattD[(n + nYears * (k - 1))] =  d[9]

    # Indirect mortality, fillling pre-allocated vector
    indirectM[(n + nYears * (k - 1))] = indirect

    # Latent estuary mortality, fillling pre-allocated vector
    latentM[(n + nYears * (k - 1))] = latent

    # Juvenile reduction factor at each dam, fillling pre-allocated vector
    juvReduction[(n + nYears * (k - 1))] = jReduction

    # Fall back, fillling pre-allocated vector
    fallback[(n + nYears * (k - 1))] = fB

    # Population below Milford, fillling pre-allocated vector
    LowerPop[(n + nYears * (k - 1))] = (
      sum(males[[1]][[2]]) +
        sum(males[[1]][[2]]) +
        sum(males[[2]][[1]]) +
        sum(males[[2]][[2]]) +
        sum(males[[3]][[1]]) +
        sum(males[[4]][[1]]) +
        sum(females[[1]][[2]]) +
        sum(females[[1]][[2]]) +
        sum(females[[2]][[1]]) +
        sum(females[[2]][[2]]) +
        sum(females[[3]][[1]]) +
        sum(females[[4]][[1]])
    ) * scalar

    # Population between Orono and Stillwater dams, fillling pre-allocated vector
    OronoPop[(n + nYears * (k - 1))] = (
      sum(males[[3]][[2]]) +
        sum(males[[4]][[2]]) +
        sum(females[[3]][[2]]) +
        sum(females[[4]][[2]])
      ) * scalar

    # Population between Stillwater Dam and Gilman Falls, fillling pre-allocated
    StillwaterPop[(n + nYears * (k - 1))] = (
      sum(males[[3]][[3]]) +
        sum(males[[4]][[3]]) +
        sum(females[[3]][[3]]) +
        sum(females[[4]][[3]])) * scalar

    # Population between Milford and West Enfield, fillling pre-allocated vector
    MilfordPop[(n + nYears * (k - 1))] = (
      sum(males[[1]][[3]]) +
        sum(males[[2]][[3]]) +
        sum(males[[3]][[4]]) +
        sum(males[[4]][[4]]) +
        sum(females[[1]][[3]]) +
        sum(females[[2]][[3]]) +
        sum(females[[3]][[4]]) +
        sum(females[[4]][[4]])
    ) * scalar

    # Population between West Enfield and Weldon, fillling pre-allocated vector
    EnfieldPop[(n + nYears * (k - 1))] = (sum(males[[2]][[4]]) +
                                            sum(males[[4]][[5]]) +
                                            sum(females[[2]][[4]]) +
                                            sum(females[[4]][[5]])) * scalar

    # Population above Weldon, fillling pre-allocated vector
    WeldonPop[(n + nYears * (k - 1))] = (sum(males[[2]][[length(males[[2]])]]) +
                                           sum(males[[4]][[length(males[[4]])]]) +
                                           sum(females[[2]][[length(females[[2]])]]) +
                                           sum(females[[4]][[length(females[[4]])]])) * scalar

    # Population between Howland and Dover dams, fillling pre-allocated vector
    HowlandPop[(n + nYears * (k - 1))] = (sum(males[[1]][[4]]) +
                                            sum(males[[3]][[5]]) +
                                            sum(females[[1]][[4]]) +
                                            sum(females[[3]][[5]])) * scalar

    # Population between Moosehead and Browns Mill dams
    MoosePop[(n + nYears * (k - 1))] = (sum(males[[1]][[5]]) +
                                          sum(males[[3]][[6]]) +
                                          sum(females[[1]][[5]]) +
                                          sum(females[[3]][[6]])) * scalar

    # Population between Browns Mill and Guilford dams, fillling pre-allocated
    BrownsPop[(n + nYears * (k - 1))] = (sum(males[[1]][[6]]) +
                                           sum(males[[3]][[7]]) +
                                           sum(females[[1]][[6]]) +
                                           sum(females[[3]][[7]])) * scalar

    # Population above Guilford Dam, fillling pre-allocated vector
    GuilfordPop[(n + nYears * (k - 1))] = (sum(males[[1]][[7]]) +
                                             sum(males[[3]][[8]]) +
                                             sum(females[[1]][[7]]) +
                                             sum(females[[3]][[8]])) * scalar

    # Proportion of repeat spawners at each age, fillling pre-allocated vector
    pRepeats[[(n + nYears * (k - 1))]] = pRepeat

    # Age-structured repeat spawners, fillling pre-allocated vector
    spawners[[(n + nYears * (k - 1))]] = spawningPool

    # Reset the scalar based on population size
    if (sum(spawningPool) <= 1e4) {
      scalar = 100
    }
    if (sum(spawningPool) > 1e4) {
      scalar = 1000
    }
    if (sum(spawningPool) >= 1e5) {
      scalar = 10000
    }
    if (sum(spawningPool) >= 1e6) {
      scalar = 100000
    }

    # Scalar variable for computational gains
    scalarVar[[(n + nYears * (k - 1))]] = scalar

    # Store the inputs for sensitivity analysis
    # Passage assumptions
    ptime[(n + nYears * (k - 1))] = timely
    pStillUP[(n + nYears * (k - 1))] = pStillwaterUp
    pStillD[(n + nYears * (k - 1))] = pStillwaterD
    pPiscUP[(n + nYears * (k - 1))] = pPiscUp

    # Population demographics and survival rates
    S.downstream[(n + nYears * (k - 1))] = mean(downstreamS)
    S.marine[(n + nYears * (k - 1))] = mean(oceanSurvival)
    F.inRiver[(n + nYears * (k - 1))] = inRiverF
    F.commercial[(n + nYears * (k - 1))] = mean(commercialF)
    F.bycatch[(n + nYears * (k - 1))] = mean(bycatchF)
    popStart[(n + nYears * (k - 1))] = pop
    p.female[(n + nYears * (k - 1))] = sex_Ratio
    S.prespawnM[(n + nYears * (k - 1))] = pre_spawn_survival_males
    S.postspawnM[(n + nYears * (k - 1))] = post_spawn_survival_males
    S.prespawnF[(n + nYears * (k - 1))] = pre_spawn_survival_females
    S.postspawnF[(n + nYears * (k - 1))] = post_spawn_survival_females
    S.juvenile[(n + nYears * (k - 1))] = juvenile_survival

    # Environmental
    # Stochasticity
    t.stoch[(n + nYears * (k - 1))] = stoch
    # Regression relating temperatures in PNR and CTR
    t.RegrInt[(n + nYears * (k - 1))] = calMod[1, 1]
    t.RegrSlp[(n + nYears * (k - 1))] = calMod[2, 1]
    # Model parameters for sex-specific arrival timing
    b.ArrRegrInt[(n + nYears * (k - 1))] = res.B[1, 1]
    b.ArrRegrSlp[(n + nYears * (k - 1))] = res.B[2, 1]
    r.ArrRegrInt[(n + nYears * (k - 1))] = res.R[1, 1]
    r.ArrRegrSlp[(n + nYears * (k - 1))] = res.R[2, 1]

    # Individual traits
    # Entry dates
    b.Arr[(n + nYears * (k - 1))] = mean(c_entryDate[c_sex == 0])
    r.Arr[(n + nYears * (k - 1))] = mean(c_entryDate[c_sex == 1])
    # Spawning ATU
    ATUspawn1[(n + nYears * (k - 1))] = mean(c_spawnATU1)
    ATUspawn2[(n + nYears * (k - 1))] = mean(c_spawnATU2)
    # Spawning dates
    Dspawn1[(n + nYears * (k - 1))] = mean(c_initial)
    Dspawn2[(n + nYears * (k - 1))] = mean(c_end)
    # Length at age
    # Females
    linF[(n + nYears * (k - 1))] = r.mat[1]
    kF[(n + nYears * (k - 1))] = r.mat[2]
    t0F[(n + nYears * (k - 1))] = r.mat[3]
    # Males
    linM[(n + nYears * (k - 1))] = b.mat[1]
    kM[(n + nYears * (k - 1))] = b.mat[2]
    t0M[(n + nYears * (k - 1))] = b.mat[3]

    # Length-weight regression parameters
    # Female
    lwF.alpha[(n + nYears * (k - 1))] = c_femaleLWalpha
    lwF.beta[(n + nYears * (k - 1))] = c_femaleLWbeta
    # Male
    lwM.alpha[(n + nYears * (k - 1))] = c_maleLWalpha
    lwM.beta[(n + nYears * (k - 1))] = c_maleLWbeta

    # Length
    b.length[(n + nYears * (k - 1))] = mean(c_male_lf)
    r.length[(n + nYears * (k - 1))] = mean(c_female_lf)

    # Fecundity
    spawnInt[(n + nYears * (k - 1))] = mean(c_SI)
    batchSize[(n + nYears * (k - 1))] = mean(c_BF)
    resTime[(n + nYears * (k - 1))] = mean(c_RAF)

    # Movement parameters
    s.Optim[(n + nYears * (k - 1))] = mean(sOptim)
    d.Max[(n + nYears * (k - 1))] = mean(dMax)
    tortuosity[(n + nYears * (k - 1))] = mean(tort)
    motivation[(n + nYears * (k - 1))] = mot
    daily.move[(n + nYears * (k - 1))] = mean(dailyMove)
    #toc()

    # PROGRESS METER UPDATE
    if (useProgress) {
      Sys.sleep(0.001) # System time-out for update
      setTxtProgressBar(pb, (n + nYears * (k - 1))) # Calculates % completed
    }

    #if (useTictoc) toc() # inner loop
  } # Year loop

  if (useTictoc) toc() # outer loop
} # Simulation loop

# JMS: Data writes generalized and moved to functions. See defineFunctions.R
# Prepare objects for write
#writeData()
# DSS: commented out because parallel
#      implementation returns a list
#      that can be dumped into rda files


# TIMING RESULTS FOR SIMULATION BENCHMARKING ------------------------------
# This section uses the timing prompts from earlier in the script to calculate
# the total run time for the simulation.
if (useTictoc) toc() #"total"


simTime <- proc.time() - ptmSim
print(' ')
#print('timeABM')
#print.proc_time(timeABM)
#print('timeDelay')
#print.proc_time(timeDelay)
print('simTime')
print.proc_time(simTime)

}