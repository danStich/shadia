# penobscotRiverModel()
# optimizations Dec 2017

penobscotRiverModel <- function(nRuns=1,
                                nYears=50,
                                timing=1,
                                upstream=list(
                                  milford = c(0,5),
                                  howland = 1,
                                  westEnfield = 1,
                                  brownsMill = 1,
                                  moosehead = 1,
                                  guilford = 1,
                                  weldon = 1
                                ),
                                downstream=list(
                                  stillwater = 1,
                                  orono = 1,
                                  milford = 1,
                                  howland = 1,
                                  westEnfield = 1,
                                  brownsMill = 1,
                                  moosehead = 1,
                                  guilford = 1,
                                  weldon = 1
                                ),
                                watershed = TRUE
                                ){
  
# Global variable assignment -----
  nRuns <<- nRuns
  nYears <<- nYears
  timing <<- timing
  
  pDraws <- upstream
  dDraws <- downstream
  
  # For watershed applications of
  # the model, all values need to
  # match
  pDraws <<- rapply(pDraws,
         function(x) ifelse(watershed==TRUE, upstream[[1]], x),
         how = "replace")
  
  dDraws <<- rapply(dDraws,
         function(x) ifelse(watershed==TRUE, upstream[[1]], x),
         how = "replace")
  
  # if(watershed){
  # cat('WARNING: when watershed is set to TRUE,
  #   upstream passage rate(s) for Milford Dam will 
  #   be used for all upstream passage efficiencies,
  #   and downstream passage rates for Stillwater Dam
  #   will be used for all downstream passage
  #   efficiencies.')
  # }
  
  # TODO: set full path to the current directory, here, e.g.:
  # Then remove the stop() line below.
  # setwd("~/projects/NOAA-stich/code")
  # stop('set the working directory in PenobscotRiverModel.R')
  # setwd("C:/Users/STICHDS/Desktop/projects/pnrShad/R")

# Set global parameters -----
  list2env(setParameters(), envir = .GlobalEnv)

# Data load and memory pre-allocation -----
  if (useTictoc) tic("Running data load...")
  list2env(setUpData(), envir = .GlobalEnv)
  list2env(defineOutputVectors(), envir = .GlobalEnv)
  if (useTictoc) toc()

# Hydro system configuration -----
  list2env(defineHydroSystem(), envir = .GlobalEnv)
  list2env(defineHabitat(), envir = .GlobalEnv)

# Timers and progress -----
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

# SIMULATION SETTINGS FOR OUTER LOOP -----
  # Outer loop for number of simulations- 
  # this is how many runs it will do
  for (k in 1:nRuns) {
    k <<- k
    if (useTictoc) tic(paste("OUTER loop", k))
  
    # JMS: load the saved sampled variables for repeatability:
    # JMS: Perform sampling and save variables for the outer-loop ONE TIME, for repeatability
    # otherwise, load previously saved variables
    if (doOuterSampling) {
      #if (useTictoc) tic("Running outer loop sampling...")
      list2env(outerLoopSampling(), envir = .GlobalEnv)
      #if (useTictoc) toc()
    } else {
      #if (useTictoc) tic("LOADING outer loop sampling...")
      load(outerLoopSamplingRData)
      #if (useTictoc) toc()
    }
  
  # . Dam passage efficiencies -----
    list2env(definePassageRates(), envir = .GlobalEnv)
    
  # . Upstream passage efficiencies and migration route -----
    # NOTE: This section is special for the PNR because of multiple routes with
    # unequal numbers of dams and unequal reach lengths
    list2env(annualUpstream(), envir = .GlobalEnv)
    
  # . In-river fishing mortality
    # Define in-river fishing mortalities for 
    # each PU in each of the four
    # possible migration routes
    list2env(fwFishingMort(), envir = .GlobalEnv)

# Starting population structure -----
# Define starting population structure for each simulation
  list2env(startingPop(), envir = .GlobalEnv)
    
# Inner loop -----
  # Run sim for nYears
  for (n in 1:nYears) {
    
    # Assign iterator to a global var so it
    # can be accessed in functions called
    n <<- n
    
    #if (useTictoc) tic(paste("inner loop", n))

    # Remove dynamically named objects from the work space so there are no
    # legacy effects in naming new objects- this could lead to negative
    # population sizes and the like
    rm(list = ls()[grep(ls(), pat = '_')])

    # Set passage efficiency at Weldon (MattaceunkUp, upEffs[[2]][5] & [[4]][6])
    # This code takes the timing scenario, and says "if the year is less than the
    # minimum year of passage implementation at Weldon, set upstream 
    # efficiency to zero"
    list2env(weldonScenarios(), envir = .GlobalEnv)
    
    # Reset the scalar based on population size
    list2env(setScalar(), envir = .GlobalEnv)
    
    # Scale the population
    list2env(scalePop(), envir = .GlobalEnv)

    # If you need to load/reuse inner loop sampling,
    # uncomment/use this stop, then call
    # the inner loop sampling code.
    #stop('halt here for testing')

    # JMS: Perform sampling and save variables for the inner-loop ONE TIME, for repeatability
    # otherwise load the variables we saved...
    if (doInnerSampling) {
      #if (useTictoc) tic("Running inner loop sampling...")
      list2env(innerLoopSampling(), envir = .GlobalEnv)
      #if (useTictoc) toc()
    } else {
      #if (useTictoc) tic("LOADING inner loop sampling...")
      load(innerLoopSamplingRData)
      #Rcpp::sourceCpp('datasets/pnrCppFuns.cpp')        # source C++ functions used in model
      #if (useTictoc) toc()
    }


  # . Process fish and eggs ----
    # Processing of populations generalized and moved into functions. See defineFunctions.R
    # JMS Dec 2017
          
    #if (useTictoc) tic("calculate counts in each PU")
    
    # Make matrices to hold fish
    list2env(populationMatrices(), envir = .GlobalEnv)
    # Fill them in and change them into cohorts
    list2env(processCohorts(), envir = .GlobalEnv)
    ### DSS: still a potential problem here
    
  # . Downstream migration -----
    #if (useTictoc) toc() 
    # post-spawning mortality, downstream migration-
    # order matters not because they are both flat rates,
    # but should probably re-order to make it logical
    list2env(postSpawnMortality(), envir = .GlobalEnv)
  
    # Define downstream migration survival rate matrices
    # and then apply them to calculate the number of adult
    # and juvenile fish surviving to the ocean.
    list2env(downstreamMigration(), envir = .GlobalEnv)
    
  # . The next generation -----
    # next year (after applying ocean survival)
    #if (useTictoc) tic("NEXT GENERATION")
    list2env(nextGeneration(), envir = .GlobalEnv)
    # if (useTictoc) toc()

  # . Store output in pre-allocated vectors -----
    list2env(fillOutputVectors(), envir = .GlobalEnv)

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
