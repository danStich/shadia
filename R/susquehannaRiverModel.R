#' Susquehanna River Model
#' 
#' Runs American shad dam passage performance
#' standard model for Penobscot River, Maine,
#' USA
#' 
#' @param nRuns The number of times that the
#' model will be run.
#' 
#' @param nYears The number of years for which
#' each run will last. The default is 50 years
#' to match common hydropower license duration.
#' 
#' @param timing The amount of time required for
#' upstream passage by individual fish (in days), 
#' where the default (1) indicates a 24-h dam
#' passage performance standard.
#' 
#' @param upstream A named list of upstream dam
#' passage efficiencies at each dam in the 
#' Susquehanna River. Passable dams currently are
#' not included in the list of values.
#' 
#' Users may specify a single value of upstream
#' passage at each dam, or a vector of upstream
#' passage efficiencies at each dam. Note that
#' passage efficiences passed as vectors are 
#' randomly sampled during each model run 
#' (not each year). Therefore, multiple model runs
#' are necessary if more than one passage efficiency
#' is supplied for any dam.
#' 
#' @param downstream A named list of downstream
#' dam passage efficiencies at each dam in the 
#' Susquehanna River. 
#' 
#' Users may specify a single value of downstream
#' passage at each dam, or a vector of downstream
#' passage efficiencies at each dam. Note that
#' passage efficiences passed as vectors are 
#' randomly sampled during each model run 
#' (not each year). Therefore, multiple model runs
#' are necessary if more than one passage efficiency
#' is supplied for any dam.
#' 
#' @param inRiverF Annual, recreational harvest of 
#' American shad downstream of Weldon Dam. 
#' Parameterized as an annual rate [0, 1].
#'
#' @param commercialF Commercial fishery mortality
#' for American shad in marine environment incurred 
#' through targeted fisheries. Parameterized as an 
#' annual rate [0, 1].
#'
#' @param bycatchF Marine bycatch mortality of
#' American shad in non-target fisheries. 
#' Parameterized as an annual rate [0, 1].
#' 
#' @param indirect Indirect mortality incurred during
#' freshwater migration as a result of dam-related
#' impacts (e.g., injury, predation, etc.).
#' 
#' @param latent Latent mortality incurred during estuary
#' passage as a result of dam-related impacts (e.g., injury,
#' delay, etc.).
#' 
#' @param watershed A logical indicating whether or not
#' to use the same dam passage efficiencies at all dams
#' for upstream and downstream. If watershed = TRUE, then
#' the first element in lists `upstream` and `downstream`
#' are recycled for all subsequent dams.
#'  
#' @return Returns a dataframe of user-defined
#' inputs and available model outputs for each use that 
#' contains \code{nRuns} x \code{nYears} number of rows. 
#' 
#' If run in parallel, returns a list
#' of dataframes.
#' 
#' The folowing named columns are returned:
#' \itemize{
#'     \item \code{year} Year of simulation
#'     
#'     \item \code{populationSize} Total number of adult spawners returning to the river
#'     
#'     \item \code{N_1A...N_9A} Production unit-specific population size after in-river fishery mortality
#' 
#'     \item \code{conowingo_up...colliersville_up} User-specified upstream passage efficiencies
#'  
#'     \item \code{conowingo_down...cooperstown_down}  User-specified downstream passage efficiencies
#'     
#'     \item \code{time} Passage timing input by user
#'     
#'     \item \code{pRepeat_Age1...Age11} Age-specific probability of repeat spawning  
#' }
#' 
#' @section Warning about serial execution and memory limits:
#' Current implementation is based on work
#' in review, and is thus subject to modification 
#' without notice. 
#' 
#' Currently, internal functions rely on \code{list2env} to return
#' lists to a temporary environment created in the 
#' \code{susquehannaRiverModel} function. Consequently, lists 
#' that are exported must be limited in size. Therefore, 
#' users currently need to limit the number of runs per 
#' call (\code{nRuns} argument) to less than 10 or R will 
#' hit memory limits quickly. In reality, serial 
#' execution is prohibitively slow unless implemented 
#' using manual parallel processing (e.g., bash scripting).
#' 
#' In order to achieve a desired number of runs for a given
#' set of inputs, the recommended approach is to use 
#' parallel execution as demonstrated using snowfall in the
#' example below.
#' 
#' @example /inst/examples/sf-exampleSSR.R
#' 
#' @export
susquehannaRiverModel <- function(
  nRuns = 1,
  nYears = 50,
  timing = 1,
  upstream = list(
    conowingo = 1,
    holtwood = 1,
    safeHarbor = 1,
    yorkHaven = 1,
    sunbury = 1,
    williamsport = 1,
    lockhaven = 1,
    rockbottom = 1,
    chasehibbard = 1,
    colliersville = 0
  ),
  downstream = list(
    conowingo = 1,
    holtwood = 1,
    safeHarbor = 1,
    yorkHaven = 1,
    sunbury = 1,
    williamsport = 1,
    lockhaven = 1,
    rockbottom = 1,
    chasehibbard = 1,    
    colliersville = 1
  ),
  inRiverF = 0,
  commercialF = 0,
  bycatchF = 0,
  indirect = 1,
  latent = 1,
  watershed = TRUE
  ){
  
# Error message for passage efficiencies
  if( (length(upstream)!=10 ) |  (length(downstream)!=10 ) ){ 
    stop('`upstream` must have 10 elements and `dowsntream` must have 10.')
  }  
  
# Create package workspace if it does not yet exist  
  if(!exists(".shadia", mode="environment"))
    .shadia <- new.env()  
  
# Assign River
  river <- 'susquehanna'
  
# Passage variable assignment -----
  pDraws <- upstream
  dDraws <- downstream
  
  # For watershed applications of
  # the model, all values need to
  # match
  pDraws <- lapply(pDraws, 
                    function(x){
                      if(watershed){
                        x <- pDraws[[1]]}
                      else
                        {x <- x}
                      }
                    )
  
  dDraws <- lapply(dDraws, 
                    function(x){
                      if(watershed){
                        x <- dDraws[[1]]}
                      else
                        {x <- x}
                      }
                    )
  
  if(watershed){
  cat('WARNING: when watershed is set to TRUE,
    upstream passage rate(s) for Conowingo Dam will
    be used for all upstream and downstream
    passage efficiencies.')
  }
  
# Set parameters -----
  environment(setParameters) <- .shadia
  list2env(setParameters(), envir = .shadia)

  
# Data load and memory pre-allocation -----
  if (.shadia$useTictoc) tic("Running data load...")
  
  environment(setUpData) <- .shadia
  list2env(setUpData(), envir = .shadia)
  
  environment(defineOutputVectors) <- .shadia
  list2env(defineOutputVectors(), envir = .shadia)
  
  if (.shadia$useTictoc) toc()

# Hydro system configuration -----
  environment(defineHydroSystem) <- .shadia
  list2env(defineHydroSystem(), envir = .shadia)
  environment(defineHabitat) <- .shadia
  list2env(defineHabitat(), envir = .shadia)


# Timers and progress -----
  # Start the timer for the simulation
  ptmSim <- proc.time()
  
  # Progress meter
  if (.shadia$useProgress) {
    pb <-
      txtProgressBar(
        min = 0,
        max = nRuns * nYears,
        style = 3,
        char = '+'
      )
  }
  
  if (.shadia$useTictoc) tic("total time")


# SIMULATION SETTINGS FOR OUTER LOOP -----
  # Outer loop for number of simulations- 
  # this is how many runs it will do
  for (k in 1:nRuns) {
    .shadia$k <- k
    if (.shadia$useTictoc) tic(paste("OUTER loop", .shadia$k))
  
    # JMS: load the saved sampled variables for repeatability:
    # JMS: Perform sampling and save variables for the outer-loop ONE TIME, for repeatability
    # otherwise, load previously saved variables
    if (.shadia$doOuterSampling) {
      #if (useTictoc) tic("Running outer loop sampling...")
      environment(outerLoopSampling) <- .shadia
      list2env(outerLoopSampling(), envir = .shadia)
      #if (useTictoc) toc()
    } else {
      #if (useTictoc) tic("LOADING outer loop sampling...")
      load(outerLoopSamplingRData)
      #if (useTictoc) toc()
    }

      
  # . Dam passage efficiencies -----
    environment(definePassageRates) <- .shadia
    list2env(definePassageRates(), envir = .shadia)

    
  # . Upstream passage efficiencies and migration route -----
    # NOTE: This section is special for the PNR because of multiple routes with
    # unequal numbers of dams and unequal reach lengths
    environment(annualUpstream) <- .shadia
    list2env(annualUpstream(), envir = .shadia)


  # . In-river fishing mortality
    # Define in-river fishing mortalities for 
    # each PU in each of the four
    # possible migration routes
    environment(fwFishingMort) <- .shadia
    list2env(fwFishingMort(), envir = .shadia)
    
        
# Starting population structure -----
# Define starting population structure for each simulation
  environment(startingPop) <- .shadia
  list2env(startingPop(), envir = .shadia)
  
    
# Inner loop -----
  # Run sim for nYears
  for (n in 1:nYears) {
    
    # Assign iterator to a var so it
    # can be accessed in functions called
    .shadia$n <- n
    
    #if (useTictoc) tic(paste("inner loop", n))

    # Remove dynamically named objects from the work space so there are no
    # legacy effects in naming new objects- this could lead to negative
    # population sizes and the like
    #rm(list = ls(.shadia)[grep(ls(.shadia), pat = '_')])

    # Set passage efficiency at Weldon 
    # (MattaceunkUp, upEffs[[2]][5] & [[4]][6])
    # This code takes the timing scenario, 
    # and says "if the year is less than the
    # minimum year of passage implementation at 
    # Weldon, set upstream 
    # efficiency to zero"
    environment(weldonScenarios) <- .shadia
    list2env(weldonScenarios(), envir = .shadia)
    
    # Reset the scalar based on population size
    environment(setScalar) <- .shadia
    list2env(setScalar(), envir = .shadia)
    
    # Scale the population
    environment(scalePop) <- .shadia
    list2env(scalePop(), envir = .shadia)

    # If you need to load/reuse inner loop sampling,
    # uncomment/use this stop, then call
    # the inner loop sampling code.
    #stop('halt here for testing')

    # JMS: Perform sampling and save variables 
    # for the inner-loop ONE TIME, for repeatability
    # otherwise load the variables we saved...
    if (.shadia$doInnerSampling) {
      #if (useTictoc) tic("Running inner loop sampling...")
      environment(innerLoopSampling) <- .shadia
      list2env(innerLoopSampling(), envir = .shadia)
      #if (useTictoc) toc()
    } else {
      #if (useTictoc) tic("LOADING inner loop sampling...")
      load(.shadia$innerLoopSamplingRData)
      #if (useTictoc) toc()
    }

    
  # . Process fish and eggs ----
    # Processing of populations generalized and
    # moved into functions. See defineFunctions.R
    # JMS Dec 2017
          
    #if (useTictoc) tic("calculate counts in each PU")
    
    # Make matrices to hold fish
    environment(populationMatrices) <- .shadia
    list2env(populationMatrices(), envir = .shadia)

    # Fill them in and change them into cohorts
    environment(processCohorts) <- .shadia
    list2env(processCohorts(), envir = .shadia)
    
    
  # . Downstream migration -----
    #if (useTictoc) toc() 
    # Post-spawning mortality
    environment(postSpawnMortality) <- .shadia
    list2env(postSpawnMortality(), envir = .shadia)

    # Define downstream migration survival rate matrices
    # and then apply them to calculate the number of adult
    # and juvenile fish surviving to the ocean.
    environment(downstreamMigration) <- .shadia
    list2env(downstreamMigration(), envir = .shadia)

    
  # . The next generation -----
    # next year (after applying ocean survival)
    #if (useTictoc) tic("NEXT GENERATION")
    environment(nextGeneration) <- .shadia
    list2env(nextGeneration(), envir = .shadia)
    # if (useTictoc) toc()
    

  # . Store output in pre-allocated vectors -----
    environment(fillOutputVectors) <- .shadia
    list2env(fillOutputVectors(), envir = .shadia)

  # PROGRESS METER UPDATE
    if (.shadia$useProgress) {
      Sys.sleep(0.001) # System time-out for update
      setTxtProgressBar(pb, (n + nYears * (k - 1))) # Calculates % completed
    }

  #if (useTictoc) toc() # inner loop
  } # Year loop

  if (.shadia$useTictoc) toc() # outer loop
  
} # Simulation loop

# Write the simulation results to an object
# that can be returned to workspace
  environment(writeData) <- .shadia
  writeData()

# TIMING RESULTS FOR SIMULATION BENCHMARKING ------------------------------
# This section uses the timing prompts from earlier in the script to calculate
# the total run time for the simulation.
# if (useTictoc) toc() #"total"
# 
# simTime <- proc.time() - ptmSim
# print(' ')
# #print('timeABM')
# #print.proc_time(timeABM)
# #print('timeDelay')
# #print.proc_time(timeDelay)
# print('simTime')
# print.proc_time(simTime)

}
