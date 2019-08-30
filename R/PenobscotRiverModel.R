#' Penobscot River Model
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
#' to match hydropower license duration in the
#' Penobscot River.
#' 
#' @param timing The amount of time required for
#' upstream passage by individual fish (in days), 
#' where the default (1) indicates a 24-h dam
#' passage performance standard.
#' 
#' @param upstream A named list of upstream dam
#' passage efficiencies at each dam in the 
#' Penobscot River. Stillwater and Orono dams are
#' not included in the list of values because all
#' fish reaching Orono Dam are "trucked" upstream.
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
#' Penobscot River (including Orono and Stillwater
#' dams). 
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
#' @param pinHarvest In-river, sustenance harvest
#' by Penobscot Indian Nation (PIN) upstream of Weldon 
#' Dam. Parameterized as an annual rate [0, 1].
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
#' @return Returns a list of two named dataframes.
#' The first dataframe (\code{res}) contains user-defined
#' inputs and available model outputs.
#'
#' If run in parallel, returns a list of lists
#' of dataframes.
#'
#' The folowing named columns are returned in \code{res}:
#' \itemize{
#'     \item \code{year} Year of simulation
#'     \item \code{time_milford...time_guilford} Passage timing input by user
#'     \item \code{milford_up...guilford_up} User-specified upstream passage efficiencies
#'     \item \code{stillwater_down...weldon_down}  User-specified downstream passage efficiencies
#'     \item \code{pRepeat_Age1...Age9} Age-specific probability of repeat spawning  
#'     \item \code{populationSize} Total number of adult spawners returning to the river
#'     \item \code{N_pu1A2A...N_pu4B} Production unit-specific population size after in-river fishery mortality
#' }
#' 
#' The following named columns are returned in \code{sens}:
#' \itemize{
#'     \item \code{pStillUP} Probability of fish using the Stillwater Branch for upstream migration
#'     \item \code{pStillD} Probability of fish using the Stillwater Branch for downstream migration
#'     \item \code{pPiscUP} Probability of fish using the Piscataquis River during upstream migration
#'     \item \code{S.downstream} Downstream survival per kilometer
#'     \item \code{S.marine} Marine survival (z, instantaneous)
#'     \item \code{popStart} Starting population size
#'     \item \code{p.female} Probability of being female
#'     \item \code{S.prespawnM} Prespawn survival rate for males
#'     \item \code{S.postspawnM} Postspawn survival rate for males
#'     \item \code{S.prespawnF} Postspawn survival rate for males
#'     \item \code{S.postspawnF} Postspawn survival rate for males
#'     \item \code{S.juvenile} Hatch to out-migrant survival rate
#'     \item \code{t.stoch} Temperature stochasticity parameter
#'     \item \code{b.Arr} Mean arrival date for males
#'     \item \code{r.Arr} Mean arrival date for females
#'     \item \code{ATUspawn1} Accumulated thermal units at initiation of spawn
#'     \item \code{ATUspawn2} Accumulated thermal units at termination of spawn
#'     \item \code{Dspawn1} Initial spawning date
#'     \item \code{Dspawn2} Terminal spawning date
#'     \item \code{linF} L-infinity parameter from the von Bertalanffy growth function for females
#'     \item \code{kF} K parameter from the von Bertalanffy growth function for females
#'     \item \code{t0F} t0 parameter from the von Bertalanffy growth function for females
#'     \item \code{linM} L-infinity parameter from the von Bertalanffy growth function for males
#'     \item \code{kM} K parameter from the von Bertalanffy growth function for males
#'     \item \code{t0M} t0 parameter from the von Bertalanffy growth function for males
#'     \item \code{b.length} Mean length of males
#'     \item \code{r.length} Mean length of females
#'     \item \code{spawnInt} Mean spawning interval
#'     \item \code{batchSize} Mean batch size
#'     \item \code{resTime} Mean residence time
#'     \item \code{s.Optim} Mean optimal ground speed
#'     \item \code{d.Max} Mean maximum daily movement rate
#'     \item \code{tortuosity} Path tortuosity parameter
#'     \item \code{motivation} Seasonal change in fish "motivation" for upstream movement
#'     \item \code{daily.move} Mean realized daily movement rate
#'     \item \code{habStoch} Habitat stochasticity
#' }
#' 
#' @section 
#' Schematic of production units:
#' Production units delineated by dams in the watershed. 
#' Circles are log proportional to carrying capacity in 
#' each unit. Black dots indicate no suitable habitat 
#' in a unit. 
#'  
#' \if{html}{\figure{penobscot.png}{Penobscot River}}
#' \if{latex}{\figure{penobscot.png}{options: width=0.5in}}  
#'   
#' @section Warning about serial execution and memory limits:
#' 
#' Currently, internal functions rely on \code{list2env} to return
#' lists to a temporary environment created in the 
#' \code{penobscotRiverModel} function. Consequently, lists 
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
#' @example /inst/examples/sf-examplePNR.R
#' 
#' @export
penobscotRiverModel <- function(
  nRuns = 1,
  nYears = 50,
  timing = list(1,1,1,1,1,1,1),
  upstream = list(
    milford = 1,
    howland = 1,
    westEnfield = 1,
    brownsMill = 1,
    moosehead = 1,
    guilford = 1,
    weldon = 1
  ),
  downstream = list(
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
  pinHarvest = 0,
  inRiverF = 0,
  commercialF = 0,
  bycatchF = 0,
  indirect = 1,
  latent = 1,
  watershed = TRUE
  ){
  
# Error message for passage efficiencies
  if( (length(upstream)!=7 ) |  (length(downstream)!=9 ) ){ 
    stop('`upstream` must have 7 elements and `dowsntream` must have 9.')
  }  
  
# Create package workspace if it does not yet exist  
  if(!exists(".shadia", mode="environment"))
    .shadia <- new.env()  
  
# Assign River
  river <- 'penobscot'
  
# Passage variable assignment -----
  pDraws <- upstream
  dDraws <- downstream
  
  # For watershed applications of
  # the model, all values need to
  # match
  sampU <- sample(pDraws[[1]], 1)
  pDraws <- lapply(pDraws, 
                    function(x){
                      if(watershed){
                        x <- sampU}
                      else
                        {x <- x}
                      }
                    )
  sampD <- sample(dDraws[[1]], 1)
  dDraws <- lapply(dDraws, 
                    function(x){
                      if(watershed){
                        x <- sampD}
                      else
                        {x <- x}
                      }
                    )
  
  if(watershed){
  cat('WARNING: when watershed is set to TRUE,
    upstream passage rate(s) for Milford Dam will
    be used for all upstream passage efficiencies,
    and downstream passage rates for Stillwater Dam
    will be used for all downstream passage
    efficiencies.')
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
