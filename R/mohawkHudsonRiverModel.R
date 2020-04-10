#' Mohawk-Hudson River Model
#' 
#' Runs American shad dam passage performance
#' standard model for Mohawk and Hudson rivers, NY,
#' USA.
#' 
#' @param nRuns The number of times that the
#' model will be run.
#' 
#' @param nYears The number of years for which
#' each run will last. The default is 40 years
#' to match default FERC license duration.
#' 
#' @param timing The amount of time required for
#' upstream passage by individual fish (in days), 
#' where the default (1) indicates a 24-h dam
#' passage performance standard and the value is 
#' specified as a proportion of 1 day.
#' 
#' @param upstream A named list of upstream dam
#' passage efficiencies at each dam in the 
#' Hudson River and its largest tributary, the 
#' Mohawk River.
#' 
#' Users may specify a single value of upstream
#' passage at each dam, or a vector of upstream
#' passage efficiencies at each dam. Note that
#' passage efficiences passed as vectors are 
#' randomly sampled during each model run 
#' (not each year). Therefore, multiple model runs
#' are necessary if more than one passage efficiency
#' is supplied for any dam. As a rough rule of thumb
#' we advise a minimum of 100 runs per combination of
#' management parameters (upstream timing and passage,
#' and downstream survival through dams).
#' 
#' @param downstream A named list of downstream
#' dam passage efficiencies at each dam in the 
#' Mohawk and Hudson rivers including
#' navigational locks. 
#' 
#' See note in \code{upstream}.
#' 
#' @param inRiverF Annual, recreational harvest of 
#' American shad. Parameterized as an annual rate [0, 1].
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
#' for upstream and downstream. If \code{watershed = TRUE}, then
#' the first element in lists \code{upstream} and \code{downstream}
#' are recycled for all subsequent dams.
#'  
#' @return Returns a list of two named dataframes.
#' The first dataframe (\code{res}) contains user-defined
#' inputs and available model outputs. The second dataframe
#' (\code{sens}) contains stochastic model inputs based on
#' empirical data and/or expert opinion.
#'
#' If run in parallel, returns a list of lists
#' of dataframes.
#'
#' The folowing named columns are returned in \code{res}:
#' \itemize{
#'     \item \code{year} Year of simulation
#'     \item \code{timing_lockwood...timing_burnham} Passage timing input by user
#'     \item \code{federal_us...E20_us} User-specified upstream passage efficiencies
#'     \item \code{federal_ds...E20_ds}  User-specified downstream passage efficiencies
#'     \item \code{pRepeat_Age1...pRepeat_Age9} Age-specific probability of repeat spawning  
#'     \item \code{populationSize} Total number of adult spawners returning to the river
#'     \item \code{N_IA...N_XIXB} Production unit-specific population size after in-river fishery mortality
#'     \item \code{pMohawk} Probability of fish using the Mohawk River during upstream migration and spawning.
#' }
#' 
#' The following named columns are returned in \code{sens}:
#' \itemize{
#'     \item \code{S.downstream} Downstream survival per kilometer
#'     \item \code{S.marine} Marine survival as an annual rate
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
# \if{html}{\figure{kennebec.png}{Kennebec River}}
# \if{latex}{\figure{kennebec.png}{options: width=0.5in}}

#' @section Warning about serial execution and memory limits:
#' 
#' Currently, internal functions rely on \code{list2env} to return
#' lists to a temporary environment created in the 
#' \code{mohawkHudsonRiverModel} function. Consequently, lists 
#' that are exported must be limited in size. Therefore, 
#' users currently need to limit the number of runs per 
#' call (\code{nRuns} argument) to less than 10 or R will 
#' hit memory limits quickly. In reality, serial 
#' execution is prohibitively slow unless implemented 
#' using manual parallel processing (e.g., bash scripting).
#' 
#' In order to achieve a desired number of runs for a given
#' set of inputs, the recommended approach is to use 
#' parallel execution as demonstrated using the \code{snowfall}
#' package in the \href{https://shadia-ui.github.io/examples.html}{website examples}.
#' 
# #' @example /inst/examples/sf-exampleKBR.R
#' 
#' @export
#' 
mohawkHudsonRiverModel <- function(
  species = 'shad',
  pMohawk = .5,
  nRuns = 1,
  nYears = 40,
  timing = rep(1, 26),
  upstream = list(
    federal = 1,
    C01 = 1, C02 = 1, C03 = 1, C04 = 1, C05 = 1, C06 = 1,
    E02 = 1, E03 = 1, E04 = 1, E05 = 1, E06 = 1,
    E07 = 1, E08 = 1, E09 = 1, E10 = 1, E11 = 1, E12 = 1,
    E13 = 1, E14 = 1, E15 = 1, E16 = 1, E17 = 1, E18 = 1,
    E19 = 1, E20 = 1
  ),
  downstream = list(
    federal = 1,
    C01 = 1, C02 = 1, C03 = 1, C04 = 1, C05 = 1, C06 = 1,
    E02 = 1, E03 = 1, E04 = 1, E05 = 1, E06 = 1,
    E07 = 1, E08 = 1, E09 = 1, E10 = 1, E11 = 1, E12 = 1,
    E13 = 1, E14 = 1, E15 = 1, E16 = 1, E17 = 1, E18 = 1,
    E19 = 1, E20 = 1
  ),
  inRiverF = 0,
  commercialF = 0,
  bycatchF = 0,
  indirect = 1,
  latent = 1,
  watershed = TRUE
  ){
  
  
# Error message for passage efficiencies
  if( (length(upstream)!=26 ) |  (length(downstream)!=26 ) ){ 
    stop('
         `upstream` must each have 26 elements.')
  }  
  
# Error message for maximum number of years
  if(as.numeric(substr(Sys.time(), start=1, stop=4)) + nYears > 2099){
    stop('
          
          Error:
          The current year plus `nYears`` must not
          exceed 2099 because the models rely on
          climate predictions that are limited to
          that time period.')
  }  
  
# Create package workspace if it does not yet exist  
  if(!exists(".shadia", mode="environment"))
    .shadia <- new.env()  
  
# Assign River
  river <- 'hudson'
  region <- 'Southern Iteroparous'
  
# Choose climate scenario
# Right now, these are set as 'current' in all
# models except Connecticut River. Hidden from
# user because we lack projections from other
# systems.
  climate <- 'current'  
  
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
    upstream and downstream passage rate(s) for
    Federal Dam will be used at all dams in the
    watershed.', '\n', '\n')
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
