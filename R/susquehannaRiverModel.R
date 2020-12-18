#' Susquehanna River Model
#' 
#' Dam passage performance standard model for 
#' Susquehanna River, USA
#' 
#' @param nRuns The number of times that the
#' model will be run.
#' 
#' @param species Species for which the model will be
#' run. Current options include American \code{'shad'} and
#' \code{'blueback'} herring.
#' 
#' @param nYears The number of years for which
#' each run will last. The default is 40 years
#' to match default FERC license duration.
#' 
#' @param n_adults Number of starting adults in 
#' population.
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
#' passage efficiencies passed as vectors are 
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
#' passage efficiencies passed as vectors are 
#' randomly sampled during each model run 
#' (not each year). Therefore, multiple model runs
#' are necessary if more than one passage efficiency
#' is supplied for any dam.
#' 
#' @param downstream_juv A named list of downstream
#' dam passage efficiencies at each dam in the 
#' Susquehanna River for juveniles. 
#' 
#' @param inRiverF Annual, recreational harvest in river. 
#' Parameterized as an annual rate [0, 1].
#'
#' @param commercialF Commercial fishery mortality
#' in marine environment incurred through targeted 
#' fisheries. Parameterized as an annual rate [0, 1].
#'
#' @param bycatchF Marine bycatch mortality of
#' species in non-target fisheries. 
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
#' the first element in lists `upstream`, `downstream`,
#' and `downstream_juv` are recycled for all subsequent dams.
#'  
#' @param k_method Method used to impose carrying capacity. The 
#' default, `cumulative`, assumes that carrying capacity is based on 
#' all available habitat through the most upstream occupied production 
#' units in a given migration route. The alternative, 'discrete' assumes
#' that carrying capacity is applied within discrete production units 
#' based on the numbers, and was the method used in Stich et al. (2019).  
#'  
#' @param sensitivity Whether to return a dataframe for sensitivity
#' analysis. The default is set to FALSE for faster run time and smaller
#' memory load in parallel processing.  
#'  
#' @return Returns a dataframe when sensitivity = FALSE (default). 
#' Returns a list of two named dataframes when sensitivity = TRUE.
#' The first dataframe (\code{res}) contains user-defined
#' inputs and available model outputs. The second dataframe (\code{sens})
#' contains input variables for sensitivity analysis if desired.
#'
#' If run in parallel, returns a list of lists of dataframes.
#'
#' The following named columns are returned in \code{res}:
#' \itemize{
#'     \item \code{year} Year of simulation
#'     \item \code{populationSize} Total number of adult spawners returning to the river
#'     \item \code{N_1A...N_10A} Production unit-specific population size after in-river fishery mortality
#'     \item \code{conowingo_up...colliersville_up} User-specified upstream passage efficiencies
#'     \item \code{conowingo_down...colliersville_down}  User-specified downstream passage efficiencies
#'     \item \code{time_conowingo...time_colliersville} Passage timing input by user
#'     \item \code{pRepeat_Age1...Age11} Age-specific probability of repeat spawning  
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
#' Production units by migration route:
#' \itemize{
#'   \item Juniata River
#'     \itemize{
#'       \item \code{PU 1A} Downstream of Conowingo
#'       \item \code{PU 2A} Conowingo to Holtwood
#'       \item \code{PU 3A} Holtwood to SafeHarbor
#'       \item \code{PU 4A} SafeHarbor to YorkHaven
#'       \item \code{PU 5A} YorkHaven  to Sunbury
#'       \item \code{PU 1B} JuniataRiver to WarriorRidge
#'     }
#'   \item West Branch
#'     \itemize{
#'       \item \code{PU 1A} Downstream of Conowingo
#'       \item \code{PU 2A} Conowingo to Holtwood
#'       \item \code{PU 3A} Holtwood to SafeHarbor
#'       \item \code{PU 4A} SafeHarbor to YorkHaven
#'       \item \code{PU 5A} YorkHaven  to Sunbury
#'       \item \code{PU 1C} Sunbury to Williamsport
#'       \item \code{PU 2C} Williamsport to LockHaven
#'       \item \code{PU 3C} LockHaven to Shawville
#'     }
#'   \item Chemung River
#'     \itemize{
#'       \item \code{PU 1A} Downstream of Conowingo
#'       \item \code{PU 2A} Conowingo to Holtwood
#'       \item \code{PU 3A} Holtwood to SafeHarbor
#'       \item \code{PU 4A} SafeHarbor to YorkHaven
#'       \item \code{PU 5A} YorkHaven  to Sunbury
#'       \item \code{PU 6A} Sunbury to NyLine
#'       \item \code{PU 1D} NyLine to PaLine
#'       \item \code{PU 2D} Upstream Chase-Hibbard
#'     }
#'   \item North Branch
#'     \itemize{
#'       \item \code{PU 1A} Downstream of Conowingo
#'       \item \code{PU 2A} Conowingo to Holtwood
#'       \item \code{PU 3A} Holtwood to SafeHarbor
#'       \item \code{PU 4A} SafeHarbor to YorkHaven
#'       \item \code{PU 5A} YorkHaven to Sunbury
#'       \item \code{PU 6A} Sunbury to NyLine
#'       \item \code{PU 7A} NyLine to RockBottom
#'       \item \code{PU 8A} RockBottom to Unadilla
#'       \item \code{PU 9A} Unadilla to Colliersville
#'       \item \code{PU 10A} Colliersville to Cooperstown
#'   }
#' }
#' 
#' 
#' @section 
#' Schematic of production units:
#' Production units delineated by dams in the watershed. Circles are log 
#' are log proportional to carrying capacity in each unit.
#' Black dots indicate no suitable habitat in a unit. 
#'  
#' \if{html}{\figure{susquehanna.png}{Susquehanna River}}
#' \if{latex}{\figure{susquehanna.png}{options: width=0.5in}}  
#' 
#' @section Warning about serial execution and memory limits:
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
  species = 'shad',
  nYears = 50,
  n_adults = 1e4,
  timing = c(1,1,1,1,1,1,1,1,1,1),
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
    colliersville = 1
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
  downstream_juv = list(
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
  watershed = FALSE,
  k_method = 'cumulative',
  sensitivity = FALSE
  ){
  
# Error message for passage efficiencies
if( (length(upstream)!=10 ) |  (length(downstream)!=10 ) ){ 
  stop('`upstream` must have 10 elements and `dowsntream` must have 10.')
}  
  
# Error message for maximum number of years
if(as.numeric(substr(Sys.time(), start=1, stop=4)) + nYears > 2099){
  stop('
        
        Error:
        The current year plus nYears must not
        exceed 2099 because the models rely on
        climate predictions that are limited to
        that time period.')
}
    
# Warning message about watershed settings and
# implications
if(watershed){
cat('WARNING: when watershed is set to TRUE,
  upstream passage rate(s) for Conowingo Dam will
  be used for all upstream and downstream
  passage efficiencies.')
}
  
  
# Set internal variables ----
# Create hidden workspace
  .shadia <- new.env()  
  
# Unlist function args to internal environment
  list2env(mget(names(formals(susquehannaRiverModel))), envir=.shadia)  
  
# Assign species
  .shadia$species <- species
  
# Assign River
  .shadia$river <- 'susquehanna'
  
# Get corresponding life-history region  
  .shadia$region <- 'Southern Iteroparous'
    
# Choose climate scenario
# Right now, these are set as 'current' in all
# models except CTR & PNR. Hidden from
# user because we lack projections from other
# systems.
  .shadia$climate <- 'current'

# Assign sensitivity option
.shadia$sensitivity <- sensitivity
    
# Passage variable assignment -----
  
  # Draw probability of using each passage route,
  # conditional on amount of habitat in each route
  .shadia$p_JuniataUp <- 0.206262
  .shadia$p_WestBranchUp <- 0.2195443
  .shadia$p_ChemungUp <- 0.2719814
  .shadia$p_NorthBranchUp <- 0.3022123  
  
  # Upstream and downstream passage
  ### DSS: would like to re-write this all
  pDraws <- upstream
  dDraws <- downstream
  djDraws <- downstream_juv
  
  # For watershed applications of
  # the model, all values need to
  # match
  # Adult upstream
  sampU <- sample(pDraws[[1]], 1)
  pDraws <- lapply(pDraws, 
                    function(x){
                      if(watershed){
                        x <- sampU}
                      else
                        {x <- x}
                      }
                    )
  .shadia$up <- as.vector(mapply(sample, pDraws, 1))
  # Adult downstream
  sampD <- sample(dDraws[[1]], 1)
  dDraws <- lapply(dDraws, 
                    function(x){
                      if(watershed){
                        x <- sampD}
                      else
                        {x <- x}
                      }
                    )
  .shadia$d <- as.vector(mapply(sample, dDraws, 1))
  # Juvenile downstream
  sampDj <- sample(djDraws[[1]], 1)
  dDjraws <- lapply(djDraws, 
                    function(x){
                      if(watershed){
                        x <- sampDj}
                      else
                        {x <- x}
                      }
                    )
  .shadia$dj <- as.vector(mapply(sample, djDraws, 1))  
  # Upstream timing
  timely <- timing

  # Survival reduction due to delay in project head ponds
  delay <- 1



  
# Memory pre-allocation and data load -----
  # Pre-allocate output vectors
  environment(defineOutputVectors) <- .shadia
  list2env(defineOutputVectors(), envir = .shadia)

# Time-invariant system-specific data ----

  # Maximum age
  if(.shadia$species == "shad"){
    .shadia$maxAge <- getMaxAge(region = .shadia$region)
  }
  if(.shadia$species == "blueback"){
    .shadia$maxAge <- 12
  }
  
  # Assign the starting population based on a seed of
  # age-1 fish and application of an ocean survival curve
  # The population size is scaled to make the models
  # run faster. Output is re-scaled
  ### DSS: This will be removed with new population
  ###      seed based on number of spawners
  .shadia$Age1 <- rpois(1, 1e4)  
  ### COME BACK AND REMOVE THIS WHEN READY. POP IS 
  ### NOW INITIALIZED BY NUMBER OF ADULTS
  
  # Define probability of recruitment to spawn
  # using regional estimates from ASMFC (2020)
  if(.shadia$species == "shad"){
    .shadia$spawnRecruit <- getMaturity(region = .shadia$region)
  }
  if(.shadia$species == "blueback"){
    .shadia$spawnRecruit <- c(0, 0.009, 0.48, 0.90, 1, 1, 1, 1, 1, 1, 1, 1)
  }
  
  ### NEED TO REPLACE WITH UPDATED ESTIMATES
  # Initial probabilities of repeat spawning - 
  # will be derived in annual loop after this
  if(.shadia$species == "shad"){  
  .shadia$pRepeat <- c(0, 0, 0, 0.03, 0.11, 0.38, 0.87, 1, 1, 1, 1, 1, 1)
  }
  if(.shadia$species == "blueback"){
    .shadia$pRepeat <- c(0, 0, 0.004, 0.21, 0.67, 1, 1, 1, 1, 1, 1, 1)
  } 
    
  # Length-weight regression parameters by region
  # and separated by sex
  ### Not used for shad, and not implemented for BBH
  .shadia$m_lw_params <- length_weight %>% subset(region == 'SI' & sex=='M')
  .shadia$f_lw_params <- length_weight %>% subset(region == 'SI' & sex=='F')
  
  # Fishing mortality
  .shadia$commercialF <- rep(commercialF, .shadia$maxAge)
  .shadia$bycatchF <- rep(bycatchF, .shadia$maxAge)    
  
  # Survival rates for various life-history stages
  # Define ocean survival for each age (1-M from Hoenig 1983 in ASMFC 2007
  # stock assessment). This is now used only to seed the population. All
  # models now use climate-informed mortality estimates for VBGF parameters
  # derived as part of the 2020 ASMFC stock assessment.
  .shadia$downstreamS <- 1                                 # Survival per km (natural)
  .shadia$oceanSurvival <- rep(rbeta(1, 320, 400), .shadia$maxAge)
  
  # Hydro system configuration
  environment(defineHydroSystem) <- .shadia
  .shadia$hydro_out <- defineHydroSystem(river = .shadia$river)
  .shadia$nRoutes <- .shadia$hydro_out$nRoutes
  .shadia$nDams <- .shadia$hydro_out$nDams
  .shadia$maxrkm <- .shadia$hydro_out$maxrkm
  .shadia$damRkms <- .shadia$hydro_out$damRkms
  .shadia$nPU <- .shadia$hydro_out$nPU
  
  # Habitat numbers and configuration
  .shadia$habitat <- defineHabitat(river = .shadia$river, 
                                   nRoutes = .shadia$nRoutes,
                                   species = .shadia$species
                                   )
    
  # Temperature data (daily averages by year)
  environment(setUpTemperatureData) <- .shadia
  .shadia$mu <- setUpTemperatureData(river = .shadia$river) 
  

### Can the outer loop be eliminated?  
# Outer loop for number of simulations (nRuns) ----
# this is how many runs it will do for each nYears
for (k in 1:nRuns) {
  .shadia$k <- k

### NONE OF THIS RELIES ON LOOPING ANYMORE  
  # Dam passage efficiencies
  environment(definePassageRates) <- .shadia
  list2env(definePassageRates(.shadia$river), envir = .shadia)
  ### WILL THROW GLOBAL BINDING NOTES ON CHECK
  
  # Upstream passage efficiencies and migration route
  environment(annualUpstream) <- .shadia
  .shadia$ann_up <- annualUpstream(.shadia$river, .shadia$damRkms)
  .shadia$times <- .shadia$ann_up$times
  .shadia$upEffs <- .shadia$ann_up$upEffs

  # Define in-river fishing mortality for 
  # each PU in each of the four
  # possible migration routes
  environment(fwFishingMort) <- .shadia
  .shadia$inriv <- fwFishingMort(.shadia$inRiverF, 
                                 river = .shadia$river,
                                 nRoutes = .shadia$nRoutes)
    
  # Starting population structure -----
  # Define starting population structure for each simulation
  environment(startingPop) <- .shadia
  .shadia$starting_pop <- simStartingPop(adults = .shadia$n_adults, 
                                         .shadia$maxAge, 
                                         .shadia$oceanSurvival,
                                         .shadia$spawnRecruit)
  .shadia$pop <- .shadia$starting_pop$pop
  .shadia$spawningPool <- .shadia$starting_pop$spawningPool
  .shadia$recruitmentPool <- .shadia$starting_pop$recruitmentPool

  
  ### THIS STAYS IN AS A LOOP UNLESS CHANGED INTERNALLY
  # Inner loop -----
    # Run sim for nYears
    for (n in 1:nYears) {
      
      # Assign iterator to a var so it
      # can be accessed in functions called
      .shadia$n <- n
      
      # Reset the scalar based on population size
      .shadia$scalar <- setScalar(.shadia$spawningPool)
      
      # Scale the population
      scaled_pop <- scalePop(.shadia$pop,
                             .shadia$spawningPool, 
                             .shadia$recruitmentPool, 
                             .shadia$scalar)
      
     .shadia$pop <- scaled_pop[[1]]
     .shadia$spawningPool <- scaled_pop[[2]]
     .shadia$recruitmentPool <- scaled_pop[[3]]     

      # Perform innerLoopSampling
      environment(innerLoopSampling) <- .shadia
      list2env(innerLoopSampling(.shadia$habitat), envir = .shadia)
  
      # Process fish and eggs
      # Make matrices to hold fish
      environment(populationMatrices) <- .shadia
      list2env(populationMatrices(), envir = .shadia)
  
      # Fill them in and change them into cohorts
      environment(processCohorts) <- .shadia
      list2env(processCohorts(), envir = .shadia)
      
    # Downstream migration
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
      environment(nextGeneration) <- .shadia
      list2env(nextGeneration(), envir = .shadia)

    # . Store output in pre-allocated vectors -----
      environment(fillOutputVectors) <- .shadia
      list2env(fillOutputVectors(), envir = .shadia)
  
    } # Year loop

} # Simulation loop

# Write the simulation results to an object
# that can be returned to workspace
  environment(writeData) <- .shadia
  writeData()

}
