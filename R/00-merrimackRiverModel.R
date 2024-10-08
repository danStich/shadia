#' Merrimack River Model
#'
#' American shad dam passage performance
#' standard model Merrimack River, USA
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
#' @param pBypass Probability of using bypass at
#' Pawtucket Dam for passage (alternative is mainstem).
#'
#' @param timing The amount of time required for
#' upstream passage by individual fish (in days),
#' where the default (1) indicates a 24-h dam
#' passage performance standard and the value is
#' specified as a proportion of 1 day.
#'
#' @param upstream A named list of upstream dam
#' passage efficiencies at each dam in the
#' Merrimack River.
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
#' Merrimack River.
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
#' @param downstream_juv A named list of downstream
#' dam passage efficiencies at each dam in the
#' Merrimack River for juveniles.
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
#' @param spatially_explicit_output Whether to return population size in each production unit.
#' 
#' @param output_years Whether to return all years (default = `NULL`) or only
#' final year of each simulation (`"last"`).
#' 
#' @param output_p_repeat A logical indicating whether to return pRepeat by
#' age (in years) with the output. The default value is `FALSE` to
#' limit output size in physical memory.
#'
#' @return Returns a dataframe when sensitivity = FALSE (default).
#' Returns a list of two named dataframes when sensitivity = TRUE.
#' The first dataframe (\code{res}) contains user-defined
#' inputs and available model outputs depending on optional arguments. 
#' The second dataframe (\code{sens}) contains input variables for 
#' sensitivity analysis if desired. If run in parallel, returns a list of
#' lists of dataframes.
#'
#' The following named columns may be returned in \code{res}:
#' 
#' \itemize{
#'     \item \code{year} Year of simulation
#'     \item \code{species} Species used for simulation
#'     \item \code{pBypassUp} User-specified probability of using bypass at Pawtucket Dam for upstream migration
#'     \item \code{pBypassD} User-specified probability of using bypass at Pawtucket Dam for downstream migration
#'     \item \code{timing_essex...timing_hookset} Passage timing input by user
#'     \item \code{essex_us...hookset_us} User-specified upstream passage efficiencies
#'     \item \code{essex_ds...hookset_ds}  User-specified downstream passage efficiencies
#'     \item \code{essex_dsj...hookset_dsj}  User-specified juvenile downstream passage efficiencies
#'     \item \code{F.inRiver} User-specified recreational fishing mortality
#'     \item \code{F.commercial} User-specified recreational fishing mortality
#'     \item \code{F.bycatch} User-specified recreational fishing mortality
#'     \item \code{indirect} User-specified indirect mortality dams
#'     \item \code{latent} User-specified latent mortality
#'     \item \code{pRepeat_Age1...pRepeat_AgeN} Age-specific probability of repeat spawning
#'     \item \code{N_I...N_V} Production unit-specific population size after in-river fishery mortality
#'     \item \code{populationSize} Number of spawners returning to the river
#' }
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
#' Schematic of production units:
#' Production units delineated by dams in the watershed. Circles are log
#' are log proportional to carrying capacity in each unit.
#' Black dots indicate no suitable habitat in a unit.
#'
#' \if{html}{\figure{merrimack.png}{Merrimack River}}
#' \if{latex}{\figure{merrimack.png}{options: width=0.5in}}
#'
#' @section Warning about serial execution and memory limits:
#' Current implementation is based on work
#' in press, and is thus subject to modification
#' without notice.
#'
#' Currently, internal functions rely on \code{list2env} to return
#' lists to a temporary environment created in the
#' \code{merrimackRiverModel} function. Consequently, lists
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
#' example at the bottom of this page.
#'
#' @example /inst/examples/sf-exampleMMR.R
#'
#' @export
merrimackRiverModel <- function(
                                nRuns = 1,
                                species = "shad",
                                nYears = 50,
                                n_adults = 1e4,
                                timing = c(1, 1, 1, 1, 1),
                                upstream = list(
                                  essex = 1,
                                  pawtucketBypass = 1,
                                  pawtucket = 1,
                                  amoskeag = 1,
                                  hookset = 1
                                ),
                                downstream = list(
                                  essex = 1,
                                  pawtucketBypass = 1,
                                  pawtucket = 1,
                                  amoskeag = 1,
                                  hookset = 1
                                ),
                                downstream_juv = list(
                                  essex = 1,
                                  pawtucketBypass = 1,
                                  pawtucket = 1,
                                  amoskeag = 1,
                                  hookset = 1
                                ),
                                pBypassUp = 1,
                                pBypassD = 1,
                                inRiverF = 0,
                                commercialF = 0,
                                bycatchF = 0,
                                indirect = 1,
                                latent = 1,
                                watershed = FALSE,
                                k_method = "cumulative",
                                sensitivity = FALSE,
                                spatially_explicit_output = FALSE,
                                output_years = NULL,
                                output_p_repeat = FALSE
                                ) {

  # Error message for passage efficiencies
  if ((length(upstream) != 5) | (length(downstream) != 5)) {
    stop("`upstream` must have 5 elements and `dowsntream` must also have 5.")
  }

  # Error message for maximum number of years
  if (as.numeric(substr(Sys.time(), start = 1, stop = 4)) + nYears > 2099) {
    stop("
          
          Error:
          The current year plus `nYears` must not
          exceed 2099 because the models rely on
          climate predictions that are limited to
          that time period.")
  }

  # Warning message for watershed applications
  if (watershed) {
    cat("WARNING: when watershed is set to TRUE,
    upstream and downstream passage rate(s) for 
    Essex Dam will be used for all dams.",
      "\n", "\n",
      sep = ""
    )
  }

  # Create package workspace if it does not yet exist
  .shadia <- new.env()

  # Assign species
  .shadia$species <- species

  # Assign River
  .shadia$river <- "merrimack"
  .shadia$region <- "Northern Iteroparous"

  # Choose climate scenario
  # Right now, these are set as 'current' in all
  # models except Connecticut River. Hidden from
  # user because we lack projections from other
  # systems.
  .shadia$climate <- "current"

  # Assign sensitivity option
  .shadia$sensitivity <- sensitivity

  # Passage variable assignment -----

  # Upstream and downstream passage
  ### DSS: would like to re-write this all
  pDraws <- upstream
  dDraws <- downstream
  djDraws <- downstream_juv
  .shadia$pBypassUp <- pBypassUp

  # For watershed applications of
  # the model, all values need to
  # match
  # Adult upstream
  sampU <- sample(pDraws[[1]], 1)
  pDraws <- lapply(
    pDraws,
    function(x) {
      if (watershed) {
        x <- sampU
      }
      else {
        x <- x
      }
    }
  )
  .shadia$up <- as.vector(mapply(sample, pDraws, 1))
  # Adult downstream
  sampD <- sample(dDraws[[1]], 1)
  dDraws <- lapply(
    dDraws,
    function(x) {
      if (watershed) {
        x <- sampD
      }
      else {
        x <- x
      }
    }
  )
  .shadia$d <- as.vector(mapply(sample, dDraws, 1))
  # Juvenile downstream
  sampDj <- sample(djDraws[[1]], 1)
  djDraws <- lapply(
    djDraws,
    function(x) {
      if (watershed) {
        x <- sampDj
      }
      else {
        x <- x
      }
    }
  )
  .shadia$dj <- as.vector(mapply(sample, djDraws, 1))
  # Upstream timing
  timely <- timing

  # Survival reduction due to delay in project head ponds
  delay <- 1


  # Data load and memory pre-allocation -----
  # Pre-allocate output vectors
  environment(defineOutputVectors) <- .shadia
  list2env(defineOutputVectors(), envir = .shadia)

  # Time-invariant system-specific data ----

  # Maximum age
  if (.shadia$species == "shad") {
    .shadia$maxAge <- getMaxAge(region = .shadia$region)
  }
  if (.shadia$species == "blueback") {
    .shadia$maxAge <- 7
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
  if (.shadia$species == "shad") {
    .shadia$spawnRecruit <- getMaturity(region = .shadia$region)
  }
  if (.shadia$species == "blueback") {
    .shadia$spawnRecruit <- c(0, 0.01, 0.48, 0.90, 1, 1, 1)
  }

  ### NEED TO REPLACE WITH UPDATED ESTIMATES
  # Initial probabilities of repeat spawning -
  # will be derived in annual loop after this
  if (.shadia$species == "shad") {
    .shadia$pRepeat <- c(0, 0, 0, 0.03, 0.11, 0.38, 0.87, 1, 1, 1, 1, 1, 1)
  }
  if (.shadia$species == "blueback") {
    .shadia$pRepeat <- c(0, 0, 0.004, 0.28, 0.83, 1, 1)
  }
  # Length-weight regression parameters by region
  # and separated by sex
  ### Not used for shad, and not implemented for BBH
  .shadia$m_lw_params <- length_weight %>% subset(region == "NI" & sex == "M")
  .shadia$f_lw_params <- length_weight %>% subset(region == "NI" & sex == "F")

  # Fishing mortality
  .shadia$commercialF <- rep(commercialF, .shadia$maxAge)
  .shadia$bycatchF <- rep(bycatchF, .shadia$maxAge)
  .shadia$inRiverF <- inRiverF

  # Survival rates for various life-history stages
  # Define ocean survival for each age (1-M from Hoenig 1983 in ASMFC 2007
  # stock assessment). This is now used only to seed the population. All
  # models now use climate-informed mortality estimates for VBGF parameters
  # derived as part of the 2020 ASMFC stock assessment.
  .shadia$downstreamS <- 1 # Survival per km (natural)
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
  .shadia$habitat <- defineHabitat(
    river = .shadia$river,
    nRoutes = .shadia$nRoutes,
    species = .shadia$species,
    k_method = k_method,
    p_up = pBypassUp
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
      nRoutes = .shadia$nRoutes
    )

    # Starting population structure -----
    # Define starting population structure for each simulation
    environment(simStartingPop) <- .shadia
    .shadia$starting_pop <- simStartingPop(
      adults = n_adults,
      .shadia$maxAge,
      .shadia$oceanSurvival,
      .shadia$spawnRecruit
    )
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
      scaled_pop <- scalePop(
        .shadia$pop,
        .shadia$spawningPool,
        .shadia$recruitmentPool,
        .shadia$scalar
      )

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
