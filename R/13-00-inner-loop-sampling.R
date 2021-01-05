#' @title Stochastic sampling of annual variables
#'
#' @description Internal function used to perform all
#' annually based stochastic sampling for the
#' individual-based upstream migration model.
#'
#' Not intended to be called directly, but visible
#' for the sake of model transparency.
#'
#' @export
#'
innerLoopSampling <- function(habitat) {


  # Habitat stochasticity -----
  # Add stochasticity to habitat per PU
  habStoch <- runif(1, 0.95, 1.05)

  # Add stochastic noise to habitat variable
  # to assess sensitivity and account for
  # fluctuations
  habitat <- addStochList(habitat, habStoch)


  # Daily in-river climate data ----
  # Use historical temperature data to predict temperature
  # on each day from a multivariate normal distribution
  predTemps <- simDailyTemperatures(mu, climate, river, current_year, n)

  # Calculate ATU for each day of simulated temperature data
  newTU <- cumsum(predTemps[, 2])


  # Create fish ----
  # Draw sex ratio for the current year
  sexRatio <- rbeta(1, 100, 100)

  # Create an object containing the age of individual fish
  # based on the number of fish in each age class and build
  age_df <- cbind(spawningPool, seq(1, length(spawningPool), 1))
  c_fishAges <- do.call("c", (mapply(rep, c(age_df[, 2]), age_df[, 1])))

  # Assign fish gender using sex ratio drawn above, females are 1
  c_sex <- rbinom(length(c_fishAges), 1, sexRatio)

  # Need to sort by sex to get sex-specific arrival date efficiently
  c_fishAges <- c_fishAges[order(c_sex)] # First fish ages
  c_sex <- sort(c_sex) # Then fish sex


  # Entry dates ----
  # Get entry date for each individual.  We used cumulative frequency distribution
  # to predict average entry date for a given fish conditional on temperature.
  # We will now use this relationship to predict individual
  # fish presence in the river for all 366 days of the year.
  # The actual draws for each fish on each day are occurring in a C++ file
  # (src/entryC.Cpp) to speed this up.  Once we have
  # probability of being in the river on a given day, we will use the first date
  # that maximizes probability of success in a random binomial draw for each
  # individual to assign their entry date. We use a size of 15 for this
  # draw to avoid spurious arrivals in January in northeast rivers.

  # Predict arrival probability on a given date using
  # stored regression coefficients sampled randomly from
  # bootstrapped fits of cumulative arrival in the CTR by
  # as a function of temperature
  res.R <- data.frame(arr.R[[sample(1:length(arr.R), 1)]])
  res.B <- data.frame(arr.B[[sample(1:length(arr.B), 1)]])
  r.prob <- invlogit(res.R[1, 1] + res.R[2, 1] * predTemps[, 2])
  b.prob <- invlogit(res.B[1, 1] + res.B[2, 1] * predTemps[, 2])

  # Make containers to hold entry date in entryC.
  b.entryDate <- matrix(0, nrow = length(c_fishAges[c_sex == 0]), ncol = length(b.prob))
  r.entryDate <- matrix(0, nrow = length(c_fishAges[c_sex == 1]), ncol = length(r.prob))
  # toc()

  b.entry <- entryC(b.prob, b.entryDate, length(c_fishAges[c_sex == 0]))
  r.entry <- entryC(r.prob, r.entryDate, length(c_fishAges[c_sex == 1]))

  # We initialize with drop = FALSE just in case there are
  # no individuals left
  entry <- rbind(b.entry, r.entry, drop = FALSE)

  # Remove last row to undo effect of drop=FALSE above if
  # there are any individuals left. Need to cast as a matrix
  # in case there is only one row left, in which case R tries
  # to default to vector
  if (nrow(entry) > 1) { # we are altering nrow inside the loop...dirty!!
    entry <- matrix(entry[1:(nrow(entry) - 1), ], ncol = nrow(predTemps))
  }
  # if we have changed the object above
  entryRowsNew <- nrow(entry)

  # Added conditional in vector pre-allocation below.
  # We do a check to see if there are any fish left.
  c_entryDate <- vector(mode = "numeric", length = max(1, entryRowsNew, na.rm = T))
  c_entryDate <- apply(
    entry, 1, function(x) {
      min(which(x == max(x, na.rm = TRUE)))
    }
  )


  # Spawning dates ----
  # Draw initial and terminal spawning dates based on daily temperature
  # Randomly draw ATU for initiation (1) and termination (2) of spawning for each
  c_spawnATU1 <- rnorm(length(c_entryDate), 750, 50)
  c_spawnATU2 <- rnorm(length(c_entryDate), 1250, 50)

  # Determine on which day the threshold ATU for each individual is reached
  # Pre-allocate vectors
  c_initial <- vector(mode = "numeric", length = length(c_entryDate))
  c_end <- vector(mode = "numeric", length = length(c_entryDate))

  # Now get initial and end dates based on ATU calculated from temperature sim
  for (i in 1:length(c_entryDate)) {
    # Initiation of spawn
    c_initial[i] <- c_entryDate[i] +
      which(cumsum(predTemps[c_entryDate[i]:nrow(predTemps), 2]) >=
        c_spawnATU1[i])[1]
    # Termination of spawn
    c_end[i] <- c_entryDate[i] +
      which(cumsum(predTemps[c_entryDate[i]:nrow(predTemps), 2]) >=
        c_spawnATU2[i])[1]
  }

  # Define the day of the year as an ordinal date
  day <- c(seq(min(c_initial), (max(c_end))))

  # Photoperiod
  photo <- getPhotoperiod(river, day)

  # Fish characteristics ----
  # Growth parameters
  # Roes
  # Get sex-specific, regional growth parameters
  environment(simGrowth) <- .shadia
  r.mat <- simGrowth(region, female = TRUE)

  # Rename them for ease of use and readability on output
  c_linF <- r.mat[1] # L-infinity females
  c_kF <- r.mat[2] # Brody growth coeff females
  c_t0F <- r.mat[3] # Intercept of VBGM females

  # Males
  # Get sex-specific, regional growth parameters
  b.mat <- simGrowth(region, female = FALSE)

  # Rename them for ease of use and readability on output
  c_linM <- b.mat[1] # L-infinity males
  c_kM <- b.mat[2] # Brody growth coeff males
  c_t0M <- b.mat[3] # Intercept of VBGM males

  # Length
  # Get columns representing logical for male and female
  c_female <- c_sex
  c_male <- as.numeric(factor(c_sex, levels = c(1, 0))) - 1
  # Calculate length of males
  c_male_lf <- c_male * c_linM * (1 - exp(-c_kM * (c_fishAges - c_t0M)))
  # Calculate length of females
  c_female_lf <- c_female * c_linF * (1 - exp(-c_kF * (c_fishAges - c_t0F)))
  # Collect fork length into one column
  c_forkLength <- c_male_lf + c_female_lf

  # Calculate fecundity
  # Calculate residence time for each fish based on entry date and exit date
  c_RT <- c_end - c_initial
  c_RT[c_RT < 1] <- 1
  # Get spawning interval for each fish
  c_SI <- rnorm(length(c_fishAges), 2.493, 0.274)
  # Get probability of repeat spawning
  c_repeat <- rbinom(length(c_fishAges), 1, pRepeat[c_fishAges])
  # Get random draws for fecundity based on whether or not fish are repeat
  # spawners.
  c_BF <- vector(mode = "numeric", length = length(c_repeat))

  # American shad batch fecundity from Hyle et al. (2014), McBride et al. (2016)
  if (species == "shad") {
    c_BF[c_repeat == 0] <- sample(
      MASS::rnegbin(10000, 20000, 10),
      length(c_repeat[c_repeat == 0]),
      replace = TRUE
    )

    c_BF[c_repeat == 1] <- sample(
      MASS::rnegbin(10000, 30000, 10),
      length(c_repeat[c_repeat == 1]),
      replace = TRUE
    )
  }

  # Blueback herring batch fecundity
  # Limburg and Blackburn (2003) report to NYSDEC
  # See also Jessop (1990)
  if (species == "blueback") {
    c_BF <- MASS::rnegbin(length(c_fishAges), 75500, 8)
  }

  # Calculate realized annual fecundity
  c_RAF <- c_BF * (c_RT / c_SI)

  # Multiply by sex variable to set male fecundity to zero
  c_fecundity <- c_female * c_RAF

  # Calculate movement rates based on Castro-Santos and Letcher (2010)
  # Optimizing ground speed in body lengths per second (BLS)
  sOptim <- runif(length(c_fecundity), .7, 1.7)
  # Get max daily movement, converting from BLS to km per day
  dMax <- (sOptim * c_forkLength * 86400) / 1e6
  # Movement tortuosity drawn from uniform distribution.  This corresponds to
  # the range used in Castro-Santos and Letcher (2010) but it's just applied
  # as a multiplier
  tort <- runif(length(c_fishAges), 0.2, 1)
  # Now scale by tortuosity and divide by two to restrict movement to day time
  dailyMove <- dMax * tort * mean(photo / 24)


  # UPSTREAM MIGRATION ROUTES ----
  environment(assignFishToRoutes) <- .shadia
  upstream_path <- assignFishToRoutes(river = .shadia$river, c_fishAges = c_fishAges)


  # COLLECT L-H PARAMETERS ----
  # Collect life-history parameters into a single matrix for c++ loop
  # NOTE: the source code for the loop was re-written to preclude the need for
  # these matrices. Instead, they are related to the ABM input and output post-
  # hoc to speed things up.
  getEm <- mget(ls(pat = "^c_"))
  for (i in 1:length(getEm)) {
    if (is.na(getEm[[i]][1])) {
      getEm[[i]][1] <- 0
    } else {
      next
    }
  }
  traits <- as.matrix(data.frame(getEm))
  colnames(traits) <- gsub(pattern = "c_", replacement = "", colnames(traits))

  # Create traits for all spawners by river system
  if (river == "penobscot" | river == "susquehanna") {
    # Re-organize the data so they match the output of the ABM below
    # Create a df for traits of Piscataquis River spawners
    traits_1 <- data.frame(
      traits[upstream_path == 1, , drop = FALSE],
      upstream_path[upstream_path == 1]
    )
    # Create a df for traits of Mainstem spawners
    traits_2 <- data.frame(
      traits[upstream_path == 2, , drop = FALSE],
      upstream_path[upstream_path == 2]
    )
    # Create a df for traits of Piscataquis River spawners
    traits_3 <- data.frame(
      traits[upstream_path == 3, , drop = FALSE],
      upstream_path[upstream_path == 3]
    )
    # Create a df for traits of Mainstem spawners
    traits_4 <- data.frame(
      traits[upstream_path == 4, , drop = FALSE],
      upstream_path[upstream_path == 4]
    )

    # Change the name of the last column in each of the dfs so they match
    names(traits_1)[ncol(traits_1)] <- "upstream_path"
    names(traits_2)[ncol(traits_2)] <- "upstream_path"
    names(traits_3)[ncol(traits_3)] <- "upstream_path"
    names(traits_4)[ncol(traits_4)] <- "upstream_path"
    # toc()
  }

  if (river == "merrimack") {
    # Re-organize the data so they match the output of the ABM below
    traits_1 <- data.frame(
      traits[upstream_path == 1, , drop = FALSE],
      upstream_path[upstream_path == 1]
    )
    traits_2 <- data.frame(
      traits[upstream_path == 2, , drop = FALSE],
      upstream_path[upstream_path == 2]
    )
    # Change the name of the last column in each of the dfs so they match
    names(traits_1)[ncol(traits_1)] <- "upstream_path"
    names(traits_2)[ncol(traits_2)] <- "upstream_path"
    # toc()
  }

  if (river == "connecticut") {
    # Re-organize the data so they match the output of the ABM below
    traits_1 <- data.frame(
      traits[upstream_path == 1, , drop = FALSE],
      upstream_path[upstream_path == 1]
    )
    traits_2 <- data.frame(
      traits[upstream_path == 2, , drop = FALSE],
      upstream_path[upstream_path == 2]
    )
    # Change the name of the last column in each of the dfs so they match
    names(traits_1)[ncol(traits_1)] <- "upstream_path"
    names(traits_2)[ncol(traits_2)] <- "upstream_path"
    # toc()
  }

  if (river == "saco") {
    # Re-organize the data so they match the output of the ABM below
    traits_1 <- data.frame(
      traits[upstream_path == 1, , drop = FALSE],
      upstream_path[upstream_path == 1]
    )
    # Change the name of the last column in each of the dfs so they match
    names(traits_1)[ncol(traits_1)] <- "upstream_path"
    # toc()
  }

  if (river == "kennebec") {
    # Re-organize the data so they match the output of the ABM below
    traits_1 <- data.frame(
      traits[upstream_path == 1, , drop = FALSE],
      upstream_path[upstream_path == 1]
    )
    traits_2 <- data.frame(
      traits[upstream_path == 2, , drop = FALSE],
      upstream_path[upstream_path == 2]
    )
    # Change the name of the last column in each of the dfs so they match
    names(traits_1)[ncol(traits_1)] <- "upstream_path"
    names(traits_2)[ncol(traits_2)] <- "upstream_path"
    # toc()
  }

  if (river == "hudson") {
    # Re-organize the data so they match the output of the ABM below
    traits_1 <- data.frame(
      traits[upstream_path == 1, , drop = FALSE],
      upstream_path[upstream_path == 1]
    )
    traits_2 <- data.frame(
      traits[upstream_path == 2, , drop = FALSE],
      upstream_path[upstream_path == 2]
    )
    # Change the name of the last column in each of the dfs so they match
    names(traits_1)[ncol(traits_1)] <- "upstream_path"
    names(traits_2)[ncol(traits_2)] <- "upstream_path"
    # toc()
  }

  # Draw carrying capacity ----
  # Carrying capacity for juvs based on potential production of adult shad in each
  # production unit based on values from the 2009 multi-species management plan
  # NOTE: These are divided by 1000 to make the simulations run faster!
  # Everything is scaled up on output
  # if (useTictoc) tic("spawn dynamics variables")
  k_pus <- vector(mode = "list", length = length(habitat))
  batch <- quantile(MASS::rnegbin(1e2, 2.5e4, 10), 0.5)[1]

  # Carrying capacity for each migration route
  for (i in 1:length(k_pus)) {
    k_pus[[i]] <- ((habitat[[i]] / scalar) * sexRatio * batch)
  }

  k_pus <- lapply(k_pus, function(x) {
    x[is.na(x)] <- 1
    x
  })


  # Mortality rates ----
  # Pre-spawning mortality. Right now, these are drawn independently. Conditional
  # draws may be more appropriate b/c pre-spawn mortality is probably affected by
  # similar factors but may differ in magnitude between males and females.
  pre_spawn_survival_males <- rbeta(1, 1e4, 50)
  pre_spawn_survival_females <- rbeta(1, 1e4, 50)
  
  # Post-spawning mortality. Right now, these are drawn independently. Conditional
  # draws may be more appropriate b/c post-spawn mortality is probably affected by
  # similar factors but may differ in magnitude between males and females.
  post_spawn_survival_males <- rbeta(1, 200, 50)
  post_spawn_survival_females <- rbeta(1, 200, 50)

  # Juvenile mortality rate
  # This really needs some data pretty bad. Right now it is just a draw from a
  # uniform probability distribution that calls it 1 in 100000 to 1 in 1000
  juvenile_survival <- runif(1, 0.0005, 0.00083)

  # Simulate marine S for current year
  environment(simMarineS) <- .shadia
  marineS <- rep(simMarineS(), maxAge)
  
  # Or use the user-specified value if provided
  if(exists("marine_s")){
    if(!is.null(marine_s)){
      marineS <- rep(marine_s, maxAge)
    }
  }

  # Time-based upstream passage at dams ----
  # Convert passage performance standards into rates
  # over time based on passage efficiency and timely
  up_effs <- upEffs
  for (i in 1:length(up_effs)) {
    up_effs[[i]] <- mapply("^", up_effs[[i]], (times[[i]]))
  }

  # Finally, assign passage efficiencies for each reach in the river
  eFFs <- vector(mode = "list", length = length(upEffs))

  # Fill with Open passage efficiency to begin and
  # replace efficiencies where there are dams.
  # The first one works for all rivers because they all have
  # at least one passage route.
  for (i in 1:length(eFFs)) {
    eFFs[[i]] <- c(rep(Open, maxrkm[i])) # Create perfect passage for group 1
    eFFs[[i]][damRkms[[i]]] <- up_effs[[i]] # Dam-specific efficiencies group 1
  }

  # Seasonal changes in migration ----
  # Make the adult fish move upstream through space and time. The functions used
  # in this section call a collection of C++ and header files that were sourced
  # the front-end of this r script.

  # Add in a motivation penalty based on photoperiod. Fish are assumed to most
  # motivated at the peak of the run. Makes a passage efficiency for each
  # rkm on each day.

  # Make a list of empty matrices to hold the results
  ppPenalty <- vector(mode = "list", length = length(eFFs))

  # Fill in the first element of the list for all rivers
  for (i in 1:length(ppPenalty)) {
    ppPenalty[[i]] <- matrix(0, length(photo), maxrkm[i])
  }

  # Multiply passage efficiency by the penalty for each day
  newTU_2 <- newTU[min(c_entryDate):max(c_end)]

  # Fill in the first element of the list for all rivers
  for (i in 1:length(ppPenalty)) {
    ppPenalty[[i]] <- motivationPenaltyC(eFFs[[i]], newTU_2, ppPenalty[[i]])
  }

  # Track the mean motivational penalty for the spawning season
  # to be used in sensitivity analyses
  mot <- mean((1 - (newTU - min(newTU)) /
    (max(newTU) - min(newTU)))[min(c_entryDate):max(c_end)])


  # Containers for individual-based migration model ----
  # Pre-allocate vectors and matrices for agent-based migration model
  # These need to be river-specific. For the Penobscot River, this can
  # be done in bulk for both Piscataquis River spawners and Mainstem spawners
  # because we use vectorization to select the appropriate elements later on.
  rkm1 <- get_rkm1(river, c_fishAges)

  # For all rivers:
  rkm2 <- matrix(0, ncol = length(day), nrow = length(c_fishAges))

  # Get max rkm for each fish. Rivers with one passage route skip
  # the C++ function because all fish have same max RKM.
  # Create a vector of potential routes
  routes <- seq(1, nRoutes, 1)
  maxR <- maxrkmC(c_fishAges, maxrkm, upstream_path, routes)



  # Individual-based migration model ----
  if (river == "penobscot") {
    # Run the ABM for main-to-piscataquis spawners
    moves_1 <- moveC(
      day,
      c_entryDate[upstream_path == 1],
      dailyMove[upstream_path == 1],
      maxR[upstream_path == 1],
      ppPenalty[[1]],
      rkm1[upstream_path == 1],
      rkm2[upstream_path == 1, , drop = FALSE],
      c_initial[upstream_path == 1]
    )
    # Run the ABM for main-to-mainstem Spawners
    moves_2 <- moveC(
      day,
      c_entryDate[upstream_path == 2],
      dailyMove[upstream_path == 2],
      maxR[upstream_path == 2],
      ppPenalty[[2]],
      rkm1[upstream_path == 2],
      rkm2[upstream_path == 2, , drop = FALSE],
      c_initial[upstream_path == 2]
    )
    # Run the ABM for stillwater-to-piscataquis Spawners
    moves_3 <- moveC(
      day,
      c_entryDate[upstream_path == 3],
      dailyMove[upstream_path == 3],
      maxR[upstream_path == 3],
      ppPenalty[[3]],
      rkm1[upstream_path == 3],
      rkm2[upstream_path == 3, , drop = FALSE],
      c_initial[upstream_path == 3]
    )
    # Run the ABM for stillwater-to-mainstem Spawners
    moves_4 <- moveC(
      day,
      c_entryDate[upstream_path == 4],
      dailyMove[upstream_path == 4],
      maxR[upstream_path == 4],
      ppPenalty[[4]],
      rkm1[upstream_path == 4],
      rkm2[upstream_path == 4, , drop = FALSE],
      c_initial[upstream_path == 4]
    )
  }

  if (river == "merrimack") {
    # Run the ABM for bypass route through Pawtucket
    moves_1 <- moveC(
      day,
      c_entryDate[upstream_path == 1],
      dailyMove[upstream_path == 1],
      maxR[upstream_path == 1],
      ppPenalty[[1]],
      rkm1[upstream_path == 1],
      rkm2[upstream_path == 1, , drop = FALSE],
      c_initial[upstream_path == 1]
    )
    # Run the ABM for mainstem route through Pawtucket
    moves_2 <- moveC(
      day,
      c_entryDate[upstream_path == 2],
      dailyMove[upstream_path == 2],
      maxR[upstream_path == 2],
      ppPenalty[[2]],
      rkm1[upstream_path == 2],
      rkm2[upstream_path == 2, , drop = FALSE],
      c_initial[upstream_path == 2]
    )
  }

  if (river == "connecticut") {
    # Spillway
    moves_1 <- moveC(
      day,
      c_entryDate[upstream_path == 1],
      dailyMove[upstream_path == 1],
      maxR[upstream_path == 1],
      ppPenalty[[1]],
      rkm1[upstream_path == 1],
      rkm2[upstream_path == 1, , drop = FALSE],
      c_initial[upstream_path == 1]
    )
    # Canal
    moves_2 <- moveC(
      day,
      c_entryDate[upstream_path == 2],
      dailyMove[upstream_path == 2],
      maxR[upstream_path == 2],
      ppPenalty[[2]],
      rkm1[upstream_path == 2],
      rkm2[upstream_path == 2, , drop = FALSE],
      c_initial[upstream_path == 2]
    )
  }

  if (river == "susquehanna") {
    # Juniata River
    moves_1 <- moveC(
      day,
      c_entryDate[upstream_path == 1],
      dailyMove[upstream_path == 1],
      maxR[upstream_path == 1],
      ppPenalty[[1]],
      rkm1[upstream_path == 1],
      rkm2[upstream_path == 1, , drop = FALSE],
      c_initial[upstream_path == 1]
    )
    # West Branch
    moves_2 <- moveC(
      day,
      c_entryDate[upstream_path == 2],
      dailyMove[upstream_path == 2],
      maxR[upstream_path == 2],
      ppPenalty[[2]],
      rkm1[upstream_path == 2],
      rkm2[upstream_path == 2, , drop = FALSE],
      c_initial[upstream_path == 2]
    )
    # Chemung
    moves_3 <- moveC(
      day,
      c_entryDate[upstream_path == 3],
      dailyMove[upstream_path == 3],
      maxR[upstream_path == 3],
      ppPenalty[[3]],
      rkm1[upstream_path == 3],
      rkm2[upstream_path == 3, , drop = FALSE],
      c_initial[upstream_path == 3]
    )
    # North Branch
    moves_4 <- moveC(
      day,
      c_entryDate[upstream_path == 4],
      dailyMove[upstream_path == 4],
      maxR[upstream_path == 4],
      ppPenalty[[4]],
      rkm1[upstream_path == 4],
      rkm2[upstream_path == 4, , drop = FALSE],
      c_initial[upstream_path == 4]
    )
  }

  if (river == "saco") {
    # Run the ABM for bypass route through Pawtucket
    moves_1 <- moveC(
      day,
      c_entryDate[upstream_path == 1],
      dailyMove[upstream_path == 1],
      maxR[upstream_path == 1],
      ppPenalty[[1]],
      rkm1[upstream_path == 1],
      rkm2[upstream_path == 1, , drop = FALSE],
      c_initial[upstream_path == 1]
    )
  }

  if (river == "kennebec") {
    # Run the ABM for bypass route through Pawtucket
    moves_1 <- moveC(
      day,
      c_entryDate[upstream_path == 1],
      dailyMove[upstream_path == 1],
      maxR[upstream_path == 1],
      ppPenalty[[1]],
      rkm1[upstream_path == 1],
      rkm2[upstream_path == 1, , drop = FALSE],
      c_initial[upstream_path == 1]
    )
    # Run the ABM for mainstem route through Pawtucket
    moves_2 <- moveC(
      day,
      c_entryDate[upstream_path == 2],
      dailyMove[upstream_path == 2],
      maxR[upstream_path == 2],
      ppPenalty[[2]],
      rkm1[upstream_path == 2],
      rkm2[upstream_path == 2, , drop = FALSE],
      c_initial[upstream_path == 2]
    )
  }

  if (river == "hudson") {
    # Run the ABM for upper hudson route
    moves_1 <- moveC(
      day,
      c_entryDate[upstream_path == 1],
      dailyMove[upstream_path == 1],
      maxR[upstream_path == 1],
      ppPenalty[[1]],
      rkm1[upstream_path == 1],
      rkm2[upstream_path == 1, , drop = FALSE],
      c_initial[upstream_path == 1]
    )
    # Run the ABM for mohawk route
    moves_2 <- moveC(
      day,
      c_entryDate[upstream_path == 2],
      dailyMove[upstream_path == 2],
      maxR[upstream_path == 2],
      ppPenalty[[2]],
      rkm1[upstream_path == 2],
      rkm2[upstream_path == 2, , drop = FALSE],
      c_initial[upstream_path == 2]
    )
  }

  ### DSS: WANT TO COMMENT OUT BECAUSE THIS IS
  ###      STILL UNUSED AND IT IS A BIG
  ###      LIFT IN TERMS OF MEMORY/TIME!
  # Calculate delay at each dam for each fish in the first
  # migration route for all rivers.
  delay_1 <- delayC(moves_1, damRkms[[1]][2:nPU[1]])
  # Remaining routes for connecticut River
  if (river == "connecticut" | river == "merrimack" | river == "kennebec" | river == "hudson") {
    # Calculate delay at each dam for each main-to-Mainstem spawners
    delay_2 <- delayC(moves_2, damRkms[[2]][2:nPU[2]])
  }
  # Remaining routes for penobscot River
  if (river == "penobscot" | river == "susquehanna") {
    # Calculate delay at each dam for each main-to-Mainstem spawners
    delay_2 <- delayC(moves_2, damRkms[[2]][2:nPU[2]])
    # Calculate delay at each dam for each still-to-Piscataquis spawners
    delay_3 <- delayC(moves_3, damRkms[[3]][2:nPU[3]])
    # Calculate delay at each dam for each still-to-Mainstem spawners
    delay_4 <- delayC(moves_4, damRkms[[4]][2:nPU[4]])
  }


  # Assign names to the newly created matrices that hold delay at each dam
  # Names for delay matrix: penobscot river
  if (river == "penobscot") {
    # Main-to-Piscataquis spawners
    colnames(delay_1) <- c(
      "dConfluence", "dMilford", "dHowland",
      "dBrownsMill", "dMoosehead", "dGuilford"
    )
    # Main-to-Piscataquis spawners
    colnames(delay_2) <- c(
      "dConfluence", "dMilford", "dWestEnfield",
      "dWeldon"
    )
    # Main-to-Piscataquis spawners
    colnames(delay_3) <- c(
      "dOrono", "dStillwater", "dGilman",
      "dHowland", "dBrownsMill", "dMoosehead",
      "dGuilford"
    )
    # Main-to-Piscataquis spawners
    colnames(delay_4) <- c(
      "Orono", "Stillwater", "Gilman",
      "dWestEnfield", "dWeldon"
    )
  }

  # Names for delay matrix: merrimack
  if (river == "merrimack") {
    colnames(delay_1) <- c("dEssex", "dPawtucket", "dAmoskeag", "dHookset")
    colnames(delay_2) <- c("dEssex", "dPawtucket", "dAmoskeag", "dHookset")
  }

  # Names for delay matrix: connecticut
  if (river == "connecticut") {
    colnames(delay_1) <- c("dHolyoke", "dCabot", "dGatehouse", "dVernon")
    colnames(delay_2) <- c("dHolyoke", "dSpillway", "dGatehouse", "dVernon")
  }

  # Names for delay matrix: penobscot river
  if (river == "susquehanna") {
    # Juniata River
    colnames(delay_1) <- c(
      "dConowingo", "dHoltwood", "dSafeHarbor",
      "dYorkHaven", "djunConf"
    )
    # West Branch
    colnames(delay_2) <- c(
      "dConowingo", "dHoltwood", "dSafeHarbor",
      "dYorkHaven", "dSunbury", "dWilliamsport",
      "dLockHaven"
    )
    # Chemung River
    colnames(delay_3) <- c(
      "dConowingo", "dHoltwood", "dSafeHarbor",
      "dYorkHaven", "dSunbury", "dNyLine",
      "dChaseHibbard"
    )
    # North Branch
    colnames(delay_4) <- c(
      "dConowingo", "dHoltwood", "dSafeHarbor",
      "dYorkHaven", "dSunbury", "dNyLine",
      "dRockBottom", "dUnadilla", "dColliersville"
    )
  }

  # Names for delay matrix: merrimack
  if (river == "saco") {
    colnames(delay_1) <- c(
      "dCataract", "dSprings", "dSkelton", "dBarmills",
      "dBuxton", "dBonny"
    )
  }

  # Names for delay matrix: kennebec
  if (river == "kennebec") {
    colnames(delay_1) <- c("dLockwood", "dHydrokenn", "dShawmut", "dWeston")
    colnames(delay_2) <- c("dBenton", "dBurnham")
  }

  # Names for delay matrix: hudson-mohawk
  if (river == "hudson") {
    colnames(delay_1) <- c(
      "dfederal",
      paste0("d", names(upstream)[grep("C", names(upstream))])
    )
    colnames(delay_2) <- c(
      "dfederal",
      paste0("d", names(upstream)[grep("E", names(upstream))])
    )
  }


  # Annual spawning dynamics ----
  # Combine the data for each fish stored in traits with the final rkm of that
  # fish and the delay experienced by each fish at each dam for each of the
  # upstream passage routes

  # Penobscot River
  if (river == "penobscot") {
    # Combine all data for mainstem to piscataquis
    # Combine all three matrices
    spawnData_1 <- cbind(traits_1, moves_1[, ncol(moves_1)], delay_1)
    # Change the name for the final rkm column
    colnames(spawnData_1)[ncol(spawnData_1) - 6] <- "finalRkm"
    # Make it into a dataframe for easy manipulation
    sp_1 <- data.frame(spawnData_1)

    # Combine all data for main-to-mainstem spawners
    # Combine all three matrices
    spawnData_2 <- cbind(traits_2, moves_2[, ncol(moves_2)], delay_2)
    # Change the name for the final rkm column
    colnames(spawnData_2)[ncol(spawnData_2) - 4] <- "finalRkm"
    # Make it into a dataframe for easy manipulation
    sp_2 <- data.frame(spawnData_2)

    # Combine all data for stillwater-to-piscataquis spawners
    # Combine all three matrices
    spawnData_3 <- cbind(traits_3, moves_3[, ncol(moves_3)], delay_3)
    # Change the name for the final rkm column
    colnames(spawnData_3)[ncol(spawnData_3) - 7] <- "finalRkm"
    # Make it into a dataframe for easy manipulation
    sp_3 <- data.frame(spawnData_3)

    # Combine all data for stillwater-to-piscataquis spawners
    # Combine all three matrices
    spawnData_4 <- cbind(traits_4, moves_4[, ncol(moves_4)], delay_4)
    # Change the name for the final rkm column
    colnames(spawnData_4)[ncol(spawnData_4) - 5] <- "finalRkm"
    # Make it into a dataframe for easy manipulation
    sp_4 <- data.frame(spawnData_4)

    # Assign each fish to a production unit before they spawn. Do this for
    # Piscataquis River spawners and Mainstem spawners
    # First, assign rkms to delineate each of the production units
    puRkm <- vector(mode = "list", length = length(nPU))
    puRkm[[1]] <- c(damRkms[[1]] + 1, (maxrkm[1] + 1))
    puRkm[[2]] <- c(damRkms[[2]] + 1, (maxrkm[2] + 1))
    puRkm[[3]] <- c(damRkms[[3]] + 1, (maxrkm[3] + 1))
    puRkm[[4]] <- c(damRkms[[4]] + 1, (maxrkm[4] + 1))
    # Create an empty list to hold the pu names for each route
    rm(list = ls()[grep(ls(), pat = "^mPU_")]) # Remove old counts
    puNames <- vector(mode = "list", length = length(nPU))
    # Dynamically assign pu names based on river kilometers that delineate them
    for (t in 1:length(puRkm)) {
      for (i in 1:(length(puRkm[[t]]) - 1)) {
        assign(paste("PU_", t, "_", i, sep = ""), puRkm[i])
      }
      # Collect the names into a list
      puNames[[t]] <- names(mget(ls(pat = paste(
        "^PU_", t,
        sep = ""
      ))))
    }
    # Determine which PU each fish ends up in based on its rkm and assign it.
    # Uses pre-compiled function 'fishPU' from source files loaded up front.
    # Main-to-piscataquis spawners
    sp_1$pus <- as.character(fishPU(puRkm[[1]], sp_1$finalRkm, puNames[[1]]))
    # Main-to-mainstem spawners
    sp_2$pus <- as.character(fishPU(puRkm[[2]], sp_2$finalRkm, puNames[[2]]))
    # Stillwater-to-piscataquis spawners
    sp_3$pus <- as.character(fishPU(puRkm[[3]], sp_3$finalRkm, puNames[[3]]))
    # Stillwater-to-mainstem spawners
    sp_4$pus <- as.character(fishPU(puRkm[[4]], sp_4$finalRkm, puNames[[4]]))

    # Replace the blank PUs for fish that ended at Veazie
    sp_1$pus[sp_1$pus == ""] <- "PU_1_1"
    sp_2$pus[sp_2$pus == ""] <- "PU_2_1"
    sp_3$pus[sp_3$pus == ""] <- "PU_3_1"
    sp_4$pus[sp_4$pus == ""] <- "PU_4_1"

    # Determine the probability that a fish survives to spawn
    # Pre-spawning mortality by sex
    sp_1$preSpawn <- sp_1$female * pre_spawn_survival_females +
      (1 - sp_1$female) * pre_spawn_survival_males
    sp_2$preSpawn <- sp_2$female * pre_spawn_survival_females +
      (1 - sp_2$female) * pre_spawn_survival_males
    sp_3$preSpawn <- sp_3$female * pre_spawn_survival_females +
      (1 - sp_3$female) * pre_spawn_survival_males
    sp_4$preSpawn <- sp_4$female * pre_spawn_survival_females +
      (1 - sp_4$female) * pre_spawn_survival_males
    
    # Determine fishing mortality by PU
    sp_1$F <- inriv[[1]][as.numeric(substrRight(sp_1$pus, 1))]
    sp_2$F <- inriv[[2]][as.numeric(substrRight(sp_2$pus, 1))]
    sp_3$F <- inriv[[3]][as.numeric(substrRight(sp_3$pus, 1))]
    sp_4$F <- inriv[[4]][as.numeric(substrRight(sp_4$pus, 1))]

    # Apply in-river fishing mortality and prespawn survival
    sp_1$surv <- rbinom(nrow(sp_1), 1, sp_1$preSpawn * (1 - sp_1$F))
    sp_2$surv <- rbinom(nrow(sp_2), 1, sp_2$preSpawn * (1 - sp_2$F))
    sp_3$surv <- rbinom(nrow(sp_3), 1, sp_3$preSpawn * (1 - sp_3$F))
    sp_4$surv <- rbinom(nrow(sp_4), 1, sp_4$preSpawn * (1 - sp_4$F))
  }

  # Merrimack River:
  if (river == "merrimack") {
    # Combine all data for bypass migrants
    # Combine both matrices
    spawnData_1 <- cbind(traits_1, moves_1[, ncol(moves_1)], delay_1)
    # Change the name for the final rkm column
    colnames(spawnData_1)[ncol(spawnData_1) - 4] <- "finalRkm"
    # Make it into a dataframe for easy manipulation
    sp_1 <- data.frame(spawnData_1)

    # Combine all data for mainstem migrants
    spawnData_2 <- cbind(traits_2, moves_2[, ncol(moves_2)], delay_2)
    # Change the name for the final rkm column
    colnames(spawnData_2)[ncol(spawnData_2) - 4] <- "finalRkm"
    # Make it into a dataframe for easy manipulation
    sp_2 <- data.frame(spawnData_2)

    # Assign each fish to a production unit before they spawn. Do this for
    # bypass and Mainstem migrants
    # First, assign rkms to delineate each of the production units
    puRkm <- vector(mode = "list", length = length(nPU))
    puRkm[[1]] <- c(0, damRkms[[1]] + 1, (maxrkm[1] + 1))
    puRkm[[2]] <- c(0, damRkms[[2]] + 1, (maxrkm[2] + 1))

    # Create an empty list to hold the pu names for each route
    rm(list = ls()[grep(ls(), pat = "^mPU_")]) # Remove old counts
    puNames <- vector(mode = "list", length = length(nPU))
    # Dynamically assign pu names based on river kilometers that delineate them
    for (t in 1:length(puRkm)) {
      for (i in 1:(length(puRkm[[t]]) - 1)) {
        assign(paste("PU_", t, "_", i, sep = ""), puRkm[i])
      }
      # Collect the names into a list
      puNames[[t]] <- names(mget(ls(pat = paste(
        "^PU_", t,
        sep = ""
      ))))
    }
    # Determine which PU each fish ends up in based on its rkm and assign it.
    # Uses pre-compiled function 'fishPU' from source files loaded up front.
    # Main-to-piscataquis spawners
    sp_1$pus <- as.character(fishPU(puRkm[[1]], sp_1$finalRkm, puNames[[1]]))

    # Replace the blank PUs for fish that ended
    # at head of tide
    sp_1$pus[sp_1$pus == ""] <- "PU_1_1"
    sp_2$pus[sp_2$pus == ""] <- "PU_2_1"

    # Determine the probability that a fish survives to spawn
    # Pre-spawning mortality by sex
    sp_1$preSpawn <- sp_1$female * pre_spawn_survival_females +
      (1 - sp_1$female) * pre_spawn_survival_males
    sp_2$preSpawn <- sp_2$female * pre_spawn_survival_females +
      (1 - sp_2$female) * pre_spawn_survival_males

    # Determine fishing mortality by PU
    sp_1$F <- inriv[[1]][as.numeric(substrRight(sp_1$pus, 1))]
    sp_2$F <- inriv[[2]][as.numeric(substrRight(sp_2$pus, 1))]

    # Apply in-river fishing mortality and prespawn survival
    sp_1$surv <- rbinom(nrow(sp_1), 1, sp_1$preSpawn * (1 - sp_1$F))
    sp_2$surv <- rbinom(nrow(sp_2), 1, sp_2$preSpawn * (1 - sp_2$F))
  }

  # Connecticut River
  if (river == "connecticut") {
    # Combine all data for spillway migrants
    # Combine both matrices
    spawnData_1 <- cbind(traits_1, moves_1[, ncol(moves_1)], delay_1)
    # Change the name for the final rkm column
    colnames(spawnData_1)[ncol(spawnData_1) - 4] <- "finalRkm"
    # Make it into a dataframe for easy manipulation
    sp_1 <- data.frame(spawnData_1)

    # Combine all data for Cabot migrants
    spawnData_2 <- cbind(traits_2, moves_2[, ncol(moves_2)], delay_2)
    # Change the name for the final rkm column
    colnames(spawnData_2)[ncol(spawnData_2) - 4] <- "finalRkm"
    # Make it into a dataframe for easy manipulation
    sp_2 <- data.frame(spawnData_2)

    # Assign each fish to a production unit before they spawn. Do this for
    # Piscataquis River spawners and Mainstem spawners
    # First, assign rkms to delineate each of the production units
    puRkm <- vector(mode = "list", length = length(nPU))
    puRkm[[1]] <- c(0, damRkms[[1]] + 1, (maxrkm[1] + 1))
    puRkm[[2]] <- c(0, damRkms[[2]] + 1, (maxrkm[2] + 1))

    # Create an empty list to hold the pu names for each route
    rm(list = ls()[grep(ls(), pat = "^mPU_")]) # Remove old counts
    puNames <- vector(mode = "list", length = length(nPU))
    # Dynamically assign pu names based on river kilometers that delineate them
    for (t in 1:length(puRkm)) {
      for (i in 1:(length(puRkm[[t]]) - 1)) {
        assign(paste("PU_", t, "_", i, sep = ""), puRkm[i])
      }
      # Collect the names into a list
      puNames[[t]] <- names(mget(ls(pat = paste(
        "^PU_", t,
        sep = ""
      ))))
    }
    # Determine which PU each fish ends up in based on its rkm and assign it.
    # Uses pre-compiled function 'fishPU' from source files loaded up front.
    # Spillway migrants
    sp_1$pus <- as.character(fishPU(puRkm[[1]], sp_1$finalRkm, puNames[[1]]))
    # Cabot migrants
    sp_2$pus <- as.character(fishPU(puRkm[[2]], sp_2$finalRkm, puNames[[2]]))

    # Replace the blank PUs for fish that ended
    # migration downstream of Holyoke
    sp_1$pus[sp_1$pus == ""] <- "PU_1_1"
    sp_2$pus[sp_2$pus == ""] <- "PU_2_1"

    # Determine the probability that a fish survives to spawn
    # Pre-spawning mortality by sex
    sp_1$preSpawn <- sp_1$female * pre_spawn_survival_females +
      (1 - sp_1$female) * pre_spawn_survival_males
    sp_2$preSpawn <- sp_2$female * pre_spawn_survival_females +
      (1 - sp_2$female) * pre_spawn_survival_males

    # Determine fishing mortality by PU
    sp_1$F <- inriv[[1]][as.numeric(substrRight(sp_1$pus, 1))]
    sp_2$F <- inriv[[2]][as.numeric(substrRight(sp_2$pus, 1))]

    # Apply in-river fishing mortality and prespawn survival
    sp_1$surv <- rbinom(nrow(sp_1), 1, sp_1$preSpawn * (1 - sp_1$F))
    sp_2$surv <- rbinom(nrow(sp_2), 1, sp_2$preSpawn * (1 - sp_2$F))
    # toc()
  }

  # Susquehanna River
  if (river == "susquehanna") {
    # Combine all data for juniata river
    spawnData_1 <- cbind(traits_1, moves_1[, ncol(moves_1)], delay_1)
    # Change the name for the final rkm column
    colnames(spawnData_1)[ncol(spawnData_1) - 5] <- "finalRkm"
    # Make it into a dataframe for easy manipulation
    sp_1 <- data.frame(spawnData_1)

    # Combine all data for west branch
    # Combine all three matrices
    spawnData_2 <- cbind(traits_2, moves_2[, ncol(moves_2)], delay_2)
    # Change the name for the final rkm column
    colnames(spawnData_2)[ncol(spawnData_2) - 7] <- "finalRkm"
    # Make it into a dataframe for easy manipulation
    sp_2 <- data.frame(spawnData_2)

    # Combine all data for chemung
    # Combine all three matrices
    spawnData_3 <- cbind(traits_3, moves_3[, ncol(moves_3)], delay_3)
    # Change the name for the final rkm column
    colnames(spawnData_3)[ncol(spawnData_3) - 7] <- "finalRkm"
    # Make it into a dataframe for easy manipulation
    sp_3 <- data.frame(spawnData_3)

    # Combine all data for north branch
    # Combine all three matrices
    spawnData_4 <- cbind(traits_4, moves_4[, ncol(moves_4)], delay_4)
    # Change the name for the final rkm column
    colnames(spawnData_4)[ncol(spawnData_4) - 9] <- "finalRkm"
    # Make it into a dataframe for easy manipulation
    sp_4 <- data.frame(spawnData_4)

    # Assign each fish to a production unit before they spawn. Do this for
    # Piscataquis River spawners and Mainstem spawners
    # First, assign rkms to delineate each of the production units
    puRkm <- vector(mode = "list", length = length(nPU))
    puRkm[[1]] <- c(0, damRkms[[1]] + 1, (maxrkm[1] + 1))
    puRkm[[2]] <- c(0, damRkms[[2]] + 1, (maxrkm[2] + 1))
    puRkm[[3]] <- c(0, damRkms[[3]] + 1, (maxrkm[3] + 1))
    puRkm[[4]] <- c(0, damRkms[[4]] + 1, (maxrkm[4] + 1))
    # Create an empty list to hold the pu names for each route
    rm(list = ls()[grep(ls(), pat = "^mPU_")]) # Remove old counts
    puNames <- vector(mode = "list", length = length(nPU))
    # Dynamically assign pu names based on river kilometers that delineate them
    for (t in 1:length(puRkm)) {
      for (i in 1:(length(puRkm[[t]]) - 1)) {
        assign(paste("PU_", t, "_", i, sep = ""), puRkm[i])
      }
      # Collect the names into a list
      puNames[[t]] <- names(mget(ls(pat = paste(
        "^PU_", t,
        sep = ""
      ))))
    }
    puNames[[4]] <- puNames[[4]][order(
      as.numeric(
        gsub(
          x = substrRight(sapply(puNames[[4]], head, 1), 2),
          pattern = "_", replacement = ""
        )
      )
    )]

    # Determine which PU each fish ends up in based on its rkm and assign it.
    # Uses pre-compiled function 'fishPU' from source files loaded up front.
    # Route 1
    sp_1$pus <- as.character(fishPU(puRkm[[1]], sp_1$finalRkm, puNames[[1]]))
    # Route 2
    sp_2$pus <- as.character(fishPU(puRkm[[2]], sp_2$finalRkm, puNames[[2]]))
    # Route 3
    sp_3$pus <- as.character(fishPU(puRkm[[3]], sp_3$finalRkm, puNames[[3]]))
    # Route 4
    sp_4$pus <- as.character(fishPU(puRkm[[4]], sp_4$finalRkm, puNames[[4]]))

    # Replace the blank PUs for fish that ended at Conowingo
    sp_1$pus[sp_1$pus == ""] <- "PU_1_1"
    sp_2$pus[sp_2$pus == ""] <- "PU_2_1"
    sp_3$pus[sp_3$pus == ""] <- "PU_3_1"
    sp_4$pus[sp_4$pus == ""] <- "PU_4_1"

    # Determine the probability that a fish survives to spawn
    # Pre-spawning mortality by sex
    sp_1$preSpawn <- sp_1$female * pre_spawn_survival_females +
      (1 - sp_1$female) * pre_spawn_survival_males
    sp_2$preSpawn <- sp_2$female * pre_spawn_survival_females +
      (1 - sp_2$female) * pre_spawn_survival_males
    sp_3$preSpawn <- sp_3$female * pre_spawn_survival_females +
      (1 - sp_3$female) * pre_spawn_survival_males
    sp_4$preSpawn <- sp_4$female * pre_spawn_survival_females +
      (1 - sp_4$female) * pre_spawn_survival_males

    # Determine fishing mortality by PU
    sp_1$F <- inriv[[1]][as.numeric(substrRight(sp_1$pus, 1))]
    sp_2$F <- inriv[[2]][as.numeric(substrRight(sp_2$pus, 1))]
    sp_3$F <- inriv[[3]][as.numeric(substrRight(sp_3$pus, 1))]
    sp_4$F <- inriv[[4]][as.numeric(substr(sp_4$pus, 6, 7))]

    # Apply in-river fishing mortality and prespawn survival
    sp_1$surv <- rbinom(nrow(sp_1), 1, sp_1$preSpawn * (1 - sp_1$F))
    sp_2$surv <- rbinom(nrow(sp_2), 1, sp_2$preSpawn * (1 - sp_2$F))
    sp_3$surv <- rbinom(nrow(sp_3), 1, sp_3$preSpawn * (1 - sp_3$F))
    sp_4$surv <- rbinom(nrow(sp_4), 1, sp_4$preSpawn * (1 - sp_4$F))
    # toc()
  }

  # Saco River:
  if (river == "saco") {
    # Combine all data for bypass migrants
    # Combine both matrices
    spawnData_1 <- cbind(traits_1, moves_1[, ncol(moves_1)], delay_1)
    # Change the name for the final rkm column
    colnames(spawnData_1)[ncol(spawnData_1) - nDams] <- "finalRkm"
    # Make it into a dataframe for easy manipulation
    sp_1 <- data.frame(spawnData_1)

    # Assign each fish to a production unit before they spawn. Do this for
    # bypass and Mainstem migrants
    # First, assign rkms to delineate each of the production units
    puRkm <- vector(mode = "list", length = length(nPU))
    puRkm[[1]] <- c(0, damRkms[[1]] + 1, (maxrkm[1] + 1))

    # Create an empty list to hold the pu names for each route
    rm(list = ls()[grep(ls(), pat = "^mPU_")]) # Remove old counts
    puNames <- vector(mode = "list", length = length(nPU))
    # Dynamically assign pu names based on river kilometers that delineate them
    for (t in 1:length(puRkm)) {
      for (i in 1:(length(puRkm[[t]]) - 1)) {
        assign(paste("PU_", t, "_", i, sep = ""), puRkm[[t]][i])
      }
      # Collect the names into a list
      puNames[[t]] <- names(mget(ls(pat = paste(
        "^PU_", t,
        sep = ""
      ))))
    }
    # Determine which PU each fish ends up in based on its rkm and assign it.
    # Uses pre-compiled function 'fishPU' from source files loaded up front.
    # Main-to-piscataquis spawners
    sp_1$pus <- as.character(fishPU(puRkm[[1]], sp_1$finalRkm, puNames[[1]]))

    # Replace the blank PUs for fish that ended
    # at head of tide
    sp_1$pus[sp_1$pus == ""] <- "PU_1_1"

    # Determine the probability that a fish survives to spawn
    # Pre-spawning mortality by sex
    sp_1$preSpawn <- sp_1$female * pre_spawn_survival_females +
      (1 - sp_1$female) * pre_spawn_survival_males

    # Determine fishing mortality by PU
    sp_1$F <- inriv[[1]][as.numeric(substrRight(sp_1$pus, 1))]

    # Apply in-river fishing mortality and prespawn survival
    sp_1$surv <- rbinom(nrow(sp_1), 1, sp_1$preSpawn * (1 - sp_1$F))
  }

  # Kennebec River
  if (river == "kennebec") {
    # Combine all data for mainstem to piscataquis
    # Combine all three matrices
    spawnData_1 <- cbind(traits_1, moves_1[, ncol(moves_1)], delay_1)
    # Change the name for the final rkm column
    colnames(spawnData_1)[ncol(spawnData_1) - 4] <- "finalRkm"
    # Make it into a dataframe for easy manipulation
    sp_1 <- data.frame(spawnData_1)

    # Combine all data for main-to-mainstem spawners
    # Combine all three matrices
    spawnData_2 <- cbind(traits_2, moves_2[, ncol(moves_2)], delay_2)
    # Change the name for the final rkm column
    colnames(spawnData_2)[ncol(spawnData_2) - 2] <- "finalRkm"
    # Make it into a dataframe for easy manipulation
    sp_2 <- data.frame(spawnData_2)

    # Assign each fish to a production unit before they spawn. Do this for
    # Piscataquis River spawners and Mainstem spawners
    # First, assign rkms to delineate each of the production units
    puRkm <- vector(mode = "list", length = length(nPU))
    puRkm[[1]] <- c(0, damRkms[[1]] + 1, (maxrkm[1] + 1))
    puRkm[[2]] <- c(0, damRkms[[2]] + 1, (maxrkm[2] + 1))
    # Create an empty list to hold the pu names for each route
    rm(list = ls()[grep(ls(), pat = "^mPU_")]) # Remove old counts
    puNames <- vector(mode = "list", length = length(nPU))
    # Dynamically assign pu names based on river kilometers that delineate them
    for (t in 1:length(puRkm)) {
      for (i in 1:(length(puRkm[[t]]) - 1)) {
        assign(paste("PU_", t, "_", i, sep = ""), puRkm[i])
      }
      # Collect the names into a list
      puNames[[t]] <- names(mget(ls(pat = paste(
        "^PU_", t,
        sep = ""
      ))))
    }
    # Determine which PU each fish ends up in based on its rkm and assign it.
    # Uses pre-compiled function 'fishPU' from source files loaded up front.
    # Mainstem
    sp_1$pus <- as.character(fishPU(puRkm[[1]], sp_1$finalRkm, puNames[[1]]))
    # Sebasticook
    sp_2$pus <- as.character(fishPU(puRkm[[2]], sp_2$finalRkm, puNames[[2]]))

    # Replace the blank PUs for fish that ended head of tide
    sp_1$pus[sp_1$pus == ""] <- "PU_1_1"
    sp_2$pus[sp_2$pus == ""] <- "PU_2_1"

    # Determine the probability that a fish survives to spawn
    # Pre-spawning mortality by sex
    sp_1$preSpawn <- sp_1$female * pre_spawn_survival_females +
      (1 - sp_1$female) * pre_spawn_survival_males
    sp_2$preSpawn <- sp_2$female * pre_spawn_survival_females +
      (1 - sp_2$female) * pre_spawn_survival_males

    # Determine fishing mortality by PU
    sp_1$F <- inriv[[1]][as.numeric(substrRight(sp_1$pus, 1))]
    sp_2$F <- inriv[[2]][as.numeric(substrRight(sp_2$pus, 1))]

    # Apply in-river fishing mortality and prespawn survival
    sp_1$surv <- rbinom(nrow(sp_1), 1, sp_1$preSpawn * (1 - sp_1$F))
    sp_2$surv <- rbinom(nrow(sp_2), 1, sp_2$preSpawn * (1 - sp_2$F))
    # toc()
  }

  # Hudson River
  if (river == "hudson") {
    # Combine all data for route 1
    spawnData_1 <- cbind(traits_1, moves_1[, ncol(moves_1)], delay_1)
    # Change the name for the final rkm column
    colnames(spawnData_1)[ncol(spawnData_1) - nDams[1]] <- "finalRkm"
    # Make it into a dataframe for easy manipulation
    sp_1 <- data.frame(spawnData_1)

    # Combine all data for route 2
    # Combine all three matrices
    spawnData_2 <- cbind(traits_2, moves_2[, ncol(moves_2)], delay_2)
    # Change the name for the final rkm column
    colnames(spawnData_2)[ncol(spawnData_2) - nDams[2]] <- "finalRkm"
    # Make it into a dataframe for easy manipulation
    sp_2 <- data.frame(spawnData_2)

    # Assign each fish to a production unit before they spawn.
    # First, assign rkms to delineate each of the production units
    puRkm <- vector(mode = "list", length = length(nPU))
    puRkm[[1]] <- c(0, damRkms[[1]] + 1, (maxrkm[1] + 1))
    puRkm[[2]] <- c(0, damRkms[[2]] + 1, (maxrkm[2] + 1))
    # Create an empty list to hold the pu names for each route
    rm(list = ls()[grep(ls(), pat = "^mPU_")]) # Remove old counts
    puNames <- vector(mode = "list", length = length(nPU))
    # Dynamically assign pu names based on river kilometers that delineate them
    for (t in 1:length(puRkm)) {
      for (i in 1:(length(puRkm[[t]]) - 1)) {
        assign(paste("PU_", t, "_", i, sep = ""), puRkm[i])
      }
      # Collect the names into a list
      puNames[[t]] <- names(mget(ls(pat = paste(
        "^PU_", t,
        sep = ""
      ))))
    }
    puNames[[2]] <- puNames[[2]][order(
      as.numeric(
        gsub(
          x = substrRight(sapply(puNames[[2]], head, 1), 2),
          pattern = "_", replacement = ""
        )
      )
    )]
    # Determine which PU each fish ends up in based on its rkm and assign it.
    # Uses pre-compiled function 'fishPU' from source files loaded up front.
    # Mainstem
    sp_1$pus <- as.character(fishPU(puRkm[[1]], sp_1$finalRkm, puNames[[1]]))
    # Sebasticook
    sp_2$pus <- as.character(fishPU(puRkm[[2]], sp_2$finalRkm, puNames[[2]]))

    # Replace the blank PUs for fish that ended head of tide
    sp_1$pus[sp_1$pus == ""] <- "PU_1_1"
    sp_2$pus[sp_2$pus == ""] <- "PU_2_1"

    # Determine the probability that a fish survives to spawn
    # Pre-spawning mortality by sex.
    sp_1$preSpawn <- sp_1$female * pre_spawn_survival_females +
      (1 - sp_1$female) * pre_spawn_survival_males
    sp_2$preSpawn <- sp_2$female * pre_spawn_survival_females +
      (1 - sp_2$female) * pre_spawn_survival_males

    # Determine fishing mortality by PU
    sp_1$F <- inriv[[1]][as.numeric(substrRight(sp_1$pus, 1))]
    sp_2$F <- inriv[[2]][as.numeric(substr(sp_2$pus, 6, 7))]

    # Fetch cumulative lock mortality by PU
    sp_1$up_mort <- up_mort[[1]][as.numeric(substrRight(sp_1$pus, 1))]
    sp_2$up_mort <- up_mort[[2]][as.numeric(substr(sp_2$pus, 6, 7))]
    
    # Apply in-river fishing mortality and prespawn survival
    sp_1$surv <- rbinom(nrow(sp_1), 1, 
                        sp_1$preSpawn * (1 - sp_1$F) * (1 - sp_1$up_mort))
    sp_2$surv <- rbinom(nrow(sp_2), 1, 
                        sp_2$preSpawn * (1 - sp_2$F) * (1 - sp_2$up_mort))
  }

  # Data return to calling environment
  # Penobscot River:
  if (river == "penobscot" | river == "susquehanna") {
    return(list(
      b.mat = b.mat,
      r.mat = r.mat,
      c_BF = c_BF,
      c_end = c_end,
      c_entryDate = c_entryDate,
      c_female_lf = c_female_lf,
      c_fishAges = c_fishAges,
      c_initial = c_initial,
      c_male_lf = c_male_lf,
      c_RAF = c_RAF,
      c_repeat = c_repeat,
      c_RT = c_RT,
      c_sex = c_sex,
      c_SI = c_SI,
      c_spawnATU1 = c_spawnATU1,
      c_spawnATU2 = c_spawnATU2,
      dailyMove = dailyMove,
      dMax = dMax,
      juvenile_survival = juvenile_survival,
      k_pus = k_pus,
      maxR = maxR,
      mot = mot,
      marineS = marineS,
      post_spawn_survival_females = post_spawn_survival_females,
      post_spawn_survival_males = post_spawn_survival_males,
      pre_spawn_survival_females = pre_spawn_survival_females,
      pre_spawn_survival_males = pre_spawn_survival_males,
      puNames = puNames,
      puRkm = puRkm,
      sexRatio = sexRatio,
      sOptim = sOptim,
      habStoch = habStoch,
      tort = tort,
      sp_1 = sp_1,
      sp_2 = sp_2,
      sp_3 = sp_3,
      sp_4 = sp_4
    ))
  }

  # Rivers that have 2 migration routes:
  if (river %in% c("connecticut", "merrimack", "kennebec", "hudson")) {
    return(list(
      b.mat = b.mat,
      r.mat = r.mat,
      c_BF = c_BF,
      c_end = c_end,
      c_entryDate = c_entryDate,
      c_female_lf = c_female_lf,
      c_fishAges = c_fishAges,
      c_initial = c_initial,
      c_male_lf = c_male_lf,
      c_RAF = c_RAF,
      c_repeat = c_repeat,
      c_RT = c_RT,
      c_sex = c_sex,
      c_SI = c_SI,
      c_spawnATU1 = c_spawnATU1,
      c_spawnATU2 = c_spawnATU2,
      dailyMove = dailyMove,
      dMax = dMax,
      juvenile_survival = juvenile_survival,
      k_pus = k_pus,
      maxR = maxR,
      mot = mot,
      marineS = marineS,
      post_spawn_survival_females = post_spawn_survival_females,
      post_spawn_survival_males = post_spawn_survival_males,
      pre_spawn_survival_females = pre_spawn_survival_females,
      pre_spawn_survival_males = pre_spawn_survival_males,
      puNames = puNames,
      puRkm = puRkm,
      sexRatio = sexRatio,
      sOptim = sOptim,
      habStoch = habStoch,
      tort = tort,
      sp_1 = sp_1,
      sp_2 = sp_2
    ))
  }

  # Saco River:
  if (river == "saco") {
    return(list(
      b.mat = b.mat,
      r.mat = r.mat,
      c_BF = c_BF,
      c_end = c_end,
      c_entryDate = c_entryDate,
      c_female_lf = c_female_lf,
      c_fishAges = c_fishAges,
      c_initial = c_initial,
      c_male_lf = c_male_lf,
      c_RAF = c_RAF,
      c_repeat = c_repeat,
      c_RT = c_RT,
      c_sex = c_sex,
      c_SI = c_SI,
      c_spawnATU1 = c_spawnATU1,
      c_spawnATU2 = c_spawnATU2,
      dailyMove = dailyMove,
      dMax = dMax,
      juvenile_survival = juvenile_survival,
      k_pus = k_pus,
      maxR = maxR,
      mot = mot,
      marineS = marineS,
      post_spawn_survival_females = post_spawn_survival_females,
      post_spawn_survival_males = post_spawn_survival_males,
      pre_spawn_survival_females = pre_spawn_survival_females,
      pre_spawn_survival_males = pre_spawn_survival_males,
      puNames = puNames,
      puRkm = puRkm,
      sexRatio = sexRatio,
      sOptim = sOptim,
      habStoch = habStoch,
      tort = tort,
      sp_1 = sp_1
    ))
  }
}
