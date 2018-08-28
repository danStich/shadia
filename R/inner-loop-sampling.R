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
innerLoopSampling <- function(){

# Habitat stochasticity -----
# Add stochasticity to habitat per PU
  habStoch <- runif(1, 0.95, 1.05)  
# Add stochastic noise to habitat variable
# to assess sensitivity and account for 
# fluctuations
  habitat <- addStochList(habitat, habStoch)  
  
# Simulate daily temperatures ------------------------------
# Use historical temperature data to predict temperature on each day from a
# multivariate normal distribution
# Make an empty matrix to hold daily temperature predictions within each year
# for annual simulations below

#if (useTictoc) tic("simulate daily temp1: pred NEW")
pred <- data.frame(matrix(0, nrow = 366, ncol = 2))

# Simulate annual temperature
Year <- sample(unique(mu$year), 1, replace = TRUE)

# JMS: precalculate before the loop:
muInYear <-  mu[, 2] == Year
uniqueMuDay <- unique(mu[, 3])
sigma <- cor(mu[muInYear, c(1, 3)])

predRowcount <- nrow(pred)
for (i in 1:predRowcount) {
  # For each day that year
  if (i %in% mu[muInYear, 3]) {
    iIndex <- muInYear & mu[, 3] == uniqueMuDay[i]

    x <- mu[, 1] [iIndex]
    z <- mu[, 3] [iIndex]
    pred[i,] <- mvrnorm(1,
                       mu = c(x, z),
                       Sigma = sigma,
                       tol = 1e-6
                       )
    pred[i,][pred[i,] < 0] <- 0
  } else {
    next
  }  # Else
}  # i
#toc() #("simulate daily temp1: pred")
rm(muInYear, uniqueMuDay, sigma, predRowcount)

#if (useTictoc) tic("simulate daily temp2: pred merge, ddply, cumsum")
pred <- pred [with(pred, order(X2)),]
pred <- pred[pred[, 2] != 0,]
pred[, 2] <- round(pred[, 2])
pred <- ddply(pred, ~ X2, summarize, X1 = mean(X1))
id <- data.frame(seq(1, 366, 1), NA)
names(id) <- c('X2', 'X1')
y <- merge(pred, id, by = 'X2', all.y = TRUE)[, c(1, 2)]
y[1, 2] <- 0
y[nrow(y), 2] <- 0
y <- na.spline(y)
y[y[, 2] < 0, 2] <- 0
predTemps <- data.frame(y)

# Calculate ATU for each day of simulated temperature data
newTU <- cumsum(predTemps[, 2])
#toc() #("simulate daily temp2: pred merge, ddply, cumsum")

# DRAW CDF for PROBABILITY OF ARRIVAL BASED ON COMMERCIAL HARVEST ---------
#if (useTictoc) tic("DRAW CDF for PROBABILITY OF ARRIVAL")
#newDay = seq(0, 365, 1)
stoch <- runif(1,-1.96, 1.96)
# Can come back and delete stoch from usage because...->

# Re-written to use regression params instead of data. These are now
# in built-in data sets for arrival regressions so that we are not
# violating data confidentiality agreements for package deployment.
res.R <- data.frame(arr.R[[sample(1:length(arr.R), 1)]])
res.B <- data.frame(arr.B[[sample(1:length(arr.B), 1)]])
r.prob <- invlogit(res.R[1, 1] + res.R[2, 1] * predTemps[, 2])
b.prob <- invlogit(res.B[1, 1] + res.B[2, 1] * predTemps[, 2])

#toc()
# ---

# CREATE AGE AND SEX STRUCTURE, DRAW ARRIVAL AND SPAWN DATES --------------
# Draw sex ratio for the current year
#if (useTictoc) tic("fish ages1")
sex_Ratio <- rbeta(1, 100, 100)

# Create an object containing the age of each fish based on the number of fish
# in each age class
fishAges <- c()
for (i in 1:length(spawningPool)) {
  fishAges <- append(fishAges, rep(names(spawningPool)[i], spawningPool[i]))
}
c_fishAges <- as.numeric(substr(fishAges, start = 4, stop = nchar(fishAges)))

# Assign fish gender using sex ratio drawn above, females are 1
c_sex <- rbinom(length(fishAges), 1, sex_Ratio)

# Need to sort by sex to get sex-specific arrival date efficiently
c_fishAges <- c_fishAges[order(c_sex)] # First fish ages
c_sex <- sort(c_sex)                   # Then fish sex

# Get entry date for each individual.  We used cumulative frequency distribution
# to predict average entry date for a given fish conditional on temperature.
# We will now use this relationship to predict individual
# fish presence in the river for all 366 days of the year conditional date.
# The actual draws for each fish on each day are occurring in a C++ file
# (entryC.Cpp) that was sourced on the front-end to speed this up.  Once we have
# probability of being in the river on a given day, we will use the first date
# that maximizes probability of success in a random binomial draw for each
# individual to assign their entry date.
# Make containers to hold entry date
b.entryDate <- matrix(0, nrow = length(fishAges[c_sex == 0]), ncol = length(b.prob))
r.entryDate <- matrix(0, nrow = length(fishAges[c_sex == 1]), ncol = length(r.prob))
#toc()

#if (useTictoc) tic("fish ages2: C++ function, entryC")
b.entry <- entryC(b.prob, b.entryDate, length(fishAges[c_sex == 0]))
r.entry <- entryC(r.prob, r.entryDate, length(fishAges[c_sex == 1]))
#toc()

# Combine male and female spawners into one matrix
#if (useTictoc) tic("fish ages3: combine into one matrix NEW")

# JMS: initialize/preallocate: get row and col counts
entryCols <- ncol(b.entry) # same for r.entry col count
entryRows <- nrow(b.entry) + nrow(r.entry)

# define size and characteristics of the data frame
entry <- data.frame(matrix(0,
                           ncol = entryCols,
                           nrow = entryRows),
                    row.names = NULL,
                    check.names=FALSE,
                    fix.empty.names = TRUE,
                    stringsAsFactors = FALSE)

# We initialize with drop = FALSE just in case there are
# no individuals left
entry <- rbind(b.entry, r.entry, drop = FALSE)

# Note that we need to cut out some of the wilder predictions.
entry[, 1:60] <- 0
entry[, 200:entryCols] <- 0

# Remove last row to undo effect of drop=FALSE above if
# there are any individuals left. Need to cast as a matrix
# in case there is only one row left, in which case R tries
# to default to vector
if (nrow(entry) > 1) { # we are altering nrow inside the loop...dirty!!
  entry <- matrix(entry[1:(nrow(entry) - 1),], ncol=366)
}
# if we have changed the object above
entryRowsNew <- nrow(entry)

# Added conditional in vector pre-allocation below.
# We do a check to see if there are any fish left.
c_entryDate <- vector(mode = 'numeric', length = max(1,entryRowsNew, na.rm=T))
for (i in 1:length(c_entryDate)) {
  c_entryDate[i] <- which(entry[i,] == max(entry[i,], na.rm = TRUE))[1]
}
rm(entryRowsNew, entryCols, entryRows)

#toc() # combine into one matrix

# Draw terminal spawning ATU from a truncated normal distribution based on
# mean and standard deviaion of mean and standard deviation of ATU at which
# minimum of 50% of the population has entered the river based on our GLM in the
# previous sections.  We use a truncated normal to prevent fish spawning before
# they ever arrive.  These data match pretty well to the results of Hyle et al.
# (2014), but our standard deviation naturally is larger.
# Draw initial and terminal spawning dates based on temperatures at Turner's
# Falls.
# Randomly draw ATU for initation (1) and termination (2) of spawning for each
c_spawnATU1 <- rnorm(length(c_entryDate), 150, 15)
c_spawnATU2 <- rnorm(length(c_entryDate), 500, 15)
# Determine on which day the threshold ATU for each individual is reached
# Pre-allocate vectors
c_initial <- vector(mode = 'numeric', length = length(c_entryDate))
c_end <- vector(mode = 'numeric', length = length(c_entryDate))
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

# Calculate photoperiod based on latitude and day
  # Penobscot River:
  if(river=='penobscot'){
    photo <- daylength(44.39, day)
  }
  # Merrimack River:
  if(river=='merrimack'){
    photo <- daylength(42.65, day)
  }
  # Connecticut River:
  if(river=='connecticut'){
    photo <- daylength(42.09, day)
  }
  # Susquehanna River:
  if(river=='susquehanna'){
    photo <- daylength(40.88, day)
  }
#toc()

# SIMULATE FISH CHARACTERISTICS FOR EACH FISH IN EACH YEAR ----------------
# Get fork length for each individual in each age class of spawning pool
# Data from each river are subsampled to fit sex-specific growth curves
# before inclusion and used predict length for roes and bucks. We fit 1,000
# growth curves for each pop using subsampled data and sample the parameter
# estimates from sex- and system-specific curves stored in built-in datasets.
# Penobscot River currently uses data from Connecticut River, and Merrimack
# and Susquehanna use lengths for juvenile shad in the Connecticut to
# supplement growth information because juvenile data are lacking for
# those systems.

#if (useTictoc) tic("simulate fish characteristics")

# Roes
  # Get the parameters from river-specific rda files. Otherwise, the
  # default parameter set for the CTR is loaded for each sex.
  r.pars <- r.parms[[sample(1:length(r.parms), 1)]]
  if(river=='merrimack'){
    r.pars <- r.parms_merrimack[[sample(1:length(r.parms_merrimack), 1)]]
  }
  if(river=='susquehanna'){
    r.pars <- r.parms_susquehanna[[sample(1:length(r.parms_susquehanna), 1)]]
  }
  r.mat <- r.pars[, 1]
  # Rename them for ease of use
  c_linF <- r.mat[1] # L-infinity females
  c_kF <- r.mat[2]   # Brody growth coeff females
  c_t0F <- r.mat[3]  # Intercept of VBGM females

# Males
  # Get the parameters
  b.pars <- b.parms[[sample(1:length(b.parms), 1)]]
  if(river=='merrimack'){
    b.pars <- b.parms_merrimack[[sample(1:length(b.parms_merrimack), 1)]]
  }
  if(river=='susquehanna'){
    b.pars <- b.parms_susquehanna[[sample(1:length(b.parms_susquehanna), 1)]]
  }  
  b.mat <- b.pars[, 1]
  # Rename them for ease of use
  c_linM <- b.mat[1] # L-infinity males
  c_kM <- b.mat[2]   # Brody growth coeff males
  c_t0M <- b.mat[3]  # Intercept of VBGM males

# Create length-weight regressions for males and females from the CTDEEP data
# Roes
roelw <- roe.lw[sample(nrow(roe.lw), 200, replace = TRUE),]
r.lw <- lm(r.w ~ r.l, data = roelw)
r.res <- data.frame(summary(r.lw)$coefficients[, 1])
c_femaleLWalpha <- r.res[1, 1]  # Alpha in male l-w relationship
c_femaleLWbeta <- r.res[2, 1]   # Beta in male l-w relationship
# Bucks
bucklw <- buck.lw[sample(nrow(buck.lw), 200, replace = TRUE),]
b.lw <- lm(b.w ~ b.l, data = bucklw)
b.res <- data.frame(summary(b.lw)$coefficients[, 1])
c_maleLWalpha <- b.res[1, 1] # Alpha in male l-w relationship
c_maleLWbeta <- b.res[2, 1]   # Beta in male l-w relationship

# Calculate length, mass, and movement rates, and fecundity
# Get columns representing logical for male and female
c_female <- c_sex
c_male <- as.numeric(factor(c_sex, levels = c(1, 0))) - 1
# Calculate length of males
c_male_lf <- c_male * c_linM * (1 - exp(-c_kM * (c_fishAges - c_t0M)))
# Calculate length of females
c_female_lf <- c_female * c_linF * (1 - exp(-c_kF * (c_fishAges - c_t0F)))
# Calculate mass of males
c_male_m <- c_male * (c_maleLWalpha + c_maleLWbeta * c_male_lf)
# Calculate mass of females
c_female_m <- c_female * (c_femaleLWalpha + c_femaleLWbeta * c_female_lf)
# Collect fork length and mass into one column each
c_forkLength <- c_male_lf + c_female_lf
c_mass <- c_male_m + c_female_m
# Convert fork length to mm from cm for movement calcs below
c_forkLength <- c_forkLength * 10

# Calculate movement rates based on Castro-Santos and Letcher (2010)
# Optimizing ground speed in body lengths per second (BLS)
sOptim <- runif(length(c_mass), .7, 1.7)
# Get max daily movement, converting from BLS to km per day
dMax <- (sOptim * c_forkLength * 86400) / 1e6
# Movement tortuosity drawn from uniform distribution.  This corresponds to
# the range used in Castro-Santos and Letcher (2010) but it's just applied
# as a multiplier
tort <- runif(length(c_fishAges), 0.2, 1)
# Now scale by tortuosity and divide by two to restrict movement to day time
dailyMove <- dMax * tort * mean(photo / 24)

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
c_BF <- vector(mode = 'numeric', length = length(c_repeat))
c_BF[c_repeat == 0] <- sample(rnegbin(10000, 20000, 10),
                             length(c_repeat[c_repeat == 0]), replace = TRUE)
c_BF[c_repeat == 1] <- sample(rnegbin(10000, 30000, 10),
                             length(c_repeat[c_repeat == 1]), replace = TRUE)
# Calculate realized annual fecundity
#c_RAF = vector(mode = 'numeric', length=length(c_RT))
#for(i in 1:length(c_RT)){
#if((c_RT[i]/c_SI[i]) < 10 ) {
c_RAF <- c_BF * (c_RT / c_SI)
#} else {
#c_RAF[i] = c_BF[i] * 10
#}
#}
# Multiply by sex variable to set male fecundity to zero
c_fecundity <- c_female * c_RAF

# Upstream and downstream migration routes for Penobscot River
if(river=='penobscot'){
  # Assign upstream and downstream migration routes probabilistically conditional
  # on flow or production potential (upstream) and flow (downstream). NOTE: NEED
  # TO ADD IN THE FLOW DATA AND CONDITIONAL RELATIONSHIPS FOR THESE PROBABILITY
  # DISTRIBUTIONS
  # Draw upstream migration path based on flow or else proportional production
  # using a random draw from a multinomial distribution with two outcomes
  #if (useTictoc) tic("upstream migration routes")
  upstream_path <- rmultinom(
    n = length(c_fishAges),
    size = 1,
    prob = c(
      pMainUp * pPiscUp,
      pMainUp * pMainstemUp,
      pStillwaterUp * pPiscUp,
      pStillwaterUp * pMainstemUp
    )
  )
  upstream_path[2,][upstream_path[2,] > 0] <- 2
  upstream_path[3,][upstream_path[3,] > 0] <- 3
  upstream_path[4,][upstream_path[4,] > 0] <- 4
  
  # A '1' is Piscataquis, and a '2' is mainstem
  upstream_path <- upstreamPathC(upstream_path)
}

# Upstream path for merrimack river
if(river=='merrimack'){
  upstream_path <- rbinom(length(c_fishAges), 1, pBypassUp)
  upstream_path[upstream_path==0] <- 2  
}

if(river=='connecticut'){
# Upstream path for connecticut river.
# Draw migration path based on user-specified value 
# of pSpill (probability of using spillway for 
# upstream migration)
  upstream_path <- rbinom(length(fishAges), 1, pSpillway)
  upstream_path[upstream_path==0] <- 2
}
  
if(river=='susquehanna'){
# Upstream path for susquehanna river.
# Draw migration path based on proportional
# distribution of habitat in each route used 
# for upstream migration)
  upstream_path <- rmultinom(
    n = length(c_fishAges),
    size = 1,
    prob = c(
      p_JuniataUp,
      p_WestBranchUp,
      p_ChemungUp,
      p_NorthBranchUp
    )
  )
  upstream_path[2,][upstream_path[2,] > 0] <- 2
  upstream_path[3,][upstream_path[3,] > 0] <- 3
  upstream_path[4,][upstream_path[4,] > 0] <- 4
  
  # A '1' is Piscataquis, and a '2' is mainstem
  upstream_path <- upstreamPathC(upstream_path)
}

# Collect life-history parameters into a single matrix for c++ loop
# NOTE: the source code for the loop was re-written to preclude the need for
# these matrices. Instead, they are related to the ABM input and output post-
# hoc to speed things up.
getEm <- mget(ls(pat = '^c_'))
for (i in 1:length(getEm)) {
  if (is.na(getEm[[i]][1])) {
    getEm[[i]][1] = 0
  } else {
    next
  }
}
traits <- as.matrix(data.frame(getEm))
colnames(traits) <- gsub(pattern = "c_", replacement = "", colnames(traits))

# Create traits for all spawners by river system
  if(river=='penobscot' | river=='susquehanna'){
    # Re-organize the data so they match the output of the ABM below
    # Create a df for traits of Piscataquis River spawners
    traits_1 <- data.frame(traits[upstream_path == 1, , drop = FALSE],
                          upstream_path[upstream_path == 1])
    # Create a df for traits of Mainstem spawners
    traits_2 <- data.frame(traits[upstream_path == 2, , drop = FALSE],
                          upstream_path[upstream_path == 2])
    # Create a df for traits of Piscataquis River spawners
    traits_3 <- data.frame(traits[upstream_path == 3, , drop = FALSE],
                          upstream_path[upstream_path == 3])
    # Create a df for traits of Mainstem spawners
    traits_4 <- data.frame(traits[upstream_path == 4, , drop = FALSE],
                          upstream_path[upstream_path == 4])
    
    # Change the name of the last column in each of the dfs so they match
    names(traits_1)[ncol(traits_1)] <- 'upstream_path'
    names(traits_2)[ncol(traits_2)] <- 'upstream_path'
    names(traits_3)[ncol(traits_3)] <- 'upstream_path'
    names(traits_4)[ncol(traits_4)] <- 'upstream_path'
    #toc()
  }

  if(river=='merrimack'){
    # Re-organize the data so they match the output of the ABM below
    # Create a df for traits of Piscataquis River spawners
    traits_1 <- data.frame(traits[upstream_path == 1, , drop = FALSE],
                          upstream_path[upstream_path == 1])
    traits_2 <- data.frame(traits[upstream_path == 2, , drop = FALSE],
                          upstream_path[upstream_path == 2])    
    # Change the name of the last column in each of the dfs so they match
    names(traits_1)[ncol(traits_1)] <- 'upstream_path'
    names(traits_2)[ncol(traits_2)] <- 'upstream_path'
    #toc()
  }

  if(river=='connecticut'){
    # Re-organize the data so they match the output of the ABM below
    # Create a df for traits of Piscataquis River spawners
    traits_1 <- data.frame(traits[upstream_path == 1, , drop = FALSE],
                          upstream_path[upstream_path == 1])
    traits_2 <- data.frame(traits[upstream_path == 2, , drop = FALSE],
                          upstream_path[upstream_path == 2])    
    # Change the name of the last column in each of the dfs so they match
    names(traits_1)[ncol(traits_1)] <- 'upstream_path'
    names(traits_2)[ncol(traits_2)] <- 'upstream_path'
    #toc()
  }

# DEFINE VARIABLES FOR SPAWNING DYNAMICS ----------------------------------
# Carrying capacity for juvs based on potential production of adult shad in each
# production unit based on values from the 2009 multi-species management plan
# NOTE: These are divided by 1000 to make the simulations run faster!
# Everything is scaled up on output
#if (useTictoc) tic("spawn dynamics variables")
k_pus <- vector(mode = 'list', length = length(habitat))
batch <- quantile(rnegbin(1e2, 2.5e4, 10), 0.5)[1]

# Carrying capacity for each migration route 
for(i in 1:length(k_pus)){
  k_pus[[i]] <- ((habitat[[i]] / scalar) * sex_Ratio * batch)
}
  
k_pus <- lapply(k_pus, function(x) {
  x[is.na(x)] = 1
  x
})

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

# Ocean survival for each year after the first year for each age class
# Can be made into age-specific survival. Right now everyone has the same
# annual survival rate in the ocean.
oceanSurvival <- rep(rbeta(1, 12, 8), maxAge)

# Juvenile mortality rate
# This really needs some data pretty bad. Right now it is just a draw from a
# uniform probability distribution that calls it 1 in 100000 to 1 in 1000
juvenile_survival <- runif(1, 0.0005, 0.00083)
#toc()

# FISH PASSAGE RATES AT DAMS ----------------------------------------------
# Convert passage performance standards into rates over time based on passage
# efficiency and timely
#if (useTictoc) tic("passage rates at dams")
up_effs <- mapply('+', upEffs, 1, SIMPLIFY = FALSE)
for(i in 1:length(up_effs)){
  up_effs[[i]] <- mapply('^', up_effs[[i]], (1 / times[[i]]))
}
up_effs <- mapply('-', up_effs, 1, SIMPLIFY = FALSE)

# Finally, assign passage efficiencies for each reach in the river
eFFs <- vector(mode = 'list', length = length(upEffs))

# Fill with Open passage efficiency to begin and
# replace efficiencies where there are dams.
# The first one works for all rivers because they all have
# at least one passage route.
for(i in 1:length(eFFs)){
  eFFs[[i]] <- c(rep(Open, maxrkm[i])) # Create perfect passage for group 1
  eFFs[[i]][damRkms[[i]]] <- up_effs[[i]] # Dam-specific efficiencies group 1
}

#toc()

# UPSTREAM MIGRATION FOR EACH FISH IN EACH YEAR ---------------------------
# Make the adult fish move upstream through space and time. The functions used
# in this section call a collection of C++ and header files that were sourced
# the front-end of this r script. # Add in a motivation penalty based on photoperiod. Fish are assumed to most
# motivated at the peak of the run. Makes a passage efficiency for each
# rkm on each day.

# Make a list of empty matrices to hold the results
ppPenalty <- vector(mode = 'list', length = length(eFFs))
  # Fill in the first element of the list for all rivers
  for(i in 1:length(ppPenalty)){
    ppPenalty[[i]] <-  matrix(0 , length(photo), maxrkm[i])
  }

# Multiply passage efficiency by the penalty for each day
  #if (useTictoc) {
  #  tic("C++ function, motivationPenaltyC")
  #}
  newTU_2 <- newTU[min(c_entryDate):max(c_end)]
  # Fill in the first element of the list for all rivers
  for(i in 1:length(ppPenalty)){
  ppPenalty[[i]] <-  motivationPenaltyC(eFFs[[i]], newTU_2, ppPenalty[[i]])
  }
  #toc() # C++ function, motivationPenaltyC

# Track the mean motivational penalty for the spawning season
# to be used in sensitivity analyses
mot <- mean((1 - (newTU - min(newTU)) /
              (max(newTU) - min(newTU)))[min(c_entryDate):max(c_end)])

# Pre-allocate vectors and matrices for agent-based migration model
# These need to be RIVER SPECIFIC. For the Penobscot River, this can
# be done in bulk for both Piscataquis River spawners and Mainstem spawners
# because we use vectorization to select the appropriate elements later on.
  # For Penobscot River:
    if(river=='penobscot'){
      rkm1 <- rep(41, length(c_fishAges))
    }
    # For Merrimack River:
    if(river=='merrimack'){
      rkm1 <- rep(35, length(c_fishAges))
    }
    # For Connecticut River:
    if(river=='connecticut'){
      rkm1 <- rep(90, length(c_fishAges))
    }
    # For Connecticut River:
    if(river=='susquehanna'){
      rkm1 <- rep(0, length(c_fishAges))
    }

    # For all rivers:
    rkm2 <- matrix(0, ncol = length(day), nrow = length(c_fishAges))

# Get max rkm for each fish. Rivers with one passage route skip
# the C++ function because all fish have same max RKM.
  # Create a vector of potential routes
  routes <- seq(1, nRoutes, 1)
  # Run the markmC C++ function to get max rkm for each fish given route
  #if (useTictoc) {
    #tic("C++ function, maxrkmC")
  #}
  maxR <- maxrkmC(c_fishAges, maxrkm, upstream_path, routes)
  #toc()
  
# Run the upstream migration model and get results
# Run the agent-based model for upstream migration
# Get start time for running ABM function in C++
# ptmABM <- proc.time() # Uncomment to time it
  #c_mid <- round(rowMeans(cbind(c_initial, c_end)))
  
#if (useTictoc) {
#  tic("C++ ABM function, moveC")
#}
if(river=='penobscot'){
# Run the ABM for main-to-piscataquis spawners
  moves_1 <- moveC(day,
                  c_entryDate[upstream_path == 1],
                  dailyMove[upstream_path == 1],
                  maxR[upstream_path == 1],
                  ppPenalty[[1]],
                  rkm1[upstream_path == 1],
                  rkm2[upstream_path == 1, , drop = FALSE],
                  c_initial[upstream_path == 1])
  # Run the ABM for main-to-mainstem Spawners
  moves_2 <- moveC(day,
                  c_entryDate[upstream_path == 2],
                  dailyMove[upstream_path == 2],
                  maxR[upstream_path == 2],
                  ppPenalty[[2]],
                  rkm1[upstream_path == 2],
                  rkm2[upstream_path == 2,  , drop = FALSE],
                  c_initial[upstream_path == 2])
  # Run the ABM for stillwater-to-piscataquis Spawners
  moves_3 <- moveC(day,
                  c_entryDate[upstream_path == 3],
                  dailyMove[upstream_path == 3],
                  maxR[upstream_path == 3],
                  ppPenalty[[3]],
                  rkm1[upstream_path == 3],
                  rkm2[upstream_path == 3,  , drop = FALSE],
                  c_initial[upstream_path == 3])
  # Run the ABM for stillwater-to-mainstem Spawners
  moves_4 <- moveC(day,
                  c_entryDate[upstream_path == 4],
                  dailyMove[upstream_path == 4],
                  maxR[upstream_path == 4],
                  ppPenalty[[4]],
                  rkm1[upstream_path == 4],
                  rkm2[upstream_path == 4, , drop = FALSE],
                  c_initial[upstream_path == 4])
}

if(river=='merrimack'){
# Run the ABM for bypass route through Pawtucket
  moves_1 <- moveC(day,
                  c_entryDate[upstream_path == 1],
                  dailyMove[upstream_path == 1],
                  maxR[upstream_path == 1],
                  ppPenalty[[1]],
                  rkm1[upstream_path == 1],
                  rkm2[upstream_path == 1, , drop = FALSE],
                  c_initial[upstream_path == 1])
  # Run the ABM for mainstem route through Pawtucket
  moves_2 <- moveC(day,
                  c_entryDate[upstream_path == 2],
                  dailyMove[upstream_path == 2],
                  maxR[upstream_path == 2],
                  ppPenalty[[2]],
                  rkm1[upstream_path == 2],
                  rkm2[upstream_path == 2,  , drop = FALSE],
                  c_initial[upstream_path == 2])
}

if(river=='connecticut'){
  # Spillway
  moves_1 <- moveC(day,
                  c_entryDate[upstream_path == 1],
                  dailyMove[upstream_path == 1],
                  maxR[upstream_path == 1],
                  ppPenalty[[1]],
                  rkm1[upstream_path == 1],
                  rkm2[upstream_path == 1, , drop = FALSE],
                  c_end[upstream_path == 1])
  # Canal
  moves_2 <- moveC(day,
                  c_entryDate[upstream_path == 2],
                  dailyMove[upstream_path == 2],
                  maxR[upstream_path == 2],
                  ppPenalty[[2]],
                  rkm1[upstream_path == 2],
                  rkm2[upstream_path == 2,  , drop = FALSE],
                  c_end[upstream_path == 2])
}

if(river=='susquehanna'){
  # Juniata River
  moves_1 <- moveC(day,
                  c_entryDate[upstream_path == 1],
                  dailyMove[upstream_path == 1],
                  maxR[upstream_path == 1],
                  ppPenalty[[1]],
                  rkm1[upstream_path == 1],
                  rkm2[upstream_path == 1, , drop = FALSE],
                  c_end[upstream_path == 1])
  # West Branch
  moves_2 <- moveC(day,
                  c_entryDate[upstream_path == 2],
                  dailyMove[upstream_path == 2],
                  maxR[upstream_path == 2],
                  ppPenalty[[2]],
                  rkm1[upstream_path == 2],
                  rkm2[upstream_path == 2,  , drop = FALSE],
                  c_end[upstream_path == 2])
  # Chemung
  moves_3 <- moveC(day,
                  c_entryDate[upstream_path == 3],
                  dailyMove[upstream_path == 3],
                  maxR[upstream_path == 3],
                  ppPenalty[[3]],
                  rkm1[upstream_path == 3],
                  rkm2[upstream_path == 3,  , drop = FALSE],
                  c_end[upstream_path == 3])
  # North Branch
  moves_4 <- moveC(day,
                  c_entryDate[upstream_path == 4],
                  dailyMove[upstream_path == 4],
                  maxR[upstream_path == 4],
                  ppPenalty[[4]],
                  rkm1[upstream_path == 4],
                  rkm2[upstream_path == 4, , drop = FALSE],
                  c_end[upstream_path == 4])  
}  
  
# Calculate total run time for ABM
#timeABM <- proc.time() - ptmABM # Uncomment to time it

#toc() # C++ ABM function, moveC

# Calculate delay for each fish in each migration route
# Start timing the delay function in C++

#if (useTictoc) {
#  tic("C++ delay function, delayC")
#}

#ptmDelay  <- proc.time() # Uncomment to time it

# Calculate delay at each dam for each fish in the first 
# migration route for all rivers. 
  delay_1 <- delayC(moves_1, damRkms[[1]][2:nPU[1]])
  # Remaining routes for connecticut River
  if(river=='connecticut' | river=='merrimack'){
  # Calculate delay at each dam for each main-to-Mainstem spawners
  delay_2 <- delayC(moves_2, damRkms[[2]][2:nPU[2]])
  }
  # Remaining routes for penobscot River
  if(river=='penobscot' | river=='susquehanna'){
  # Calculate delay at each dam for each main-to-Mainstem spawners
  delay_2 <- delayC(moves_2, damRkms[[2]][2:nPU[2]])
  # Calculate delay at each dam for each still-to-Piscataquis spawners
  delay_3 <- delayC(moves_3, damRkms[[3]][2:nPU[3]])
  # Calculate delay at each dam for each still-to-Mainstem spawners
  delay_4 <- delayC(moves_4, damRkms[[4]][2:nPU[4]])
  }


# Assign names to the newly created matrices that hold delay at each dam
  # Names for delay matrix: penobscot river
  if(river=='penobscot'){
    # Main-to-Piscataquis spawners
    colnames(delay_1) <- c('dConfluence', 'dMilford', 'dHowland',
                          'dBrownsMill', 'dMoosehead', 'dGuilford')
    # Main-to-Piscataquis spawners
    colnames(delay_2) <- c('dConfluence', 'dMilford', 'dWestEnfield',
                           'dWeldon')
    # Main-to-Piscataquis spawners
    colnames(delay_3) <- c('dOrono', 'dStillwater', 'dGilman',
                           'dHowland', 'dBrownsMill', 'dMoosehead',
                           'dGuilford')
    # Main-to-Piscataquis spawners
    colnames(delay_4) <- c('Orono', 'Stillwater', 'Gilman',
                          'dWestEnfield', 'dWeldon')
  }

  # Names for delay matrix: merrimack
  if(river=='merrimack'){
    colnames(delay_1) <- c('dEssex', 'dPawtucket', 'dAmoskeag', 'dHookset')
    colnames(delay_2) <- c('dEssex', 'dPawtucket', 'dAmoskeag', 'dHookset')
  }
  
  # Names for delay matrix: merrimack
  if(river=='connecticut'){
    colnames(delay_1) <- c('dHolyoke', 'dCabot', 'dGatehouse', 'dVernon')
    colnames(delay_2) <- c('dHolyoke', 'dSpillway', 'dGatehouse', 'dVernon')
  }

  # Names for delay matrix: penobscot river
  if(river=='susquehanna'){
    # Juniata River
    colnames(delay_1) <- c('dConowingo', 'dHoltwood', 'dSafeHarbor',
                           'dYorkHaven', 'djunConf')
    # West Branch
    colnames(delay_2) <- c('dConowingo', 'dHoltwood', 'dSafeHarbor',
                           'dYorkHaven', 'dSunbury', 'dWilliamsport',
                           'dLockHaven')
    # Chemung River
    colnames(delay_3) <- c('dConowingo', 'dHoltwood', 'dSafeHarbor',
                           'dYorkHaven', 'dSunbury', 'dNyLine',
                           'dChaseHibbard')
    # North Branch
    colnames(delay_4) <- c('dConowingo', 'dHoltwood', 'dSafeHarbor',
                           'dYorkHaven', 'dSunbury', 'dNyLine',
                           'dRockBottom', 'dUnadilla', 'dColliersville')
  }
  
# Calculate run time for delay function in C++
  #timeDelay <- proc.time() - ptmDelay
  #toc() # C++ delay function, delayC

# SPAWNING DYNAMICS FOR EACH YEAR WITH ANNUAL VARIABILITY -----------------
# Combine the data for each fish stored in traits with the final rkm of that
# fish and the delay experienced by each fish at each dam for each of the
# upstream passage routes
#if (useTictoc) tic("SPAWNING DYNAMICS FOR EACH YEAR WITH ANNUAL VARIABILITY")

  # Penobscot River
  if(river=='penobscot'){
    # Combine all data for mainstem to piscataquis
    # Combine all three matrices
    spawnData_1 <- cbind(traits_1, moves_1[, ncol(moves_1)], delay_1)
    # Change the name for the final rkm column
    colnames(spawnData_1)[ncol(spawnData_1) - 6] = 'finalRkm'
    # Make it into a dataframe for easy manipulation
    sp_1 <- data.frame(spawnData_1)
    
    # Combine all data for main-to-mainstem spawners
    # Combine all three matrices
    spawnData_2 <- cbind(traits_2, moves_2[, ncol(moves_2)], delay_2)
    # Change the name for the final rkm column
    colnames(spawnData_2)[ncol(spawnData_2) - 4] = 'finalRkm'
    # Make it into a dataframe for easy manipulation
    sp_2 <- data.frame(spawnData_2)
    
    # Combine all data for stillwater-to-piscataquis spawners
    # Combine all three matrices
    spawnData_3 <- cbind(traits_3, moves_3[, ncol(moves_3)], delay_3)
    # Change the name for the final rkm column
    colnames(spawnData_3)[ncol(spawnData_3) - 7] = 'finalRkm'
    # Make it into a dataframe for easy manipulation
    sp_3 <- data.frame(spawnData_3)
    
    # Combine all data for stillwater-to-piscataquis spawners
    # Combine all three matrices
    spawnData_4 <- cbind(traits_4, moves_4[, ncol(moves_4)], delay_4)
    # Change the name for the final rkm column
    colnames(spawnData_4)[ncol(spawnData_4) - 5] = 'finalRkm'
    # Make it into a dataframe for easy manipulation
    sp_4 <- data.frame(spawnData_4)  
    
    # Assign each fish to a production unit before they spawn. Do this for
    # Piscataquis River spawners and Mainstem spawners
    # First, assign rkms to delineate each of the production units
    puRkm <- vector(mode = 'list', length = length(nPU))
    puRkm[[1]] <- c(damRkms[[1]] + 1, (maxrkm[1] + 1))
    puRkm[[2]] <- c(damRkms[[2]] + 1, (maxrkm[2] + 1))
    puRkm[[3]] <- c(damRkms[[3]] + 1, (maxrkm[3] + 1))
    puRkm[[4]] <- c(damRkms[[4]] + 1, (maxrkm[4] + 1))
    # Create an empty list to hold the pu names for each route
    rm(list = ls()[grep(ls(), pat = '^mPU_')]) # Remove old counts
    puNames <- vector(mode = 'list', length = length(nPU))
    # Dynamically assign pu names based on river kilometers that delineate them
    for (t in 1:length(puRkm)) {
      for (i in 1:(length(puRkm[[t]]) - 1)) {
        assign(paste('PU_', t, '_', i, sep = ''), puRkm[i])
      }
      # Collect the names into a list
      puNames[[t]] <- names(mget(ls(pat = paste(
        '^PU_', t, sep = ''
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
    #toc()
  }

  # Merrimack River:
  if(river=='merrimack'){
   # Combine all data for bypass migrants
    # Combine both matrices
    spawnData_1 <- cbind(traits_1, moves_1[, ncol(moves_1)], delay_1)
    # Change the name for the final rkm column
    colnames(spawnData_1)[ncol(spawnData_1) - 4] = 'finalRkm'
    # Make it into a dataframe for easy manipulation
    sp_1 <- data.frame(spawnData_1)
    
    # Combine all data for Cabot migrants
    spawnData_2 <- cbind(traits_2, moves_2[, ncol(moves_2)], delay_2)
    # Change the name for the final rkm column
    colnames(spawnData_2)[ncol(spawnData_2) - 4] = 'finalRkm'
    # Make it into a dataframe for easy manipulation
    sp_2 <- data.frame(spawnData_2)
    
    # Assign each fish to a production unit before they spawn. Do this for
    # Piscataquis River spawners and Mainstem spawners
    # First, assign rkms to delineate each of the production units
    puRkm <- vector(mode = 'list', length = length(nPU))
    puRkm[[1]] <- c(0, damRkms[[1]] + 1, (maxrkm[1] + 1))
    puRkm[[2]] <- c(0, damRkms[[2]] + 1, (maxrkm[2] + 1))

    # Create an empty list to hold the pu names for each route
    rm(list = ls()[grep(ls(), pat = '^mPU_')]) # Remove old counts
    puNames <- vector(mode = 'list', length = length(nPU))
    # Dynamically assign pu names based on river kilometers that delineate them
    for (t in 1:length(puRkm)) {
      for (i in 1:(length(puRkm[[t]]) - 1)) {
        assign(paste('PU_', t, '_', i, sep = ''), puRkm[i])
      }
      # Collect the names into a list
      puNames[[t]] <- names(mget(ls(pat = paste(
        '^PU_', t, sep = ''
      ))))
    }
    # Determine which PU each fish ends up in based on its rkm and assign it.
    # Uses pre-compiled function 'fishPU' from source files loaded up front.
    # Main-to-piscataquis spawners
    sp_1$pus <- as.character(fishPU(puRkm[[1]], sp_1$finalRkm, puNames[[1]]))
    
    # Replace the blank PUs for fish that ended at head of tide
    sp_1$pus[sp_1$pus == ""] <- "PU_1_1"
    
    # Determine the probability that a fish survives to spawn
    # Pre-spawning survival by sex
    sp_1$preSpawn <- sp_1$female * pre_spawn_survival_females +
      (1 - sp_1$female) * pre_spawn_survival_males
    
    # Determine fishing mortality by PU
    sp_1$F <- inriv[[1]][as.numeric(substrRight(sp_1$pus, 1))]
    
    # Apply in-river fishing mortality and prespawn survival
    sp_1$surv <- rbinom(nrow(sp_1), 1, sp_1$preSpawn * (1 - sp_1$F))
    
    #toc()
  }

  # Connecticut River
  if(river=='connecticut'){
    # Combine all data for spillway migrants
    # Combine both matrices
    spawnData_1 <- cbind(traits_1, moves_1[, ncol(moves_1)], delay_1)
    # Change the name for the final rkm column
    colnames(spawnData_1)[ncol(spawnData_1) - 4] = 'finalRkm'
    # Make it into a dataframe for easy manipulation
    sp_1 <- data.frame(spawnData_1)
    
    # Combine all data for Cabot migrants
    spawnData_2 <- cbind(traits_2, moves_2[, ncol(moves_2)], delay_2)
    # Change the name for the final rkm column
    colnames(spawnData_2)[ncol(spawnData_2) - 4] = 'finalRkm'
    # Make it into a dataframe for easy manipulation
    sp_2 <- data.frame(spawnData_2)
    
    # Assign each fish to a production unit before they spawn. Do this for
    # Piscataquis River spawners and Mainstem spawners
    # First, assign rkms to delineate each of the production units
    puRkm <- vector(mode = 'list', length = length(nPU))
    puRkm[[1]] <- c(0, damRkms[[1]] + 1, (maxrkm[1] + 1))
    puRkm[[2]] <- c(0, damRkms[[2]] + 1, (maxrkm[2] + 1))

    # Create an empty list to hold the pu names for each route
    rm(list = ls()[grep(ls(), pat = '^mPU_')]) # Remove old counts
    puNames <- vector(mode = 'list', length = length(nPU))
    # Dynamically assign pu names based on river kilometers that delineate them
    for (t in 1:length(puRkm)) {
      for (i in 1:(length(puRkm[[t]]) - 1)) {
        assign(paste('PU_', t, '_', i, sep = ''), puRkm[i])
      }
      # Collect the names into a list
      puNames[[t]] <- names(mget(ls(pat = paste(
        '^PU_', t, sep = ''
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
    #toc()
  }  
  
  # Susquehanna River
  if(river=='susquehanna'){
    # Combine all data for juniata river
    spawnData_1 <- cbind(traits_1, moves_1[, ncol(moves_1)], delay_1)
    # Change the name for the final rkm column
    colnames(spawnData_1)[ncol(spawnData_1) - 5] = 'finalRkm'
    # Make it into a dataframe for easy manipulation
    sp_1 <- data.frame(spawnData_1)
    
    # Combine all data for west branch
    # Combine all three matrices
    spawnData_2 <- cbind(traits_2, moves_2[, ncol(moves_2)], delay_2)
    # Change the name for the final rkm column
    colnames(spawnData_2)[ncol(spawnData_2) - 7] = 'finalRkm'
    # Make it into a dataframe for easy manipulation
    sp_2 <- data.frame(spawnData_2)
    
    # Combine all data for chemung
    # Combine all three matrices
    spawnData_3 <- cbind(traits_3, moves_3[, ncol(moves_3)], delay_3)
    # Change the name for the final rkm column
    colnames(spawnData_3)[ncol(spawnData_3) - 7] = 'finalRkm'
    # Make it into a dataframe for easy manipulation
    sp_3 <- data.frame(spawnData_3)
    
    # Combine all data for north branch
    # Combine all three matrices
    spawnData_4 <- cbind(traits_4, moves_4[, ncol(moves_4)], delay_4)
    # Change the name for the final rkm column
    colnames(spawnData_4)[ncol(spawnData_4) - 9] = 'finalRkm'
    # Make it into a dataframe for easy manipulation
    sp_4 <- data.frame(spawnData_4)  
    
    # Assign each fish to a production unit before they spawn. Do this for
    # Piscataquis River spawners and Mainstem spawners
    # First, assign rkms to delineate each of the production units
    puRkm <- vector(mode = 'list', length = length(nPU))
    puRkm[[1]] <- c(0, damRkms[[1]] + 1, (maxrkm[1] + 1))
    puRkm[[2]] <- c(0, damRkms[[2]] + 1, (maxrkm[2] + 1))
    puRkm[[3]] <- c(0, damRkms[[3]] + 1, (maxrkm[3] + 1))
    puRkm[[4]] <- c(0, damRkms[[4]] + 1, (maxrkm[4] + 1))
    # Create an empty list to hold the pu names for each route
    rm(list = ls()[grep(ls(), pat = '^mPU_')]) # Remove old counts
    puNames <- vector(mode = 'list', length = length(nPU))
    # Dynamically assign pu names based on river kilometers that delineate them
    for (t in 1:length(puRkm)) {
      for (i in 1:(length(puRkm[[t]]) - 1)) {
        assign(paste('PU_', t, '_', i, sep = ''), puRkm[i])
      }
      # Collect the names into a list
      puNames[[t]] <- names(mget(ls(pat = paste(
        '^PU_', t, sep = ''
      ))))
    }
    puNames[[4]] <- puNames[[4]][order(
      as.numeric(
        gsub(
          x=substrRight(sapply(puNames[[4]], head, 1), 2),
          pattern="_", replacement = '')
        )
      )]
    
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
    sp_4$F <- inriv[[4]][as.numeric(
                          gsub(
                            x=substrRight(sapply(sp_4$pus[[4]], head, 1), 2),
                            pattern="_", replacement = '')
                          )
                        ]
    
    # Apply in-river fishing mortality and prespawn survival
    sp_1$surv <- rbinom(nrow(sp_1), 1, sp_1$preSpawn * (1 - sp_1$F))
    sp_2$surv <- rbinom(nrow(sp_2), 1, sp_2$preSpawn * (1 - sp_2$F))
    sp_3$surv <- rbinom(nrow(sp_3), 1, sp_3$preSpawn * (1 - sp_3$F))
    sp_4$surv <- rbinom(nrow(sp_4), 1, sp_4$preSpawn * (1 - sp_4$F))
    #toc()
  }
  
  
# Data return to calling environment
  # Penobscot River:
  if(river=='penobscot' | river=='susquehanna'){
    return(list(
    b.entry = b.entry,
    b.entryDate = b.entryDate,
    b.lw = b.lw,
    b.mat = b.mat,
    b.pars = b.pars,
    b.prob = b.prob,
    b.res = b.res,
    batch = batch,
    bucklw = bucklw,
    c_BF = c_BF,
    c_end = c_end,
    c_entryDate = c_entryDate,
    c_fecundity = c_fecundity,
    c_female = c_female,
    c_female_lf = c_female_lf,
    c_female_m = c_female_m,
    c_femaleLWalpha = c_femaleLWalpha,
    c_femaleLWbeta = c_femaleLWbeta,
    c_fishAges = c_fishAges,
    c_forkLength = c_forkLength,
    c_initial = c_initial,
    c_kF = c_kF,
    c_kM = c_kM,
    c_linF = c_linF,
    c_linM = c_linM,
    c_male = c_male,
    c_male_lf = c_male_lf,
    c_male_m = c_male_m,
    c_maleLWalpha = c_maleLWalpha,
    c_maleLWbeta = c_maleLWbeta,
    c_mass = c_mass,
    c_RAF = c_RAF,
    c_repeat = c_repeat,
    c_RT = c_RT,
    c_sex = c_sex,
    c_SI = c_SI,
    c_spawnATU1 = c_spawnATU1,
    c_spawnATU2 = c_spawnATU2,
    c_t0F = c_t0F,
    c_t0M = c_t0M,
    dailyMove = dailyMove,
    day = day,
    delay_1 = delay_1,
    delay_2 = delay_2,
    delay_3 = delay_3,
    delay_4 = delay_4,
    dMax = dMax,
    eFFs = eFFs,
    entry = entry,
    fishAges = fishAges,
    id = id,
    juvenile_survival = juvenile_survival,
    k_pus = k_pus,
    maxR = maxR,
    mot = mot,
    moves_1 = moves_1,
    moves_2 = moves_2,
    moves_3 = moves_3,
    moves_4 = moves_4,
    newTU = newTU,
    newTU_2 = newTU_2,
    oceanSurvival = oceanSurvival,
    photo = photo,
    post_spawn_survival_females = post_spawn_survival_females,
    post_spawn_survival_males = post_spawn_survival_males,
    ppPenalty = ppPenalty,
    pre_spawn_survival_females = pre_spawn_survival_females,
    pre_spawn_survival_males = pre_spawn_survival_males,
    pred = pred,
    predTemps = predTemps,
    puNames = puNames,
    puRkm = puRkm,
    res.B = res.B,
    res.R = res.R,
    r.entry = r.entry,
    r.entryDate = r.entryDate,
    r.lw = r.lw,
    r.mat = r.mat,
    r.pars = r.pars,
    r.prob = r.prob,
    r.res = r.res,
    rkm1 = rkm1,
    rkm2 = rkm2,
    roelw = roelw,
    routes = routes,
    sex_Ratio = sex_Ratio,
    sOptim = sOptim,
    sp_1 = sp_1,
    sp_2 = sp_2,
    sp_3 = sp_3,
    sp_4 = sp_4,
    spawnData_1 = spawnData_1,
    spawnData_2 = spawnData_2,
    spawnData_3 = spawnData_3,
    spawnData_4 = spawnData_4,
    stoch = stoch,
    tort = tort,
    traits = traits,
    traits_1 = traits_1,
    traits_2 = traits_2,
    traits_3 = traits_3,
    traits_4 = traits_4,
    up_effs = up_effs,
    upstream_path = upstream_path,
    y = y,
    Year = Year,
    habStoch = habStoch
    ))
  }

  # Merrimack River:
  if(river=='merrimack'){
    return(list(
    b.entry = b.entry,
    b.entryDate = b.entryDate,
    b.lw = b.lw,
    b.mat = b.mat,
    b.pars = b.pars,
    b.prob = b.prob,
    b.res = b.res,
    batch = batch,
    bucklw = bucklw,
    c_BF = c_BF,
    c_end = c_end,
    c_entryDate = c_entryDate,
    c_fecundity = c_fecundity,
    c_female = c_female,
    c_female_lf = c_female_lf,
    c_female_m = c_female_m,
    c_femaleLWalpha = c_femaleLWalpha,
    c_femaleLWbeta = c_femaleLWbeta,
    c_fishAges = c_fishAges,
    c_forkLength = c_forkLength,
    c_initial = c_initial,
    c_kF = c_kF,
    c_kM = c_kM,
    c_linF = c_linF,
    c_linM = c_linM,
    c_male = c_male,
    c_male_lf = c_male_lf,
    c_male_m = c_male_m,
    c_maleLWalpha = c_maleLWalpha,
    c_maleLWbeta = c_maleLWbeta,
    c_mass = c_mass,
    c_RAF = c_RAF,
    c_repeat = c_repeat,
    c_RT = c_RT,
    c_sex = c_sex,
    c_SI = c_SI,
    c_spawnATU1 = c_spawnATU1,
    c_spawnATU2 = c_spawnATU2,
    c_t0F = c_t0F,
    c_t0M = c_t0M,
    dailyMove = dailyMove,
    day = day,
    delay_1 = delay_1,
    dMax = dMax,
    eFFs = eFFs,
    entry = entry,
    fishAges = fishAges,
    id = id,
    juvenile_survival = juvenile_survival,
    k_pus = k_pus,
    maxR = maxR,
    mot = mot,
    moves_1 = moves_1,
    newTU = newTU,
    newTU_2 = newTU_2,
    oceanSurvival = oceanSurvival,
    photo = photo,
    post_spawn_survival_females = post_spawn_survival_females,
    post_spawn_survival_males = post_spawn_survival_males,
    ppPenalty = ppPenalty,
    pre_spawn_survival_females = pre_spawn_survival_females,
    pre_spawn_survival_males = pre_spawn_survival_males,
    pred = pred,
    predTemps = predTemps,
    puNames = puNames,
    puRkm = puRkm,
    res.B = res.B,
    res.R = res.R,
    r.entry = r.entry,
    r.entryDate = r.entryDate,
    r.lw = r.lw,
    r.mat = r.mat,
    r.pars = r.pars,
    r.prob = r.prob,
    r.res = r.res,
    rkm1 = rkm1,
    rkm2 = rkm2,
    roelw = roelw,
    routes = routes,
    sex_Ratio = sex_Ratio,
    sOptim = sOptim,
    sp_1 = sp_1,
    sp_2 = sp_2,
    spawnData_1 = spawnData_1,
    spawnData_2 = spawnData_2,
    stoch = stoch,
    tort = tort,
    traits = traits,
    traits_1 = traits_1,
    traits_2 = traits_2,
    up_effs = up_effs,
    upstream_path = upstream_path,
    y = y,
    Year = Year,
    habStoch = habStoch
    ))
  }

  # Connecticut:
  if(river=='connecticut'){
    return(list(
    b.entry = b.entry,
    b.entryDate = b.entryDate,
    b.lw = b.lw,
    b.mat = b.mat,
    b.pars = b.pars,
    b.prob = b.prob,
    b.res = b.res,
    batch = batch,
    bucklw = bucklw,
    c_BF = c_BF,
    c_end = c_end,
    c_entryDate = c_entryDate,
    c_fecundity = c_fecundity,
    c_female = c_female,
    c_female_lf = c_female_lf,
    c_female_m = c_female_m,
    c_femaleLWalpha = c_femaleLWalpha,
    c_femaleLWbeta = c_femaleLWbeta,
    c_fishAges = c_fishAges,
    c_forkLength = c_forkLength,
    c_initial = c_initial,
    c_kF = c_kF,
    c_kM = c_kM,
    c_linF = c_linF,
    c_linM = c_linM,
    c_male = c_male,
    c_male_lf = c_male_lf,
    c_male_m = c_male_m,
    c_maleLWalpha = c_maleLWalpha,
    c_maleLWbeta = c_maleLWbeta,
    c_mass = c_mass,
    c_RAF = c_RAF,
    c_repeat = c_repeat,
    c_RT = c_RT,
    c_sex = c_sex,
    c_SI = c_SI,
    c_spawnATU1 = c_spawnATU1,
    c_spawnATU2 = c_spawnATU2,
    c_t0F = c_t0F,
    c_t0M = c_t0M,
    dailyMove = dailyMove,
    day = day,
    delay_1 = delay_1,
    delay_2 = delay_2,
    dMax = dMax,
    eFFs = eFFs,
    entry = entry,
    fishAges = fishAges,
    id = id,
    juvenile_survival = juvenile_survival,
    k_pus = k_pus,
    #maxrkm = maxrkm,
    maxR = maxR,
    mot = mot,
    moves_1 = moves_1,
    moves_2 = moves_2,
    newTU = newTU,
    newTU_2 = newTU_2,
    oceanSurvival = oceanSurvival,
    photo = photo,
    post_spawn_survival_females = post_spawn_survival_females,
    post_spawn_survival_males = post_spawn_survival_males,
    ppPenalty = ppPenalty,
    pre_spawn_survival_females = pre_spawn_survival_females,
    pre_spawn_survival_males = pre_spawn_survival_males,
    pred = pred,
    predTemps = predTemps,
    puNames = puNames,
    puRkm = puRkm,
    res.B = res.B,
    res.R = res.R,
    r.entry = r.entry,
    r.entryDate = r.entryDate,
    r.lw = r.lw,
    r.mat = r.mat,
    r.pars = r.pars,
    r.prob = r.prob,
    r.res = r.res,
    rkm1 = rkm1,
    rkm2 = rkm2,
    roelw = roelw,
    routes = routes,
    sex_Ratio = sex_Ratio,
    sOptim = sOptim,
    sp_1 = sp_1,
    sp_2 = sp_2,
    spawnData_1 = spawnData_1,
    spawnData_2 = spawnData_2,
    stoch = stoch,
    tort = tort,
    traits = traits,
    traits_1 = traits_1,
    traits_2 = traits_2,
    up_effs = up_effs,
    upstream_path = upstream_path,
    y = y,
    Year = Year,
    habStoch = habStoch
    ))
  }

}




# To save only inner loop sampling variables: uncomment
# filename can be adjusted in setParameters.R
# JMS
# filename can be adjusted in setParameters.R
# rm(additionalEggsProcessing, Age1, Age2, Age3, Age4, Age5, Age6, Age7, Age8, Age9, 
#    assignFishToRoutes, ATUspawn1, ATUspawn2, b.Arr, b.ArrRegrInt, b.ArrRegrSlp, 
#    baseDirectory, batchSize, b.l, b.length, b.lw, BMillD, BMillUp, b.mod, 
#    Bmod, b.pars, b.res, BrownsMillD, BrownsMillDj, BrownsMillUp, BrownsPop, 
#    bucklw, buck.lw, bucks, b.w, bycatchF, calMod, CI, commercialF, Confluence, cpue, 
#    createPUMatrix, ctr, d, daily.move, damRkms, dat, daylength, dDraws, delay, 
#    delayC, directory, d.Max, doInnerSampling, doOuterSampling, downstreamS, Dspawn1, 
#    Dspawn2, EnfieldPop, entryC, fallback, fB, F.bycatch, F.commercial, filename, 
#    F.inRiver, fish, fishAges, fishPU, fishw, GilmanD, GilmanDj, GilmanUp, GilmD, 
#    GilmUp, growth, GuilfD, GuilfordD, GuilfordDj, GuilfordPop, GuilfordUp, GuilfUp, 
#    habitat, HdD, HdUp, hmu, HowlandD, HowlandDj, HowlandPop, HowlandUp, i, iIndex, 
#    ildProduct, indirect, indirectM, inner.lim, innerLoopSamplingRData, innerLoopSamplingSource, 
#    inriv, inRiverF, invlogit, jReduction, juvReduction, k, kF, kM, latent, latentM, ldat, 
#    linF, linM, LowerPop, lwF.alpha, lwF.beta, lwM.alpha, lwM.beta, MattaceunkD, MattaceunkDj, 
#    MattaceunkUp, MattD, MattUp, maxAge, maxrkm, maxrkmC, MdD, MdUp, MilfordD, MilfordDj, 
#    MilfordPop, MilfordUp, MooseD, MooseheadD, MooseheadDj, MooseheadUp, MoosePop, MooseUp, 
#    motivation, motivationPenaltyC, moveC, mu, n, nDams, newDay, nPU, nRoutes, nRuns, nYears, 
#    Open, OpenD, OrD, OronoD, OronoDj, OronoHabitat, OronoPop, OronoUp, OrUp, 
#    outerLoopSamplingRData, outerLoopSamplingSource, pDraws, p.female, pinHarvest, pMainD, 
#    pMainstemUp, pMainUp, pnr, pop, popStart, populationSize, pPiscUp, pPiscUP, pRepeat, 
#    pRepeats, processPopulation, pStillD, pStillUP, pStillwaterD, pStillwaterUp, ptime, 
#    ptmABM, ptmDelay, ptmSim, PU_1_1, PU_1_2, PU_1_3, PU_1_4, PU_1_5, PU_1_6, PU_1_7, 
#    PU_2_1, PU_2_2, PU_2_3, PU_2_4, PU_2_5, PU_3_1, PU_3_2, PU_3_3, PU_3_4, PU_3_5, PU_3_6, 
#    PU_3_7, PU_3_8, PU_4_1, PU_4_2, PU_4_3, PU_4_4, PU_4_5, PU_4_6, r.Arr, r.ArrRegrInt, 
#    r.ArrRegrSlp, recruitmentPool, res.B, res.R, resTime, rleC, r.length, Rmod, roelw, roe.lw, 
#    roes, scalar, scalarVar, scen, scenario, S.downstream, S.juvenile, S.marine, s.Optim, 
#    spawnDateC, spawners, spawningPool, spawnInt, spawnRecruit, S.postspawnF, S.postspawnM, 
#    S.prespawnF, S.prespawnM, StD, StillwaterD, StillwaterDj, StillwaterHabitat, StillwaterPop, 
#    StillwaterUp, StUp, substrRight, t, t0F, t0M, tempD, tempData, test,  
#    timely, tortuosity, traits, t.RegrInt, t.RegrSlp, t.stoch, tz, up, upEffs, upstreamPathC, 
#    useProgress, useTictoc, weldon, WeldonPop, WEnfD, WEnfUp, WestEnfieldD, WestEnfieldDj, 
#    WestEnfieldUp, writeData, writeSenData, writeSimData, x, yday, year, years, z)
#save.image(paste(baseDirectory, innerLoopSamplingRData, sep='/'))
