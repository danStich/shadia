
# inner-loop-sampling.R
#
# Attempt to perform all draws needed for inner loop,
# then save them for re-use and reproducible runs.

# DSS: commented out for package 
# implementation because the functions
# are included in the source code
#Rcpp::sourceCpp('datasets/pnrCppFuns.cpp')        # C++ functions used in model

innerLoopSampling <- function(){
  
# SIMULATE DAILY TEMPERATURE IN PNR EACH DAY ------------------------------
# Use historical temperature data to predict temperature on each day from a
# multivariate normal distribution
# Use historical temperature data to predict temperature on each day from a
# multivariate normal distribution
# Make an empty matrix to hold daily temperature predictions within each year
# for annual simulations below

#if (useTictoc) tic("simulate daily temp1: pred NEW")
pred = data.frame(matrix(0, nrow = 366, ncol = 2))

# Simulate annual temperature
Year = sample(unique(mu$year), 1, replace = TRUE)

# JMS: precalculate before the loop:
muInYear <-  mu[, 2] == Year
uniqueMuDay <- unique(mu[, 3])
sigma = cor(mu[muInYear, c(1, 3)])

predRowcount <- nrow(pred)
for (i in 1:predRowcount) {
  # For each day that year
  if (i %in% mu[muInYear, 3]) {
    iIndex <- muInYear & mu[, 3] == uniqueMuDay[i]

    x = mu[, 1] [iIndex]
    z = mu[, 3] [iIndex]
    pred[i,] = mvrnorm(1,
                       mu = c(x, z),
                       Sigma = sigma,
                       tol = 1e-6)
    pred[i,][pred[i,] < 0] = 0
  } else {
    next
  }  # Else
}  # i
#toc() #("simulate daily temp1: pred")
rm(muInYear, uniqueMuDay, sigma, predRowcount)

#if (useTictoc) tic("simulate daily temp2: pred merge, ddply, cumsum")
pred = pred [with(pred, order(X2)),]
pred = pred[pred[, 2] != 0,]
pred[, 2] = round(pred[, 2])
pred = ddply(pred, ~ X2, summarize, X1 = mean(X1))
id = data.frame(seq(1, 366, 1), NA)
names(id) = c('X2', 'X1')
y = merge(pred, id, by = 'X2', all.y = TRUE)[, c(1, 2)]
y[1, 2] = 0
y[nrow(y), 2] = 0
y = na.spline(y)
y[y[, 2] < 0, 2] = 0
predTemps = data.frame(y)

# Calculate ATU for each day of simulated temperature data
newTU = cumsum(predTemps[, 2])
#toc() #("simulate daily temp2: pred merge, ddply, cumsum")

# DRAW CDF for PROBABILITY OF ARRIVAL BASED ON COMMERCIAL HARVEST ---------
#if (useTictoc) tic("DRAW CDF for PROBABILITY OF ARRIVAL")
#newDay = seq(0, 365, 1)

# Randomly sample sex-specific regression
# coefficients for time of arrival from
# built-in data sets.
res.R <<- data.frame(sample(arr.R,  1))
res.B <<- data.frame(sample(arr.B,  1))

r.prob <<- invlogit(res.R[1, 1] + res.R[2, 1] * predTemps[, 2])
b.prob <<- invlogit(res.B[1, 1] + res.B[2, 1] * predTemps[, 2])

#toc()
# ---

# CREATE AGE AND SEX STRUCTURE, DRAW ARRIVAL AND SPAWN DATES --------------
# Draw sex ratio for the current year
#if (useTictoc) tic("fish ages1")
sex_Ratio <<- rbeta(1, 100, 100)

# Create an object containing the age of each fish based on the number of fish
# in each age class
fishAges = c()
for (i in 1:length(spawningPool)) {
  fishAges = append(fishAges, rep(names(spawningPool)[i], spawningPool[i]))
}
c_fishAges <<- as.numeric(substr(fishAges, start = 4, stop = 4))

# Assign fish gender using sex ratio drawn above, females are 1
c_sex <<- rbinom(length(fishAges), 1, sex_Ratio)

# Need to sort by sex to get sex-specific arrival date efficiently
c_fishAges <<- c_fishAges[order(c_sex)] # First fish ages
c_sex <<- sort(c_sex)                   # Then fish sex

# Get entry date for each individual.  We used cumulative frequency distribution
# to predict average entry date for a given fish conditional on temperature.
# We will now use this relationship to predict individual
# fish presence in the river for all 366 days of the year conditional date.
# The actual draws for each fish on each day are occurring in a C++ file
# (entryC.Cpp) that was sourced on the front-end to speed this up.  Once we have
# probability of being in the river on a given day, we will use the first date
# that maximizes probability of success in a random binomial draw for each
# individual to assign their entry date.
# Make containers to hold entry date.
b.entryDate = matrix(0, nrow = length(fishAges[c_sex == 0]), ncol = length(b.prob))
r.entryDate = matrix(0, nrow = length(fishAges[c_sex == 1]), ncol = length(r.prob))
#toc()

#if (useTictoc) tic("fish ages2: C++ function, entryC")
b.entry = entryC(b.prob, b.entryDate, length(fishAges[c_sex == 0]))
r.entry = entryC(r.prob, r.entryDate, length(fishAges[c_sex == 1]))
#toc()

# Combine male and female spawners into one matrix
#if (useTictoc) tic("fish ages3: combine into one matrix NEW")

# JMS: initialize/preallocate: get row and col counts
entryCols <- ncol(b.entry) # same for r.entry col count
entryRows <- nrow(b.entry) + nrow(r.entry)

# define size and characteristics of the data frame
entry <- data.frame(matrix(ncol = entryCols,
                           nrow = entryRows),
                    row.names = NULL,
                    check.names=FALSE,
                    fix.empty.names = TRUE,
                    stringsAsFactors = FALSE)

entry <- rbind(b.entry, r.entry, drop = FALSE)

# Note that we need to cut out some of the wilder predictions.
entry[, 1:60] = 0
entry[, 200:entryCols] = 0
if (nrow(entry) > 1) { # we are altering nrow inside the loop...dirty!!
  entry = entry[1:(nrow(entry) - 1),]
}
# if we have changed it above:
entryRowsNew <- nrow(entry)

c_entryDate = vector(mode = 'numeric', length = entryRowsNew)
for (i in 1:entryRowsNew) {
  c_entryDate[i] = which(entry[i,] == max(entry[i,], na.rm = TRUE))[1]
}
rm(entryRowsNew, entryCols, entryRows)
c_entryDate <<- c_entryDate
#toc() # combine into one matrix

# Assign upstream and downstream migration routes probabilistically conditional
# on flow or production potential (upstream) and flow (downstream). NOTE: NEED
# TO ADD IN THE FLOW DATA AND CONDITIONAL RELATIONSHIPS FOR THESE PROBABILITY
# DISTRIBUTIONS
# Draw upstream migration path based on flow or else proportional production
# using a random draw from a multinomial distribution with two outcomes
#if (useTictoc) tic("upstream migration routes")
upstream_path = rmultinom(
  n = length(c_fishAges),
  size = 1,
  prob = c(
    pMainUp * pPiscUp,
    pMainUp * pMainstemUp,
    pStillwaterUp * pPiscUp,
    pStillwaterUp * pMainstemUp
  )
)
upstream_path[2,][upstream_path[2,] > 0] = 2
upstream_path[3,][upstream_path[3,] > 0] = 3
upstream_path[4,][upstream_path[4,] > 0] = 4

# A '1' is Piscataquis, and a '2' is mainstem
upstream_path <<- upstreamPathC(upstream_path)

# Draw terminal spawning ATU from a truncated normal distribution based on
# mean and standard deviaion of mean and standard deviation of ATU at which
# minimum of 50% of the population has entered the river based on our GLM in the
# previous sections.  We use a truncated normal to prevent fish spawning before
# they ever arrive.  These data match pretty well to the results of Hyle et al.
# (2014), but our standard deviation naturally is larger.
# Draw initial and terminal spawning dates based on temperatures at Turner's
# Falls.
# Randomly draw ATU for initation (1) and termination (2) of spawning for each
c_spawnATU1 <<- rnorm(length(c_entryDate), 150, 15)
c_spawnATU2 <<- rnorm(length(c_entryDate), 500, 15)
# Determine on which day the threshold ATU for each individual is reached
# Pre-allocate vectors
c_initial = vector(mode = 'numeric', length = length(c_entryDate))
c_end = vector(mode = 'numeric', length = length(c_entryDate))
# Now get initial and end dates based on ATU calculated from temperature sim
for (i in 1:length(c_entryDate)) {
  # Initiation of spawn
  c_initial[i] = c_entryDate[i] +
    which(cumsum(predTemps[c_entryDate[i]:nrow(predTemps), 2]) >=
            c_spawnATU1[i])[1]
  # Termination of spawn
  c_end[i] = c_entryDate[i] +
    which(cumsum(predTemps[c_entryDate[i]:nrow(predTemps), 2]) >=
            c_spawnATU2[i])[1]
}
c_initial <<- c_initial
c_end <<- c_end 

# Define the day of the year as an ordinal date
day = c(seq(min(c_initial), (max(c_end))))
# Calculate photoperiod based on latitude and day
photo = daylength(44.39, day)
#toc()

# SIMULATE FISH CHARACTERISTICS FOR EACH FISH IN EACH YEAR ----------------
# Get fork length for each individual in each age class of spawning pool
# Subsample fish data from CTDEEP to fit sex-specific growth curves and predict
# length for roes and bucks
#if (useTictoc) tic("simulate fish characteristics")
# Roes
# Randomly sample some number of rows from the data
samp = roes[sample(nrow(roes), 1000),]
# Fit the model and save the parameters
r.mod = growth(
  size = samp$fl[samp$sex == 'R'],
  age = samp$age[samp$sex == 'R'],
  Sinf = max(samp$fl),
  K = .5,
  t0 = -3,
  error = 1,
  graph = FALSE
)
# Get the parameters
r.pars = data.frame(summary(r.mod$vout)$parameters)
r.mat <<- r.pars[, 1]
# Rename them for ease of use
c_linF <<- r.mat[1] # L-infinity females
c_kF <<- r.mat[2]   # Brody growth coeff females
c_t0F <<- r.mat[3]  # Intercept of VBGM females

# Males
# Randomly sample some number of rows from the data
samp = bucks[sample(nrow(bucks), 1000),]
# Fit the model and save the parameters
b.mod = growth(
  size = samp$fl[samp$sex == 'B'],
  age = samp$age[samp$sex == 'B'],
  Sinf = max(samp$fl),
  K = .5,
  t0 = -3,
  error = 1,
  graph = FALSE
)
# Get the parameters
b.pars = data.frame(summary(b.mod$vout)$parameters)
b.mat <<- b.pars[, 1]
# Rename them for ease of use
c_linM <<- b.mat[1] # L-infinity males
c_kM <<- b.mat[2]   # Brody growth coeff males
c_t0M <<- b.mat[3]  # Intercept of VBGM males

# Create length-weight regressions for males and females from the CTDEEP data
# Roes
roelw = roe.lw[sample(nrow(roe.lw), 200, replace = TRUE),]
r.lw = lm(r.w ~ r.l, data = roelw)
r.res = data.frame(summary(r.lw)$coefficients[, 1])
c_femaleLWalpha <<- r.res[1, 1]  # Alpha in male l-w relationship
c_femaleLWbeta <<- r.res[2, 1]   # Beta in male l-w relationship
# Bucks
bucklw = buck.lw[sample(nrow(buck.lw), 200, replace = TRUE),]
b.lw = lm(b.w ~ b.l, data = bucklw)
b.res = data.frame(summary(b.lw)$coefficients[, 1])
c_maleLWalpha <<- b.res[1, 1] # Alpha in male l-w relationship
c_maleLWbeta <<- b.res[2, 1]   # Beta in male l-w relationship

# Calculate length, mass, and movement rates, and fecundity
# Get columns representing logical for male and female
c_female <<- c_sex
c_male <<- as.numeric(factor(c_sex, levels = c(1, 0))) - 1
# Calculate length of males
c_male_lf <<- c_male * c_linM * (1 - exp(-c_kM * (c_fishAges - c_t0M)))
# Calculate length of females
c_female_lf <<- c_female * c_linF * (1 - exp(-c_kF * (c_fishAges - c_t0F)))
# Calculate mass of males
c_male_m <<- c_male * (c_maleLWalpha + c_maleLWbeta * c_male_lf)
# Calculate mass of females
c_female_m <<- c_female * (c_femaleLWalpha + c_femaleLWbeta * c_female_lf)
# Collect fork length and mass into one column each
c_forkLength = c_male_lf + c_female_lf
c_mass = c_male_m + c_female_m
# Convert fork length to mm from cm for movement calcs below
c_forkLength <<- c_forkLength * 10

# Calculate movement rates based on Castro-Santos and Letcher (2010)
# Optimizing ground speed in body lengths per second (BLS)
sOptim <<- runif(length(c_mass), .7, 1.7)
# Get max daily movement, converting from BLS to km per day
dMax<<- (sOptim * c_forkLength * 86400) / 1e6
# Movement tortuosity drawn from uniform distribution.  This corresponds to
# the range used in Castro-Santos and Letcher (2010) but it's just applied
# as a multiplier
tort <<- runif(length(c_mass), 0.2, 1)
# Now scale by tortuosity and divide by two to restrict movement to day time
dailyMove <<- dMax * tort * mean(photo / 24)

# Calculate fecundity
# Calculate residence time for each fish based on entry date and exit date
c_RT = c_end - c_initial
c_RT[c_RT < 1] = 1
c_RT <<- c_RT
# Get spawning interval for each fish
c_SI <<- rnorm(length(c_fishAges), 2.493, 0.274)
# Get probability of repeat spawning
c_repeat <<- rbinom(length(c_fishAges), 1, pRepeat[c_fishAges])
# Get random draws for fecundity based on whether or not fish are repeat
# spawners.
c_BF = vector(mode = 'numeric', length = length(c_repeat))
c_BF[c_repeat == 0] = sample(rnegbin(10000, 20000, 10),
                             length(c_repeat[c_repeat == 0]), replace = TRUE)
c_BF[c_repeat == 1] = sample(rnegbin(10000, 30000, 10),
                             length(c_repeat[c_repeat == 1]), replace = TRUE)
c_BF <<- c_BF

# Calculate realized annual fecundity
#c_RAF = vector(mode = 'numeric', length=length(c_RT))
#for(i in 1:length(c_RT)){
#if((c_RT[i]/c_SI[i]) < 10 ) {
c_RAF <<- c_BF * (c_RT / c_SI)
#} else {
#c_RAF[i] = c_BF[i] * 10
#}
#}
# Multiply by sex variable to set male fecundity to zero
c_fecundity <<- c_female * c_RAF

# Collect life-history parameters into a single matrix for c++ loop
# NOTE: the source code for the loop was re-written to preclude the need for
# these matrices. Instead, they are related to the ABM input and output post-
# hoc to speed things up.
getEm = mget(ls(pat = '^c_'))
for (i in 1:length(getEm)) {
  if (is.na(getEm[[i]][1])) {
    getEm[[i]][1] = 0
  } else {
    next

  }
}

traits = as.matrix(data.frame(getEm))
colnames(traits) = gsub(pattern = "c_",
                        replacement = "",
                        colnames(traits))

# Re-organize the data so they match the output of the ABM below
# Create a df for traits of Piscataquis River spawners
traits_1 = data.frame(traits[upstream_path == 1, , drop = FALSE],
                      upstream_path[upstream_path == 1])
# Create a df for traits of Mainstem spawners
traits_2 = data.frame(traits[upstream_path == 2, , drop = FALSE],
                      upstream_path[upstream_path == 2])
# Create a df for traits of Piscataquis River spawners
traits_3 = data.frame(traits[upstream_path == 3, , drop = FALSE],
                      upstream_path[upstream_path == 3])
# Create a df for traits of Mainstem spawners
traits_4 = data.frame(traits[upstream_path == 4, , drop = FALSE],
                      upstream_path[upstream_path == 4])

# Change the name of the last column in each of the dfs so they match
names(traits_1)[ncol(traits_1)] = 'upstream_path'
names(traits_2)[ncol(traits_2)] = 'upstream_path'
names(traits_3)[ncol(traits_3)] = 'upstream_path'
names(traits_4)[ncol(traits_4)] = 'upstream_path'
#toc()

# DEFINE VARIABLES FOR SPAWNING DYNAMICS ----------------------------------
# Carrying capacity for juvs based on potential production of adult shad in each
# production unit based on values from the 2009 multi-species management plan
# NOTE: These are divided by 1000 to make the simulations run faster!
# Everything is scaled up on output
#if (useTictoc) tic("spawn dynamics variables")
k_pus = vector(mode = 'list', length = length(habitat))
batch = quantile(rnegbin(1e2, 2.5e4, 10), 0.5)[1]
k_pus[[1]] = ((habitat[[1]] / scalar) * sex_Ratio * batch)
k_pus[[2]] = ((habitat[[2]] / scalar) * sex_Ratio * batch)
k_pus[[3]] = ((habitat[[3]] / scalar) * sex_Ratio * batch)
k_pus[[4]] = ((habitat[[4]] / scalar) * sex_Ratio * batch)
k_pus = lapply(k_pus, function(x) {
  x[is.na(x)] = 1
  x
})
k_pus <<- k_pus

# Pre-spawning mortality. Right now, these are drawn independently. Conditional
# draws may be more appropriate b/c pre-spawn mortality is probably affected by
# similar factors but may differ in magnitude between males and females.
pre_spawn_survival_males <<- rbeta(1, 1e4, 50)
pre_spawn_survival_females <<- rbeta(1, 1e4, 50)

# Post-spawning mortality. Right now, these are drawn independently. Conditional
# draws may be more appropriate b/c post-spawn mortality is probably affected by
# similar factors but may differ in magnitude between males and females.
post_spawn_survival_males <<- rbeta(1, 200, 50)
post_spawn_survival_females <<- rbeta(1, 200, 50)

# Ocean survival for each year after the first year for each age class
# Can be made into age-specific survival. Right now everyone has the same
# annual survival rate in the ocean.
oceanSurvival <<- rep(rbeta(1, 12, 8), maxAge)

# Juvenile mortality rate
# This really needs some data pretty bad. Right now it is just a draw from a
# uniform probability distribution that calls it 1 in 100000 to 1 in 1000
juvenile_survival <<- runif(1, 0.0005, 0.00083)
#toc()

# FISH PASSAGE RATES AT DAMS ----------------------------------------------
# Convert passage performance standards into rates over time based on passage
# efficiency and timely
#if (useTictoc) tic("passage rates at dams")
up_effs = mapply('+', upEffs, 1)
up_effs = mapply('^', up_effs, (1 / timely))
up_effs = mapply('-', up_effs, 1)

# Finally, assign passage efficiencies for each reach in the river
eFFs = vector(mode = 'list', length = length(up_effs))
eFFs[[1]] = c(rep(Open, maxrkm[1])) # Create perfect passage for group 1
eFFs[[2]] = c(rep(Open, maxrkm[2])) # Create perfect passage for group 2
eFFs[[3]] = c(rep(Open, maxrkm[1])) # Create perfect passage for group 3
eFFs[[4]] = c(rep(Open, maxrkm[2])) # Create perfect passage for group 4
# And replace efficiencies where there are dams
eFFs[[1]][damRkms[[1]]] = up_effs[[1]] # Dam-specific efficiencies group 1
eFFs[[2]][damRkms[[2]]] = up_effs[[2]] # Dam-specific efficiencies group 2
eFFs[[3]][damRkms[[3]]] = up_effs[[3]] # Dam-specific efficiencies group 3
eFFs[[4]][damRkms[[4]]] = up_effs[[4]] # Dam-specific efficiencies group 4
#toc()
eFFs <<- eFFs

# UPSTREAM MIGRATION FOR EACH FISH IN EACH YEAR ---------------------------
# Make the adult fish move upstream through space and time. The functions used
# in this section call a collection of C++ and header files that were sourced
# the front-end of this r script.
# Add in a motivation penalty based on photoperiod. Fish are assumed to most
# motivated at the peak of the run. Makes a passage efficiency for each
# rkm on each day
# Make an empty matrix to hold the results
ppPenalty = vector(mode = 'list', length = length(eFFs))
ppPenalty[[1]] =  matrix(0 , length(photo), maxrkm)
ppPenalty[[2]] =  matrix(0 , length(photo), maxrkm)
ppPenalty[[3]] =  matrix(0 , length(photo), maxrkm)
ppPenalty[[4]] =  matrix(0 , length(photo), maxrkm)

# Multiply passage efficiency by the penalty for each day

#if (useTictoc) {
#  tic("C++ function, motivationPenaltyC")
#}

newTU_2 = newTU[min(c_entryDate):max(c_end)]
ppPenalty[[1]] =  motivationPenaltyC(eFFs[[1]], newTU_2, ppPenalty[[1]])
ppPenalty[[2]] =  motivationPenaltyC(eFFs[[2]], newTU_2, ppPenalty[[2]])
ppPenalty[[3]] =  motivationPenaltyC(eFFs[[3]], newTU_2, ppPenalty[[3]])
ppPenalty[[4]] =  motivationPenaltyC(eFFs[[4]], newTU_2, ppPenalty[[4]])

#toc() # C++ function, motivationPenaltyC

# Track the mean motivational penalty for the spawning season
mot <<- mean((1 - (newTU - min(newTU)) /
              (max(newTU) - min(newTU)))[min(c_entryDate):max(c_end)])

# Pre-allocate vectors and matrices for agent-based migration model, this can
# be done in bulk for both Piscataquis River spawners and Mainstem spawners
# because we use vectorization to select the appropriate elements later on.
rkm1 = rep(41, length(c_fishAges))
rkm2 = matrix(0, ncol = length(day), nrow = length(c_fishAges))

# Get max rkm for each fish
# Create a vector of potential routes
routes = seq(1, nRoutes, 1)
# Run the markmC C++ function to get max rkm for each fish given route
#if (useTictoc) {
  #tic("C++ function, maxrkmC")
#}
maxR = maxrkmC(c_fishAges, maxrkm, upstream_path, routes)
#toc()

# Run the upstream migration model and get results
# Run the agent-based model for upstream migration
# Get start time for running ABM function in C++
ptmABM  <-  proc.time() # Uncomment to time it

#if (useTictoc) {
#  tic("C++ ABM function, moveC")
#}

# Run the ABM for main-to-piscataquis spawners
moves_1 = moveC(day,
                c_entryDate[upstream_path == 1],
                dailyMove[upstream_path == 1],
                maxR[upstream_path == 1],
                ppPenalty[[1]],
                rkm1[upstream_path == 1],
                rkm2[upstream_path == 1, , drop = FALSE],
                c_initial[upstream_path == 1])
# Run the ABM for main-to-mainstem Spawners
moves_2 = moveC(day,
                c_entryDate[upstream_path == 2],
                dailyMove[upstream_path == 2],
                maxR[upstream_path == 2],
                ppPenalty[[2]],
                rkm1[upstream_path == 2],
                rkm2[upstream_path == 2,  , drop = FALSE],
                c_initial[upstream_path == 2])
# Run the ABM for stillwater-to-piscataquis Spawners
moves_3 = moveC(day,
                c_entryDate[upstream_path == 3],
                dailyMove[upstream_path == 3],
                maxR[upstream_path == 3],
                ppPenalty[[3]],
                rkm1[upstream_path == 3],
                rkm2[upstream_path == 3,  , drop = FALSE],
                c_initial[upstream_path == 3])
# Run the ABM for stillwater-to-mainstem Spawners
moves_4 = moveC(day,
                c_entryDate[upstream_path == 4],
                dailyMove[upstream_path == 4],
                maxR[upstream_path == 4],
                ppPenalty[[4]],
                rkm1[upstream_path == 4],
                rkm2[upstream_path == 4, , drop = FALSE],
                c_initial[upstream_path == 4])
# Calculate total run time for ABM
timeABM <- proc.time() - ptmABM # Uncomment to time it

#toc() # C++ ABM function, moveC

# Calculate delay for each fish in each migration route
# Start timing the delay function in C++

#if (useTictoc) {
#  tic("C++ delay function, delayC")
#}

ptmDelay  <- proc.time() # Uncomment to time it
# Calculate delay at each dam for each main-to-Piscataquis spawners
delay_1 = delayC(moves_1, damRkms[[1]][2:nPU[1]])
# Calculate delay at each dam for each main-to-Mainstem spawners
delay_2 = delayC(moves_2, damRkms[[2]][2:nPU[2]])
# Calculate delay at each dam for each still-to-Piscataquis spawners
delay_3 = delayC(moves_3, damRkms[[3]][2:nPU[3]])
# Calculate delay at each dam for each still-to-Mainstem spawners
delay_4 = delayC(moves_4, damRkms[[4]][2:nPU[4]])

# Assign names to the newly created matrices that hold delay at each dam
# Main-to-Piscataquis spawners
colnames(delay_1) = c('dConfluence',
                      'dMilford',
                      'dHowland',
                      'dBrownsMill',
                      'dMoosehead',
                      'dGuilford')
# Main-to-Piscataquis spawners
colnames(delay_2) = c('dConfluence',
                      'dMilford',
                      'dWestEnfield',
                      'dWeldon')
# Main-to-Piscataquis spawners
colnames(delay_3) = c(
  'dOrono',
  'dStillwater',
  'Gilman',
  'dHowland',
  'dBrownsMill',
  'dMoosehead',
  'dGuilford'
)
# Main-to-Piscataquis spawners
colnames(delay_4) = c('Orono',
                      'Stillwater',
                      'Gilman',
                      'dWestEnfield',
                      'dWeldon')

# Calculate run time for delay function in C++
timeDelay <- proc.time() - ptmDelay
#toc() # C++ delay function, delayC

# SPAWNING DYNAMICS FOR EACH YEAR WITH ANNUAL VARIABILITY -----------------
# Combine the data for each fish stored in traits with the final rkm of that
# fish and the delay experienced by each fish at each dam for each of the
# upstream passage routes
#if (useTictoc) tic("SPAWNING DYNAMICS FOR EACH YEAR WITH ANNUAL VARIABILITY")

# Combine all data for main-to-piscataquis spawners
# Combine all three matrices
spawnData_1 <<- cbind(traits_1, moves_1[, ncol(moves_1)], delay_1)
# Change the name for the final rkm column
colnames(spawnData_1)[ncol(spawnData_1) - 6] = 'finalRkm'
# Make it into a dataframe for easy manipulation
sp_1 <- data.frame(spawnData_1)

# Combine all data for main-to-mainstem spawners
# Combine all three matrices
spawnData_2 <<- cbind(traits_2, moves_2[, ncol(moves_2)], delay_2)
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
puRkm <<- list(
c(damRkms[[1]] + 1, (maxrkm[1] + 1)),
c(damRkms[[2]] + 1, (maxrkm[2] + 1)),
c(damRkms[[3]] + 1, (maxrkm[3] + 1)),
c(damRkms[[4]] + 1, (maxrkm[4] + 1))
)

# Create an empty list to hold the pu names for each route
rm(list = ls()[grep(ls(), pat = '^mPU_')]) # Remove old counts
puNames_temp <- vector(mode = 'list', length = length(nPU))
# Dynamically assign pu names based on river kilometers that delineate them
for (t in 1:length(puRkm)) {
  for (i in 1:(length(puRkm[[t]]) - 1)) {
    assign(paste('PU_', t, '_', i, sep = ''), puRkm[i])
  }
  # Collect the names into a list
  puNames_temp[[t]] = names(mget(ls(pat = paste(
    '^PU_', t, sep = ''
  ))))
}
puNames <<- puNames_temp

# Determine which PU each fish ends up in based on its rkm and assign it.
# Uses pre-compiled function 'fishPU' from source files loaded up front.
# Main-to-piscataquis spawners
sp_1$pus = as.character(fishPU(puRkm[[1]], sp_1$finalRkm, puNames[[1]]))
# Main-to-mainstem spawners
sp_2$pus = as.character(fishPU(puRkm[[2]], sp_2$finalRkm, puNames[[2]]))
# Stillwater-to-piscataquis spawners
sp_3$pus = as.character(fishPU(puRkm[[3]], sp_3$finalRkm, puNames[[3]]))
# Stillwater-to-mainstem spawners
sp_4$pus = as.character(fishPU(puRkm[[4]], sp_4$finalRkm, puNames[[4]]))

# Replace the blank PUs for fish that ended at Veazie
sp_1$pus[sp_1$pus == ""] = "PU_1_1"
sp_2$pus[sp_2$pus == ""] = "PU_2_1"
sp_3$pus[sp_3$pus == ""] = "PU_3_1"
sp_4$pus[sp_4$pus == ""] = "PU_4_1"

# Determine the probability that a fish survives to spawn
# Pre-spawning mortality by sex
sp_1$preSpawn = sp_1$female * pre_spawn_survival_females +
  (1 - sp_1$female) * pre_spawn_survival_males
sp_2$preSpawn = sp_2$female * pre_spawn_survival_females +
  (1 - sp_2$female) * pre_spawn_survival_males
sp_3$preSpawn = sp_3$female * pre_spawn_survival_females +
  (1 - sp_3$female) * pre_spawn_survival_males
sp_4$preSpawn = sp_4$female * pre_spawn_survival_females +
  (1 - sp_4$female) * pre_spawn_survival_males

# Determine fishing mortality by PU
sp_1$F = inriv[[1]][as.numeric(substrRight(sp_1$pus, 1))]
sp_2$F = inriv[[2]][as.numeric(substrRight(sp_2$pus, 1))]
sp_3$F = inriv[[3]][as.numeric(substrRight(sp_3$pus, 1))]
sp_4$F = inriv[[4]][as.numeric(substrRight(sp_4$pus, 1))]


# Apply in-river fishing mortality and prespawn survival
sp_1$surv = rbinom(nrow(sp_1), 1, sp_1$preSpawn * (1 - sp_1$F))
sp_2$surv = rbinom(nrow(sp_2), 1, sp_2$preSpawn * (1 - sp_2$F))
sp_3$surv = rbinom(nrow(sp_3), 1, sp_3$preSpawn * (1 - sp_3$F))
sp_4$surv = rbinom(nrow(sp_4), 1, sp_4$preSpawn * (1 - sp_4$F))
#toc()

sp_1 <<- sp_1
sp_2 <<- sp_2
sp_3 <<- sp_3
sp_4 <<- sp_4

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
#    useProgress, useTictoc, weldon, Weldonpost_spawn, WEnfD, WEnfUp, WestEnfieldD, WestEnfieldDj, 
#    WestEnfieldUp, writeData, writeSenData, writeSimData, x, yday, year, years, z)
#save.image(paste(baseDirectory, innerLoopSamplingRData, sep='/'))

}
