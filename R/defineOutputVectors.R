# defineOutputVectors.R

# INPUT/OUTPUT VECTOR PRE-ALLOCATION --------------------------------------
# This file is called by PenobscotRiverModel.R

# Simulation settings for outer loop
# The number of runs and years is
# now inherited from penobscotRiverModel()
# as arguments to that function
nRuns = nruns
nYears = nyears

if (useTictoc | useProgress) {
  print(paste('nRuns = ',nRuns))
  print(paste('nYears = ',nYears))
}

# Define empty vectors to hold results for outer loop
# Empty container to hold year
years = vector(mode = 'numeric', length = nYears * nRuns)

# Empty container to hold Weldon scenario
scen = vector(mode = 'numeric', length = nYears * nRuns)

# Upstream passage effeciencies
OrUp = vector(mode = 'numeric', length = nYears * nRuns)
StUp = vector(mode = 'numeric', length = nYears * nRuns)
GilmUp = vector(mode = 'numeric', length = nYears * nRuns)
MdUp = vector(mode = 'numeric', length = nYears * nRuns)
HdUp = vector(mode = 'numeric', length = nYears * nRuns)
WEnfUp = vector(mode = 'numeric', length = nYears * nRuns)
BMillUp = vector(mode = 'numeric', length = nYears * nRuns)
MooseUp = vector(mode = 'numeric', length = nYears * nRuns)
GuilfUp = vector(mode = 'numeric', length = nYears * nRuns)
MattUp = vector(mode = 'numeric', length = nYears * nRuns)

# Downstream passage efficiencies
OrD = vector(mode = 'numeric', length = nYears * nRuns)
StD = vector(mode = 'numeric', length = nYears * nRuns)
GilmD = vector(mode = 'numeric', length = nYears * nRuns)
MdD = vector(mode = 'numeric', length = nYears * nRuns)
HdD = vector(mode = 'numeric', length = nYears * nRuns)
WEnfD = vector(mode = 'numeric', length = nYears * nRuns)
BMillD = vector(mode = 'numeric', length = nYears * nRuns)
MooseD = vector(mode = 'numeric', length = nYears * nRuns)
GuilfD = vector(mode = 'numeric', length = nYears * nRuns)
MattD = vector(mode = 'numeric', length = nYears * nRuns)

# Indirect mortality during downstream passage
indirectM = vector(mode = 'numeric', length = nYears * nRuns)

# Latent estuary mortality during downstream passage
latentM = vector(mode = 'numeric', length = nYears * nRuns)

# Juvenile survival reduction at each dam during downstream passage
juvReduction = vector(mode = 'numeric', length = nYears * nRuns)

# Fall-back during upstream passage
fallback = vector(mode = 'numeric', length = nYears * nRuns)

# Population abundance in each production unit
# Population abundance below Milford Dam in main-stem
LowerPop = vector(mode = 'numeric', length = nYears * nRuns)
# Population abundance between Orono and Stillwater dams
OronoPop = vector(mode = 'numeric', length = nYears * nRuns)
# Population abundance between Stillwater Dam and Gilman Falls
StillwaterPop = vector(mode = 'numeric', length = nYears * nRuns)
# Population abundance between Milford Dam and confluence
MilfordPop = vector(mode = 'numeric', length = nYears * nRuns)
# Population abundance between West Enfield and Weldon dams
EnfieldPop = vector(mode = 'numeric', length = nYears * nRuns)
# Population abundance above Weldon Dam
WeldonPop = vector(mode = 'numeric', length = nYears * nRuns)
# Population abundance between Howland and Moosehead dams
HowlandPop = vector(mode = 'numeric', length = nYears * nRuns)
# Population abundance between Moosehead and Browns Mill dams
MoosePop = vector(mode = 'numeric', length = nYears * nRuns)
# Population abundance between Browns Mill and Guilford dams
BrownsPop = vector(mode = 'numeric', length = nYears * nRuns)
# Population abundance above Guilford Dam
GuilfordPop = vector(mode = 'numeric', length = nYears * nRuns)

# Age-structured spawning population
spawners = vector(mode = 'list', length = nYears * nRuns)

# Proportion of repeat spawners in each age class
pRepeats = vector(mode = 'list', length = nYears * nRuns)

# Catchment-wide population abundance
populationSize = vector(mode = 'numeric', length = nYears * nRuns)

# Store the scale
scalarVar = vector(mode = 'numeric', length = nYears * nRuns)

# Store the inputs for sensitivity analysis
# Passage assumptions
ptime = vector(mode = 'numeric', length = nYears * nRuns)
pStillUP = vector(mode = 'numeric', length = nYears * nRuns)
pStillD = vector(mode = 'numeric', length = nYears * nRuns)
pPiscUP = vector(mode = 'numeric', length = nYears * nRuns)

# Population demographics
S.downstream = vector(mode = 'numeric', length = nYears * nRuns)
S.marine = vector(mode = 'numeric', length = nYears * nRuns)
F.inRiver = vector(mode = 'numeric', length = nYears * nRuns)
F.commercial = vector(mode = 'numeric', length = nYears * nRuns)
F.bycatch = vector(mode = 'numeric', length = nYears * nRuns)
popStart = vector(mode = 'numeric', length = nYears * nRuns)
p.female = vector(mode = 'numeric', length = nYears * nRuns)
p.female = vector(mode = 'numeric', length = nYears * nRuns)
S.prespawnM = vector(mode = 'numeric', length = nYears * nRuns)
S.postspawnM = vector(mode = 'numeric', length = nYears * nRuns)
S.prespawnF = vector(mode = 'numeric', length = nYears * nRuns)
S.postspawnF = vector(mode = 'numeric', length = nYears * nRuns)
S.juvenile = vector(mode = 'numeric', length = nYears * nRuns)

# Environmental
# Stochasticity
t.stoch = vector(mode = 'numeric', length = nYears * nRuns)
# Regression relating temperatures in PNR and CTR
t.RegrInt = vector(mode = 'numeric', length = nYears * nRuns)
t.RegrSlp = vector(mode = 'numeric', length = nYears * nRuns)
# Model parameters for sex-specific arrival timing
b.ArrRegrInt = vector(mode = 'numeric', length = nYears * nRuns)
b.ArrRegrSlp = vector(mode = 'numeric', length = nYears * nRuns)
r.ArrRegrInt = vector(mode = 'numeric', length = nYears * nRuns)
r.ArrRegrSlp = vector(mode = 'numeric', length = nYears * nRuns)

# Individual traits
# Entry dates
b.Arr = vector(mode = 'numeric', length = nYears * nRuns)
r.Arr = vector(mode = 'numeric', length = nYears * nRuns)
# Spawning ATU
ATUspawn1 = vector(mode = 'numeric', length = nYears * nRuns)
ATUspawn2 = vector(mode = 'numeric', length = nYears * nRuns)
# Spawning dates
Dspawn1 = vector(mode = 'numeric', length = nYears * nRuns)
Dspawn2 = vector(mode = 'numeric', length = nYears * nRuns)
# Length at age
# Females
linF = vector(mode = 'numeric', length = nYears * nRuns)
kF = vector(mode = 'numeric', length = nYears * nRuns)
t0F = vector(mode = 'numeric', length = nYears * nRuns)
# Males
linM = vector(mode = 'numeric', length = nYears * nRuns)
kM = vector(mode = 'numeric', length = nYears * nRuns)
t0M = vector(mode = 'numeric', length = nYears * nRuns)

# Length-weight regression parameters
# Female
lwF.alpha = vector(mode = 'numeric', length = nYears * nRuns)
lwF.beta = vector(mode = 'numeric', length = nYears * nRuns)
# Male
lwM.alpha = vector(mode = 'numeric', length = nYears * nRuns)
lwM.beta = vector(mode = 'numeric', length = nYears * nRuns)

# Lengths and mass
b.length = vector(mode = 'numeric', length = nYears * nRuns)
r.length = vector(mode = 'numeric', length = nYears * nRuns)

# Fecundity
spawnInt = vector(mode = 'numeric', length = nYears * nRuns)
batchSize = vector(mode = 'numeric', length = nYears * nRuns)
resTime = vector(mode = 'numeric', length = nYears * nRuns)

# Movement parameters
s.Optim = vector(mode = 'numeric', length = nYears * nRuns)
d.Max = vector(mode = 'numeric', length = nYears * nRuns)
tortuosity = vector(mode = 'numeric', length = nYears * nRuns)
motivation = vector(mode = 'numeric', length = nYears * nRuns)
daily.move = vector(mode = 'numeric', length = nYears * nRuns)
