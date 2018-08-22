#' @title Fill output vectors on loop completion
#' 
#' @description Internal function used to assign
#' variables to positions corresponding to simulation
#' run and year in output containers allocated in
#' \code{\link{defineOutputVectors}}.
#' 
#' Not intended to be called directly, but visible
#' for the sake of model transparency.
#' 
#' @export
#' 
fillOutputVectors <- function(){
  
if(river=='penobscot'){ 
  # Store output in pre-allocated vectors 
  # if (useTictoc) tic("store output")

  # Year, fillling pre-allocated vector with this year
  years[(n + nYears * (k - 1))] <-  n

  # Scenario for passage timing at Weldon
  scen[(n + nYears * (k - 1))] <-  scenario

  # Upstream passage efficiencies, fillling pre-allocated vectors
  OrUp[(n + nYears * (k - 1))] <-  OronoUp
  StUp[(n + nYears * (k - 1))] <-  StillwaterUp
  GilmUp[(n + nYears * (k - 1))] <-  GilmanUp
  MdUp[(n + nYears * (k - 1))] <-  up[1]
  HdUp[(n + nYears * (k - 1))] <-  up[2]
  WEnfUp[(n + nYears * (k - 1))] <-  up[3]
  BMillUp[(n + nYears * (k - 1))] <-  up[4]
  MooseUp[(n + nYears * (k - 1))] <-  up[5]
  GuilfUp[(n + nYears * (k - 1))] <-  up[6]
  MattUp[(n + nYears * (k - 1))] <-   upEffs[[2]][5]#up[7]

  # Downstream passage efficiencies, fillling pre-allocated vectors
  OrD[(n + nYears * (k - 1))] <-  d[2]
  StD[(n + nYears * (k - 1))] <-  d[1]
  GilmD[(n + nYears * (k - 1))] <-  1
  MdD[(n + nYears * (k - 1))] <-  d[3]
  HdD[(n + nYears * (k - 1))] <-  d[4]
  WEnfD[(n + nYears * (k - 1))] <-  d[5]
  BMillD[(n + nYears * (k - 1))] <-  d[6]
  MooseD[(n + nYears * (k - 1))] <-  d[7]
  GuilfD[(n + nYears * (k - 1))] <-  d[8]
  MattD[(n + nYears * (k - 1))] <-   d[9]

  # Indirect mortality, fillling pre-allocated vector
  indirectM[(n + nYears * (k - 1))] <-  indirect

  # Latent estuary mortality, fillling pre-allocated vector
  latentM[(n + nYears * (k - 1))] <-  latent

  # Juvenile reduction factor at each dam, fillling pre-allocated vector
  juvReduction[(n + nYears * (k - 1))] <-  jReduction

  # Fall back, fillling pre-allocated vector
  fallback[(n + nYears * (k - 1))] <-  fB

  # Population below Milford, fillling pre-allocated vector
  LowerPop[(n + nYears * (k - 1))] <-  (
      sum(males2res[[1]][[1]]) +
      sum(males2res[[1]][[2]]) +
      sum(males2res[[2]][[1]]) + 
      sum(males2res[[2]][[2]]) +
      sum(males2res[[3]][[1]]) +
      sum(males2res[[4]][[1]]) +
      sum(females2res[[1]][[2]]) + 
      sum(females2res[[1]][[2]]) +
      sum(females2res[[2]][[1]]) + 
      sum(females2res[[2]][[2]]) +
      sum(females2res[[3]][[1]]) +
      sum(females2res[[4]][[1]])
  ) * scalar

  # Population between Orono and Stillwater dams, fillling pre-allocated vector
  OronoPop[(n + nYears * (k - 1))] <-  (
    sum(males2res[[3]][[2]]) +
      sum(males2res[[4]][[2]]) +
      sum(females2res[[3]][[2]]) +
      sum(females2res[[4]][[2]])
    ) * scalar

  # Population between Stillwater Dam and Gilman Falls, fillling pre-allocated
  StillwaterPop[(n + nYears * (k - 1))] <-  (
    sum(males2res[[3]][[3]]) +
      sum(males2res[[4]][[3]]) +
      sum(females2res[[3]][[3]]) +
      sum(females2res[[4]][[3]])
    ) * scalar

  # Population between Milford and West Enfield, fillling pre-allocated vector
  MilfordPop[(n + nYears * (k - 1))] <-  (
    sum(males2res[[1]][[3]]) +
      sum(males2res[[2]][[3]]) +
      sum(males2res[[3]][[4]]) +
      sum(males2res[[4]][[4]]) +
      sum(females2res[[1]][[3]]) +
      sum(females2res[[2]][[3]]) +
      sum(females2res[[3]][[4]]) +
      sum(females2res[[4]][[4]])
    ) * scalar + OronoPop[(n + nYears * (k - 1))] +
    StillwaterPop[(n + nYears * (k - 1))]

  # Population between West Enfield and Weldon, fillling pre-allocated vector
  EnfieldPop[(n + nYears * (k - 1))] <-  (
    sum(males2res[[2]][[4]]) +
      sum(males2res[[4]][[5]]) +
      sum(females2res[[2]][[4]]) +
      sum(females2res[[4]][[5]])
    ) * scalar

  # Population above Weldon, fillling pre-allocated vector
  WeldonPop[(n + nYears * (k - 1))] <-  (
    sum(males2res[[2]][[length(males2res[[2]])]]) +
      sum(males2res[[4]][[length(males2res[[4]])]]) +
      sum(females2res[[2]][[length(females2res[[2]])]]) +
      sum(females2res[[4]][[length(females2res[[4]])]])
    ) * scalar

  # Population between Howland and Dover dams, fillling pre-allocated vector
  HowlandPop[(n + nYears * (k - 1))] <-  (
    sum(males2res[[1]][[4]]) +
      sum(males2res[[3]][[5]]) +
      sum(females2res[[1]][[4]]) +
      sum(females2res[[3]][[5]])
    ) * scalar

  # Population between Moosehead and Browns Mill dams
  MoosePop[(n + nYears * (k - 1))] <-  (
    sum(males2res[[1]][[5]]) +
      sum(males2res[[3]][[6]]) +
      sum(females2res[[1]][[5]]) +
      sum(females2res[[3]][[6]])
    ) * scalar

  # Population between Browns Mill and Guilford dams, fillling pre-allocated
  BrownsPop[(n + nYears * (k - 1))] <-  (
    sum(males2res[[1]][[6]]) +
      sum(males2res[[3]][[7]]) +
      sum(females2res[[1]][[6]]) +
      sum(females2res[[3]][[7]])
    ) * scalar

  # Population above Guilford Dam, fillling pre-allocated vector
  GuilfordPop[(n + nYears * (k - 1))] <-  (
    sum(males2res[[1]][[7]]) +
      sum(males2res[[3]][[8]]) +
      sum(females2res[[1]][[7]]) +
      sum(females2res[[3]][[8]])
    ) * scalar

  
  # Population size
  populationSize[(n + nYears * (k - 1))] <-
   LowerPop[(n + nYears * (k - 1))] +
    MilfordPop[(n + nYears * (k - 1))] +
    EnfieldPop[(n + nYears * (k - 1))] +
    WeldonPop[(n + nYears * (k - 1))] +
    HowlandPop[(n + nYears * (k - 1))] +
    MoosePop[(n + nYears * (k - 1))] +
    BrownsPop[(n + nYears * (k - 1))] +
    GuilfordPop[(n + nYears * (k - 1))]

  # Proportion of repeat spawners at each age, fillling pre-allocated vector
  pRepeats[[(n + nYears * (k - 1))]] <-  pRepeat

  # Age-structured repeat spawners, fillling pre-allocated vector
  spawners[[(n + nYears * (k - 1))]] <-  spawningPool

  # Reset the scalar based on population size
  list2env(setScalar(), envir = .shadia)

  # Scalar variable for computational gains
  scalarVar[[(n + nYears * (k - 1))]] <-  scalar

  # Store the inputs for sensitivity analysis
  # Passage assumptions
  ptime[(n + nYears * (k - 1))] <-  timely
  pStillUP[(n + nYears * (k - 1))] <-  pStillwaterUp
  pStillD[(n + nYears * (k - 1))] <-  pStillwaterD
  pPiscUP[(n + nYears * (k - 1))] <-  pPiscUp

  # Population demographics and survival rates
  S.downstream[(n + nYears * (k - 1))] <-  mean(downstreamS)
  S.marine[(n + nYears * (k - 1))] <-  mean(oceanSurvival)
  F.inRiver[(n + nYears * (k - 1))] <-  inRiverF
  F.commercial[(n + nYears * (k - 1))] <-  mean(commercialF)
  F.bycatch[(n + nYears * (k - 1))] <-  mean(bycatchF)
  popStart[(n + nYears * (k - 1))] <-  pop
  p.female[(n + nYears * (k - 1))] <-  sex_Ratio
  S.prespawnM[(n + nYears * (k - 1))] <-  pre_spawn_survival_males
  S.postspawnM[(n + nYears * (k - 1))] <-  post_spawn_survival_males
  S.prespawnF[(n + nYears * (k - 1))] <-  pre_spawn_survival_females
  S.postspawnF[(n + nYears * (k - 1))] <-  post_spawn_survival_females
  S.juvenile[(n + nYears * (k - 1))] <-  juvenile_survival

  # Environmental
  # Stochasticity
  t.stoch[(n + nYears * (k - 1))] <-  stoch
  # Regression relating temperatures in PNR and CTR
  t.RegrInt[(n + nYears * (k - 1))] <-  calMod[1, 1]
  t.RegrSlp[(n + nYears * (k - 1))] <-  calMod[2, 1]
  # Model parameters for sex-specific arrival timing
  b.ArrRegrInt[(n + nYears * (k - 1))] <-  res.B[1, 1]
  b.ArrRegrSlp[(n + nYears * (k - 1))] <-  res.B[2, 1]
  r.ArrRegrInt[(n + nYears * (k - 1))] <-  res.R[1, 1]
  r.ArrRegrSlp[(n + nYears * (k - 1))] <-  res.R[2, 1]

  # Individual traits
  # Entry dates
  b.Arr[(n + nYears * (k - 1))] <-  mean(c_entryDate[c_sex == 0])
  r.Arr[(n + nYears * (k - 1))] <-  mean(c_entryDate[c_sex == 1])
  # Spawning ATU
  ATUspawn1[(n + nYears * (k - 1))] <-  mean(c_spawnATU1)
  ATUspawn2[(n + nYears * (k - 1))] <-  mean(c_spawnATU2)
  # Spawning dates
  Dspawn1[(n + nYears * (k - 1))] <-  mean(c_initial)
  Dspawn2[(n + nYears * (k - 1))] <-  mean(c_end)
  # Length at age
  # Females
  linF[(n + nYears * (k - 1))] <-  r.mat[1]
  kF[(n + nYears * (k - 1))] <-  r.mat[2]
  t0F[(n + nYears * (k - 1))] <-  r.mat[3]
  # Males
  linM[(n + nYears * (k - 1))] <-  b.mat[1]
  kM[(n + nYears * (k - 1))] <-  b.mat[2]
  t0M[(n + nYears * (k - 1))] <-  b.mat[3]

  # Length-weight regression parameters
  # Female
  lwF.alpha[(n + nYears * (k - 1))] <-  c_femaleLWalpha
  lwF.beta[(n + nYears * (k - 1))] <-  c_femaleLWbeta
  # Male
  lwM.alpha[(n + nYears * (k - 1))] <-  c_maleLWalpha
  lwM.beta[(n + nYears * (k - 1))] <-  c_maleLWbeta

  # Length
  b.length[(n + nYears * (k - 1))] <-  mean(c_male_lf[c_male_lf!=0])
  r.length[(n + nYears * (k - 1))] <-  mean(c_female_lf[c_male_lf!=0])

  # Fecundity
  spawnInt[(n + nYears * (k - 1))] <-  mean(c_SI)
  batchSize[(n + nYears * (k - 1))] <-  mean(c_BF)
  resTime[(n + nYears * (k - 1))] <-  mean(c_RAF)

  # Movement parameters
  s.Optim[(n + nYears * (k - 1))] <-  mean(sOptim)
  d.Max[(n + nYears * (k - 1))] <-  mean(dMax)
  tortuosity[(n + nYears * (k - 1))] <-  mean(tort)
  motivation[(n + nYears * (k - 1))] <-  mot
  daily.move[(n + nYears * (k - 1))] <-  mean(dailyMove)
  #toc()

return(list(
scalar = scalar,
populationSize = populationSize,
years = years,
scen = scen,
OrUp = OrUp,
StUp = StUp,
GilmUp = GilmUp,
MdUp = MdUp,
HdUp = HdUp,
WEnfUp = WEnfUp,
BMillUp = BMillUp,
MooseUp = MooseUp,
GuilfUp = GuilfUp,
MattUp = MattUp,
OrD = OrD,
StD = StD,
GilmD = GilmD,
MdD = MdD,
HdD = HdD,
WEnfD = WEnfD,
BMillD = BMillD,
MooseD = MooseD,
GuilfD = GuilfD,
MattD = MattD,
indirectM = indirectM,
latentM = latentM,
juvReduction = juvReduction,
fallback = fallback,
LowerPop = LowerPop,
OronoPop = OronoPop,
StillwaterPop = StillwaterPop,
MilfordPop = MilfordPop,
EnfieldPop = EnfieldPop,
WeldonPop = WeldonPop,
HowlandPop = HowlandPop,
MoosePop = MoosePop,
BrownsPop = BrownsPop,
GuilfordPop = GuilfordPop,
pRepeats = pRepeats,
spawners = spawners,
scalarVar = scalarVar,
ptime = ptime,
pStillUP = pStillUP,
pStillD = pStillD,
pPiscUP = pPiscUP,
S.downstream = S.downstream,
S.marine = S.marine,
F.inRiver = F.inRiver,
F.commercial = F.commercial,
F.bycatch = F.bycatch,
popStart = popStart,
p.female = p.female,
S.prespawnM = S.prespawnM,
S.postspawnM = S.postspawnM,
S.prespawnF = S.prespawnF,
S.postspawnF = S.postspawnF,
S.juvenile = S.juvenile,
t.stoch = t.stoch,
t.RegrInt = t.RegrInt,
t.RegrSlp = t.RegrSlp,
b.ArrRegrInt = b.ArrRegrInt,
b.ArrRegrSlp = b.ArrRegrSlp,
r.ArrRegrInt = r.ArrRegrInt,
r.ArrRegrSlp = r.ArrRegrSlp,
b.Arr = b.Arr,
r.Arr = r.Arr,
ATUspawn1 = ATUspawn1,
ATUspawn2 = ATUspawn2,
Dspawn1 = Dspawn1,
Dspawn2 = Dspawn2,
linF = linF,
kF = kF,
t0F = t0F,
linM = linM,
kM = kM,
t0M = t0M,
lwF.alpha = lwF.alpha,
lwF.beta = lwF.beta,
lwM.alpha = lwM.alpha,
lwM.beta = lwM.beta,
b.length = b.length,
r.length = r.length,
spawnInt = spawnInt,
batchSize = batchSize,
resTime = resTime,
s.Optim = s.Optim,
d.Max = d.Max,
tortuosity = tortuosity,
motivation = motivation,
daily.move = daily.move
))
}  
  
  
if(river=='merrimack'){ 
  # Store output in pre-allocated vectors 
  # if (useTictoc) tic("store output")

  # Year, fillling pre-allocated vector with this year
  years[(n + nYears * (k - 1))] <-  n

  # Upstream passage efficiencies, fillling pre-allocated vectors
  EssUp[(n + nYears * (k - 1))] <- EssexUp 
  PawBUp[(n + nYears * (k - 1))] <- PawtucketBypassUp
  PawUp[(n + nYears * (k - 1))] <- PawtucketUp
  AmosUp[(n + nYears * (k - 1))] <- AmoskeagUp
  HookUp[(n + nYears * (k - 1))] <- HooksetUp

  # Downstream passage efficiencies, fillling pre-allocated vectors
  EssD[(n + nYears * (k - 1))] <-  EssexD
  PawBD[(n + nYears * (k - 1))] <- PawtucketBypassD
  PawD[(n + nYears * (k - 1))] <-  PawtucketD
  AmosD[(n + nYears * (k - 1))] <- AmoskeagD
  HookD[(n + nYears * (k - 1))] <- HooksetD

  # Probability of using bypass route at pawtucket
  pBypassUS[(n + nYears * (k - 1))] <- pBypassUp
  pBypassDS[(n + nYears * (k - 1))] <- pBypassD
  
  # Indirect mortality, fillling pre-allocated vector
  indirectM[(n + nYears * (k - 1))] <-  indirect

  # Latent estuary mortality, fillling pre-allocated vector
  latentM[(n + nYears * (k - 1))] <-  latent

  # Juvenile reduction factor at each dam, fillling pre-allocated vector
  juvReduction[(n + nYears * (k - 1))] <-  jReduction

  # Fall back, fillling pre-allocated vector
  fallback[(n + nYears * (k - 1))] <-  fB

  # Population below Essex, fillling pre-allocated vector
  popI[(n + nYears * (k - 1))] <-  (
    sum(males2res[[1]][[1]]) + 
      sum(females2res[[1]][[1]])) * scalar

  # Population between Essex and Pawtucket dams
  popII[(n + nYears * (k - 1))] <-  (
    sum(males2res[[1]][[2]]) + 
      sum(females2res[[1]][[2]])) * scalar

  # Population between Pawtucket and Amoskeag dams
  popIII[(n + nYears * (k - 1))] <-  (
    sum(males2res[[1]][[3]]) + 
      sum(females2res[[1]][[3]])) * scalar

  # Population between Amoskeag and Hookset dams
  popIV[(n + nYears * (k - 1))] <-  (
    sum(males2res[[1]][[4]]) + 
      sum(females2res[[1]][[4]])) * scalar
  
  # Population upstream of Hookset Dam
  popV[(n + nYears * (k - 1))] <-  (
    sum(males2res[[1]][[5]]) + 
      sum(females2res[[1]][[5]])) * scalar

  # Population size
  populationSize[(n + nYears * (k - 1))] <- 
    popI[(n + nYears * (k - 1))] +
    popII[(n + nYears * (k - 1))] +
    popIII[(n + nYears * (k - 1))] +
    popIV[(n + nYears * (k - 1))] +
    popV[(n + nYears * (k - 1))]
  
  # Proportion of repeat spawners at each age, fillling pre-allocated vector
  pRepeats[[(n + nYears * (k - 1))]] <-  pRepeat

  # Age-structured repeat spawners, fillling pre-allocated vector
  spawners[[(n + nYears * (k - 1))]] <-  spawningPool

  # Reset the scalar based on population size
  list2env(setScalar(), envir = .shadia)

  # Scalar variable for computational gains
  scalarVar[[(n + nYears * (k - 1))]] <-  scalar

  # Store the inputs for sensitivity analysis
  # Passage assumptions
  ptime[(n + nYears * (k - 1))] <-  timely

  # Population demographics and survival rates
  S.downstream[(n + nYears * (k - 1))] <-  mean(downstreamS)
  S.marine[(n + nYears * (k - 1))] <-  mean(oceanSurvival)
  F.inRiver[(n + nYears * (k - 1))] <-  inRiverF
  F.commercial[(n + nYears * (k - 1))] <-  mean(commercialF)
  F.bycatch[(n + nYears * (k - 1))] <-  mean(bycatchF)
  popStart[(n + nYears * (k - 1))] <-  pop
  p.female[(n + nYears * (k - 1))] <-  sex_Ratio
  S.prespawnM[(n + nYears * (k - 1))] <-  pre_spawn_survival_males
  S.postspawnM[(n + nYears * (k - 1))] <-  post_spawn_survival_males
  S.prespawnF[(n + nYears * (k - 1))] <-  pre_spawn_survival_females
  S.postspawnF[(n + nYears * (k - 1))] <-  post_spawn_survival_females
  S.juvenile[(n + nYears * (k - 1))] <-  juvenile_survival

  # Environmental
  # Stochasticity
  t.stoch[(n + nYears * (k - 1))] <-  stoch
  # Regression relating temperatures in PNR and CTR
  t.RegrInt[(n + nYears * (k - 1))] <-  calMod[1, 1]
  t.RegrSlp[(n + nYears * (k - 1))] <-  calMod[2, 1]
  # Model parameters for sex-specific arrival timing
  b.ArrRegrInt[(n + nYears * (k - 1))] <-  res.B[1, 1]
  b.ArrRegrSlp[(n + nYears * (k - 1))] <-  res.B[2, 1]
  r.ArrRegrInt[(n + nYears * (k - 1))] <-  res.R[1, 1]
  r.ArrRegrSlp[(n + nYears * (k - 1))] <-  res.R[2, 1]

  # Individual traits
  # Entry dates
  b.Arr[(n + nYears * (k - 1))] <-  mean(c_entryDate[c_sex == 0])
  r.Arr[(n + nYears * (k - 1))] <-  mean(c_entryDate[c_sex == 1])
  # Spawning ATU
  ATUspawn1[(n + nYears * (k - 1))] <-  mean(c_spawnATU1)
  ATUspawn2[(n + nYears * (k - 1))] <-  mean(c_spawnATU2)
  # Spawning dates
  Dspawn1[(n + nYears * (k - 1))] <-  mean(c_initial)
  Dspawn2[(n + nYears * (k - 1))] <-  mean(c_end)
  # Length at age
  # Females
  linF[(n + nYears * (k - 1))] <-  r.mat[1]
  kF[(n + nYears * (k - 1))] <-  r.mat[2]
  t0F[(n + nYears * (k - 1))] <-  r.mat[3]
  # Males
  linM[(n + nYears * (k - 1))] <-  b.mat[1]
  kM[(n + nYears * (k - 1))] <-  b.mat[2]
  t0M[(n + nYears * (k - 1))] <-  b.mat[3]

  # Length-weight regression parameters
  # Female
  lwF.alpha[(n + nYears * (k - 1))] <-  c_femaleLWalpha
  lwF.beta[(n + nYears * (k - 1))] <-  c_femaleLWbeta
  # Male
  lwM.alpha[(n + nYears * (k - 1))] <-  c_maleLWalpha
  lwM.beta[(n + nYears * (k - 1))] <-  c_maleLWbeta

  # Length
  b.length[(n + nYears * (k - 1))] <-  mean(c_male_lf[c_male_lf!=0])
  r.length[(n + nYears * (k - 1))] <-  mean(c_female_lf[c_male_lf!=0])

  # Fecundity
  spawnInt[(n + nYears * (k - 1))] <-  mean(c_SI)
  batchSize[(n + nYears * (k - 1))] <-  mean(c_BF)
  resTime[(n + nYears * (k - 1))] <-  mean(c_RAF)

  # Movement parameters
  s.Optim[(n + nYears * (k - 1))] <-  mean(sOptim)
  d.Max[(n + nYears * (k - 1))] <-  mean(dMax)
  tortuosity[(n + nYears * (k - 1))] <-  mean(tort)
  motivation[(n + nYears * (k - 1))] <-  mot
  daily.move[(n + nYears * (k - 1))] <-  mean(dailyMove)
  #toc()

return(list(
  scalar = scalar,
  populationSize = populationSize,
  years = years,
  EssUp = EssUp,
  PawUp = PawUp,
  AmosUp = AmosUp,
  HookUp = HookUp,
  EssD = EssD,
  PawD = PawD,
  AmosD = AmosD,
  HookD = HookD,
  indirectM = indirectM,
  latentM = latentM,
  juvReduction = juvReduction,
  fallback = fallback,
  popI = popI,
  popII = popII,
  popIII = popIII,
  popIV = popIV,
  popV = popV,
  pRepeats = pRepeats,
  spawners = spawners,
  scalarVar = scalarVar,
  ptime = ptime,
  S.downstream = S.downstream,
  S.marine = S.marine,
  F.inRiver = F.inRiver,
  F.commercial = F.commercial,
  F.bycatch = F.bycatch,
  popStart = popStart,
  p.female = p.female,
  S.prespawnM = S.prespawnM,
  S.postspawnM = S.postspawnM,
  S.prespawnF = S.prespawnF,
  S.postspawnF = S.postspawnF,
  S.juvenile = S.juvenile,
  t.stoch = t.stoch,
  t.RegrInt = t.RegrInt,
  t.RegrSlp = t.RegrSlp,
  b.ArrRegrInt = b.ArrRegrInt,
  b.ArrRegrSlp = b.ArrRegrSlp,
  r.ArrRegrInt = r.ArrRegrInt,
  r.ArrRegrSlp = r.ArrRegrSlp,
  b.Arr = b.Arr,
  r.Arr = r.Arr,
  ATUspawn1 = ATUspawn1,
  ATUspawn2 = ATUspawn2,
  Dspawn1 = Dspawn1,
  Dspawn2 = Dspawn2,
  linF = linF,
  kF = kF,
  t0F = t0F,
  linM = linM,
  kM = kM,
  t0M = t0M,
  lwF.alpha = lwF.alpha,
  lwF.beta = lwF.beta,
  lwM.alpha = lwM.alpha,
  lwM.beta = lwM.beta,
  b.length = b.length,
  r.length = r.length,
  spawnInt = spawnInt,
  batchSize = batchSize,
  resTime = resTime,
  s.Optim = s.Optim,
  d.Max = d.Max,
  tortuosity = tortuosity,
  motivation = motivation,
  daily.move = daily.move
  ))
}    

  
if(river=='connecticut'){ 
  # Store output in pre-allocated vectors 
  # if (useTictoc) tic("store output")
  # Population size
  #populationSize[(n + nYears * (k - 1))] <-  sum(spawningPool)

  # Year, fillling pre-allocated vector with this year
  years[(n + nYears * (k - 1))] <-  n

  # Probability of using spillway for migration through TF
  pSpill[(n + nYears * (k - 1))] <- pSpillway
  
  # Upstream passage efficiencies
  HolUp[(n + nYears * (k - 1))] <-   HolyokeUp 
  CabUp[(n + nYears * (k - 1))] <-   CabotUp
  SpillUp[(n + nYears * (k - 1))] <- SpillwayUp
  GateUp[(n + nYears * (k - 1))] <-  GatehouseUp
  VernUp[(n + nYears * (k - 1))] <-  VernonUp

  # Downstream passage efficiencies
  HolD[(n + nYears * (k - 1))] <-  HolyokeD
  CabD[(n + nYears * (k - 1))] <-  CabotD
  GateD[(n + nYears * (k - 1))] <- GatehouseD
  VernD[(n + nYears * (k - 1))] <- VernonD

  # Northfield Mountain take 
  NorthFieldV[(n + nYears * (k - 1))] <- northfield$vernonJ
  NorthFieldT[(n + nYears * (k - 1))] <- northfield$turnersJ
  NorthFieldVa[(n + nYears * (k - 1))] <- northfield$vernonA
  NorthFieldTa[(n + nYears * (k - 1))] <- northfield$turnersA
  
  # Indirect mortality, fillling pre-allocated vector
  indirectM[(n + nYears * (k - 1))] <-  indirect

  # Latent estuary mortality, fillling pre-allocated vector
  latentM[(n + nYears * (k - 1))] <-  latent

  # Juvenile reduction factor at each dam, fillling pre-allocated vector
  juvReduction[(n + nYears * (k - 1))] <-  jReduction

  # Fall back, fillling pre-allocated vector
  fallback[(n + nYears * (k - 1))] <-  fB

  # Population below Essex, fillling pre-allocated vector
  popI[(n + nYears * (k - 1))] <-  (
    sum(males2res[[1]][[1]]) + 
    sum(females2res[[1]][[1]]) +
    sum(males2res[[2]][[1]]) + 
    sum(females2res[[2]][[1]])) * scalar
  # Population between Essex and Pawtucket dams
  popII[(n + nYears * (k - 1))] <-  (
    sum(males2res[[1]][[2]]) + 
    sum(females2res[[1]][[2]]) +
    sum(males2res[[2]][[2]]) + 
    sum(females2res[[2]][[2]])) * scalar
  # Population between Pawtucket and Amoskeag dams
  popIII[(n + nYears * (k - 1))] <-  (
    sum(males2res[[1]][[3]]) + 
    sum(females2res[[1]][[3]]) +
    sum(males2res[[2]][[3]]) + 
    sum(females2res[[2]][[3]])) * scalar
  # Population between Amoskeag and Hookset dams
  popIV[(n + nYears * (k - 1))] <-  (
    sum(males2res[[1]][[4]]) +
    sum(females2res[[1]][[4]]) +
    sum(males2res[[2]][[4]]) + 
    sum(females2res[[2]][[4]])) * scalar
  # Population upstream of Hookset Dam
  popV[(n + nYears * (k - 1))] <-  (
    sum(males2res[[1]][[5]]) + 
    sum(females2res[[1]][[5]]) +
    sum(males2res[[2]][[5]]) + 
    sum(females2res[[2]][[5]])) * scalar
  
  # Population size
  populationSize[(n + nYears * (k - 1))] <- 
    popI[(n + nYears * (k - 1))] +
    popII[(n + nYears * (k - 1))] +
    popIII[(n + nYears * (k - 1))] +
    popIV[(n + nYears * (k - 1))] +
    popV[(n + nYears * (k - 1))]
    
  # Proportion of repeat spawners at each age, fillling pre-allocated vector
  pRepeats[[(n + nYears * (k - 1))]] <-  pRepeat

  # Age-structured repeat spawners, fillling pre-allocated vector
  spawners[[(n + nYears * (k - 1))]] <-  spawningPool

  # Reset the scalar based on population size
  list2env(setScalar(), envir = .shadia)

  # Scalar variable for computational gains
  scalarVar[[(n + nYears * (k - 1))]] <-  scalar

  # Store the inputs for sensitivity analysis
  # Passage assumptions
  ptime[(n + nYears * (k - 1))] <-  timely

  # Population demographics and survival rates
  S.downstream[(n + nYears * (k - 1))] <-  mean(downstreamS)
  S.marine[(n + nYears * (k - 1))] <-  mean(oceanSurvival)
  F.inRiver[(n + nYears * (k - 1))] <-  inRiverF
  F.commercial[(n + nYears * (k - 1))] <-  mean(commercialF)
  F.bycatch[(n + nYears * (k - 1))] <-  mean(bycatchF)
  popStart[(n + nYears * (k - 1))] <-  pop
  p.female[(n + nYears * (k - 1))] <-  sex_Ratio
  S.prespawnM[(n + nYears * (k - 1))] <-  pre_spawn_survival_males
  S.postspawnM[(n + nYears * (k - 1))] <-  post_spawn_survival_males
  S.prespawnF[(n + nYears * (k - 1))] <-  pre_spawn_survival_females
  S.postspawnF[(n + nYears * (k - 1))] <-  post_spawn_survival_females
  S.juvenile[(n + nYears * (k - 1))] <-  juvenile_survival

  # Environmental
  # Stochasticity
  t.stoch[(n + nYears * (k - 1))] <-  stoch

  # Model parameters for sex-specific arrival timing
  b.ArrRegrInt[(n + nYears * (k - 1))] <-  res.B[1, 1]
  b.ArrRegrSlp[(n + nYears * (k - 1))] <-  res.B[2, 1]
  r.ArrRegrInt[(n + nYears * (k - 1))] <-  res.R[1, 1]
  r.ArrRegrSlp[(n + nYears * (k - 1))] <-  res.R[2, 1]

  # Individual traits
  # Entry dates
  b.Arr[(n + nYears * (k - 1))] <-  mean(c_entryDate[c_sex == 0])
  r.Arr[(n + nYears * (k - 1))] <-  mean(c_entryDate[c_sex == 1])
  # Spawning ATU
  ATUspawn1[(n + nYears * (k - 1))] <-  mean(c_spawnATU1)
  ATUspawn2[(n + nYears * (k - 1))] <-  mean(c_spawnATU2)
  # Spawning dates
  Dspawn1[(n + nYears * (k - 1))] <-  mean(c_initial)
  Dspawn2[(n + nYears * (k - 1))] <-  mean(c_end)
  # Length at age
  # Females
  linF[(n + nYears * (k - 1))] <-  r.mat[1]
  kF[(n + nYears * (k - 1))] <-  r.mat[2]
  t0F[(n + nYears * (k - 1))] <-  r.mat[3]
  # Males
  linM[(n + nYears * (k - 1))] <-  b.mat[1]
  kM[(n + nYears * (k - 1))] <-  b.mat[2]
  t0M[(n + nYears * (k - 1))] <-  b.mat[3]

  # Length-weight regression parameters
  # Female
  lwF.alpha[(n + nYears * (k - 1))] <-  c_femaleLWalpha
  lwF.beta[(n + nYears * (k - 1))] <-  c_femaleLWbeta
  # Male
  lwM.alpha[(n + nYears * (k - 1))] <-  c_maleLWalpha
  lwM.beta[(n + nYears * (k - 1))] <-  c_maleLWbeta

  # Length
  b.length[(n + nYears * (k - 1))] <-  mean(c_male_lf[c_male_lf!=0])
  r.length[(n + nYears * (k - 1))] <-  mean(c_female_lf[c_female_lf!=0])

  # Fecundity
  spawnInt[(n + nYears * (k - 1))] <-  mean(c_SI)
  batchSize[(n + nYears * (k - 1))] <-  mean(c_BF)
  resTime[(n + nYears * (k - 1))] <-  mean(c_RAF)

  # Movement parameters
  s.Optim[(n + nYears * (k - 1))] <-  mean(sOptim)
  d.Max[(n + nYears * (k - 1))] <-  mean(dMax)
  tortuosity[(n + nYears * (k - 1))] <-  mean(tort)
  motivation[(n + nYears * (k - 1))] <-  mot
  daily.move[(n + nYears * (k - 1))] <-  mean(dailyMove)
  #toc()

  return(list(
    scalar = scalar,
    populationSize = populationSize,
    years = years,
    pSpill = pSpill,
    HolUp = HolUp,
    CabUp = CabUp,
    SpillUp = SpillUp,
    GateUp = GateUp,
    VernUp = VernUp,
    HolD = HolD,
    CabD = CabD,
    GateD = GateD,
    VernD = VernD,
    NorthFieldV = NorthFieldV,
    NorthFieldT = NorthFieldT,
    NorthFieldVa = NorthFieldVa,
    NorthFieldTa = NorthFieldTa,
    indirectM = indirectM,
    latentM = latentM,
    juvReduction = juvReduction,
    fallback = fallback,
    popI = popI,
    popII = popII,
    popIII = popIII,
    popIV = popIV,
    popV = popV,
    pRepeats = pRepeats,
    spawners = spawners,
    scalarVar = scalarVar,
    ptime = ptime,
    S.downstream = S.downstream,
    S.marine = S.marine,
    F.inRiver = F.inRiver,
    F.commercial = F.commercial,
    F.bycatch = F.bycatch,
    popStart = popStart,
    p.female = p.female,
    S.prespawnM = S.prespawnM,
    S.postspawnM = S.postspawnM,
    S.prespawnF = S.prespawnF,
    S.postspawnF = S.postspawnF,
    S.juvenile = S.juvenile,
    t.stoch = t.stoch,
    b.ArrRegrInt = b.ArrRegrInt,
    b.ArrRegrSlp = b.ArrRegrSlp,
    r.ArrRegrInt = r.ArrRegrInt,
    r.ArrRegrSlp = r.ArrRegrSlp,
    b.Arr = b.Arr,
    r.Arr = r.Arr,
    ATUspawn1 = ATUspawn1,
    ATUspawn2 = ATUspawn2,
    Dspawn1 = Dspawn1,
    Dspawn2 = Dspawn2,
    linF = linF,
    kF = kF,
    t0F = t0F,
    linM = linM,
    kM = kM,
    t0M = t0M,
    lwF.alpha = lwF.alpha,
    lwF.beta = lwF.beta,
    lwM.alpha = lwM.alpha,
    lwM.beta = lwM.beta,
    b.length = b.length,
    r.length = r.length,
    spawnInt = spawnInt,
    batchSize = batchSize,
    resTime = resTime,
    s.Optim = s.Optim,
    d.Max = d.Max,
    tortuosity = tortuosity,
    motivation = motivation,
    daily.move = daily.move
  ))
}    
  
  
if(river=='susquehanna'){ 
  # Store output in pre-allocated vectors 
  # if (useTictoc) tic("store output")

  # Year, fillling pre-allocated vector with this year
  years[(n + nYears * (k - 1))] <-  n

  # Upstream passage efficiencies, fillling pre-allocated vectors
  ConUp[(n + nYears * (k - 1))] <- ConowingoUp 
  HoltUp[(n + nYears * (k - 1))] <- HoltwoodUp
  SafeUp[(n + nYears * (k - 1))] <- SafeHarborUp
  YorkUp[(n + nYears * (k - 1))] <- YorkHavenUp
  SunUp[(n + nYears * (k - 1))] <- SunburyUp  
  WillUp[(n + nYears * (k - 1))] <- WilliamsportUp  
  ChasUp[(n + nYears * (k - 1))] <- ChaseHibbardUp  
  LockUp[(n + nYears * (k - 1))] <- LockHavenUp
  RockUp[(n + nYears * (k - 1))] <- RockBottomUp
  CollUp[(n + nYears * (k - 1))] <- ColliersvilleUp
  
  # Downstream passage efficiencies, fillling pre-allocated vectors
  ConD[(n + nYears * (k - 1))] <- ConowingoD 
  HoltD[(n + nYears * (k - 1))] <- HoltwoodD
  SafeD[(n + nYears * (k - 1))] <- SafeHarborD
  YorkD[(n + nYears * (k - 1))] <- YorkHavenD
  SunD[(n + nYears * (k - 1))] <- SunburyD  
  WillD[(n + nYears * (k - 1))] <- WilliamsportD  
  ChasD[(n + nYears * (k - 1))] <- ChaseHibbardD  
  LockD[(n + nYears * (k - 1))] <- LockHavenD
  RockD[(n + nYears * (k - 1))] <- RockBottomD
  CollD[(n + nYears * (k - 1))] <- ColliersvilleD
  
  # Path choice probabilities
  pJuniataUp[(n + nYears * (k - 1))] <- p_JuniataUp
  pWestBranchUp[(n + nYears * (k - 1))] <- p_WestBranchUp
  pChemungUp[(n + nYears * (k - 1))] <- p_ChemungUp
  pNorthBranchUp[(n + nYears * (k - 1))] <- p_NorthBranchUp
  
  # Indirect mortality, fillling pre-allocated vector
  indirectM[(n + nYears * (k - 1))] <-  indirect

  # Latent estuary mortality, fillling pre-allocated vector
  latentM[(n + nYears * (k - 1))] <- latent

  # Juvenile reduction factor at each dam, fillling pre-allocated vector
  juvReduction[(n + nYears * (k - 1))] <- jReduction

  # Fall back, fillling pre-allocated vector
  fallback[(n + nYears * (k - 1))] <-  fB

  # Population abundance in each PU
  # Abundance downstream of conowingo
  LowPop[(n + nYears * (k - 1))] <- (
    sum(males2res[[1]][[1]]) + 
    sum(females2res[[1]][[1]]) +
    sum(males2res[[2]][[1]]) + 
    sum(females2res[[2]][[1]]) +    
    sum(males2res[[3]][[1]]) + 
    sum(females2res[[3]][[1]]) +      
    sum(males2res[[4]][[1]]) + 
    sum(females2res[[4]][[1]])) * scalar
  
  # Abundance between Conowingo and Holtwood
  ConPop[(n + nYears * (k - 1))] <- (
    sum(males2res[[1]][[2]]) + 
    sum(females2res[[1]][[2]]) +
    sum(males2res[[2]][[2]]) + 
    sum(females2res[[2]][[2]]) +    
    sum(males2res[[3]][[2]]) + 
    sum(females2res[[3]][[2]]) +      
    sum(males2res[[4]][[2]]) + 
    sum(females2res[[4]][[2]])) * scalar
  
  # Abundance between Holtwood and Safe Harbor
  HolPop[(n + nYears * (k - 1))] <- (
    sum(males2res[[1]][[3]]) + 
    sum(females2res[[1]][[3]]) +
    sum(males2res[[2]][[3]]) + 
    sum(females2res[[2]][[3]]) +    
    sum(males2res[[3]][[3]]) + 
    sum(females2res[[3]][[3]]) +      
    sum(males2res[[4]][[3]]) + 
    sum(females2res[[4]][[3]])) * scalar
  
  # Abundance between Safe Harbor and York Haven  
  SafPop[(n + nYears * (k - 1))] <- (
    sum(males2res[[1]][[4]]) + 
    sum(females2res[[1]][[4]]) +
    sum(males2res[[2]][[4]]) + 
    sum(females2res[[2]][[4]]) +    
    sum(males2res[[3]][[4]]) + 
    sum(females2res[[3]][[4]]) +      
    sum(males2res[[4]][[4]]) + 
    sum(females2res[[4]][[4]])) * scalar
  
  # Abundance between York Haven and Sunbury  
  YorPop[(n + nYears * (k - 1))] <- (
    sum(males2res[[1]][[5]]) + 
    sum(females2res[[1]][[5]]) +
    sum(males2res[[2]][[5]]) + 
    sum(females2res[[2]][[5]]) +    
    sum(males2res[[3]][[5]]) + 
    sum(females2res[[3]][[5]]) +      
    sum(males2res[[4]][[5]]) + 
    sum(females2res[[4]][[5]])) * scalar
  
  # Abundance between Sunbury and PA/NY line
  SunPop[(n + nYears * (k - 1))] <-(
    sum(males2res[[3]][[6]]) + 
    sum(females2res[[3]][[6]]) +      
    sum(males2res[[4]][[6]]) + 
    sum(females2res[[4]][[6]])) * scalar
  
  # Abundance in Juniata River downstream of Warrior Ridge Dam
  JunPop[(n + nYears * (k - 1))] <- (
    sum(males2res[[1]][[6]]) +
    sum(females2res[[1]][[6]])) * scalar
  
  # Abundance in the West Branch down stream of Williamsport
  WesPop[(n + nYears * (k - 1))] <- (
    sum(males2res[[2]][[6]]) +
    sum(females2res[[2]][[6]])) * scalar
  
  # Abunance in West Branch between Williamsport and Lock Haven
  WilPop[(n + nYears * (k - 1))] <- (
    sum(males2res[[2]][[7]]) +
    sum(females2res[[2]][[7]])) * scalar
    
  # Abundance in West Branch upstream of Lock Haven
  LocPop[(n + nYears * (k - 1))] <- (
    sum(males2res[[2]][[8]]) +
    sum(females2res[[2]][[8]])) * scalar
    
  # Abundance upstream of Chase-Hibbard in West Branch
  ChaPop[(n + nYears * (k - 1))] <- (
    sum(males2res[[3]][[7]]) +
    sum(females2res[[3]][[7]])
    ) * scalar
  
  # Abundance in Chemung River between NY/PA lines
  ChePop[(n + nYears * (k - 1))] <- (
    sum(males2res[[3]][[8]]) +
    sum(females2res[[3]][[8]])
    ) * scalar
    
  # Abundance between PA/NY line and Rock Bottom (Binghamton)
  NorPop[(n + nYears * (k - 1))] <- (
    sum(males2res[[4]][[7]]) +
    sum(females2res[[4]][[7]])) * scalar
    
  # Abundance between Rock Bottom and Unadilla
  RocPop[(n + nYears * (k - 1))] <- (
    sum(males2res[[4]][[8]]) +
    sum(females2res[[4]][[8]])) * scalar
    
  # Abundance between Unadilla and Colliersville
  UnaPop[(n + nYears * (k - 1))] <- (
    sum(males2res[[4]][[9]]) +
    sum(females2res[[4]][[9]])) * scalar
    
  # Abundance upstream of Colliersville Dam
  ColPop[(n + nYears * (k - 1))] <- (
    sum(males2res[[4]][[10]]) +
    sum(females2res[[4]][[10]])) * scalar
    
  # Population size
  populationSize[(n + nYears * (k - 1))] <- 
    LowPop[(n + nYears * (k - 1))] +
    ConPop[(n + nYears * (k - 1))] +
    HolPop[(n + nYears * (k - 1))] +
    SafPop[(n + nYears * (k - 1))] +
    YorPop[(n + nYears * (k - 1))] +
    JunPop[(n + nYears * (k - 1))] +
    WesPop[(n + nYears * (k - 1))] +
    WilPop[(n + nYears * (k - 1))] +
    LocPop[(n + nYears * (k - 1))] +
    ChaPop[(n + nYears * (k - 1))] +
    SunPop[(n + nYears * (k - 1))] +  
    ChePop[(n + nYears * (k - 1))] +
    NorPop[(n + nYears * (k - 1))] +
    UnaPop[(n + nYears * (k - 1))] +
    RocPop[(n + nYears * (k - 1))] +
    ColPop[(n + nYears * (k - 1))]
  
  # Proportion of repeat spawners at each age, fillling pre-allocated vector
  pRepeats[[(n + nYears * (k - 1))]] <-  pRepeat

  # Age-structured repeat spawners, fillling pre-allocated vector
  spawners[[(n + nYears * (k - 1))]] <-  spawningPool

  # Reset the scalar based on population size
  list2env(setScalar(), envir = .shadia)

  # Scalar variable for computational gains
  scalarVar[[(n + nYears * (k - 1))]] <-  scalar

  # Store the inputs for sensitivity analysis
  # Passage assumptions
  ptime[(n + nYears * (k - 1))] <-  timely

  # Population demographics and survival rates
  S.downstream[(n + nYears * (k - 1))] <-  mean(downstreamS)
  S.marine[(n + nYears * (k - 1))] <-  mean(oceanSurvival)
  F.inRiver[(n + nYears * (k - 1))] <-  inRiverF
  F.commercial[(n + nYears * (k - 1))] <-  mean(commercialF)
  F.bycatch[(n + nYears * (k - 1))] <-  mean(bycatchF)
  popStart[(n + nYears * (k - 1))] <-  pop
  p.female[(n + nYears * (k - 1))] <-  sex_Ratio
  S.prespawnM[(n + nYears * (k - 1))] <-  pre_spawn_survival_males
  S.postspawnM[(n + nYears * (k - 1))] <-  post_spawn_survival_males
  S.prespawnF[(n + nYears * (k - 1))] <-  pre_spawn_survival_females
  S.postspawnF[(n + nYears * (k - 1))] <-  post_spawn_survival_females
  S.juvenile[(n + nYears * (k - 1))] <-  juvenile_survival

  # Environmental
  # Stochasticity
  t.stoch[(n + nYears * (k - 1))] <-  stoch
  # Regression relating temperatures in PNR and CTR
  t.RegrInt[(n + nYears * (k - 1))] <-  calMod[1, 1]
  t.RegrSlp[(n + nYears * (k - 1))] <-  calMod[2, 1]
  # Model parameters for sex-specific arrival timing
  b.ArrRegrInt[(n + nYears * (k - 1))] <-  res.B[1, 1]
  b.ArrRegrSlp[(n + nYears * (k - 1))] <-  res.B[2, 1]
  r.ArrRegrInt[(n + nYears * (k - 1))] <-  res.R[1, 1]
  r.ArrRegrSlp[(n + nYears * (k - 1))] <-  res.R[2, 1]

  # Individual traits
  # Entry dates
  b.Arr[(n + nYears * (k - 1))] <-  mean(c_entryDate[c_sex == 0])
  r.Arr[(n + nYears * (k - 1))] <-  mean(c_entryDate[c_sex == 1])
  # Spawning ATU
  ATUspawn1[(n + nYears * (k - 1))] <-  mean(c_spawnATU1)
  ATUspawn2[(n + nYears * (k - 1))] <-  mean(c_spawnATU2)
  # Spawning dates
  Dspawn1[(n + nYears * (k - 1))] <-  mean(c_initial)
  Dspawn2[(n + nYears * (k - 1))] <-  mean(c_end)
  # Length at age
  # Females
  linF[(n + nYears * (k - 1))] <-  r.mat[1]
  kF[(n + nYears * (k - 1))] <-  r.mat[2]
  t0F[(n + nYears * (k - 1))] <-  r.mat[3]
  # Males
  linM[(n + nYears * (k - 1))] <-  b.mat[1]
  kM[(n + nYears * (k - 1))] <-  b.mat[2]
  t0M[(n + nYears * (k - 1))] <-  b.mat[3]

  # Length-weight regression parameters
  # Female
  lwF.alpha[(n + nYears * (k - 1))] <-  c_femaleLWalpha
  lwF.beta[(n + nYears * (k - 1))] <-  c_femaleLWbeta
  # Male
  lwM.alpha[(n + nYears * (k - 1))] <-  c_maleLWalpha
  lwM.beta[(n + nYears * (k - 1))] <-  c_maleLWbeta

  # Length
  b.length[(n + nYears * (k - 1))] <-  mean(c_male_lf[c_male_lf!=0])
  r.length[(n + nYears * (k - 1))] <-  mean(c_female_lf[c_male_lf!=0])

  # Fecundity
  spawnInt[(n + nYears * (k - 1))] <-  mean(c_SI)
  batchSize[(n + nYears * (k - 1))] <-  mean(c_BF)
  resTime[(n + nYears * (k - 1))] <-  mean(c_RAF)

  # Movement parameters
  s.Optim[(n + nYears * (k - 1))] <-  mean(sOptim)
  d.Max[(n + nYears * (k - 1))] <-  mean(dMax)
  tortuosity[(n + nYears * (k - 1))] <-  mean(tort)
  motivation[(n + nYears * (k - 1))] <-  mot
  daily.move[(n + nYears * (k - 1))] <-  mean(dailyMove)
  #toc()

return(list(
  scalar = scalar,
  years = years,  
  populationSize = populationSize,
  LowPop = LowPop,
  ConPop = ConPop,
  HolPop = HolPop,
  SafPop = SafPop,
  YorPop = YorPop,
  JunPop = JunPop,
  WesPop = WesPop,
  WilPop = WilPop,
  LocPop = LocPop,
  ChaPop = ChaPop,
  SunPop = SunPop,
  ChePop = ChePop,
  NorPop = NorPop,
  UnaPop = UnaPop,
  RocPop = RocPop,
  ColPop = ColPop,
  ConUp = ConUp,
  HoltUp = HoltUp,
  SafeUp = SafeUp,
  YorkUp = YorkUp,
  SunUp = SunUp,
  WillUp = WillUp,
  ChasUp = ChasUp,
  LockUp = LockUp,
  RockUp = RockUp,
  CollUp = CollUp,
  ConD = ConD,
  HoltD = HoltD,
  SafeD = SafeD,
  YorkD = YorkD,
  SunD = SunD,
  WillD = WillD,
  LockD = LockD,
  ChasD = ChasD,
  RockD = RockD,
  CollD = CollD,
  pJuniataUp = pJuniataUp,
  pWestBranchUp = pWestBranchUp,
  pChemungUp = pChemungUp,
  pNorthBranchUp = pNorthBranchUp,
  indirectM = indirectM,
  latentM = latentM,
  juvReduction = juvReduction,
  fallback = fallback,
  pRepeats = pRepeats,
  spawners = spawners,
  scalarVar = scalarVar,
  ptime = ptime,
  S.downstream = S.downstream,
  S.marine = S.marine,
  F.inRiver = F.inRiver,
  F.commercial = F.commercial,
  F.bycatch = F.bycatch,
  popStart = popStart,
  p.female = p.female,
  S.prespawnM = S.prespawnM,
  S.postspawnM = S.postspawnM,
  S.prespawnF = S.prespawnF,
  S.postspawnF = S.postspawnF,
  S.juvenile = S.juvenile,
  t.stoch = t.stoch,
  t.RegrInt = t.RegrInt,
  t.RegrSlp = t.RegrSlp,
  b.ArrRegrInt = b.ArrRegrInt,
  b.ArrRegrSlp = b.ArrRegrSlp,
  r.ArrRegrInt = r.ArrRegrInt,
  r.ArrRegrSlp = r.ArrRegrSlp,
  b.Arr = b.Arr,
  r.Arr = r.Arr,
  ATUspawn1 = ATUspawn1,
  ATUspawn2 = ATUspawn2,
  Dspawn1 = Dspawn1,
  Dspawn2 = Dspawn2,
  linF = linF,
  kF = kF,
  t0F = t0F,
  linM = linM,
  kM = kM,
  t0M = t0M,
  lwF.alpha = lwF.alpha,
  lwF.beta = lwF.beta,
  lwM.alpha = lwM.alpha,
  lwM.beta = lwM.beta,
  b.length = b.length,
  r.length = r.length,
  spawnInt = spawnInt,
  batchSize = batchSize,
  resTime = resTime,
  s.Optim = s.Optim,
  d.Max = d.Max,
  tortuosity = tortuosity,
  motivation = motivation,
  daily.move = daily.move
  ))
}    
  
  
  
  
}