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
  # Population size
  populationSize[(n + nYears * (k - 1))] <-  sum(spawningPool)

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
    sum(males[[1]][[2]]) + 
      sum(males[[1]][[2]]) +
      sum(males[[2]][[1]]) + 
      sum(males[[2]][[2]]) +
      sum(males[[3]][[1]]) +
      sum(males[[4]][[1]]) +
      sum(females[[1]][[2]]) + 
      sum(females[[1]][[2]]) +
      sum(females[[2]][[1]]) + 
      sum(females[[2]][[2]]) +
      sum(females[[3]][[1]]) +
      sum(females[[4]][[1]])
  ) * scalar

  # Population between Orono and Stillwater dams, fillling pre-allocated vector
  OronoPop[(n + nYears * (k - 1))] <-  (
    sum(males[[3]][[2]]) +
      sum(males[[4]][[2]]) +
      sum(females[[3]][[2]]) +
      sum(females[[4]][[2]])
    ) * scalar

  # Population between Stillwater Dam and Gilman Falls, fillling pre-allocated
  StillwaterPop[(n + nYears * (k - 1))] <-  (
    sum(males[[3]][[3]]) +
      sum(males[[4]][[3]]) +
      sum(females[[3]][[3]]) +
      sum(females[[4]][[3]])
    ) * scalar

  # Population between Milford and West Enfield, fillling pre-allocated vector
  MilfordPop[(n + nYears * (k - 1))] <-  (
    sum(males[[1]][[3]]) +
      sum(males[[2]][[3]]) +
      sum(males[[3]][[4]]) +
      sum(males[[4]][[4]]) +
      sum(females[[1]][[3]]) +
      sum(females[[2]][[3]]) +
      sum(females[[3]][[4]]) +
      sum(females[[4]][[4]])
    ) * scalar

  # Population between West Enfield and Weldon, fillling pre-allocated vector
  EnfieldPop[(n + nYears * (k - 1))] <-  (
    sum(males[[2]][[4]]) +
      sum(males[[4]][[5]]) +
      sum(females[[2]][[4]]) +
      sum(females[[4]][[5]])
    ) * scalar

  # Population above Weldon, fillling pre-allocated vector
  WeldonPop[(n + nYears * (k - 1))] <-  (
    sum(males[[2]][[length(males[[2]])]]) +
      sum(males[[4]][[length(males[[4]])]]) +
      sum(females[[2]][[length(females[[2]])]]) +
      sum(females[[4]][[length(females[[4]])]])
    ) * scalar

  # Population between Howland and Dover dams, fillling pre-allocated vector
  HowlandPop[(n + nYears * (k - 1))] <-  (
    sum(males[[1]][[4]]) +
      sum(males[[3]][[5]]) +
      sum(females[[1]][[4]]) +
      sum(females[[3]][[5]])
    ) * scalar

  # Population between Moosehead and Browns Mill dams
  MoosePop[(n + nYears * (k - 1))] <-  (
    sum(males[[1]][[5]]) +
      sum(males[[3]][[6]]) +
      sum(females[[1]][[5]]) +
      sum(females[[3]][[6]])
    ) * scalar

  # Population between Browns Mill and Guilford dams, fillling pre-allocated
  BrownsPop[(n + nYears * (k - 1))] <-  (
    sum(males[[1]][[6]]) +
      sum(males[[3]][[7]]) +
      sum(females[[1]][[6]]) +
      sum(females[[3]][[7]])
    ) * scalar

  # Population above Guilford Dam, fillling pre-allocated vector
  GuilfordPop[(n + nYears * (k - 1))] <-  (
    sum(males[[1]][[7]]) +
      sum(males[[3]][[8]]) +
      sum(females[[1]][[7]]) +
      sum(females[[3]][[8]])
    ) * scalar

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
  b.length[(n + nYears * (k - 1))] <-  mean(c_male_lf)
  r.length[(n + nYears * (k - 1))] <-  mean(c_female_lf)

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
  # Population size
  populationSize[(n + nYears * (k - 1))] <-  sum(spawningPool)

  # Year, fillling pre-allocated vector with this year
  years[(n + nYears * (k - 1))] <-  n

  # Scenario for passage timing at Weldon
  scen[(n + nYears * (k - 1))] <-  scenario

  # Upstream passage efficiencies, fillling pre-allocated vectors
  EssUp[(n + nYears * (k - 1))] <- EssexUp 
  PawUp[(n + nYears * (k - 1))] <- PawtucketUp
  AmosUp[(n + nYears * (k - 1))] <- AmoskeagUp
  HookUp[(n + nYears * (k - 1))] <- HooksetUp

  # Downstream passage efficiencies, fillling pre-allocated vectors
  EssD[(n + nYears * (k - 1))] <-  EssexD
  PawD[(n + nYears * (k - 1))] <-  PawtucketD
  AmosD[(n + nYears * (k - 1))] <- AmoskeagD
  HookD[(n + nYears * (k - 1))] <- HooksetD

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
    sum(males[[1]][[1]]) + 
      sum(females[[1]][[1]])) * scalar

  # Population between Essex and Pawtucket dams
  popII[(n + nYears * (k - 1))] <-  (
    sum(males[[2]][[1]]) + 
      sum(females[[2]][[1]])) * scalar

  # Population between Pawtucket and Amoskeag dams
  popIII[(n + nYears * (k - 1))] <-  (
    sum(males[[3]][[1]]) + 
      sum(females[[3]][[1]])) * scalar

  # Population between Amoskeag and Hookset dams
  popIV[(n + nYears * (k - 1))] <-  (
    sum(males[[4]][[1]]) + 
      sum(females[[4]][[1]])) * scalar
  
  # Population upstream of Hookset Dam
  popV[(n + nYears * (k - 1))] <-  (
    sum(males[[5]][[1]]) + 
      sum(females[[5]][[1]])) * scalar

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
  b.length[(n + nYears * (k - 1))] <-  mean(c_male_lf)
  r.length[(n + nYears * (k - 1))] <-  mean(c_female_lf)

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
  
}