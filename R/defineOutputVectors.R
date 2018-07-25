#' @title Define simulation output vectors and pre-allocate memory
#' 
#' @description Internal function to pre-allocate output 
#' vectors to hold the results of simulation runs before
#' they are output to the global workspace. 
#' 
#' Not intended to be called directly, but included 
#' for model transparency.
#' 
#' @return A list of all stored simulation parameters (90 
#' total, some unused but retained for transparency
#' in previous management strategy evaluations and 
#' hydropower licensing negotiations).
#' 
#' @export
#' 
defineOutputVectors <- function(){
  
if(river=='penobscot'){  
  # if (useTictoc | useProgress) {
  #   print(paste('nRuns = ',nRuns))
  #   print(paste('nYears = ',nYears))
  # }
  
  # Define empty vectors to hold results for outer loop
    
  # Empty container to hold year
  years <- vector(mode = 'numeric', length = nYears * nRuns)
  
  # Empty container to hold Weldon scenario (depricated)
  scen <- vector(mode = 'numeric', length = nYears * nRuns)
  
  # Upstream passage effeciencies
  OrUp <- vector(mode = 'numeric', length = nYears * nRuns)
  StUp <- vector(mode = 'numeric', length = nYears * nRuns)
  GilmUp <- vector(mode = 'numeric', length = nYears * nRuns)
  MdUp <- vector(mode = 'numeric', length = nYears * nRuns)
  HdUp <- vector(mode = 'numeric', length = nYears * nRuns)
  WEnfUp <- vector(mode = 'numeric', length = nYears * nRuns)
  BMillUp <- vector(mode = 'numeric', length = nYears * nRuns)
  MooseUp <- vector(mode = 'numeric', length = nYears * nRuns)
  GuilfUp <- vector(mode = 'numeric', length = nYears * nRuns)
  MattUp <- vector(mode = 'numeric', length = nYears * nRuns)
  
  # Downstream passage efficiencies
  OrD <- vector(mode = 'numeric', length = nYears * nRuns)
  StD <- vector(mode = 'numeric', length = nYears * nRuns)
  GilmD <- vector(mode = 'numeric', length = nYears * nRuns)
  MdD <- vector(mode = 'numeric', length = nYears * nRuns)
  HdD <- vector(mode = 'numeric', length = nYears * nRuns)
  WEnfD <- vector(mode = 'numeric', length = nYears * nRuns)
  BMillD <- vector(mode = 'numeric', length = nYears * nRuns)
  MooseD <- vector(mode = 'numeric', length = nYears * nRuns)
  GuilfD <- vector(mode = 'numeric', length = nYears * nRuns)
  MattD <- vector(mode = 'numeric', length = nYears * nRuns)
  
  # Indirect mortality during downstream passage
  indirectM <- vector(mode = 'numeric', length = nYears * nRuns)
  
  # Latent estuary mortality during downstream passage
  latentM <- vector(mode = 'numeric', length = nYears * nRuns)
  
  # Juvenile survival reduction at each dam during downstream passage
  juvReduction <- vector(mode = 'numeric', length = nYears * nRuns)
  
  # Fall-back during upstream passage
  fallback <- vector(mode = 'numeric', length = nYears * nRuns)
  
  # Population abundance in each production unit
  # Population abundance below Milford Dam in main-stem
  LowerPop <- vector(mode = 'numeric', length = nYears * nRuns)
  # Population abundance between Orono and Stillwater dams
  OronoPop <- vector(mode = 'numeric', length = nYears * nRuns)
  # Population abundance between Stillwater Dam and Gilman Falls
  StillwaterPop <- vector(mode = 'numeric', length = nYears * nRuns)
  # Population abundance between Milford Dam and confluence
  MilfordPop <- vector(mode = 'numeric', length = nYears * nRuns)
  # Population abundance between West Enfield and Weldon dams
  EnfieldPop <- vector(mode = 'numeric', length = nYears * nRuns)
  # Population abundance above Weldon Dam
  WeldonPop <- vector(mode = 'numeric', length = nYears * nRuns)
  # Population abundance between Howland and Moosehead dams
  HowlandPop <- vector(mode = 'numeric', length = nYears * nRuns)
  # Population abundance between Moosehead and Browns Mill dams
  MoosePop <- vector(mode = 'numeric', length = nYears * nRuns)
  # Population abundance between Browns Mill and Guilford dams
  BrownsPop <- vector(mode = 'numeric', length = nYears * nRuns)
  # Population abundance above Guilford Dam
  GuilfordPop <- vector(mode = 'numeric', length = nYears * nRuns)
  
  # Age-structured spawning population
  spawners <- vector(mode = 'list', length = nYears * nRuns)
  
  # Proportion of repeat spawners in each age class
  pRepeats <- vector(mode = 'list', length = nYears * nRuns)
  
  # Catchment-wide population abundance
  populationSize <- vector(mode = 'numeric', length = nYears * nRuns)
  
  # Store the scale
  scalarVar <- vector(mode = 'numeric', length = nYears * nRuns)
  
  # Store the inputs for sensitivity analysis
  # Passage assumptions
  ptime <- vector(mode = 'numeric', length = nYears * nRuns)
  pStillUP <- vector(mode = 'numeric', length = nYears * nRuns)
  pStillD <- vector(mode = 'numeric', length = nYears * nRuns)
  pPiscUP <- vector(mode = 'numeric', length = nYears * nRuns)
  
  # Population demographics
  S.downstream <- vector(mode = 'numeric', length = nYears * nRuns)
  S.marine <- vector(mode = 'numeric', length = nYears * nRuns)
  F.inRiver <- vector(mode = 'numeric', length = nYears * nRuns)
  F.commercial <- vector(mode = 'numeric', length = nYears * nRuns)
  F.bycatch <- vector(mode = 'numeric', length = nYears * nRuns)
  popStart <- vector(mode = 'numeric', length = nYears * nRuns)
  p.female <- vector(mode = 'numeric', length = nYears * nRuns)
  p.female <- vector(mode = 'numeric', length = nYears * nRuns)
  S.prespawnM <- vector(mode = 'numeric', length = nYears * nRuns)
  S.postspawnM <- vector(mode = 'numeric', length = nYears * nRuns)
  S.prespawnF <- vector(mode = 'numeric', length = nYears * nRuns)
  S.postspawnF <- vector(mode = 'numeric', length = nYears * nRuns)
  S.juvenile <- vector(mode = 'numeric', length = nYears * nRuns)
  
  # Environmental
  # Stochasticity
  t.stoch <- vector(mode = 'numeric', length = nYears * nRuns)
  # Regression relating temperatures in PNR and CTR
  t.RegrInt <- vector(mode = 'numeric', length = nYears * nRuns)
  t.RegrSlp <- vector(mode = 'numeric', length = nYears * nRuns)
  # Model parameters for sex-specific arrival timing
  b.ArrRegrInt <- vector(mode = 'numeric', length = nYears * nRuns)
  b.ArrRegrSlp <- vector(mode = 'numeric', length = nYears * nRuns)
  r.ArrRegrInt <- vector(mode = 'numeric', length = nYears * nRuns)
  r.ArrRegrSlp <- vector(mode = 'numeric', length = nYears * nRuns)
  
  # Individual traits
  # Entry dates
  b.Arr <- vector(mode = 'numeric', length = nYears * nRuns)
  r.Arr <- vector(mode = 'numeric', length = nYears * nRuns)
  # Spawning ATU
  ATUspawn1 <- vector(mode = 'numeric', length = nYears * nRuns)
  ATUspawn2 <- vector(mode = 'numeric', length = nYears * nRuns)
  # Spawning dates
  Dspawn1 <- vector(mode = 'numeric', length = nYears * nRuns)
  Dspawn2 <- vector(mode = 'numeric', length = nYears * nRuns)
  # Length at age
  # Females
  linF <- vector(mode = 'numeric', length = nYears * nRuns)
  kF <- vector(mode = 'numeric', length = nYears * nRuns)
  t0F <- vector(mode = 'numeric', length = nYears * nRuns)
  # Males
  linM <- vector(mode = 'numeric', length = nYears * nRuns)
  kM <- vector(mode = 'numeric', length = nYears * nRuns)
  t0M <- vector(mode = 'numeric', length = nYears * nRuns)
  
  # Length-weight regression parameters
  # Female
  lwF.alpha <- vector(mode = 'numeric', length = nYears * nRuns)
  lwF.beta <- vector(mode = 'numeric', length = nYears * nRuns)
  # Male
  lwM.alpha <- vector(mode = 'numeric', length = nYears * nRuns)
  lwM.beta <- vector(mode = 'numeric', length = nYears * nRuns)
  
  # Lengths and mass
  b.length <- vector(mode = 'numeric', length = nYears * nRuns)
  r.length <- vector(mode = 'numeric', length = nYears * nRuns)
  
  # Fecundity
  spawnInt <- vector(mode = 'numeric', length = nYears * nRuns)
  batchSize <- vector(mode = 'numeric', length = nYears * nRuns)
  resTime <- vector(mode = 'numeric', length = nYears * nRuns)
  
  # Movement parameters
  s.Optim <- vector(mode = 'numeric', length = nYears * nRuns)
  d.Max <- vector(mode = 'numeric', length = nYears * nRuns)
  tortuosity <- vector(mode = 'numeric', length = nYears * nRuns)
  motivation <- vector(mode = 'numeric', length = nYears * nRuns)
  daily.move <- vector(mode = 'numeric', length = nYears * nRuns)

  return(
    list(
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
      spawners = spawners,
      pRepeats = pRepeats,
      populationSize = populationSize,
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
      p.female = p.female,
      S.prespawnM = S.prespawnM,
      S.postspawnM = S.postspawnM,
      S.prespawnF = S.prespawnF,
      S.postspawnF = S.postspawnF,
      S.juvenile = S.juvenile,
      t.RegrInt = t.RegrInt,
      t.RegrSlp = t.RegrSlp,
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
    )
  )
}
  
if(river=='merrimack'){  
  # if (useTictoc | useProgress) {
  #   print(paste('nRuns = ',nRuns))
  #   print(paste('nYears = ',nYears))
  # }
  
  # Define empty vectors to hold results for outer loop
    
  # Empty container to hold year
  years <- vector(mode = 'numeric', length = nYears * nRuns)
  
  # Empty container to hold Weldon scenario (depricated)
  scen <- vector(mode = 'numeric', length = nYears * nRuns)
  
  # Upstream passage efficiencies
  EssUp <- vector(mode = 'numeric', length = nYears * nRuns)
  PawUp <- vector(mode = 'numeric', length = nYears * nRuns)
  AmosUp <- vector(mode = 'numeric', length = nYears * nRuns)
  HookUp <- vector(mode = 'numeric', length = nYears * nRuns)
  
  # Downstream passage efficiencies
  EssD <- vector(mode = 'numeric', length = nYears * nRuns)
  PawD <- vector(mode = 'numeric', length = nYears * nRuns)
  AmosD <- vector(mode = 'numeric', length = nYears * nRuns)
  HookD <- vector(mode = 'numeric', length = nYears * nRuns)
      
  # Timing
  ptime <- vector(mode = 'numeric', length = nYears * nRuns)

  # Indirect mortality during downstream passage
  indirectM <- vector(mode = 'numeric', length = nYears * nRuns)
  
  # Latent estuary mortality during downstream passage
  latentM <- vector(mode = 'numeric', length = nYears * nRuns)
  
  # Juvenile survival reduction at each dam during downstream passage
  juvReduction <- vector(mode = 'numeric', length = nYears * nRuns)
  
  # Fall-back during upstream passage
  fallback <- vector(mode = 'numeric', length = nYears * nRuns)
  
  # Population abundance in each production unit
  popI <- vector(mode = 'numeric', length = nYears * nRuns)
  popII <- vector(mode = 'numeric', length = nYears * nRuns)
  popIII <- vector(mode = 'numeric', length = nYears * nRuns)
  popIV <- vector(mode = 'numeric', length = nYears * nRuns)
  popV <- vector(mode = 'numeric', length = nYears * nRuns)
  
  # Age-structured spawning population
  spawners <- vector(mode = 'list', length = nYears * nRuns)
  
  # Proportion of repeat spawners in each age class
  pRepeats <- vector(mode = 'list', length = nYears * nRuns)
  
  # Catchment-wide population abundance
  populationSize <- vector(mode = 'numeric', length = nYears * nRuns)
  
  # Store the scale
  scalarVar <- vector(mode = 'numeric', length = nYears * nRuns)
  
  # Population demographics
  S.downstream <- vector(mode = 'numeric', length = nYears * nRuns)
  S.marine <- vector(mode = 'numeric', length = nYears * nRuns)
  F.inRiver <- vector(mode = 'numeric', length = nYears * nRuns)
  F.commercial <- vector(mode = 'numeric', length = nYears * nRuns)
  F.bycatch <- vector(mode = 'numeric', length = nYears * nRuns)
  popStart <- vector(mode = 'numeric', length = nYears * nRuns)
  p.female <- vector(mode = 'numeric', length = nYears * nRuns)
  S.prespawnM <- vector(mode = 'numeric', length = nYears * nRuns)
  S.postspawnM <- vector(mode = 'numeric', length = nYears * nRuns)
  S.prespawnF <- vector(mode = 'numeric', length = nYears * nRuns)
  S.postspawnF <- vector(mode = 'numeric', length = nYears * nRuns)
  S.juvenile <- vector(mode = 'numeric', length = nYears * nRuns)
  
  # Environmental
  # Stochasticity
  t.stoch <- vector(mode = 'numeric', length = nYears * nRuns)
  # Regression relating temperatures in PNR and CTR
  t.RegrInt <- vector(mode = 'numeric', length = nYears * nRuns)
  t.RegrSlp <- vector(mode = 'numeric', length = nYears * nRuns)
  # Model parameters for sex-specific arrival timing
  b.ArrRegrInt <- vector(mode = 'numeric', length = nYears * nRuns)
  b.ArrRegrSlp <- vector(mode = 'numeric', length = nYears * nRuns)
  r.ArrRegrInt <- vector(mode = 'numeric', length = nYears * nRuns)
  r.ArrRegrSlp <- vector(mode = 'numeric', length = nYears * nRuns)
  
  # Individual traits
  # Entry dates
  b.Arr <- vector(mode = 'numeric', length = nYears * nRuns)
  r.Arr <- vector(mode = 'numeric', length = nYears * nRuns)
  # Spawning ATU
  ATUspawn1 <- vector(mode = 'numeric', length = nYears * nRuns)
  ATUspawn2 <- vector(mode = 'numeric', length = nYears * nRuns)
  # Spawning dates
  Dspawn1 <- vector(mode = 'numeric', length = nYears * nRuns)
  Dspawn2 <- vector(mode = 'numeric', length = nYears * nRuns)
  # Length at age
  # Females
  linF <- vector(mode = 'numeric', length = nYears * nRuns)
  kF <- vector(mode = 'numeric', length = nYears * nRuns)
  t0F <- vector(mode = 'numeric', length = nYears * nRuns)
  # Males
  linM <- vector(mode = 'numeric', length = nYears * nRuns)
  kM <- vector(mode = 'numeric', length = nYears * nRuns)
  t0M <- vector(mode = 'numeric', length = nYears * nRuns)
  
  # Length-weight regression parameters
  # Female
  lwF.alpha <- vector(mode = 'numeric', length = nYears * nRuns)
  lwF.beta <- vector(mode = 'numeric', length = nYears * nRuns)
  # Male
  lwM.alpha <- vector(mode = 'numeric', length = nYears * nRuns)
  lwM.beta <- vector(mode = 'numeric', length = nYears * nRuns)
  
  # Lengths and mass
  b.length <- vector(mode = 'numeric', length = nYears * nRuns)
  r.length <- vector(mode = 'numeric', length = nYears * nRuns)
  
  # Fecundity
  spawnInt <- vector(mode = 'numeric', length = nYears * nRuns)
  batchSize <- vector(mode = 'numeric', length = nYears * nRuns)
  resTime <- vector(mode = 'numeric', length = nYears * nRuns)
  
  # Movement parameters
  s.Optim <- vector(mode = 'numeric', length = nYears * nRuns)
  d.Max <- vector(mode = 'numeric', length = nYears * nRuns)
  tortuosity <- vector(mode = 'numeric', length = nYears * nRuns)
  motivation <- vector(mode = 'numeric', length = nYears * nRuns)
  daily.move <- vector(mode = 'numeric', length = nYears * nRuns)

  return(
    list(
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
      spawners = spawners,
      pRepeats = pRepeats,
      populationSize = populationSize,
      scalarVar = scalarVar,
      ptime = ptime,
      S.downstream = S.downstream,
      S.marine = S.marine,
      F.inRiver = F.inRiver,
      F.commercial = F.commercial,
      F.bycatch = F.bycatch,
      popStart = popStart,
      p.female = p.female,
      p.female = p.female,
      S.prespawnM = S.prespawnM,
      S.postspawnM = S.postspawnM,
      S.prespawnF = S.prespawnF,
      S.postspawnF = S.postspawnF,
      S.juvenile = S.juvenile,
      t.RegrInt = t.RegrInt,
      t.RegrSlp = t.RegrSlp,
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
    )
  )
}

if(river=='connecticut'){  
  # if (useTictoc | useProgress) {
  #   print(paste('nRuns = ',nRuns))
  #   print(paste('nYears = ',nYears))
  # }
  
  # Define empty vectors to hold results for outer loop
    
  # Empty container to hold year
  years <- vector(mode = 'numeric', length = nYears * nRuns)
  
  # Empty container to hold Weldon scenario (depricated)
  scen <- vector(mode = 'numeric', length = nYears * nRuns)
  
  # Upstream passage efficiencies
  HolUp <- vector(mode = 'numeric', length = nYears * nRuns)
  CabUp <- vector(mode = 'numeric', length = nYears * nRuns)
  SpillUp <- vector(mode = 'numeric', length = nYears * nRuns)
  GateUp <- vector(mode = 'numeric', length = nYears * nRuns)
  VernUp <- vector(mode = 'numeric', length = nYears * nRuns)

  # Downstream passage efficiencies
  HolD <- vector(mode = 'numeric', length = nYears * nRuns)
  CabD <- vector(mode = 'numeric', length = nYears * nRuns)
  SpillD <- vector(mode = 'numeric', length = nYears * nRuns)
  VernD <- vector(mode = 'numeric', length = nYears * nRuns)

  # Downstream dam passage performance standards juvenile
  HolDj = vector(mode='numeric', length = nYears*nRuns)
  CabDj = vector(mode='numeric', length = nYears*nRuns)
  SpillDj = vector(mode='numeric', length = nYears*nRuns)
  VernDj = vector(mode='numeric', length = nYears*nRuns)

  # Northfield mountain take
  NorthFieldV = vector(mode='numeric', length = nYears*nRuns)
  NorthFieldT = vector(mode='numeric', length = nYears*nRuns)
  NorthFieldVa = vector(mode='numeric', length = nYears*nRuns)
  NorthFieldTa = vector(mode='numeric', length = nYears*nRuns)
   
  # Timing
  ptime <- vector(mode = 'numeric', length = nYears * nRuns)

  # Indirect mortality during downstream passage
  indirectM <- vector(mode = 'numeric', length = nYears * nRuns)
  
  # Latent estuary mortality during downstream passage
  latentM <- vector(mode = 'numeric', length = nYears * nRuns)
  
  # Juvenile survival reduction at each dam during downstream passage
  juvReduction <- vector(mode = 'numeric', length = nYears * nRuns)
  
  # Fall-back during upstream passage
  fallback <- vector(mode = 'numeric', length = nYears * nRuns)
  
  # Population abundance in each production unit
  popI <- vector(mode = 'numeric', length = nYears * nRuns)
  popII <- vector(mode = 'numeric', length = nYears * nRuns)
  popIII <- vector(mode = 'numeric', length = nYears * nRuns)
  popIV <- vector(mode = 'numeric', length = nYears * nRuns)
  popV <- vector(mode = 'numeric', length = nYears * nRuns)

  # Age-structured spawning population
  spawners <- vector(mode = 'list', length = nYears * nRuns)
  
  # Proportion of repeat spawners in each age class
  pRepeats <- vector(mode = 'list', length = nYears * nRuns)
  
  # Catchment-wide population abundance
  populationSize <- vector(mode = 'numeric', length = nYears * nRuns)
  
  # Store the scale
  scalarVar <- vector(mode = 'numeric', length = nYears * nRuns)
  
  # Population demographics
  S.downstream <- vector(mode = 'numeric', length = nYears * nRuns)
  S.marine <- vector(mode = 'numeric', length = nYears * nRuns)
  F.inRiver <- vector(mode = 'numeric', length = nYears * nRuns)
  F.commercial <- vector(mode = 'numeric', length = nYears * nRuns)
  F.bycatch <- vector(mode = 'numeric', length = nYears * nRuns)
  popStart <- vector(mode = 'numeric', length = nYears * nRuns)
  p.female <- vector(mode = 'numeric', length = nYears * nRuns)
  S.prespawnM <- vector(mode = 'numeric', length = nYears * nRuns)
  S.postspawnM <- vector(mode = 'numeric', length = nYears * nRuns)
  S.prespawnF <- vector(mode = 'numeric', length = nYears * nRuns)
  S.postspawnF <- vector(mode = 'numeric', length = nYears * nRuns)
  S.juvenile <- vector(mode = 'numeric', length = nYears * nRuns)
  
  # Environmental
  # Stochasticity
  t.stoch <- vector(mode = 'numeric', length = nYears * nRuns)

  # Model parameters for sex-specific arrival timing
  b.ArrRegrInt <- vector(mode = 'numeric', length = nYears * nRuns)
  b.ArrRegrSlp <- vector(mode = 'numeric', length = nYears * nRuns)
  r.ArrRegrInt <- vector(mode = 'numeric', length = nYears * nRuns)
  r.ArrRegrSlp <- vector(mode = 'numeric', length = nYears * nRuns)
  
  # Individual traits
  # Entry dates
  b.Arr <- vector(mode = 'numeric', length = nYears * nRuns)
  r.Arr <- vector(mode = 'numeric', length = nYears * nRuns)
  # Spawning ATU
  ATUspawn1 <- vector(mode = 'numeric', length = nYears * nRuns)
  ATUspawn2 <- vector(mode = 'numeric', length = nYears * nRuns)
  # Spawning dates
  Dspawn1 <- vector(mode = 'numeric', length = nYears * nRuns)
  Dspawn2 <- vector(mode = 'numeric', length = nYears * nRuns)
  # Length at age
  # Females
  linF <- vector(mode = 'numeric', length = nYears * nRuns)
  kF <- vector(mode = 'numeric', length = nYears * nRuns)
  t0F <- vector(mode = 'numeric', length = nYears * nRuns)
  # Males
  linM <- vector(mode = 'numeric', length = nYears * nRuns)
  kM <- vector(mode = 'numeric', length = nYears * nRuns)
  t0M <- vector(mode = 'numeric', length = nYears * nRuns)
  
  # Length-weight regression parameters
  # Female
  lwF.alpha <- vector(mode = 'numeric', length = nYears * nRuns)
  lwF.beta <- vector(mode = 'numeric', length = nYears * nRuns)
  # Male
  lwM.alpha <- vector(mode = 'numeric', length = nYears * nRuns)
  lwM.beta <- vector(mode = 'numeric', length = nYears * nRuns)
  
  # Lengths and mass
  b.length <- vector(mode = 'numeric', length = nYears * nRuns)
  r.length <- vector(mode = 'numeric', length = nYears * nRuns)
  
  # Fecundity
  spawnInt <- vector(mode = 'numeric', length = nYears * nRuns)
  batchSize <- vector(mode = 'numeric', length = nYears * nRuns)
  resTime <- vector(mode = 'numeric', length = nYears * nRuns)
  
  # Movement parameters
  s.Optim <- vector(mode = 'numeric', length = nYears * nRuns)
  d.Max <- vector(mode = 'numeric', length = nYears * nRuns)
  tortuosity <- vector(mode = 'numeric', length = nYears * nRuns)
  motivation <- vector(mode = 'numeric', length = nYears * nRuns)
  daily.move <- vector(mode = 'numeric', length = nYears * nRuns)

  return(
    list(
      years = years,
      scen = scen,
      HolUp = HolUp,
      CabUp = CabUp,
      SpillUp = SpillUp,
      GateUp = GateUp,
      VernUp = VernUp,
      HolD = HolD,
      CabD = CabD,
      SpillD = SpillD,
      VernD = VernD,
      HolDj = HolDj,
      CabDj = CabDj,
      SpillDj = SpillDj,
      VernDj = VernDj,
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
      spawners = spawners,
      pRepeats = pRepeats,
      populationSize = populationSize,
      scalarVar = scalarVar,
      ptime = ptime,
      S.downstream = S.downstream,
      S.marine = S.marine,
      F.inRiver = F.inRiver,
      F.commercial = F.commercial,
      F.bycatch = F.bycatch,
      popStart = popStart,
      p.female = p.female,
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
    )
  )
}  
    
}