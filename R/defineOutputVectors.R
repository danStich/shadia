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
  ptime <- vector(mode = 'list', length = nYears * nRuns)
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
  RAF <- vector(mode = 'numeric', length = nYears * nRuns)
  
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
      RAF = RAF,
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
  PawBUp <- vector(mode = 'numeric', length = nYears * nRuns)
  PawUp <- vector(mode = 'numeric', length = nYears * nRuns)
  AmosUp <- vector(mode = 'numeric', length = nYears * nRuns)
  HookUp <- vector(mode = 'numeric', length = nYears * nRuns)
  
  # Downstream passage efficiencies
  EssD <- vector(mode = 'numeric', length = nYears * nRuns)
  PawBD <- vector(mode = 'numeric', length = nYears * nRuns)
  PawD <- vector(mode = 'numeric', length = nYears * nRuns)
  AmosD <- vector(mode = 'numeric', length = nYears * nRuns)
  HookD <- vector(mode = 'numeric', length = nYears * nRuns)
      
  # Probability of using bypass route at pawtucket
  pBypassUS <- vector(mode = 'numeric', length = nYears * nRuns)
  pBypassDS <- vector(mode = 'numeric', length = nYears * nRuns)
  
  # Timing
  ptime <- vector(mode = 'list', length = nYears * nRuns)

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
  RAF <- vector(mode = 'numeric', length = nYears * nRuns)
  
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
      PawBUp = PawBUp,      
      AmosUp = AmosUp,
      HookUp = HookUp,
      EssD = EssD,
      PawD = PawD,
      PawBD = PawBD,      
      AmosD = AmosD,
      HookD = HookD,
      pBypassUS = pBypassUS,
      pBypassDS = pBypassDS,
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
      RAF = RAF,
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
  climate_scen <- vector(mode = 'character', length = nYears * nRuns)
  
  # Probability of using the spillway passage route for
  # upstream migration
  pSpill <- vector(mode = 'numeric', length = nYears * nRuns)
  
  # Upstream passage efficiencies
  HolUp <- vector(mode = 'numeric', length = nYears * nRuns)
  CabUp <- vector(mode = 'numeric', length = nYears * nRuns)
  SpillUp <- vector(mode = 'numeric', length = nYears * nRuns)
  GateUp <- vector(mode = 'numeric', length = nYears * nRuns)
  VernUp <- vector(mode = 'numeric', length = nYears * nRuns)

  # Downstream passage efficiencies
  HolD <- vector(mode = 'numeric', length = nYears * nRuns)
  CabD <- vector(mode = 'numeric', length = nYears * nRuns)
  GateD <- vector(mode = 'numeric', length = nYears * nRuns)
  VernD <- vector(mode = 'numeric', length = nYears * nRuns)

  # Downstream dam passage performance standards juvenile
  HolDj = vector(mode='numeric', length = nYears*nRuns)
  CabDj = vector(mode='numeric', length = nYears*nRuns)
  GateDj = vector(mode='numeric', length = nYears*nRuns)
  VernDj = vector(mode='numeric', length = nYears*nRuns)

  # Northfield mountain take
  NorthFieldV = vector(mode='numeric', length = nYears*nRuns)
  NorthFieldT = vector(mode='numeric', length = nYears*nRuns)
  NorthFieldVa = vector(mode='numeric', length = nYears*nRuns)
  NorthFieldTa = vector(mode='numeric', length = nYears*nRuns)
   
  # Timing
  ptime <- vector(mode = 'list', length = nYears * nRuns)

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
  RAF <- vector(mode = 'numeric', length = nYears * nRuns)
  
  # Movement parameters
  s.Optim <- vector(mode = 'numeric', length = nYears * nRuns)
  d.Max <- vector(mode = 'numeric', length = nYears * nRuns)
  tortuosity <- vector(mode = 'numeric', length = nYears * nRuns)
  motivation <- vector(mode = 'numeric', length = nYears * nRuns)
  daily.move <- vector(mode = 'numeric', length = nYears * nRuns)

  return(
    list(
      years = years,
      climate_scen = climate_scen,
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
      HolDj = HolDj,
      CabDj = CabDj,
      GateDj = GateDj,
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
      RAF = RAF,
      s.Optim = s.Optim,
      d.Max = d.Max,
      tortuosity = tortuosity,
      motivation = motivation,
      daily.move = daily.move
    )
  )
}  

if(river=='susquehanna'){  
  # if (useTictoc | useProgress) {
  #   print(paste('nRuns = ',nRuns))
  #   print(paste('nYears = ',nYears))
  # }
  
  # Define empty vectors to hold results for outer loop
    
  # Empty container to hold year
  years <- vector(mode = 'numeric', length = nYears * nRuns)
  
  # Upstream passage effeciencies
  ConUp <- vector(mode = 'numeric', length = nYears * nRuns)
  HoltUp <- vector(mode = 'numeric', length = nYears * nRuns)
  SafeUp <- vector(mode = 'numeric', length = nYears * nRuns)
  YorkUp <- vector(mode = 'numeric', length = nYears * nRuns)
  SunUp <- vector(mode = 'numeric', length = nYears * nRuns)
  WillUp <- vector(mode = 'numeric', length = nYears * nRuns)
  ChasUp <- vector(mode = 'numeric', length = nYears * nRuns)
  LockUp <- vector(mode = 'numeric', length = nYears * nRuns)
  RockUp <- vector(mode = 'numeric', length = nYears * nRuns)
  CollUp <- vector(mode = 'numeric', length = nYears * nRuns)

  # Downstream passage efficiencies
  ConD <- vector(mode = 'numeric', length = nYears * nRuns)
  HoltD <- vector(mode = 'numeric', length = nYears * nRuns)
  SafeD <- vector(mode = 'numeric', length = nYears * nRuns)
  YorkD <- vector(mode = 'numeric', length = nYears * nRuns)
  SunD <- vector(mode = 'numeric', length = nYears * nRuns)
  WillD <- vector(mode = 'numeric', length = nYears * nRuns)
  LockD <- vector(mode = 'numeric', length = nYears * nRuns)
  ChasD <- vector(mode = 'numeric', length = nYears * nRuns)
  RockD <- vector(mode = 'numeric', length = nYears * nRuns)
  CollD <- vector(mode = 'numeric', length = nYears * nRuns)
  
  # Indirect mortality during downstream passage
  indirectM <- vector(mode = 'numeric', length = nYears * nRuns)
  
  # Latent estuary mortality during downstream passage
  latentM <- vector(mode = 'numeric', length = nYears * nRuns)
  
  # Juvenile survival reduction at each dam during downstream passage
  juvReduction <- vector(mode = 'numeric', length = nYears * nRuns)
  
  # Fall-back during upstream passage
  fallback <- vector(mode = 'numeric', length = nYears * nRuns)
  
  # Population abundance in each production unit
  LowPop <- vector(mode = 'numeric', length = nYears * nRuns)
  ConPop <- vector(mode = 'numeric', length = nYears * nRuns)
  HolPop <- vector(mode = 'numeric', length = nYears * nRuns)
  SafPop <- vector(mode = 'numeric', length = nYears * nRuns)
  YorPop <- vector(mode = 'numeric', length = nYears * nRuns)
  JunPop <- vector(mode = 'numeric', length = nYears * nRuns)
  WesPop <- vector(mode = 'numeric', length = nYears * nRuns)
  WilPop <- vector(mode = 'numeric', length = nYears * nRuns)
  LocPop <- vector(mode = 'numeric', length = nYears * nRuns)
  ChaPop <- vector(mode = 'numeric', length = nYears * nRuns)
  SunPop <- vector(mode = 'numeric', length = nYears * nRuns)  
  ChePop <- vector(mode = 'numeric', length = nYears * nRuns)
  NorPop <- vector(mode = 'numeric', length = nYears * nRuns)
  UnaPop <- vector(mode = 'numeric', length = nYears * nRuns)
  RocPop <- vector(mode = 'numeric', length = nYears * nRuns)
  ColPop <- vector(mode = 'numeric', length = nYears * nRuns)
  
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
  ptime <- vector(mode = 'list', length = nYears * nRuns)
  pJuniataUp <- vector(mode = 'numeric', length = nYears * nRuns)
  pWestBranchUp <- vector(mode = 'numeric', length = nYears * nRuns)
  pChemungUp <- vector(mode = 'numeric', length = nYears * nRuns)
  pNorthBranchUp <- vector(mode = 'numeric', length = nYears * nRuns)
  
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
  
  # Environmental stochasticity
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
  RAF <- vector(mode = 'numeric', length = nYears * nRuns)
  
  # Movement parameters
  s.Optim <- vector(mode = 'numeric', length = nYears * nRuns)
  d.Max <- vector(mode = 'numeric', length = nYears * nRuns)
  tortuosity <- vector(mode = 'numeric', length = nYears * nRuns)
  motivation <- vector(mode = 'numeric', length = nYears * nRuns)
  daily.move <- vector(mode = 'numeric', length = nYears * nRuns)

  return(
    list(
      years = years,
      ConUp = ConUp,
      HoltUp = HoltUp,
      SafeUp = SafeUp,
      YorkUp = YorkUp,
      SunUp = SunUp,
      WillUp = WillUp,
      LockUp = LockUp,
      ChasUp = ChasUp,
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
      indirectM = indirectM,
      latentM = latentM,
      juvReduction = juvReduction,
      fallback = fallback,
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
      spawners = spawners,
      pRepeats = pRepeats,
      populationSize = populationSize,
      scalarVar = scalarVar,
      ptime = ptime,
      pJuniataUp = pJuniataUp,
      pWestBranchUp = pWestBranchUp,
      pChemungUp = pChemungUp,
      pNorthBranchUp = pNorthBranchUp,
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
      RAF = RAF,
      s.Optim = s.Optim,
      d.Max = d.Max,
      tortuosity = tortuosity,
      motivation = motivation,
      daily.move = daily.move
    )
  )
}
  
if(river=='saco'){  
  # if (useTictoc | useProgress) {
  #   print(paste('nRuns = ',nRuns))
  #   print(paste('nYears = ',nYears))
  # }
  
  # Define empty vectors to hold results for outer loop
    
  # Empty container to hold year
  years <- vector(mode = 'numeric', length = nYears * nRuns)
  
  # Upstream passage efficiencies - first three letters each dam
  catUp <- vector(mode = 'numeric', length = nYears * nRuns)
  sprUp <- vector(mode = 'numeric', length = nYears * nRuns)
  skeUp <- vector(mode = 'numeric', length = nYears * nRuns)
  barUp <- vector(mode = 'numeric', length = nYears * nRuns)
  buxUp <- vector(mode = 'numeric', length = nYears * nRuns)
  bonUp <- vector(mode = 'numeric', length = nYears * nRuns)
  
  # Downstream passage efficiencies - first three letters each dam
  catD <- vector(mode = 'numeric', length = nYears * nRuns)
  sprD <- vector(mode = 'numeric', length = nYears * nRuns)
  skeD <- vector(mode = 'numeric', length = nYears * nRuns)
  barD <- vector(mode = 'numeric', length = nYears * nRuns)
  buxD <- vector(mode = 'numeric', length = nYears * nRuns)
  bonD <- vector(mode = 'numeric', length = nYears * nRuns)
  
  # Timing
  ptime <- vector(mode = 'list', length = nYears * nRuns)

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
  popVI <- vector(mode = 'numeric', length = nYears * nRuns)
  popVII <- vector(mode = 'numeric', length = nYears * nRuns)
  
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
  RAF <- vector(mode = 'numeric', length = nYears * nRuns)
  
  # Movement parameters
  s.Optim <- vector(mode = 'numeric', length = nYears * nRuns)
  d.Max <- vector(mode = 'numeric', length = nYears * nRuns)
  tortuosity <- vector(mode = 'numeric', length = nYears * nRuns)
  motivation <- vector(mode = 'numeric', length = nYears * nRuns)
  daily.move <- vector(mode = 'numeric', length = nYears * nRuns)

  return(
    list(
      years = years,
      catUp = catUp,
      sprUp = sprUp,
      skeUp = skeUp,
      barUp = barUp,
      buxUp = buxUp,
      bonUp = bonUp,
      catD = catD,
      sprD = sprD,
      skeD = skeD,
      barD = barD,
      buxD = buxD,
      bonD = bonD,   
      indirectM = indirectM,
      latentM = latentM,
      juvReduction = juvReduction,
      fallback = fallback,
      popI = popI,
      popII = popII,
      popIII = popIII,
      popIV = popIV,
      popV = popV,
      popVI = popVI,
      popVII = popVII,
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
      RAF = RAF,
      s.Optim = s.Optim,
      d.Max = d.Max,
      tortuosity = tortuosity,
      motivation = motivation,
      daily.move = daily.move
    )
  )
}  

if(river=='kennebec'){  
  # if (useTictoc | useProgress) {
  #   print(paste('nRuns = ',nRuns))
  #   print(paste('nYears = ',nYears))
  # }
  
  # Define empty vectors to hold results for outer loop
    
  # Empty container to hold year
  years <- vector(mode = 'numeric', length = nYears * nRuns)
  
  # Probability of using Sebasticook
  sebasticook <- vector(mode = 'numeric', length = nYears * nRuns)
  
  # Upstream passage efficiencies - first three letters each dam
  locUp <- vector(mode = 'numeric', length = nYears * nRuns)
  hydUp <- vector(mode = 'numeric', length = nYears * nRuns)
  shaUp <- vector(mode = 'numeric', length = nYears * nRuns)
  wesUp <- vector(mode = 'numeric', length = nYears * nRuns)
  benUp <- vector(mode = 'numeric', length = nYears * nRuns)
  burUp <- vector(mode = 'numeric', length = nYears * nRuns)
  
  # Downstream passage efficiencies - first three letters each dam
  locD <- vector(mode = 'numeric', length = nYears * nRuns)
  hydD <- vector(mode = 'numeric', length = nYears * nRuns)
  shaD <- vector(mode = 'numeric', length = nYears * nRuns)
  wesD <- vector(mode = 'numeric', length = nYears * nRuns)
  benD <- vector(mode = 'numeric', length = nYears * nRuns)
  burD <- vector(mode = 'numeric', length = nYears * nRuns)
  
  # Timing
  ptime <- vector(mode = 'list', length = nYears * nRuns)

  # Indirect mortality during downstream passage
  indirectM <- vector(mode = 'numeric', length = nYears * nRuns)
  
  # Latent estuary mortality during downstream passage
  latentM <- vector(mode = 'numeric', length = nYears * nRuns)
  
  # Juvenile survival reduction at each dam during downstream passage
  juvReduction <- vector(mode = 'numeric', length = nYears * nRuns)
  
  # Fall-back during upstream passage
  fallback <- vector(mode = 'numeric', length = nYears * nRuns)
  
  # Population abundance in each production unit
  pop1a <- vector(mode = 'numeric', length = nYears * nRuns)
  pop2a <- vector(mode = 'numeric', length = nYears * nRuns)
  pop3a <- vector(mode = 'numeric', length = nYears * nRuns)
  pop4a <- vector(mode = 'numeric', length = nYears * nRuns)
  pop5a <- vector(mode = 'numeric', length = nYears * nRuns)
  pop6a <- vector(mode = 'numeric', length = nYears * nRuns)
  pop1b <- vector(mode = 'numeric', length = nYears * nRuns)
  pop2b <- vector(mode = 'numeric', length = nYears * nRuns)
  pop3b <- vector(mode = 'numeric', length = nYears * nRuns)
  
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
  RAF <- vector(mode = 'numeric', length = nYears * nRuns)
  
  # Movement parameters
  s.Optim <- vector(mode = 'numeric', length = nYears * nRuns)
  d.Max <- vector(mode = 'numeric', length = nYears * nRuns)
  tortuosity <- vector(mode = 'numeric', length = nYears * nRuns)
  motivation <- vector(mode = 'numeric', length = nYears * nRuns)
  daily.move <- vector(mode = 'numeric', length = nYears * nRuns)

  return(
    list(
      years = years,
      sebasticook = sebasticook,
      locUp	= locUp,
      hydUp	= hydUp,
      shaUp	= shaUp,
      wesUp	= wesUp,
      benUp	= benUp,
      burUp	= burUp,
      locD	= locD,
      hydD	= hydD,
      shaD	= shaD,
      wesD	= wesD,
      benD	= benD,
      burD	= burD,
      indirectM = indirectM,
      latentM = latentM,
      juvReduction = juvReduction,
      fallback = fallback,
      pop1a	=	pop1a,
      pop2a =	pop2a,
      pop3a =	pop3a,
      pop4a =	pop4a,
      pop5a =	pop5a,
      pop6a =	pop6a,
      pop1b =	pop1b,
      pop2b =	pop2b,
      pop3b =	pop3b,
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
      RAF = RAF,
      s.Optim = s.Optim,
      d.Max = d.Max,
      tortuosity = tortuosity,
      motivation = motivation,
      daily.move = daily.move
    )
  )
}      
}