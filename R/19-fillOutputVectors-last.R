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
fillOutputVectors_last <- function() {

  # Year, filling pre-allocated vector with this year
  years <- n

  # Upstream passage timing
  ptime[[k]] <- timely

  # Climate scenario
  climate_scen <- climate
  
  # Indirect mortality, filling pre-allocated vector
  indirectM <- indirect

  # Latent estuary mortality, filling pre-allocated vector
  latentM <- latent

  # Proportion of repeat spawners at each age, filling pre-allocated vector
  pRepeats[[k]] <- pRepeat

  # Age-structured repeat spawners, filling pre-allocated vector
  spawners[[k]] <- .shadia$spawningPool

  # # Reset the scalar based on population size
  # .shadia$scalar <- setScalar(.shadia$spawningPool)

  # Scalar variable for computational gains
  scalarVar <- .shadia$scalar

  # Population demographics and survival rates
  S.downstream <- mean(downstreamS)
  S.marine <- marineS[1]
  # Extract user-defined S.marine if provided
  if(exists("marine_s")){
    if(!is.null(marine_s)){
      S.marine <- marine_s
    }
  }  
  
  F.inRiver <- inRiverF
  F.commercial <- mean(commercialF)
  F.bycatch <- mean(bycatchF)
  popStart <- n_adults
  p.female <- sexRatio
  S.prespawnM <- pre_spawn_survival_males
  S.postspawnM <- post_spawn_survival_males
  S.prespawnF <- pre_spawn_survival_females
  S.postspawnF <- post_spawn_survival_females
  S.juvenile <- juvenile_survival

  # Individual traits
  # Entry dates
  b.Arr <- mean(c_entryDate[c_sex == 0])
  r.Arr <- mean(c_entryDate[c_sex == 1])
  # Spawning ATU
  ATUspawn1 <- mean(c_spawnATU1)
  ATUspawn2 <- mean(c_spawnATU2)
  # Spawning dates
  Dspawn1 <- mean(c_initial)
  Dspawn2 <- mean(c_end)
  # Length at age
  # Females
  linF <- r.mat[1]
  kF <- r.mat[2]
  t0F <- r.mat[3]
  # Males
  linM <- b.mat[1]
  kM <- b.mat[2]
  t0M <- b.mat[3]

  # Length
  b.length <- mean(c_male_lf[c_male_lf != 0])
  r.length <- mean(c_female_lf[c_female_lf != 0])

  # Fecundity
  spawnInt <- mean(c_SI)
  batchSize <- mean(c_BF)
  RAF <- mean(c_RAF)

  # Movement parameters
  s.Optim <- mean(sOptim)
  d.Max <- mean(dMax)
  tortuosity <- mean(tort)
  motivation <- mot
  daily.move <- mean(dailyMove)


  if (river == "penobscot") {
    # Population below Milford, filling pre-allocated vector
    LowerPop <- (
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

    # Population between Orono and Stillwater dams, filling pre-allocated vector
    OronoPop <- (
      sum(males2res[[3]][[2]]) +
        sum(males2res[[4]][[2]]) +
        sum(females2res[[3]][[2]]) +
        sum(females2res[[4]][[2]])
    ) * scalar

    # Population between Stillwater Dam and Gilman Falls, filling pre-allocated
    StillwaterPop <- (
      sum(males2res[[3]][[3]]) +
        sum(males2res[[4]][[3]]) +
        sum(females2res[[3]][[3]]) +
        sum(females2res[[4]][[3]])
    ) * scalar

    # Population between Milford and West Enfield, filling pre-allocated vector
    MilfordPop <- (
      sum(males2res[[1]][[3]]) +
        sum(males2res[[2]][[3]]) +
        sum(males2res[[3]][[4]]) +
        sum(males2res[[4]][[4]]) +
        sum(females2res[[1]][[3]]) +
        sum(females2res[[2]][[3]]) +
        sum(females2res[[3]][[4]]) +
        sum(females2res[[4]][[4]])
    ) * scalar + OronoPop +
      StillwaterPop

    # Population between West Enfield and Weldon, filling pre-allocated vector
    EnfieldPop <- (
      sum(males2res[[2]][[4]]) +
        sum(males2res[[4]][[5]]) +
        sum(females2res[[2]][[4]]) +
        sum(females2res[[4]][[5]])
    ) * scalar

    # Population above Weldon, filling pre-allocated vector
    WeldonPop <- (
      sum(males2res[[2]][[length(males2res[[2]])]]) +
        sum(males2res[[4]][[length(males2res[[4]])]]) +
        sum(females2res[[2]][[length(females2res[[2]])]]) +
        sum(females2res[[4]][[length(females2res[[4]])]])
    ) * scalar

    # Population between Howland and Dover dams, filling pre-allocated vector
    HowlandPop <- (
      sum(males2res[[1]][[4]]) +
        sum(males2res[[3]][[5]]) +
        sum(females2res[[1]][[4]]) +
        sum(females2res[[3]][[5]])
    ) * scalar

    # Population between Moosehead and Browns Mill dams
    MoosePop <- (
      sum(males2res[[1]][[5]]) +
        sum(males2res[[3]][[6]]) +
        sum(females2res[[1]][[5]]) +
        sum(females2res[[3]][[6]])
    ) * scalar

    # Population between Browns Mill and Guilford dams, filling pre-allocated
    BrownsPop <- (
      sum(males2res[[1]][[6]]) +
        sum(males2res[[3]][[7]]) +
        sum(females2res[[1]][[6]]) +
        sum(females2res[[3]][[7]])
    ) * scalar

    # Population above Guilford Dam, filling pre-allocated vector
    GuilfordPop <- (
      sum(males2res[[1]][[7]]) +
        sum(males2res[[3]][[8]]) +
        sum(females2res[[1]][[7]]) +
        sum(females2res[[3]][[8]])
    ) * scalar


    # Population size
    populationSize <-
      LowerPop +
      MilfordPop +
      EnfieldPop +
      WeldonPop +
      HowlandPop +
      MoosePop +
      BrownsPop +
      GuilfordPop


    # Store the inputs for sensitivity analysis
    # Passage assumptions
    pStillUP <- pStillwaterUp
    pStillD <- pStillwaterD
    pPiscUP <- pPiscUp

    return(list(
      scalar = scalar,
      populationSize = populationSize,
      years = years,
      climate_scen = climate_scen,
      indirectM = indirectM,
      latentM = latentM,
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
    ))
  }

  if (river == "merrimack") {
    # Probability of using bypass route at pawtucket
    pBypassUS <- pBypassUp
    pBypassDS <- pBypassD

    # Population below Essex, filling pre-allocated vector
    popI <- (
      sum(males2res[[1]][[1]]) +
        sum(females2res[[1]][[1]])) * scalar

    # Population between Essex and Pawtucket dams
    popII <- (
      sum(males2res[[1]][[2]]) +
        sum(females2res[[1]][[2]])) * scalar

    # Population between Pawtucket and Amoskeag dams
    popIII <- (
      sum(males2res[[1]][[3]]) +
        sum(females2res[[1]][[3]])) * scalar

    # Population between Amoskeag and Hookset dams
    popIV <- (
      sum(males2res[[1]][[4]]) +
        sum(females2res[[1]][[4]])) * scalar

    # Population upstream of Hookset Dam
    popV <- (
      sum(males2res[[1]][[5]]) +
        sum(females2res[[1]][[5]])) * scalar

    # Population size
    populationSize <-
      popI +
      popII +
      popIII +
      popIV +
      popV

    return(list(
      scalar = scalar,
      populationSize = populationSize,
      years = years,
      indirectM = indirectM,
      latentM = latentM,
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
    ))
  }

  if (river == "connecticut") {
    # Probability of using spillway for migration through TF
    pSpill <- pSpillway

    # Northfield Mountain take
    NorthFieldV <- northfield$vernonJ
    NorthFieldT <- northfield$turnersJ
    NorthFieldVa <- northfield$vernonA
    NorthFieldTa <- northfield$turnersA

    # Population below Essex, filling pre-allocated vector
    popI <- (
      sum(males2res[[1]][[1]]) +
        sum(females2res[[1]][[1]]) +
        sum(males2res[[2]][[1]]) +
        sum(females2res[[2]][[1]])) * scalar
    # Population between Essex and Pawtucket dams
    popII <- (
      sum(males2res[[1]][[2]]) +
        sum(females2res[[1]][[2]]) +
        sum(males2res[[2]][[2]]) +
        sum(females2res[[2]][[2]])) * scalar
    # Population between Pawtucket and Amoskeag dams
    popIII <- (
      sum(males2res[[1]][[3]]) +
        sum(females2res[[1]][[3]]) +
        sum(males2res[[2]][[3]]) +
        sum(females2res[[2]][[3]])) * scalar
    # Population between Amoskeag and Hookset dams
    popIV <- (
      sum(males2res[[1]][[4]]) +
        sum(females2res[[1]][[4]]) +
        sum(males2res[[2]][[4]]) +
        sum(females2res[[2]][[4]])) * scalar
    # Population upstream of Hookset Dam
    popV <- (
      sum(males2res[[1]][[5]]) +
        sum(females2res[[1]][[5]]) +
        sum(males2res[[2]][[5]]) +
        sum(females2res[[2]][[5]])) * scalar
    # Population size
    populationSize <-
      popI +
      popII +
      popIII +
      popIV +
      popV

    return(list(
      scalar = scalar,
      populationSize = populationSize,
      years = years,
      climate_scen = climate_scen,
      pSpill = pSpill,
      NorthFieldV = NorthFieldV,
      NorthFieldT = NorthFieldT,
      NorthFieldVa = NorthFieldVa,
      NorthFieldTa = NorthFieldTa,
      indirectM = indirectM,
      latentM = latentM,
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
    ))
  }

  if (river == "susquehanna") {
    # Store output in pre-allocated vectors
    # Path choice probabilities
    pJuniataUp <- p_JuniataUp
    pWestBranchUp <- p_WestBranchUp
    pChemungUp <- p_ChemungUp
    pNorthBranchUp <- p_NorthBranchUp

    # Population abundance in each PU
    # Abundance downstream of conowingo
    LowPop <- (
      sum(males2res[[1]][[1]]) +
        sum(females2res[[1]][[1]]) +
        sum(males2res[[2]][[1]]) +
        sum(females2res[[2]][[1]]) +
        sum(males2res[[3]][[1]]) +
        sum(females2res[[3]][[1]]) +
        sum(males2res[[4]][[1]]) +
        sum(females2res[[4]][[1]])) * scalar

    # Abundance between Conowingo and Holtwood
    ConPop <- (
      sum(males2res[[1]][[2]]) +
        sum(females2res[[1]][[2]]) +
        sum(males2res[[2]][[2]]) +
        sum(females2res[[2]][[2]]) +
        sum(males2res[[3]][[2]]) +
        sum(females2res[[3]][[2]]) +
        sum(males2res[[4]][[2]]) +
        sum(females2res[[4]][[2]])) * scalar

    # Abundance between Holtwood and Safe Harbor
    HolPop <- (
      sum(males2res[[1]][[3]]) +
        sum(females2res[[1]][[3]]) +
        sum(males2res[[2]][[3]]) +
        sum(females2res[[2]][[3]]) +
        sum(males2res[[3]][[3]]) +
        sum(females2res[[3]][[3]]) +
        sum(males2res[[4]][[3]]) +
        sum(females2res[[4]][[3]])) * scalar

    # Abundance between Safe Harbor and York Haven
    SafPop <- (
      sum(males2res[[1]][[4]]) +
        sum(females2res[[1]][[4]]) +
        sum(males2res[[2]][[4]]) +
        sum(females2res[[2]][[4]]) +
        sum(males2res[[3]][[4]]) +
        sum(females2res[[3]][[4]]) +
        sum(males2res[[4]][[4]]) +
        sum(females2res[[4]][[4]])) * scalar

    # Abundance between York Haven and Sunbury
    YorPop <- (
      sum(males2res[[1]][[5]]) +
        sum(females2res[[1]][[5]]) +
        sum(males2res[[2]][[5]]) +
        sum(females2res[[2]][[5]]) +
        sum(males2res[[3]][[5]]) +
        sum(females2res[[3]][[5]]) +
        sum(males2res[[4]][[5]]) +
        sum(females2res[[4]][[5]])) * scalar

    # Abundance between Sunbury and PA/NY line
    SunPop <- (
      sum(males2res[[3]][[6]]) +
        sum(females2res[[3]][[6]]) +
        sum(males2res[[4]][[6]]) +
        sum(females2res[[4]][[6]])) * scalar

    # Abundance in Juniata River downstream of Warrior Ridge Dam
    JunPop <- (
      sum(males2res[[1]][[6]]) +
        sum(females2res[[1]][[6]])) * scalar

    # Abundance in the West Branch down stream of Williamsport
    WesPop <- (
      sum(males2res[[2]][[6]]) +
        sum(females2res[[2]][[6]])) * scalar

    # Abunance in West Branch between Williamsport and Lock Haven
    WilPop <- (
      sum(males2res[[2]][[7]]) +
        sum(females2res[[2]][[7]])) * scalar

    # Abundance in West Branch upstream of Lock Haven
    LocPop <- (
      sum(males2res[[2]][[8]]) +
        sum(females2res[[2]][[8]])) * scalar

    # Abundance upstream of Chase-Hibbard in West Branch
    ChaPop <- (
      sum(males2res[[3]][[7]]) +
        sum(females2res[[3]][[7]])
    ) * scalar

    # Abundance in Chemung River between NY/PA lines
    ChePop <- (
      sum(males2res[[3]][[8]]) +
        sum(females2res[[3]][[8]])
    ) * scalar

    # Abundance between PA/NY line and Rock Bottom (Binghamton)
    NorPop <- (
      sum(males2res[[4]][[7]]) +
        sum(females2res[[4]][[7]])) * scalar

    # Abundance between Rock Bottom and Unadilla
    RocPop <- (
      sum(males2res[[4]][[8]]) +
        sum(females2res[[4]][[8]])) * scalar

    # Abundance between Unadilla and Colliersville
    UnaPop <- (
      sum(males2res[[4]][[9]]) +
        sum(females2res[[4]][[9]])) * scalar

    # Abundance upstream of Colliersville Dam
    ColPop <- (
      sum(males2res[[4]][[10]]) +
        sum(females2res[[4]][[10]])) * scalar

    # Population size
    populationSize <-
      LowPop +
      ConPop +
      HolPop +
      SafPop +
      YorPop +
      JunPop +
      WesPop +
      WilPop +
      LocPop +
      ChaPop +
      SunPop +
      ChePop +
      NorPop +
      UnaPop +
      RocPop +
      ColPop

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
      pJuniataUp = pJuniataUp,
      pWestBranchUp = pWestBranchUp,
      pChemungUp = pChemungUp,
      pNorthBranchUp = pNorthBranchUp,
      indirectM = indirectM,
      latentM = latentM,
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
    ))
  }

  if (river == "saco") {
    # Store output in pre-allocated vectors
    # Population below the Cataract Project,
    # filling pre-allocated vector
    popI <- (
      sum(males2res[[1]][[1]]) +
        sum(females2res[[1]][[1]])) * scalar

    # Population between Cataract and Springs and Bradbury
    popII <- (
      sum(males2res[[1]][[2]]) +
        sum(females2res[[1]][[2]])) * scalar

    # Population between Bradbury and Skelton Dam
    popIII <- (
      sum(males2res[[1]][[3]]) +
        sum(females2res[[1]][[3]])) * scalar

    # Population between Skelton and Bar Mills Dam
    popIV <- (
      sum(males2res[[1]][[4]]) +
        sum(females2res[[1]][[4]])) * scalar

    # Population between Bar Mills Dam and West Buxton
    popV <- (
      sum(males2res[[1]][[5]]) +
        sum(females2res[[1]][[5]])) * scalar

    # Population between West Buxton and Bonny Eagle
    popVI <- (
      sum(males2res[[1]][[6]]) +
        sum(females2res[[1]][[6]])) * scalar

    # Population between Bonny Eagle and Hiram Falls
    popVII <- (
      sum(males2res[[1]][[7]]) +
        sum(females2res[[1]][[7]])) * scalar

    # Population size
    populationSize <-
      popI +
      popII +
      popIII +
      popIV +
      popV +
      popVI +
      popVII

    return(list(
      scalar = scalar,
      populationSize = populationSize,
      years = years,
      indirectM = indirectM,
      latentM = latentM,
      popI = popI,
      popII = popII,
      popIII = popIII,
      popIV = popIV,
      popV = popV,
      popVI = popVI,
      popVII = popVII,
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
    ))
  }

  if (river == "kennebec") {
    # Store output in pre-allocated vectors

    # Probability of using sebasticook river
    sebasticook <- p_sebasticook

    # Population downstream of lockwood dam
    pop1a <- (
      sum(males2res[[1]][[1]]) +
        sum(females2res[[1]][[1]]) +
        sum(males2res[[2]][[1]]) +
        sum(females2res[[2]][[1]])) * scalar

    # Population between lockwood and hydro-kennebec
    pop2a <- (
      sum(males2res[[1]][[2]]) +
        sum(females2res[[1]][[2]])) * scalar

    # Population between hydro-kennebec and shawmut
    pop3a <- (
      sum(males2res[[1]][[3]]) +
        sum(females2res[[1]][[3]])) * scalar

    # Population between shawmut and weston,
    # including Sandy River
    pop4a <- (
      sum(males2res[[1]][[4]]) +
        sum(females2res[[1]][[4]])) * scalar

    # Population between weston and abenaki,
    # including Sandy River
    pop5a <- (
      sum(males2res[[1]][[5]]) +
        sum(females2res[[1]][[5]])) * scalar

    # Population between benton falls and burnham dam
    pop1b <- (
      sum(males2res[[2]][[2]]) +
        sum(females2res[[2]][[2]])) * scalar

    # Population upstream of burnham dam
    pop2b <- (
      sum(males2res[[2]][[3]]) +
        sum(females2res[[2]][[3]])) * scalar

    # Population size
    populationSize <-
      pop1a +
      pop2a +
      pop3a +
      pop4a +
      pop5a +
      pop1b +
      pop2b

    return(list(
      scalar = scalar,
      populationSize = populationSize,
      years = years,
      sebasticook = sebasticook,
      pop1a = pop1a,
      pop2a = pop2a,
      pop3a = pop3a,
      pop4a = pop4a,
      pop5a = pop5a,
      pop1b = pop1b,
      pop2b = pop2b,
      indirectM = indirectM,
      latentM = latentM,
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
    ))
  }

  if (river == "hudson") {
    # Population downstream of Troy Federal Dam
    pop01a <- (
      sum(males2res[[1]][[1]]) +
        sum(females2res[[1]][[1]]) +
        sum(males2res[[2]][[1]]) +
        sum(females2res[[2]][[1]])) * scalar

    # Population sizes for Upper Hudson (Champlain Canal)
    # Federal - C1
    pop02a <- (
      sum(males2res[[1]][[2]]) +
        sum(females2res[[1]][[2]])) * scalar

    # C1-C2
    pop03a <- (
      sum(males2res[[1]][[3]]) +
        sum(females2res[[1]][[3]])) * scalar

    # C2-C3
    pop04a <- (
      sum(males2res[[1]][[4]]) +
        sum(females2res[[1]][[4]])) * scalar

    # C3-C4
    pop05a <- (
      sum(males2res[[1]][[5]]) +
        sum(females2res[[1]][[5]])) * scalar

    # C4-C5
    pop06a <- (
      sum(males2res[[1]][[6]]) +
        sum(females2res[[1]][[6]])) * scalar

    # C5-C6
    pop07a <- (
      sum(males2res[[1]][[7]]) +
        sum(females2res[[1]][[7]])) * scalar

    # C6-Hudson Falls
    pop08a <- (
      sum(males2res[[1]][[8]]) +
        sum(females2res[[1]][[8]])) * scalar

    # Population sizes for Mohawk River
    # Troy - E2
    pop02b <- (
      sum(males2res[[2]][[2]]) +
        sum(females2res[[2]][[2]])) * scalar

    # E2-E3
    pop03b <- (
      sum(males2res[[2]][[3]]) +
        sum(females2res[[2]][[3]])) * scalar

    # E3-E4
    pop04b <- (
      sum(males2res[[2]][[4]]) +
        sum(females2res[[2]][[4]])) * scalar

    # E4-E5
    pop05b <- (
      sum(males2res[[2]][[5]]) +
        sum(females2res[[2]][[5]])) * scalar

    # E5-E6
    pop06b <- (
      sum(males2res[[2]][[6]]) +
        sum(females2res[[2]][[6]])) * scalar

    # E6-E7
    pop07b <- (
      sum(males2res[[2]][[7]]) +
        sum(females2res[[2]][[7]])) * scalar

    # E7-E8
    pop08b <- (
      sum(males2res[[2]][[8]]) +
        sum(females2res[[2]][[8]])) * scalar

    # E8-E9
    pop09b <- (
      sum(males2res[[2]][[9]]) +
        sum(females2res[[2]][[9]])) * scalar

    # E9-E10
    pop10b <- (
      sum(males2res[[2]][[10]]) +
        sum(females2res[[2]][[10]])) * scalar

    # E10-E11
    pop11b <- (
      sum(males2res[[2]][[11]]) +
        sum(females2res[[2]][[11]])) * scalar

    # E11-E12
    pop12b <- (
      sum(males2res[[2]][[12]]) +
        sum(females2res[[2]][[12]])) * scalar

    # E12-E13
    pop13b <- (
      sum(males2res[[2]][[13]]) +
        sum(females2res[[2]][[13]])) * scalar

    # E13-E14
    pop14b <- (
      sum(males2res[[2]][[14]]) +
        sum(females2res[[2]][[14]])) * scalar

    # E14-E15
    pop15b <- (
      sum(males2res[[2]][[15]]) +
        sum(females2res[[2]][[15]])) * scalar

    # E15-E16
    pop16b <- (
      sum(males2res[[2]][[16]]) +
        sum(females2res[[2]][[16]])) * scalar

    # E16-E17
    pop17b <- (
      sum(males2res[[2]][[17]]) +
        sum(females2res[[2]][[17]])) * scalar

    # E17-E18
    pop18b <- (
      sum(males2res[[2]][[18]]) +
        sum(females2res[[2]][[18]])) * scalar

    # E18-E19
    pop19b <- (
      sum(males2res[[2]][[19]]) +
        sum(females2res[[2]][[19]])) * scalar

    # E19-E20
    pop20b <- (
      sum(males2res[[2]][[20]]) +
        sum(females2res[[2]][[20]])) * scalar

    # E20-Rome
    pop21b <- (
      sum(males2res[[2]][[21]]) +
        sum(females2res[[2]][[21]])) * scalar

    # Population size
    populationSize <-
      pop01a +
      pop02a +
      pop03a +
      pop04a +
      pop05a +
      pop06a +
      pop07a +
      pop08a +
      pop02b +
      pop03b +
      pop04b +
      pop05b +
      pop06b +
      pop07b +
      pop08b +
      pop09b +
      pop10b +
      pop11b +
      pop12b +
      pop13b +
      pop14b +
      pop15b +
      pop16b +
      pop17b +
      pop18b +
      pop19b +
      pop20b +
      pop21b

    return(list(
      scalar = scalar,
      populationSize = populationSize,
      years = years,
      indirectM = indirectM,
      latentM = latentM,
      pop01a = pop01a,
      pop02a = pop02a,
      pop03a = pop03a,
      pop04a = pop04a,
      pop05a = pop05a,
      pop06a = pop06a,
      pop07a = pop07a,
      pop08a = pop08a,
      pop02b = pop02b,
      pop03b = pop03b,
      pop04b = pop04b,
      pop05b = pop05b,
      pop06b = pop06b,
      pop07b = pop07b,
      pop08b = pop08b,
      pop09b = pop09b,
      pop10b = pop10b,
      pop11b = pop11b,
      pop12b = pop12b,
      pop13b = pop13b,
      pop14b = pop14b,
      pop15b = pop15b,
      pop16b = pop16b,
      pop17b = pop17b,
      pop18b = pop18b,
      pop19b = pop19b,
      pop20b = pop20b,
      pop21b = pop21b,
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
    ))
  }

  if (river == "androscoggin"){
    # Store output in pre-allocated vectors

    # Probability of using sebasticook river
    sabattus <- p_sabattus

    # Population summation by reach & overall
    
    # NOTE THIS SKIPS THE PU DOWNSTREAM OF BRUNSWICK
    # COPY AND PASTE FROM KENNEBEC IF YOU NEED A NEW
    # TEMPLATE!
    
    # Population between brunswick and pejepscot
    pop01a <- (
      sum(males2res[[1]][[2]]) +
        sum(females2res[[1]][[2]]) +
        sum(males2res[[2]][[2]]) +
        sum(females2res[[2]][[2]])) * scalarVar

    # Population between pejepscot and worumbo
    pop02a <- (
      sum(males2res[[1]][[3]]) +
        sum(females2res[[1]][[3]]) +
        sum(males2res[[2]][[3]]) +
        sum(females2res[[2]][[3]])) * scalarVar

    # Population between worumbo and lower barker
    pop03a <- (
      sum(males2res[[1]][[4]]) +
        sum(females2res[[1]][[4]])) * scalarVar

    # Population between lower barker and upper barker
    pop04a <- (
      sum(males2res[[1]][[5]]) +
        sum(females2res[[1]][[5]])) * scalarVar
    
    # Population between upper barker and littlefield
    pop05a <- (
      sum(males2res[[1]][[6]]) +
        sum(females2res[[1]][[6]])) * scalarVar
    
    # Population between upper littlefield and hacketts mills
    pop06a <- (
      sum(males2res[[1]][[7]]) +
        sum(females2res[[1]][[7]])) * scalarVar   
    
    # Population between hacketts mills and marcal (mechanic falls)
    pop07a <- (
      sum(males2res[[1]][[8]]) +
        sum(females2res[[1]][[8]])) * scalarVar      
    
    # Population between marcal (mechanic falls) and welchville
    pop08a <- (
      sum(males2res[[1]][[9]]) +
        sum(females2res[[1]][[9]])) * scalarVar          
    
    # Population between welchville and paris
    pop09a <- (
      sum(males2res[[1]][[10]]) +
        sum(females2res[[1]][[10]])) * scalarVar            
    
    # Population between paris and bisco
    pop10a <- (
      sum(males2res[[1]][[11]]) +
        sum(females2res[[1]][[11]])) * scalarVar       
        
    # Population between farwell and fortier
    pop4b <- (
      sum(males2res[[2]][[5]]) +
        sum(females2res[[2]][[5]])) * scalarVar

    # Population between fortier and sabattus 
    pop5b <- (
      sum(males2res[[2]][[6]]) +
        sum(females2res[[2]][[6]])) * scalarVar
    
    # Population size
    populationSize <-
      pop01a +
      pop02a +
      pop03a +
      pop04a +
      pop05a +
      pop06a +
      pop07a +
      pop08a +
      pop09a +
      pop10a +      
      pop4b +
      pop5b
    
    return(list(
      scalar = scalar,
      populationSize = populationSize,
      years = years,
      sabattus = sabattus,
      pop01a = pop01a,
      pop02a = pop02a,
      pop03a = pop03a,
      pop04a = pop04a,
      pop05a = pop05a,
      pop06a = pop06a,
      pop07a = pop07a,
      pop08a = pop08a,
      pop09a = pop09a,
      pop10a = pop10a,      
      pop4b = pop4b,
      pop5b = pop5b,
      indirectM = indirectM,
      latentM = latentM,
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
    ))
  }  

}
