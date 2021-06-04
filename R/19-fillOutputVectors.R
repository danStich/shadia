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
fillOutputVectors <- function() {

  # Year, filling pre-allocated vector with this year
  years[(n + nYears * (k - 1))] <- n

  # Upstream passage timing
  ptime[[(n + nYears * (k - 1))]] <- timely

  # Climate scenario
  climate_scen[(n + nYears * (k - 1))] <- climate
  
  # Indirect mortality, filling pre-allocated vector
  indirectM[(n + nYears * (k - 1))] <- indirect

  # Latent estuary mortality, filling pre-allocated vector
  latentM[(n + nYears * (k - 1))] <- latent

  # Proportion of repeat spawners at each age, filling pre-allocated vector
  pRepeats[[(n + nYears * (k - 1))]] <- pRepeat

  # Age-structured repeat spawners, filling pre-allocated vector
  spawners[[(n + nYears * (k - 1))]] <- .shadia$spawningPool

  # Reset the scalar based on population size
  # .shadia$scalar <- setScalar(.shadia$spawningPool)

  # Scalar variable for computational gains
  scalarVar[[(n + nYears * (k - 1))]] <- .shadia$scalar

  # Population demographics and survival rates
  S.downstream[(n + nYears * (k - 1))] <- mean(downstreamS)
  S.marine[(n + nYears * (k - 1))] <- marineS[1]
  # Extract user-defined S.marine if provided
  if(exists("marine_s")){
    if(!is.null(marine_s)){
      S.marine[(n + nYears * (k - 1))] <- marine_s
    }
  }  
  
  F.inRiver[(n + nYears * (k - 1))] <- inRiverF
  F.commercial[(n + nYears * (k - 1))] <- mean(commercialF)
  F.bycatch[(n + nYears * (k - 1))] <- mean(bycatchF)
  popStart[(n + nYears * (k - 1))] <- n_adults
  p.female[(n + nYears * (k - 1))] <- sexRatio
  S.prespawnM[(n + nYears * (k - 1))] <- pre_spawn_survival_males
  S.postspawnM[(n + nYears * (k - 1))] <- post_spawn_survival_males
  S.prespawnF[(n + nYears * (k - 1))] <- pre_spawn_survival_females
  S.postspawnF[(n + nYears * (k - 1))] <- post_spawn_survival_females
  S.juvenile[(n + nYears * (k - 1))] <- juvenile_survival

  # Individual traits
  # Entry dates
  b.Arr[(n + nYears * (k - 1))] <- mean(c_entryDate[c_sex == 0])
  r.Arr[(n + nYears * (k - 1))] <- mean(c_entryDate[c_sex == 1])
  # Spawning ATU
  ATUspawn1[(n + nYears * (k - 1))] <- mean(c_spawnATU1)
  ATUspawn2[(n + nYears * (k - 1))] <- mean(c_spawnATU2)
  # Spawning dates
  Dspawn1[(n + nYears * (k - 1))] <- mean(c_initial)
  Dspawn2[(n + nYears * (k - 1))] <- mean(c_end)
  # Length at age
  # Females
  linF[(n + nYears * (k - 1))] <- r.mat[1]
  kF[(n + nYears * (k - 1))] <- r.mat[2]
  t0F[(n + nYears * (k - 1))] <- r.mat[3]
  # Males
  linM[(n + nYears * (k - 1))] <- b.mat[1]
  kM[(n + nYears * (k - 1))] <- b.mat[2]
  t0M[(n + nYears * (k - 1))] <- b.mat[3]

  # Length
  b.length[(n + nYears * (k - 1))] <- mean(c_male_lf[c_male_lf != 0])
  r.length[(n + nYears * (k - 1))] <- mean(c_female_lf[c_female_lf != 0])

  # Fecundity
  spawnInt[(n + nYears * (k - 1))] <- mean(c_SI)
  batchSize[(n + nYears * (k - 1))] <- mean(c_BF)
  RAF[(n + nYears * (k - 1))] <- mean(c_RAF)

  # Movement parameters
  s.Optim[(n + nYears * (k - 1))] <- mean(sOptim)
  d.Max[(n + nYears * (k - 1))] <- mean(dMax)
  tortuosity[(n + nYears * (k - 1))] <- mean(tort)
  motivation[(n + nYears * (k - 1))] <- mot
  daily.move[(n + nYears * (k - 1))] <- mean(dailyMove)


  if (river == "penobscot") {
    # Population below Milford, filling pre-allocated vector
    LowerPop[(n + nYears * (k - 1))] <- (
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
    OronoPop[(n + nYears * (k - 1))] <- (
      sum(males2res[[3]][[2]]) +
        sum(males2res[[4]][[2]]) +
        sum(females2res[[3]][[2]]) +
        sum(females2res[[4]][[2]])
    ) * scalar

    # Population between Stillwater Dam and Gilman Falls, filling pre-allocated
    StillwaterPop[(n + nYears * (k - 1))] <- (
      sum(males2res[[3]][[3]]) +
        sum(males2res[[4]][[3]]) +
        sum(females2res[[3]][[3]]) +
        sum(females2res[[4]][[3]])
    ) * scalar

    # Population between Milford and West Enfield, filling pre-allocated vector
    MilfordPop[(n + nYears * (k - 1))] <- (
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

    # Population between West Enfield and Weldon, filling pre-allocated vector
    EnfieldPop[(n + nYears * (k - 1))] <- (
      sum(males2res[[2]][[4]]) +
        sum(males2res[[4]][[5]]) +
        sum(females2res[[2]][[4]]) +
        sum(females2res[[4]][[5]])
    ) * scalar

    # Population above Weldon, filling pre-allocated vector
    WeldonPop[(n + nYears * (k - 1))] <- (
      sum(males2res[[2]][[length(males2res[[2]])]]) +
        sum(males2res[[4]][[length(males2res[[4]])]]) +
        sum(females2res[[2]][[length(females2res[[2]])]]) +
        sum(females2res[[4]][[length(females2res[[4]])]])
    ) * scalar

    # Population between Howland and Dover dams, filling pre-allocated vector
    HowlandPop[(n + nYears * (k - 1))] <- (
      sum(males2res[[1]][[4]]) +
        sum(males2res[[3]][[5]]) +
        sum(females2res[[1]][[4]]) +
        sum(females2res[[3]][[5]])
    ) * scalar

    # Population between Moosehead and Browns Mill dams
    MoosePop[(n + nYears * (k - 1))] <- (
      sum(males2res[[1]][[5]]) +
        sum(males2res[[3]][[6]]) +
        sum(females2res[[1]][[5]]) +
        sum(females2res[[3]][[6]])
    ) * scalar

    # Population between Browns Mill and Guilford dams, filling pre-allocated
    BrownsPop[(n + nYears * (k - 1))] <- (
      sum(males2res[[1]][[6]]) +
        sum(males2res[[3]][[7]]) +
        sum(females2res[[1]][[6]]) +
        sum(females2res[[3]][[7]])
    ) * scalar

    # Population above Guilford Dam, filling pre-allocated vector
    GuilfordPop[(n + nYears * (k - 1))] <- (
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


    # Store the inputs for sensitivity analysis
    # Passage assumptions
    pStillUP[(n + nYears * (k - 1))] <- pStillwaterUp
    pStillD[(n + nYears * (k - 1))] <- pStillwaterD
    pPiscUP[(n + nYears * (k - 1))] <- pPiscUp

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
    pBypassUS[(n + nYears * (k - 1))] <- pBypassUp
    pBypassDS[(n + nYears * (k - 1))] <- pBypassD

    # Population below Essex, filling pre-allocated vector
    popI[(n + nYears * (k - 1))] <- (
      sum(males2res[[1]][[1]]) +
        sum(females2res[[1]][[1]])) * scalar

    # Population between Essex and Pawtucket dams
    popII[(n + nYears * (k - 1))] <- (
      sum(males2res[[1]][[2]]) +
        sum(females2res[[1]][[2]])) * scalar

    # Population between Pawtucket and Amoskeag dams
    popIII[(n + nYears * (k - 1))] <- (
      sum(males2res[[1]][[3]]) +
        sum(females2res[[1]][[3]])) * scalar

    # Population between Amoskeag and Hookset dams
    popIV[(n + nYears * (k - 1))] <- (
      sum(males2res[[1]][[4]]) +
        sum(females2res[[1]][[4]])) * scalar

    # Population upstream of Hookset Dam
    popV[(n + nYears * (k - 1))] <- (
      sum(males2res[[1]][[5]]) +
        sum(females2res[[1]][[5]])) * scalar

    # Population size
    populationSize[(n + nYears * (k - 1))] <-
      popI[(n + nYears * (k - 1))] +
      popII[(n + nYears * (k - 1))] +
      popIII[(n + nYears * (k - 1))] +
      popIV[(n + nYears * (k - 1))] +
      popV[(n + nYears * (k - 1))]

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
    pSpill[(n + nYears * (k - 1))] <- pSpillway

    # Northfield Mountain take
    NorthFieldV[(n + nYears * (k - 1))] <- northfield$vernonJ
    NorthFieldT[(n + nYears * (k - 1))] <- northfield$turnersJ
    NorthFieldVa[(n + nYears * (k - 1))] <- northfield$vernonA
    NorthFieldTa[(n + nYears * (k - 1))] <- northfield$turnersA

    # Population below Essex, filling pre-allocated vector
    popI[(n + nYears * (k - 1))] <- (
      sum(males2res[[1]][[1]]) +
        sum(females2res[[1]][[1]]) +
        sum(males2res[[2]][[1]]) +
        sum(females2res[[2]][[1]])) * scalar
    # Population between Essex and Pawtucket dams
    popII[(n + nYears * (k - 1))] <- (
      sum(males2res[[1]][[2]]) +
        sum(females2res[[1]][[2]]) +
        sum(males2res[[2]][[2]]) +
        sum(females2res[[2]][[2]])) * scalar
    # Population between Pawtucket and Amoskeag dams
    popIII[(n + nYears * (k - 1))] <- (
      sum(males2res[[1]][[3]]) +
        sum(females2res[[1]][[3]]) +
        sum(males2res[[2]][[3]]) +
        sum(females2res[[2]][[3]])) * scalar
    # Population between Amoskeag and Hookset dams
    popIV[(n + nYears * (k - 1))] <- (
      sum(males2res[[1]][[4]]) +
        sum(females2res[[1]][[4]]) +
        sum(males2res[[2]][[4]]) +
        sum(females2res[[2]][[4]])) * scalar
    # Population upstream of Hookset Dam
    popV[(n + nYears * (k - 1))] <- (
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
    pJuniataUp[(n + nYears * (k - 1))] <- p_JuniataUp
    pWestBranchUp[(n + nYears * (k - 1))] <- p_WestBranchUp
    pChemungUp[(n + nYears * (k - 1))] <- p_ChemungUp
    pNorthBranchUp[(n + nYears * (k - 1))] <- p_NorthBranchUp

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
    SunPop[(n + nYears * (k - 1))] <- (
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
    popI[(n + nYears * (k - 1))] <- (
      sum(males2res[[1]][[1]]) +
        sum(females2res[[1]][[1]])) * scalar

    # Population between Cataract and Springs and Bradbury
    popII[(n + nYears * (k - 1))] <- (
      sum(males2res[[1]][[2]]) +
        sum(females2res[[1]][[2]])) * scalar

    # Population between Bradbury and Skelton Dam
    popIII[(n + nYears * (k - 1))] <- (
      sum(males2res[[1]][[3]]) +
        sum(females2res[[1]][[3]])) * scalar

    # Population between Skelton and Bar Mills Dam
    popIV[(n + nYears * (k - 1))] <- (
      sum(males2res[[1]][[4]]) +
        sum(females2res[[1]][[4]])) * scalar

    # Population between Bar Mills Dam and West Buxton
    popV[(n + nYears * (k - 1))] <- (
      sum(males2res[[1]][[5]]) +
        sum(females2res[[1]][[5]])) * scalar

    # Population between West Buxton and Bonny Eagle
    popVI[(n + nYears * (k - 1))] <- (
      sum(males2res[[1]][[6]]) +
        sum(females2res[[1]][[6]])) * scalar

    # Population between Bonny Eagle and Hiram Falls
    popVII[(n + nYears * (k - 1))] <- (
      sum(males2res[[1]][[7]]) +
        sum(females2res[[1]][[7]])) * scalar

    # Population size
    populationSize[(n + nYears * (k - 1))] <-
      popI[(n + nYears * (k - 1))] +
      popII[(n + nYears * (k - 1))] +
      popIII[(n + nYears * (k - 1))] +
      popIV[(n + nYears * (k - 1))] +
      popV[(n + nYears * (k - 1))] +
      popVI[(n + nYears * (k - 1))] +
      popVII[(n + nYears * (k - 1))]

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
    sebasticook[(n + nYears * (k - 1))] <- p_sebasticook

    # Population downstream of lockwood dam
    pop1a[(n + nYears * (k - 1))] <- (
      sum(males2res[[1]][[1]]) +
        sum(females2res[[1]][[1]]) +
        sum(males2res[[2]][[1]]) +
        sum(females2res[[2]][[1]])) * scalar

    # Population between lockwood and hydro-kennebec
    pop2a[(n + nYears * (k - 1))] <- (
      sum(males2res[[1]][[2]]) +
        sum(females2res[[1]][[2]])) * scalar

    # Population between hydro-kennebec and shawmut
    pop3a[(n + nYears * (k - 1))] <- (
      sum(males2res[[1]][[3]]) +
        sum(females2res[[1]][[3]])) * scalar

    # Population between shawmut and weston,
    # including Sandy River
    pop4a[(n + nYears * (k - 1))] <- (
      sum(males2res[[1]][[4]]) +
        sum(females2res[[1]][[4]])) * scalar

    # Population between weston and abenaki,
    # including Sandy River
    pop5a[(n + nYears * (k - 1))] <- (
      sum(males2res[[1]][[5]]) +
        sum(females2res[[1]][[5]])) * scalar

    # Population between benton falls and burnham dam
    pop1b[(n + nYears * (k - 1))] <- (
      sum(males2res[[2]][[2]]) +
        sum(females2res[[2]][[2]])) * scalar

    # Population upstream of burnham dam
    pop2b[(n + nYears * (k - 1))] <- (
      sum(males2res[[2]][[3]]) +
        sum(females2res[[2]][[3]])) * scalar

    # Population size
    populationSize[(n + nYears * (k - 1))] <-
      pop1a[(n + nYears * (k - 1))] +
      pop2a[(n + nYears * (k - 1))] +
      pop3a[(n + nYears * (k - 1))] +
      pop4a[(n + nYears * (k - 1))] +
      pop5a[(n + nYears * (k - 1))] +
      pop1b[(n + nYears * (k - 1))] +
      pop2b[(n + nYears * (k - 1))]

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
    pop01a[(n + nYears * (k - 1))] <- (
      sum(males2res[[1]][[1]]) +
        sum(females2res[[1]][[1]]) +
        sum(males2res[[2]][[1]]) +
        sum(females2res[[2]][[1]])) * scalar

    # Population sizes for Upper Hudson (Champlain Canal)
    # Federal - C1
    pop02a[(n + nYears * (k - 1))] <- (
      sum(males2res[[1]][[2]]) +
        sum(females2res[[1]][[2]])) * scalar

    # C1-C2
    pop03a[(n + nYears * (k - 1))] <- (
      sum(males2res[[1]][[3]]) +
        sum(females2res[[1]][[3]])) * scalar

    # C2-C3
    pop04a[(n + nYears * (k - 1))] <- (
      sum(males2res[[1]][[4]]) +
        sum(females2res[[1]][[4]])) * scalar

    # C3-C4
    pop05a[(n + nYears * (k - 1))] <- (
      sum(males2res[[1]][[5]]) +
        sum(females2res[[1]][[5]])) * scalar

    # C4-C5
    pop06a[(n + nYears * (k - 1))] <- (
      sum(males2res[[1]][[6]]) +
        sum(females2res[[1]][[6]])) * scalar

    # C5-C6
    pop07a[(n + nYears * (k - 1))] <- (
      sum(males2res[[1]][[7]]) +
        sum(females2res[[1]][[7]])) * scalar

    # C6-Hudson Falls
    pop08a[(n + nYears * (k - 1))] <- (
      sum(males2res[[1]][[8]]) +
        sum(females2res[[1]][[8]])) * scalar

    # Population sizes for Mohawk River
    # Troy - E2
    pop02b[(n + nYears * (k - 1))] <- (
      sum(males2res[[2]][[2]]) +
        sum(females2res[[2]][[2]])) * scalar

    # E2-E3
    pop03b[(n + nYears * (k - 1))] <- (
      sum(males2res[[2]][[3]]) +
        sum(females2res[[2]][[3]])) * scalar

    # E3-E4
    pop04b[(n + nYears * (k - 1))] <- (
      sum(males2res[[2]][[4]]) +
        sum(females2res[[2]][[4]])) * scalar

    # E4-E5
    pop05b[(n + nYears * (k - 1))] <- (
      sum(males2res[[2]][[5]]) +
        sum(females2res[[2]][[5]])) * scalar

    # E5-E6
    pop06b[(n + nYears * (k - 1))] <- (
      sum(males2res[[2]][[6]]) +
        sum(females2res[[2]][[6]])) * scalar

    # E6-E7
    pop07b[(n + nYears * (k - 1))] <- (
      sum(males2res[[2]][[7]]) +
        sum(females2res[[2]][[7]])) * scalar

    # E7-E8
    pop08b[(n + nYears * (k - 1))] <- (
      sum(males2res[[2]][[8]]) +
        sum(females2res[[2]][[8]])) * scalar

    # E8-E9
    pop09b[(n + nYears * (k - 1))] <- (
      sum(males2res[[2]][[9]]) +
        sum(females2res[[2]][[9]])) * scalar

    # E9-E10
    pop10b[(n + nYears * (k - 1))] <- (
      sum(males2res[[2]][[10]]) +
        sum(females2res[[2]][[10]])) * scalar

    # E10-E11
    pop11b[(n + nYears * (k - 1))] <- (
      sum(males2res[[2]][[11]]) +
        sum(females2res[[2]][[11]])) * scalar

    # E11-E12
    pop12b[(n + nYears * (k - 1))] <- (
      sum(males2res[[2]][[12]]) +
        sum(females2res[[2]][[12]])) * scalar

    # E12-E13
    pop13b[(n + nYears * (k - 1))] <- (
      sum(males2res[[2]][[13]]) +
        sum(females2res[[2]][[13]])) * scalar

    # E13-E14
    pop14b[(n + nYears * (k - 1))] <- (
      sum(males2res[[2]][[14]]) +
        sum(females2res[[2]][[14]])) * scalar

    # E14-E15
    pop15b[(n + nYears * (k - 1))] <- (
      sum(males2res[[2]][[15]]) +
        sum(females2res[[2]][[15]])) * scalar

    # E15-E16
    pop16b[(n + nYears * (k - 1))] <- (
      sum(males2res[[2]][[16]]) +
        sum(females2res[[2]][[16]])) * scalar

    # E16-E17
    pop17b[(n + nYears * (k - 1))] <- (
      sum(males2res[[2]][[17]]) +
        sum(females2res[[2]][[17]])) * scalar

    # E17-E18
    pop18b[(n + nYears * (k - 1))] <- (
      sum(males2res[[2]][[18]]) +
        sum(females2res[[2]][[18]])) * scalar

    # E18-E19
    pop19b[(n + nYears * (k - 1))] <- (
      sum(males2res[[2]][[19]]) +
        sum(females2res[[2]][[19]])) * scalar

    # E19-E20
    pop20b[(n + nYears * (k - 1))] <- (
      sum(males2res[[2]][[20]]) +
        sum(females2res[[2]][[20]])) * scalar

    # E20-Rome
    pop21b[(n + nYears * (k - 1))] <- (
      sum(males2res[[2]][[21]]) +
        sum(females2res[[2]][[21]])) * scalar

    # Population size
    populationSize[(n + nYears * (k - 1))] <-
      pop01a[(n + nYears * (k - 1))] +
      pop02a[(n + nYears * (k - 1))] +
      pop03a[(n + nYears * (k - 1))] +
      pop04a[(n + nYears * (k - 1))] +
      pop05a[(n + nYears * (k - 1))] +
      pop06a[(n + nYears * (k - 1))] +
      pop07a[(n + nYears * (k - 1))] +
      pop08a[(n + nYears * (k - 1))] +
      pop02b[(n + nYears * (k - 1))] +
      pop03b[(n + nYears * (k - 1))] +
      pop04b[(n + nYears * (k - 1))] +
      pop05b[(n + nYears * (k - 1))] +
      pop06b[(n + nYears * (k - 1))] +
      pop07b[(n + nYears * (k - 1))] +
      pop08b[(n + nYears * (k - 1))] +
      pop09b[(n + nYears * (k - 1))] +
      pop10b[(n + nYears * (k - 1))] +
      pop11b[(n + nYears * (k - 1))] +
      pop12b[(n + nYears * (k - 1))] +
      pop13b[(n + nYears * (k - 1))] +
      pop14b[(n + nYears * (k - 1))] +
      pop15b[(n + nYears * (k - 1))] +
      pop16b[(n + nYears * (k - 1))] +
      pop17b[(n + nYears * (k - 1))] +
      pop18b[(n + nYears * (k - 1))] +
      pop19b[(n + nYears * (k - 1))] +
      pop20b[(n + nYears * (k - 1))] +
      pop21b[(n + nYears * (k - 1))]

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
    sabattus[(n + nYears * (k - 1))] <- p_sabattus

    # Population summation by reach & overall
    
    # NOTE THIS SKIPS THE PU DOWNSTREAM OF BRUNSWICK
    # COPY AND PASTE FROM KENNEBEC IF YOU NEED A NEW
    # TEMPLATE!
    
    # Population between brunswick and pejebscot
    pop01a[(n + nYears * (k - 1))] <- (
      sum(males2res[[1]][[2]]) +
        sum(females2res[[1]][[2]]) +
        sum(males2res[[2]][[2]]) +
        sum(females2res[[2]][[2]])) * scalar

    # Population between pejebscot and worumbo
    pop02a[(n + nYears * (k - 1))] <- (
      sum(males2res[[1]][[3]]) +
        sum(females2res[[1]][[3]]) +
        sum(males2res[[2]][[3]]) +
        sum(females2res[[2]][[3]])) * scalar

    # Population between worumbo and lower barker
    pop03a[(n + nYears * (k - 1))] <- (
      sum(males2res[[1]][[4]]) +
        sum(females2res[[1]][[4]])) * scalar

    # Population between lower barker and upper barker
    pop04a[(n + nYears * (k - 1))] <- (
      sum(males2res[[1]][[5]]) +
        sum(females2res[[1]][[5]])) * scalar
    
    # Population between upper barker and littlefield
    pop05a[(n + nYears * (k - 1))] <- (
      sum(males2res[[1]][[6]]) +
        sum(females2res[[1]][[6]])) * scalar    
    
    # Population between upper littlefield and hacketts mills
    pop06a[(n + nYears * (k - 1))] <- (
      sum(males2res[[1]][[7]]) +
        sum(females2res[[1]][[7]])) * scalar       
    
    # Population between hacketts mills and marcal (mechanic falls)
    pop07a[(n + nYears * (k - 1))] <- (
      sum(males2res[[1]][[8]]) +
        sum(females2res[[1]][[8]])) * scalar         
    
    # Population between marcal (mechanic falls) and welchville
    pop08a[(n + nYears * (k - 1))] <- (
      sum(males2res[[1]][[9]]) +
        sum(females2res[[1]][[9]])) * scalar           
    
    # Population between welchville and paris
    pop09a[(n + nYears * (k - 1))] <- (
      sum(males2res[[1]][[10]]) +
        sum(females2res[[1]][[10]])) * scalar             
    
    # Population between paris and bisco
    pop10a[(n + nYears * (k - 1))] <- (
      sum(males2res[[1]][[11]]) +
        sum(females2res[[1]][[11]])) * scalar             
        

    # Population between farwell and fortier
    pop4b[(n + nYears * (k - 1))] <- (
      sum(males2res[[2]][[5]]) +
        sum(females2res[[2]][[5]])) * scalar

    # Population between fortier and sabattus 
    pop5b[(n + nYears * (k - 1))] <- (
      sum(males2res[[2]][[6]]) +
        sum(females2res[[2]][[6]])) * scalar
    
    # Population size
    populationSize[(n + nYears * (k - 1))] <-
      pop01a[(n + nYears * (k - 1))] +
      pop02a[(n + nYears * (k - 1))] +
      pop03a[(n + nYears * (k - 1))] +
      pop04a[(n + nYears * (k - 1))] +
      pop05a[(n + nYears * (k - 1))] +
      pop06a[(n + nYears * (k - 1))] +
      pop07a[(n + nYears * (k - 1))] +
      pop08a[(n + nYears * (k - 1))] +
      pop09a[(n + nYears * (k - 1))] +
      pop10a[(n + nYears * (k - 1))] +      
      pop4b[(n + nYears * (k - 1))] +
      pop5b[(n + nYears * (k - 1))]
    
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
