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
defineOutputVectors <- function() {
  # Shared parameters

  # Empty container to hold year
  years <- vector(mode = "numeric", length = nYears * nRuns)

  # Define empty vectors to hold results for outer loop
  climate_scen <- vector(mode = "character", length = nYears * nRuns)

  # Age-structured spawning population
  spawners <- vector(mode = "list", length = nYears * nRuns)

  # Proportion of repeat spawners in each age class
  pRepeats <- vector(mode = "list", length = nYears * nRuns)

  # Catchment-wide population abundance
  populationSize <- vector(mode = "numeric", length = nYears * nRuns)

  # Store the scale
  scalarVar <- vector(mode = "numeric", length = nYears * nRuns)

  # Store the inputs for sensitivity analysis
  # Passage assumptions
  ptime <- vector(mode = "list", length = nYears * nRuns)

  # Population demographics
  S.downstream <- vector(mode = "numeric", length = nYears * nRuns)
  S.marine <- vector(mode = "numeric", length = nYears * nRuns)
  F.inRiver <- vector(mode = "numeric", length = nYears * nRuns)
  F.commercial <- vector(mode = "numeric", length = nYears * nRuns)
  F.bycatch <- vector(mode = "numeric", length = nYears * nRuns)
  popStart <- vector(mode = "numeric", length = nYears * nRuns)
  p.female <- vector(mode = "numeric", length = nYears * nRuns)
  p.female <- vector(mode = "numeric", length = nYears * nRuns)
  S.prespawnM <- vector(mode = "numeric", length = nYears * nRuns)
  S.postspawnM <- vector(mode = "numeric", length = nYears * nRuns)
  S.prespawnF <- vector(mode = "numeric", length = nYears * nRuns)
  S.postspawnF <- vector(mode = "numeric", length = nYears * nRuns)
  S.juvenile <- vector(mode = "numeric", length = nYears * nRuns)
  indirectM <- vector(mode = "numeric", length = nYears * nRuns)
  latentM <- vector(mode = "numeric", length = nYears * nRuns)

  # Individual traits
  # Entry dates
  b.Arr <- vector(mode = "numeric", length = nYears * nRuns)
  r.Arr <- vector(mode = "numeric", length = nYears * nRuns)
  # Spawning ATU
  ATUspawn1 <- vector(mode = "numeric", length = nYears * nRuns)
  ATUspawn2 <- vector(mode = "numeric", length = nYears * nRuns)
  # Spawning dates
  Dspawn1 <- vector(mode = "numeric", length = nYears * nRuns)
  Dspawn2 <- vector(mode = "numeric", length = nYears * nRuns)
  # Length at age
  # Females
  linF <- vector(mode = "numeric", length = nYears * nRuns)
  kF <- vector(mode = "numeric", length = nYears * nRuns)
  t0F <- vector(mode = "numeric", length = nYears * nRuns)
  # Males
  linM <- vector(mode = "numeric", length = nYears * nRuns)
  kM <- vector(mode = "numeric", length = nYears * nRuns)
  t0M <- vector(mode = "numeric", length = nYears * nRuns)

  # Lengths and mass
  b.length <- vector(mode = "numeric", length = nYears * nRuns)
  r.length <- vector(mode = "numeric", length = nYears * nRuns)

  # Fecundity
  spawnInt <- vector(mode = "numeric", length = nYears * nRuns)
  batchSize <- vector(mode = "numeric", length = nYears * nRuns)
  RAF <- vector(mode = "numeric", length = nYears * nRuns)

  # Movement parameters
  s.Optim <- vector(mode = "numeric", length = nYears * nRuns)
  d.Max <- vector(mode = "numeric", length = nYears * nRuns)
  tortuosity <- vector(mode = "numeric", length = nYears * nRuns)
  motivation <- vector(mode = "numeric", length = nYears * nRuns)
  daily.move <- vector(mode = "numeric", length = nYears * nRuns)


  if (river == "penobscot") {

    # Population abundance in each production unit
    # Population abundance below Milford Dam in main-stem
    LowerPop <- vector(mode = "numeric", length = nYears * nRuns)
    # Population abundance between Orono and Stillwater dams
    OronoPop <- vector(mode = "numeric", length = nYears * nRuns)
    # Population abundance between Stillwater Dam and Gilman Falls
    StillwaterPop <- vector(mode = "numeric", length = nYears * nRuns)
    # Population abundance between Milford Dam and confluence
    MilfordPop <- vector(mode = "numeric", length = nYears * nRuns)
    # Population abundance between West Enfield and Weldon dams
    EnfieldPop <- vector(mode = "numeric", length = nYears * nRuns)
    # Population abundance above Weldon Dam
    WeldonPop <- vector(mode = "numeric", length = nYears * nRuns)
    # Population abundance between Howland and Moosehead dams
    HowlandPop <- vector(mode = "numeric", length = nYears * nRuns)
    # Population abundance between Moosehead and Browns Mill dams
    MoosePop <- vector(mode = "numeric", length = nYears * nRuns)
    # Population abundance between Browns Mill and Guilford dams
    BrownsPop <- vector(mode = "numeric", length = nYears * nRuns)
    # Population abundance above Guilford Dam
    GuilfordPop <- vector(mode = "numeric", length = nYears * nRuns)

    # System-specific passage probabilities
    pStillUP <- vector(mode = "numeric", length = nYears * nRuns)
    pStillD <- vector(mode = "numeric", length = nYears * nRuns)
    pPiscUP <- vector(mode = "numeric", length = nYears * nRuns)

    return(
      list(
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
      )
    )
  }

  if (river == "merrimack") {

    # Probability of using bypass route at pawtucket
    pBypassUS <- vector(mode = "numeric", length = nYears * nRuns)
    pBypassDS <- vector(mode = "numeric", length = nYears * nRuns)

    # Population abundance in each production unit
    popI <- vector(mode = "numeric", length = nYears * nRuns)
    popII <- vector(mode = "numeric", length = nYears * nRuns)
    popIII <- vector(mode = "numeric", length = nYears * nRuns)
    popIV <- vector(mode = "numeric", length = nYears * nRuns)
    popV <- vector(mode = "numeric", length = nYears * nRuns)

    return(
      list(
        years = years,
        climate_scen = climate_scen,
        pBypassUS = pBypassUS,
        pBypassDS = pBypassDS,
        indirectM = indirectM,
        latentM = latentM,
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
      )
    )
  }

  if (river == "connecticut") {
    # Probability of using the spillway passage route for
    # upstream migration
    pSpill <- vector(mode = "numeric", length = nYears * nRuns)

    # Northfield mountain take
    NorthFieldV <- vector(mode = "numeric", length = nYears * nRuns)
    NorthFieldT <- vector(mode = "numeric", length = nYears * nRuns)
    NorthFieldVa <- vector(mode = "numeric", length = nYears * nRuns)
    NorthFieldTa <- vector(mode = "numeric", length = nYears * nRuns)

    # Population abundance in each production unit
    popI <- vector(mode = "numeric", length = nYears * nRuns)
    popII <- vector(mode = "numeric", length = nYears * nRuns)
    popIII <- vector(mode = "numeric", length = nYears * nRuns)
    popIV <- vector(mode = "numeric", length = nYears * nRuns)
    popV <- vector(mode = "numeric", length = nYears * nRuns)

    return(
      list(
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
      )
    )
  }

  if (river == "susquehanna") {
    # Define empty vectors to hold results for outer loop
    # Population abundance in each production unit
    LowPop <- vector(mode = "numeric", length = nYears * nRuns)
    ConPop <- vector(mode = "numeric", length = nYears * nRuns)
    HolPop <- vector(mode = "numeric", length = nYears * nRuns)
    SafPop <- vector(mode = "numeric", length = nYears * nRuns)
    YorPop <- vector(mode = "numeric", length = nYears * nRuns)
    JunPop <- vector(mode = "numeric", length = nYears * nRuns)
    WesPop <- vector(mode = "numeric", length = nYears * nRuns)
    WilPop <- vector(mode = "numeric", length = nYears * nRuns)
    LocPop <- vector(mode = "numeric", length = nYears * nRuns)
    ChaPop <- vector(mode = "numeric", length = nYears * nRuns)
    SunPop <- vector(mode = "numeric", length = nYears * nRuns)
    ChePop <- vector(mode = "numeric", length = nYears * nRuns)
    NorPop <- vector(mode = "numeric", length = nYears * nRuns)
    UnaPop <- vector(mode = "numeric", length = nYears * nRuns)
    RocPop <- vector(mode = "numeric", length = nYears * nRuns)
    ColPop <- vector(mode = "numeric", length = nYears * nRuns)

    # Store the inputs for sensitivity analysis
    # Passage assumptions
    pJuniataUp <- vector(mode = "numeric", length = nYears * nRuns)
    pWestBranchUp <- vector(mode = "numeric", length = nYears * nRuns)
    pChemungUp <- vector(mode = "numeric", length = nYears * nRuns)
    pNorthBranchUp <- vector(mode = "numeric", length = nYears * nRuns)

    return(
      list(
        years = years,
        climate_scen = climate_scen,
        indirectM = indirectM,
        latentM = latentM,
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
      )
    )
  }

  if (river == "saco") {
    # Define empty vectors to hold results for outer loop
    # Population abundance in each production unit
    popI <- vector(mode = "numeric", length = nYears * nRuns)
    popII <- vector(mode = "numeric", length = nYears * nRuns)
    popIII <- vector(mode = "numeric", length = nYears * nRuns)
    popIV <- vector(mode = "numeric", length = nYears * nRuns)
    popV <- vector(mode = "numeric", length = nYears * nRuns)
    popVI <- vector(mode = "numeric", length = nYears * nRuns)
    popVII <- vector(mode = "numeric", length = nYears * nRuns)

    return(
      list(
        years = years,
        climate_scen = climate_scen,
        indirectM = indirectM,
        latentM = latentM,
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
      )
    )
  }

  if (river == "kennebec") {
    # Probability of using Sebasticook
    sebasticook <- vector(mode = "numeric", length = nYears * nRuns)

    # Population abundance in each production unit
    pop1a <- vector(mode = "numeric", length = nYears * nRuns)
    pop2a <- vector(mode = "numeric", length = nYears * nRuns)
    pop3a <- vector(mode = "numeric", length = nYears * nRuns)
    pop4a <- vector(mode = "numeric", length = nYears * nRuns)
    pop5a <- vector(mode = "numeric", length = nYears * nRuns)
    pop1b <- vector(mode = "numeric", length = nYears * nRuns)
    pop2b <- vector(mode = "numeric", length = nYears * nRuns)

    return(
      list(
        years = years,
        climate_scen = climate_scen,
        sebasticook = sebasticook,
        indirectM = indirectM,
        latentM = latentM,
        pop1a = pop1a,
        pop2a = pop2a,
        pop3a = pop3a,
        pop4a = pop4a,
        pop5a = pop5a,
        pop1b = pop1b,
        pop2b = pop2b,
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
      )
    )
  }

  if (river == "hudson") {
    # Define empty vectors to hold results for outer loop
    # Population abundance in each production unit
    pop01a <- vector(mode = "numeric", length = nYears * nRuns)
    pop02a <- vector(mode = "numeric", length = nYears * nRuns)
    pop03a <- vector(mode = "numeric", length = nYears * nRuns)
    pop04a <- vector(mode = "numeric", length = nYears * nRuns)
    pop05a <- vector(mode = "numeric", length = nYears * nRuns)
    pop06a <- vector(mode = "numeric", length = nYears * nRuns)
    pop07a <- vector(mode = "numeric", length = nYears * nRuns)
    pop08a <- vector(mode = "numeric", length = nYears * nRuns)
    pop02b <- vector(mode = "numeric", length = nYears * nRuns)
    pop03b <- vector(mode = "numeric", length = nYears * nRuns)
    pop04b <- vector(mode = "numeric", length = nYears * nRuns)
    pop05b <- vector(mode = "numeric", length = nYears * nRuns)
    pop06b <- vector(mode = "numeric", length = nYears * nRuns)
    pop07b <- vector(mode = "numeric", length = nYears * nRuns)
    pop08b <- vector(mode = "numeric", length = nYears * nRuns)
    pop09b <- vector(mode = "numeric", length = nYears * nRuns)
    pop10b <- vector(mode = "numeric", length = nYears * nRuns)
    pop11b <- vector(mode = "numeric", length = nYears * nRuns)
    pop12b <- vector(mode = "numeric", length = nYears * nRuns)
    pop13b <- vector(mode = "numeric", length = nYears * nRuns)
    pop14b <- vector(mode = "numeric", length = nYears * nRuns)
    pop15b <- vector(mode = "numeric", length = nYears * nRuns)
    pop16b <- vector(mode = "numeric", length = nYears * nRuns)
    pop17b <- vector(mode = "numeric", length = nYears * nRuns)
    pop18b <- vector(mode = "numeric", length = nYears * nRuns)
    pop19b <- vector(mode = "numeric", length = nYears * nRuns)
    pop20b <- vector(mode = "numeric", length = nYears * nRuns)
    pop21b <- vector(mode = "numeric", length = nYears * nRuns)

    return(
      list(
        years = years,
        climate_scen = climate_scen,
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
      )
    )
  }
  
  if (river == "androscoggin") {
    # Probability of using Sabattus River
    sabattus <- vector(mode = "numeric", length = nYears * nRuns)

    # Population abundance in each production unit
    pop01a <- vector(mode = "numeric", length = nYears * nRuns)
    pop02a <- vector(mode = "numeric", length = nYears * nRuns)
    pop03a <- vector(mode = "numeric", length = nYears * nRuns)
    pop04a <- vector(mode = "numeric", length = nYears * nRuns)
    pop05a <- vector(mode = "numeric", length = nYears * nRuns)
    pop06a <- vector(mode = "numeric", length = nYears * nRuns)
    pop07a <- vector(mode = "numeric", length = nYears * nRuns)
    pop08a <- vector(mode = "numeric", length = nYears * nRuns)
    pop09a <- vector(mode = "numeric", length = nYears * nRuns) 
    pop10a <- vector(mode = "numeric", length = nYears * nRuns) 
    
    pop4b <- vector(mode = "numeric", length = nYears * nRuns)
    pop5b <- vector(mode = "numeric", length = nYears * nRuns)

    return(
      list(
        years = years,
        climate_scen = climate_scen,
        sabattus = sabattus,
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
        pop09a = pop09a,
        pop10a = pop10a,
        pop4b = pop4b,
        pop5b = pop5b,
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
      )
    )
  }
  
  
}
