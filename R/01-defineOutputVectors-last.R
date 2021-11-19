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
defineOutputVectors_last <- function() {
  # Shared parameters

  # Empty container to hold year
  years <- vector(mode = "numeric")

  # Define empty vectors to hold results for outer loop
  climate_scen <- vector(mode = "character")

  # Age-structured spawning population
  spawners <- vector(mode = "list")

  # Proportion of repeat spawners in each age class
  pRepeats <- vector(mode = "list")

  # Catchment-wide population abundance
  populationSize <- vector(mode = "numeric")

  # Store the scale
  scalarVar <- vector(mode = "numeric")

  # Store the inputs for sensitivity analysis
  # Passage assumptions
  ptime <- vector(mode = "list")

  # Population demographics
  S.downstream <- vector(mode = "numeric")
  S.marine <- vector(mode = "numeric")
  F.inRiver <- vector(mode = "numeric")
  F.commercial <- vector(mode = "numeric")
  F.bycatch <- vector(mode = "numeric")
  popStart <- vector(mode = "numeric")
  p.female <- vector(mode = "numeric")
  p.female <- vector(mode = "numeric")
  S.prespawnM <- vector(mode = "numeric")
  S.postspawnM <- vector(mode = "numeric")
  S.prespawnF <- vector(mode = "numeric")
  S.postspawnF <- vector(mode = "numeric")
  S.juvenile <- vector(mode = "numeric")
  indirectM <- vector(mode = "numeric")
  latentM <- vector(mode = "numeric")

  # Individual traits
  # Entry dates
  b.Arr <- vector(mode = "numeric")
  r.Arr <- vector(mode = "numeric")
  # Spawning ATU
  ATUspawn1 <- vector(mode = "numeric")
  ATUspawn2 <- vector(mode = "numeric")
  # Spawning dates
  Dspawn1 <- vector(mode = "numeric")
  Dspawn2 <- vector(mode = "numeric")
  # Length at age
  # Females
  linF <- vector(mode = "numeric")
  kF <- vector(mode = "numeric")
  t0F <- vector(mode = "numeric")
  # Males
  linM <- vector(mode = "numeric")
  kM <- vector(mode = "numeric")
  t0M <- vector(mode = "numeric")

  # Lengths and mass
  b.length <- vector(mode = "numeric")
  r.length <- vector(mode = "numeric")

  # Fecundity
  spawnInt <- vector(mode = "numeric")
  batchSize <- vector(mode = "numeric")
  RAF <- vector(mode = "numeric")

  # Movement parameters
  s.Optim <- vector(mode = "numeric")
  d.Max <- vector(mode = "numeric")
  tortuosity <- vector(mode = "numeric")
  motivation <- vector(mode = "numeric")
  daily.move <- vector(mode = "numeric")


  if (river == "penobscot") {

    # Population abundance in each production unit
    # Population abundance below Milford Dam in main-stem
    LowerPop <- vector(mode = "numeric")
    # Population abundance between Orono and Stillwater dams
    OronoPop <- vector(mode = "numeric")
    # Population abundance between Stillwater Dam and Gilman Falls
    StillwaterPop <- vector(mode = "numeric")
    # Population abundance between Milford Dam and confluence
    MilfordPop <- vector(mode = "numeric")
    # Population abundance between West Enfield and Weldon dams
    EnfieldPop <- vector(mode = "numeric")
    # Population abundance above Weldon Dam
    WeldonPop <- vector(mode = "numeric")
    # Population abundance between Howland and Moosehead dams
    HowlandPop <- vector(mode = "numeric")
    # Population abundance between Moosehead and Browns Mill dams
    MoosePop <- vector(mode = "numeric")
    # Population abundance between Browns Mill and Guilford dams
    BrownsPop <- vector(mode = "numeric")
    # Population abundance above Guilford Dam
    GuilfordPop <- vector(mode = "numeric")

    # System-specific passage probabilities
    pStillUP <- vector(mode = "numeric")
    pStillD <- vector(mode = "numeric")
    pPiscUP <- vector(mode = "numeric")

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
    pBypassUS <- vector(mode = "numeric")
    pBypassDS <- vector(mode = "numeric")

    # Population abundance in each production unit
    popI <- vector(mode = "numeric")
    popII <- vector(mode = "numeric")
    popIII <- vector(mode = "numeric")
    popIV <- vector(mode = "numeric")
    popV <- vector(mode = "numeric")

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
    pSpill <- vector(mode = "numeric")

    # Northfield mountain take
    NorthFieldV <- vector(mode = "numeric")
    NorthFieldT <- vector(mode = "numeric")
    NorthFieldVa <- vector(mode = "numeric")
    NorthFieldTa <- vector(mode = "numeric")

    # Population abundance in each production unit
    popI <- vector(mode = "numeric")
    popII <- vector(mode = "numeric")
    popIII <- vector(mode = "numeric")
    popIV <- vector(mode = "numeric")
    popV <- vector(mode = "numeric")

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
    LowPop <- vector(mode = "numeric")
    ConPop <- vector(mode = "numeric")
    HolPop <- vector(mode = "numeric")
    SafPop <- vector(mode = "numeric")
    YorPop <- vector(mode = "numeric")
    JunPop <- vector(mode = "numeric")
    WesPop <- vector(mode = "numeric")
    WilPop <- vector(mode = "numeric")
    LocPop <- vector(mode = "numeric")
    ChaPop <- vector(mode = "numeric")
    SunPop <- vector(mode = "numeric")
    ChePop <- vector(mode = "numeric")
    NorPop <- vector(mode = "numeric")
    UnaPop <- vector(mode = "numeric")
    RocPop <- vector(mode = "numeric")
    ColPop <- vector(mode = "numeric")

    # Store the inputs for sensitivity analysis
    # Passage assumptions
    pJuniataUp <- vector(mode = "numeric")
    pWestBranchUp <- vector(mode = "numeric")
    pChemungUp <- vector(mode = "numeric")
    pNorthBranchUp <- vector(mode = "numeric")

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
    popI <- vector(mode = "numeric")
    popII <- vector(mode = "numeric")
    popIII <- vector(mode = "numeric")
    popIV <- vector(mode = "numeric")
    popV <- vector(mode = "numeric")
    popVI <- vector(mode = "numeric")
    popVII <- vector(mode = "numeric")

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
    sebasticook <- vector(mode = "numeric")

    # Population abundance in each production unit
    pop1a <- vector(mode = "numeric")
    pop2a <- vector(mode = "numeric")
    pop3a <- vector(mode = "numeric")
    pop4a <- vector(mode = "numeric")
    pop5a <- vector(mode = "numeric")
    pop1b <- vector(mode = "numeric")
    pop2b <- vector(mode = "numeric")

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
    pop01a <- vector(mode = "numeric")
    pop02a <- vector(mode = "numeric")
    pop03a <- vector(mode = "numeric")
    pop04a <- vector(mode = "numeric")
    pop05a <- vector(mode = "numeric")
    pop06a <- vector(mode = "numeric")
    pop07a <- vector(mode = "numeric")
    pop08a <- vector(mode = "numeric")
    pop02b <- vector(mode = "numeric")
    pop03b <- vector(mode = "numeric")
    pop04b <- vector(mode = "numeric")
    pop05b <- vector(mode = "numeric")
    pop06b <- vector(mode = "numeric")
    pop07b <- vector(mode = "numeric")
    pop08b <- vector(mode = "numeric")
    pop09b <- vector(mode = "numeric")
    pop10b <- vector(mode = "numeric")
    pop11b <- vector(mode = "numeric")
    pop12b <- vector(mode = "numeric")
    pop13b <- vector(mode = "numeric")
    pop14b <- vector(mode = "numeric")
    pop15b <- vector(mode = "numeric")
    pop16b <- vector(mode = "numeric")
    pop17b <- vector(mode = "numeric")
    pop18b <- vector(mode = "numeric")
    pop19b <- vector(mode = "numeric")
    pop20b <- vector(mode = "numeric")
    pop21b <- vector(mode = "numeric")

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
    sabattus <- vector(mode = "numeric")

    # Population abundance in each production unit
    pop01a <- vector(mode = "numeric")
    pop02a <- vector(mode = "numeric")
    pop03a <- vector(mode = "numeric")
    pop04a <- vector(mode = "numeric")
    pop05a <- vector(mode = "numeric")
    pop06a <- vector(mode = "numeric")
    pop07a <- vector(mode = "numeric")
    pop08a <- vector(mode = "numeric")
    pop09a <- vector(mode = "numeric") 
    pop10a <- vector(mode = "numeric") 
    
    pop4b <- vector(mode = "numeric")
    pop5b <- vector(mode = "numeric")

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
