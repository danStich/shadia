#' @title Assign upstream and downstream passage efficiencies
#'
#' @description Internal function passing user-defined
#' arguments from call to \code{\link{penobscotRiverModel}}
#' and other river models
#' to named objects used to define passage rates at
#' dam rkms in the individual-based migration model.
#'
#' Not intended to be called directly, but visible for
#' the sake of model transparency.
#'
#' @return A list of upstream and downstream dam passage
#' efficiencies at each dam in the hydro system.
#'
#' @export
#'

definePassageRates <- function(river) {
  if (river == "penobscot") {
    # Upstream passage rates
    Open <- 1.00
    Confluence <- 1.00
    OronoUp <- 1 # Orono upstream passage
    StillwaterUp <- 1 # Stillwater upstream passage
    GilmanUp <- 1 # Gilman Falls upstream passage
    MilfordUp <- up[1] # Milford upstream passage
    HowlandUp <- up[2] # Howland upstream passage
    WestEnfieldUp <- up[3] # West Enfield upstream passage
    BrownsMillUp <- up[4] # Browns Mill upstream passage
    MooseheadUp <- up[5] # Moosehead (Dover) passage
    GuilfordUp <- up[6] # Guilford Passage
    MattaceunkUp <- up[7] # Mattaceunk (Weldon) passage

    # Downstream passage efficiencies
    # Define downstream passage efficiencies at each of the dams
    OpenD <- 1.00 # Perfect passage open reaches
    GilmanD <- 1.00 * delay # Gilman passage
    StillwaterD <- d[1] * indirect * latent * delay # Stillwater passage
    OronoD <- d[2] * indirect * latent * delay # Orono passage
    MilfordD <- d[3] * indirect * latent * delay # Milford passage
    HowlandD <- d[4] * indirect * latent * delay # Howland passage
    WestEnfieldD <- d[5] * indirect * latent * delay # West Enfield passage
    BrownsMillD <- d[6] * indirect * latent * delay # Browns Mill passage
    MooseheadD <- d[7] * indirect * latent * delay # Moosehead (Dover) passage
    GuilfordD <- d[8] * indirect * latent * delay # Guilford Passage
    MattaceunkD <- d[9] * indirect * latent * delay # Mattaceunk (Weldon) passage

    # Make downstream survival probabilities for juveniles
    OpenDj <- 1.00 # Perfect passage open reaches
    GilmanDj <- 1.00 * delay # Gilman passage
    StillwaterDj <- dj[1] * indirect * latent * delay # Stillwater passage
    OronoDj <- dj[2] * indirect * latent * delay # Orono passage
    MilfordDj <- dj[3] * indirect * latent * delay # Milford passage
    HowlandDj <- dj[4] * indirect * latent * delay # Howland passage
    WestEnfieldDj <- dj[5] * indirect * latent * delay # West Enfield passage
    BrownsMillDj <- dj[6] * indirect * latent * delay # Browns Mill passage
    MooseheadDj <- dj[7] * indirect * latent * delay # Moosehead (Dover) passage
    GuilfordDj <- dj[8] * indirect * latent * delay # Guilford Passage
    MattaceunkDj <- dj[9] * indirect * latent * delay # Mattaceunk (Weldon) passage

    return(
      list(
        BrownsMillD = BrownsMillD,
        BrownsMillDj = BrownsMillDj,
        BrownsMillUp = BrownsMillUp,
        Confluence = Confluence,
        GilmanD = GilmanD,
        GilmanDj = GilmanDj,
        GilmanUp = GilmanUp,
        GuilfordD = GuilfordD,
        GuilfordDj = GuilfordDj,
        GuilfordUp = GuilfordUp,
        HowlandD = HowlandD,
        HowlandDj = HowlandDj,
        HowlandUp = HowlandUp,
        MattaceunkD = MattaceunkD,
        MattaceunkDj = MattaceunkDj,
        MattaceunkUp = MattaceunkUp,
        MilfordD = MilfordD,
        MilfordDj = MilfordDj,
        MilfordUp = MilfordUp,
        MooseheadD = MooseheadD,
        MooseheadDj = MooseheadDj,
        MooseheadUp = MooseheadUp,
        Open = Open,
        OpenD = OpenD,
        OpenDj = OpenDj,
        OronoD = OronoD,
        OronoDj = OronoDj,
        OronoUp = OronoUp,
        StillwaterD = StillwaterD,
        StillwaterDj = StillwaterDj,
        StillwaterUp = StillwaterUp,
        WestEnfieldD = WestEnfieldD,
        WestEnfieldDj = WestEnfieldDj,
        WestEnfieldUp = WestEnfieldUp
      )
    )
  }

  if (river == "merrimack") {
    # Upstream passage rates
    Open <- 1.00
    EssexUp <- up[1]
    PawtucketBypassUp <- up[2]
    PawtucketUp <- up[3]
    AmoskeagUp <- up[4]
    HooksetUp <- up[5]

    # Downstream passage efficiencies
    # Define downstream passage efficiencies at each of the dams
    OpenD <- 1.00
    EssexD <- d[1] * indirect * latent * delay
    PawtucketBypassD <- d[2] * indirect * latent * delay
    PawtucketD <- d[3] * indirect * latent * delay
    AmoskeagD <- d[4] * indirect * latent * delay
    HooksetD <- d[5] * indirect * latent * delay

    # Make downstream survival probabilities for juveniles
    EssexDj <- dj[1] * indirect * latent * delay
    PawtucketBypassDj <- dj[2] * indirect * latent * delay
    PawtucketDj <- dj[3] * indirect * latent * delay
    AmoskeagDj <- dj[4] * indirect * latent * delay
    HooksetDj <- dj[5] * indirect * latent * delay

    return(
      list(
        Open = Open,
        EssexUp = EssexUp,
        PawtucketUp = PawtucketUp,
        PawtucketBypassUp = PawtucketBypassUp,
        AmoskeagUp = AmoskeagUp,
        HooksetUp = HooksetUp,
        EssexD = EssexD,
        PawtucketBypassD = PawtucketBypassD,
        PawtucketD = PawtucketD,
        AmoskeagD = AmoskeagD,
        HooksetD = HooksetD,
        EssexDj = EssexDj,
        PawtucketBypassDj = PawtucketBypassDj,
        PawtucketDj = PawtucketDj,
        AmoskeagDj = AmoskeagDj,
        HooksetDj = HooksetDj
      )
    )
  }

  if (river == "connecticut") {
    # Upstream passage rates
    Open <- 1.00
    HolyokeUp <- up[1]
    CabotUp <- up[2]
    SpillwayUp <- up[3]
    GatehouseUp <- up[4]
    VernonUp <- up[4]

    # Downstream passage efficiencies
    # Define downstream passage efficiencies at each of the dams
    OpenD <- 1.00
    HolyokeD <- d[1] * indirect * latent * delay
    CabotD <- d[2] * indirect * latent * delay
    GatehouseD <- d[3] * indirect * latent * delay
    VernonD <- d[4] * indirect * latent * delay

    # Make downstream survival probabilities for juveniles.
    HolyokeDj <- dj[1] * indirect * latent * delay
    CabotDj <- dj[2] * indirect * latent * delay
    GatehouseDj <- dj[3] * indirect * latent * delay
    VernonDj <- dj[4] * indirect * latent * delay

    return(
      list(
        Open = Open,
        HolyokeUp = HolyokeUp,
        CabotUp = CabotUp,
        SpillwayUp = SpillwayUp,
        GatehouseUp = GatehouseUp,
        VernonUp = VernonUp,
        HolyokeD = HolyokeD,
        CabotD = CabotD,
        GatehouseD = GatehouseD,
        VernonD = VernonD,
        HolyokeDj = HolyokeDj,
        CabotDj = CabotDj,
        GatehouseDj = GatehouseDj,
        VernonDj = VernonDj
      )
    )
  }

  if (river == "susquehanna") {
    # Upstream passage rates
    Open <- 1.00
    ConowingoUp <- up[1]
    HoltwoodUp <- up[2]
    SafeHarborUp <- up[3]
    YorkHavenUp <- up[4]
    junConfluenceUp <- 1
    SunburyUp <- up[5]
    WilliamsportUp <- up[6]
    LockHavenUp <- up[7]
    NyUp <- 1
    ChaseHibbardUp <- up[8]
    RockBottomUp <- up[9]
    UnadillaReachUp <- 1
    ColliersvilleUp <- up[10]

    # Downstream passage efficiencies
    ConowingoD <- d[1] * indirect * latent * delay
    HoltwoodD <- d[2] * indirect * latent * delay
    SafeHarborD <- d[3] * indirect * latent * delay
    YorkHavenD <- d[4] * indirect * latent * delay
    junConfluenceD <- 1
    SunburyD <- d[5] * indirect * latent * delay
    WilliamsportD <- d[6] * indirect * latent * delay
    LockHavenD <- d[7] * indirect * latent * delay
    NyD <- 1
    ChaseHibbardD <- d[8] * indirect * latent * delay
    RockBottomD <- d[9] * indirect * latent * delay
    UnadillaReachD <- 1
    ColliersvilleD <- d[10] * indirect * latent * delay

    # Make downstream survival probabilities for juveniles.
    ConowingoDj <- dj[1] * indirect * latent * delay
    HoltwoodDj <- dj[2] * indirect * latent * delay
    SafeHarborDj <- dj[3] * indirect * latent * delay
    YorkHavenDj <- dj[4] * indirect * latent * delay
    junConfluenceDj <- 1
    SunburyDj <- dj[5] * indirect * latent * delay
    WilliamsportDj <- dj[6] * indirect * latent * delay
    LockHavenDj <- dj[7] * indirect * latent * delay
    NyDj <- 1
    ChaseHibbardDj <- dj[8] * indirect * latent * delay
    RockBottomDj <- dj[9] * indirect * latent * delay
    UnadillaReachDj <- 1
    ColliersvilleDj <- dj[10] * indirect * latent * delay

    return(
      list(
        Open = Open,
        ConowingoUp = ConowingoUp,
        HoltwoodUp = HoltwoodUp,
        SafeHarborUp = SafeHarborUp,
        YorkHavenUp = YorkHavenUp,
        junConfluenceUp = junConfluenceUp,
        SunburyUp = SunburyUp,
        WilliamsportUp = WilliamsportUp,
        LockHavenUp = LockHavenUp,
        NyUp = NyUp,
        ChaseHibbardUp = ChaseHibbardUp,
        RockBottomUp = RockBottomUp,
        UnadillaReachUp = UnadillaReachUp,
        ColliersvilleUp = ColliersvilleUp,
        ConowingoD = ConowingoD,
        HoltwoodD = HoltwoodD,
        SafeHarborD = SafeHarborD,
        YorkHavenD = YorkHavenD,
        junConfluenceD = junConfluenceD,
        SunburyD = SunburyD,
        WilliamsportD = WilliamsportD,
        LockHavenD = LockHavenD,
        NyD = NyD,
        ChaseHibbardD = ChaseHibbardD,
        RockBottomD = RockBottomD,
        UnadillaReachD = UnadillaReachD,
        ColliersvilleD = ColliersvilleD,
        ConowingoDj = ConowingoDj,
        HoltwoodDj = HoltwoodDj,
        SafeHarborDj = SafeHarborDj,
        YorkHavenDj = YorkHavenDj,
        junConfluenceDj = junConfluenceDj,
        SunburyDj = SunburyDj,
        WilliamsportDj = WilliamsportDj,
        LockHavenDj = LockHavenDj,
        NyDj = NyDj,
        ChaseHibbardDj = ChaseHibbardDj,
        RockBottomDj = RockBottomDj,
        UnadillaReachDj = UnadillaReachDj,
        ColliersvilleDj = ColliersvilleDj
      )
    )
  }

  if (river == "saco") {
    # Upstream passage rates
    Open <- 1.00
    cataractUp <- up[1]
    springUp <- up[2]
    skeltonUp <- up[3]
    barmillsUp <- up[4]
    buxtonUp <- up[5]
    bonnyUp <- up[6]

    # Downstream passage efficiencies
    # Define downstream passage efficiencies at each of the dams
    OpenD <- 1.00
    cataractD <- d[1] * indirect * latent * delay
    springD <- d[2] * indirect * latent * delay
    skeltonD <- d[3] * indirect * latent * delay
    barmillsD <- d[4] * indirect * latent * delay
    buxtonD <- d[5] * indirect * latent * delay
    bonnyD <- d[6] * indirect * latent * delay

    # Make downstream survival probabilities for juveniles
    cataractDj <- dj[1] * indirect * latent * delay
    springDj <- dj[2] * indirect * latent * delay
    skeltonDj <- dj[3] * indirect * latent * delay
    barmillsDj <- dj[4] * indirect * latent * delay
    buxtonDj <- dj[5] * indirect * latent * delay
    bonnyDj <- dj[6] * indirect * latent * delay

    return(
      list(
        Open = Open,
        cataractUp = cataractUp,
        springUp = springUp,
        skeltonUp = skeltonUp,
        barmillsUp = barmillsUp,
        buxtonUp = buxtonUp,
        bonnyUp = bonnyUp,
        cataractD = cataractD,
        springD = springD,
        skeltonD = skeltonD,
        barmillsD = barmillsD,
        buxtonD = buxtonD,
        bonnyD = bonnyD,
        cataractDj = cataractDj,
        springDj = springDj,
        skeltonDj = skeltonDj,
        barmillsDj = barmillsDj,
        buxtonDj = buxtonDj,
        bonnyDj = bonnyDj
      )
    )
  }

  if (river == "kennebec") {
    # Upstream passage rates
    Open <- 1.00
    lockwoodUp <- up[1]
    hydrokennUp <- up[2]
    shawmutUp <- up[3]
    westonUp <- up[4]
    bentonUp <- up[5]
    burnhamUp <- up[6]

    # Downstream passage efficiencies
    # Define downstream passage efficiencies at each of the dams
    OpenD <- 1.00
    lockwoodD <- d[1] * indirect * latent * delay
    hydrokennD <- d[2] * indirect * latent * delay
    shawmutD <- d[3] * indirect * latent * delay
    westonD <- d[4] * indirect * latent * delay
    bentonD <- d[5] * indirect * latent * delay
    burnhamD <- d[6] * indirect * latent * delay

    # Make downstream survival probabilities for juveniles
    # Unused. Moved to separate calculations in the
    # Connecticut River Model. All runs in the Penobscot
    # River to date have used equal performance standards.
    # Will update this model to use the same approach.
    lockwoodDj <- dj[1] * indirect * latent * delay
    hydrokennDj <- dj[2] * indirect * latent * delay
    shawmutDj <- dj[3] * indirect * latent * delay
    westonDj <- dj[4] * indirect * latent * delay
    bentonDj <- dj[5] * indirect * latent * delay
    burnhamDj <- dj[6] * indirect * latent * delay

    return(
      list(
        Open = Open,
        lockwoodUp = lockwoodUp,
        hydrokennUp = hydrokennUp,
        shawmutUp = shawmutUp,
        westonUp = westonUp,
        bentonUp = bentonUp,
        burnhamUp = burnhamUp,
        OpenD = OpenD,
        lockwoodD = lockwoodD,
        hydrokennD = hydrokennD,
        shawmutD = shawmutD,
        westonD = westonD,
        bentonD = bentonD,
        burnhamD = burnhamD,
        lockwoodDj = lockwoodDj,
        hydrokennDj = hydrokennDj,
        shawmutDj = shawmutDj,
        westonDj = westonDj,
        bentonDj = bentonDj,
        burnhamDj = burnhamDj
      )
    )
  }

  if (river == "hudson") {
    # Upstream passage rates
    Open <- 1.00
    federalUp <- up[1]
    for (i in 1:length(grep("C", names(upstream)))) {
      assign(paste0(names(upstream)[i + 1], "Up"), up[i + 1])
    }

    for (i in 1:length(grep("E", names(upstream)))) {
      assign(paste0(names(upstream)[i + 7], "Up"), up[i + 7])
    }

    # Downstream passage efficiencies
    # Define downstream passage efficiencies at each of the dams
    other <- indirect * latent * delay
    OpenD <- 1.00
    federalD <- d[1] * other
    for (i in 1:length(grep("C", names(downstream)))) {
      assign(paste0(names(downstream)[i + 1], "D"), d[i + 1] * other)
    }

    for (i in 1:length(grep("E", names(downstream)))) {
      assign(paste0(names(downstream)[i + 7], "D"), d[i + 7] * other)
    }

    # Make downstream survival probabilities for juveniles
    OpenDj <- 1.00
    federalDj <- d[1] * other
    for (i in 1:length(grep("C", names(downstream_juv)))) {
      assign(paste0(names(downstream_juv)[i + 1], "Dj"), dj[i + 1] * other)
    }

    for (i in 1:length(grep("E", names(downstream_juv)))) {
      assign(paste0(names(downstream_juv)[i + 7], "Dj"), dj[i + 7] * other)
    }


    return(
      list(
        Open = Open,
        federalUp = federalUp,
        C01Up = C01Up,
        C02Up = C02Up,
        C03Up = C03Up,
        C04Up = C04Up,
        C05Up = C05Up,
        C06Up = C06Up,
        E02Up = E02Up,
        E03Up = E03Up,
        E04Up = E04Up,
        E05Up = E05Up,
        E06Up = E06Up,
        E07Up = E07Up,
        E08Up = E08Up,
        E09Up = E09Up,
        E10Up = E10Up,
        E11Up = E11Up,
        E12Up = E12Up,
        E13Up = E13Up,
        E14Up = E14Up,
        E15Up = E15Up,
        E16Up = E16Up,
        E17Up = E17Up,
        E18Up = E18Up,
        E19Up = E19Up,
        E20Up = E20Up,
        OpenD = OpenD,
        federalD = federalD,
        C01D = C01D,
        C02D = C02D,
        C03D = C03D,
        C04D = C04D,
        C05D = C05D,
        C06D = C06D,
        E02D = E02D,
        E03D = E03D,
        E04D = E04D,
        E05D = E05D,
        E06D = E06D,
        E07D = E07D,
        E08D = E08D,
        E09D = E09D,
        E10D = E10D,
        E11D = E11D,
        E12D = E12D,
        E13D = E13D,
        E14D = E14D,
        E15D = E15D,
        E16D = E16D,
        E17D = E17D,
        E18D = E18D,
        E19D = E19D,
        E20D = E20D,
        OpenDj = OpenDj,
        federalDj = federalDj,
        C01Dj = C01Dj,
        C02Dj = C02Dj,
        C03Dj = C03Dj,
        C04Dj = C04Dj,
        C05Dj = C05Dj,
        C06Dj = C06Dj,
        E02Dj = E02Dj,
        E03Dj = E03Dj,
        E04Dj = E04Dj,
        E05Dj = E05Dj,
        E06Dj = E06Dj,
        E07Dj = E07Dj,
        E08Dj = E08Dj,
        E09Dj = E09Dj,
        E10Dj = E10Dj,
        E11Dj = E11Dj,
        E12Dj = E12Dj,
        E13Dj = E13Dj,
        E14Dj = E14Dj,
        E15Dj = E15Dj,
        E16Dj = E16Dj,
        E17Dj = E17Dj,
        E18Dj = E18Dj,
        E19Dj = E19Dj,
        E20Dj = E20Dj
      )
    )
  }
  
  if (river == "androscoggin") {
    # Upstream passage rates
    Open <- 1.00
    brunswick <- up[1]
    pejepscot <- up[2]
    worumbo <- up[3]
    lbarker <- up[4]
    ubarker <- up[5]
    littlefield <- up[6]                                 
    hackett <- up[7]
    marcal <- up[8]
    welchville <- up[9]
    paris <- up[10]
    farwell <- up[11]
    fortier <- up[12]
    
    # Downstream passage efficiencies
    # Define downstream passage efficiencies at each of the dams
    OpenD <- 1.00
    brunswickD <- d[1] * indirect * latent * delay
    pejepscotD <- d[2] * indirect * latent * delay
    worumboD <- d[3] * indirect * latent * delay
    lbarkerD <- d[4] * indirect * latent * delay
    ubarkerD <- d[5] * indirect * latent * delay
    littlefieldD <- d[6] * indirect * latent * delay                                 
    hackettD <- d[7] * indirect * latent * delay
    marcalD <- d[8] * indirect * latent * delay
    welchvilleD <- d[9] * indirect * latent * delay
    parisD <- d[10] * indirect * latent * delay
    farwellD <- d[11] * indirect * latent * delay
    fortierD <- d[12] * indirect * latent * delay

    # Make downstream survival probabilities for juveniles
    # Unused. Moved to separate calculations in the
    # Connecticut River Model. All runs in the Penobscot
    # River to date have used equal performance standards.
    # Will update this model to use the same approach.
    OpenD <- 1.00
    brunswickDj <- dj[1] * indirect * latent * delay
    pejepscotDj <- dj[2] * indirect * latent * delay
    worumboDj <- dj[3] * indirect * latent * delay
    lbarkerDj <- dj[4] * indirect * latent * delay
    ubarkerDj <- dj[5] * indirect * latent * delay
    littlefieldDj <- dj[6] * indirect * latent * delay                                 
    hackettDj <- dj[7] * indirect * latent * delay
    marcalDj <- dj[8] * indirect * latent * delay
    welchvilleDj <- dj[9] * indirect * latent * delay
    parisDj <- dj[10] * indirect * latent * delay
    farwellDj <- dj[11] * indirect * latent * delay
    fortierDj <- dj[12] * indirect * latent * delay

    return(
      list(
        Open = Open,
        brunswickUp = brunswick,
        pejepscotUp = pejepscot,
        worumboUp = worumbo,
        lbarkerUp = lbarker,
        ubarkerUp = ubarker,
        littlefieldUp = littlefield,
        hackettUp = hackett,
        marcalUp = marcal,
        welchvilleUp = welchville,
        parisUp = paris,
        farwellUp = farwell,
        fortierUp = fortier,
        OpenD = OpenD,
        brunswickD = brunswickD,
        pejepscotD = pejepscotD,
        worumboD = worumboD,
        lbarkerD = lbarkerD,
        ubarkerD = ubarkerD,
        littlefieldD = littlefieldD,
        hackettD = hackettD,
        marcalD = marcalD,
        welchvilleD = welchvilleD,
        parisD = parisD,
        farwellD = farwellD,
        fortierD = fortierD,
        brunswickDj = brunswickDj,
        pejepscotDj = pejepscotDj,
        worumboDj = worumboDj,
        lbarkerDj = lbarkerDj,
        ubarkerDj = ubarkerDj,
        littlefieldDj = littlefieldDj,
        hackettDj = hackettDj,
        marcalDj = marcalDj,
        welchvilleDj = welchvilleDj,
        parisDj = parisDj,
        farwellDj = farwellDj,
        fortierDj = fortierDj
      )
    )
  }  
  
}
