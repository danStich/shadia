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

definePassageRates <- function(){

if(river=='penobscot'){  
  # Upstream passage rates
  Open <- 1.00
  Confluence <- 1.00 * fB
  OronoUp <- 1 * fB             # Orono upstream passage
  StillwaterUp <- 1             # Stillwater upstream passage
  GilmanUp <- 1                 # Gilman Falls upstream passage
  MilfordUp <- up[1] * fB       # Milford upstream passage
  HowlandUp <- up[2] * fB       # Howland upstream passage
  WestEnfieldUp <- up[3] * fB   # West Enfield upstream passage
  BrownsMillUp <- up[4] * fB    # Browns Mill upstream passage
  MooseheadUp <- up[5] * fB     # Moosehead (Dover) passage
  GuilfordUp <- up[6] * fB      # Guilford Passage
  MattaceunkUp <- up[7] * fB    # Mattaceunk (Weldon) passage

  # Downstream passage efficiencies
  # Define downstream passage efficiencies at each of the dams
  OpenD <- 1.00                                      # Perfect passage open reaches
  GilmanD <- 1.00 * delay                            # Gilman passage
  StillwaterD <- d[1] * indirect * latent * delay    # Stillwater passage
  OronoD <- d[2] * indirect * latent * delay         # Orono passage
  MilfordD <- d[3] * indirect * latent * delay       # Milford passage
  HowlandD <- d[4] * indirect * latent * delay       # Howland passage
  WestEnfieldD <- d[5] * indirect * latent * delay   # West Enfield passage
  BrownsMillD <- d[6] * indirect * latent * delay    # Browns Mill passage
  MooseheadD <- d[7] * indirect * latent * delay     # Moosehead (Dover) passage
  GuilfordD <- d[8] * indirect * latent * delay      # Guilford Passage
  MattaceunkD <- d[9] * indirect * latent * delay    # Mattaceunk (Weldon) passage

  # Make downstream survival probabilities for juveniles
  # Unused. Moved to separate calculations in the 
  # Connecticut River Model. All runs in the Penobscot
  # River to date have used equal performance standards.
  # Will update this model to use the same approach.
  GilmanDj <- 1.00                            # Gilman passage
  StillwaterDj <- StillwaterD * jReduction    # Stillwater passage
  OronoDj <- OronoD * jReduction              # Orono passage
  MilfordDj <- MilfordD * jReduction          # Milford passage
  HowlandDj <- HowlandD * jReduction          # Howland passage
  WestEnfieldDj <- WestEnfieldD * jReduction  # West Enfield passage
  BrownsMillDj <- BrownsMillD * jReduction    # Browns Mill passage
  MooseheadDj <- MooseheadD * jReduction      # Moosehead (Dover) passage
  GuilfordDj <- GuilfordD * jReduction        # Guilford Passage
  MattaceunkDj <- MattaceunkD * jReduction    # Mattaceunk (Weldon) passage

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
  
if(river=='merrimack'){  
  # Upstream passage rates
  Open <- 1.00
  EssexUp <- up[1] * fB
  PawtucketBypassUp <- up[2] * fB
  PawtucketUp <- up[3] * fB
  AmoskeagUp <- up[4] * fB
  HooksetUp <- up[5] * fB

  # Downstream passage efficiencies
  # Define downstream passage efficiencies at each of the dams
  OpenD <- 1.00
  EssexD <- d[1] * indirect * latent * delay
  PawtucketBypassD <- d[2] * indirect * latent * delay
  PawtucketD <- d[3] * indirect * latent * delay
  AmoskeagD <- d[4] * indirect * latent * delay
  HooksetD <- d[5] * indirect * latent * delay
  
  # Make downstream survival probabilities for juveniles
  # Unused. Moved to separate calculations in the 
  # Connecticut River Model. All runs in the Penobscot
  # River to date have used equal performance standards.
  # Will update this model to use the same approach.
  EssexDj <- d[1] * indirect * latent * delay
  PawtucketBypassDj <- d[2] * indirect * latent * delay
  PawtucketDj <- d[3] * indirect * latent * delay
  AmoskeagDj <- d[4] * indirect * latent * delay
  HooksetDj <- d[5] * indirect * latent * delay  
  
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
   
if(river=='connecticut'){  
  # Upstream passage rates
  Open <- 1.00
  HolyokeUp <- up[1] * fB
  CabotUp <- up[2] * fB
  SpillwayUp <- up[3] * fB
  GatehouseUp <- up[4] * fB
  VernonUp <- up[4] * fB
  
  # Downstream passage efficiencies
  # Define downstream passage efficiencies at each of the dams
  OpenD <- 1.00
  HolyokeD <- d[1] * indirect * latent * delay
  CabotD <- d[2] * indirect * latent * delay
  GatehouseD <- d[3] * indirect * latent * delay
  VernonD <- d[4] * indirect * latent * delay
  
  # Make downstream survival probabilities for juveniles.
  # Unused. Moved to separate calculations in the 
  # Connecticut River Model. All runs in the Penobscot
  # River to date have used equal performance standards.
  # Will update this model to use the same approach.
  HolyokeDj <- d[1] * indirect * latent * delay
  CabotDj <- d[2] * indirect * latent * delay
  GatehouseDj <- d[3] * indirect * latent * delay
  VernonDj <- d[4] * indirect * latent * delay
  
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
  
  
if(river=='susquehanna'){  
  # Upstream passage rates
  Open <- 1.00
  ConowingoUp <- up[1] * fB
  HoltwoodUp <- up[2]
  SafeHarborUp <- up[3] * fB
  YorkHavenUp <- up[4] * fB
  junConfluenceUp <- 1
  SunburyUp <- up[5] * fB
  WilliamsportUp <- up[6] * fB
  LockHavenUp <- up[7] * fB
  NyUp <- 1
  ChaseHibbardUp <- up[8] * fB
  RockBottomUp <- up[9] * fB
  UnadillaReachUp <- 1
  ColliersvilleUp <- up[10] * fB

  # Downstream passage efficiencies
  ConowingoD <- d[1] * indirect * latent * delay
  HoltwoodD <- d[2] * indirect * latent * delay
  SafeHarborD <- d[3] * indirect * latent * delay
  YorkHavenD <- d[4] * indirect * latent * delay
  junConfluenceD <- 1
  SunburyD <- d[5]* indirect * latent * delay
  WilliamsportD <- d[6] * indirect * latent * delay
  LockHavenD <- d[7] * indirect * latent * delay
  NyD <- 1
  ChaseHibbardD <- d[8] * indirect * latent * delay
  RockBottomD <- d[9]  * indirect * latent * delay
  UnadillaReachD <- 1
  ColliersvilleD <- d[10] * indirect * latent * delay

  # Make downstream survival probabilities for juveniles
  # Unused. Moved to separate calculations in the 
  # Connecticut River Model. All runs in the Penobscot
  # River to date have used equal performance standards.
  # Will update this model to use the same approach.
  ConowingoDj <- d[1] * indirect * latent * delay
  HoltwoodDj <- d[2] * indirect * latent * delay
  SafeHarborDj <- d[3] * indirect * latent * delay
  YorkHavenDj <- d[4] * indirect * latent * delay
  junConfluenceDj <- 1
  SunburyDj <- d[5]* indirect * latent * delay
  WilliamsportDj <- d[6] * indirect * latent * delay
  LockHavenDj <- d[7] * indirect * latent * delay
  NyDj <- 1
  ChaseHibbardDj <- d[8] * indirect * latent * delay
  RockBottomDj <- d[9]  * indirect * latent * delay
  UnadillaReachDj <- 1
  ColliersvilleDj <- d[10] * indirect * latent * delay


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
  
  
if(river=='saco'){  
  # Upstream passage rates
  Open <- 1.00
  catUp <- up[1] * fB
  springUp <- up[2] * fB
  skeltonUp <- up[3] * fB
  barmillsUp <- up[4] * fB
  buxtonUp <- up[5] * fB
  bonnyUp <- up[6] * fB

  # Downstream passage efficiencies
  # Define downstream passage efficiencies at each of the dams
  OpenD <- 1.00
  catD <- d[1] * indirect * latent * delay
  springD <- d[2] * indirect * latent * delay
  skeltonD <- d[3] * indirect * latent * delay
  barmillsD <- d[4] * indirect * latent * delay
  buxtonD <- d[5] * indirect * latent * delay
  bonnyD <- d[6] * indirect * latent * delay
  
  # Make downstream survival probabilities for juveniles
  # Unused. Moved to separate calculations in the 
  # Connecticut River Model. All runs in the Penobscot
  # River to date have used equal performance standards.
  # Will update this model to use the same approach.
  catDj <- d[1] * indirect * latent * delay
  springDj <- d[2] * indirect * latent * delay
  skeltonDj <- d[3] * indirect * latent * delay
  barmillsDj <- d[4] * indirect * latent * delay
  buxtonDj <- d[5] * indirect * latent * delay  
  bonnyDj <- d[6] * indirect * latent * delay  

  return(
    list(
      Open = Open,
      catUp = catUp,
      springUp = springUp,
      skeltonUp = skeltonUp,
      barmillsUp = barmillsUp,
      buxtonUp = buxtonUp,
      bonnyUp = bonnyUp,
      catD = catD,
      springD = springD,
      skeltonD = skeltonD,
      barmillsD = barmillsD,
      buxtonD = buxtonD,
      bonnyD = bonnyD,
      catDj = catDj,
      springDj = springDj,
      skeltonDj = skeltonDj,
      barmillsDj = barmillsDj,
      buxtonDj = buxtonDj,
      bonnyDj = bonnyDj      
    )
  )
}  
  
  
  
}  