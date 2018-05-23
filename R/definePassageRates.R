#' @title Assign upstream and downstream passage efficiencies
#' 
#' @description Internal function passing user-defined
#' arguments from call to \code{\link{penobscotRiverModel}}
#' to named objects used to define passage rates at 
#' dam rkms in the individual-based migration model.
#' 
#' Not intended to be called directly, but visible for
#' the sake of model transparency.
#' 
#' @return A list of upstream and downstream dam passage
#' efficiencies at each dam in the hydro system. 
#' 

definePassageRates <- function(){

  # Upstream passage rates
  Open = 1.00
  Confluence = 1.00 * fB
  OronoUp = 1 * fB             # Orono upstream passage
  StillwaterUp = 1             # Stillwater upstream passage
  GilmanUp = 1                 # Gilman Falls upstream passage
  MilfordUp = up[1] * fB       # Milford upstream passage
  HowlandUp = up[2] * fB       # Howland upstream passage
  WestEnfieldUp = up[3] * fB   # West Enfield upstream passage
  BrownsMillUp = up[4] * fB    # Browns Mill upstream passage
  MooseheadUp = up[5] * fB     # Moosehead (Dover) passage
  GuilfordUp = up[6] * fB      # Guilford Passage
  MattaceunkUp = up[7] * fB    # Mattaceunk (Weldon) passage

  # Downstream passage efficiencies
  # Define downstream passage efficiencies at each of the dams
  OpenD = 1.00 * delay                              # Perfect passage open reaches
  GilmanD = 1.00 * delay                            # Gilman passage
  StillwaterD = d[1] * indirect * latent * delay    # Stillwater passage
  OronoD = d[2] * indirect * latent * delay         # Orono passage
  MilfordD = d[3] * indirect * latent * delay       # Milford passage
  HowlandD = d[4] * indirect * latent * delay       # Howland passage
  WestEnfieldD = d[5] * indirect * latent * delay   # West Enfield passage
  BrownsMillD = d[6] * indirect * latent * delay    # Browns Mill passage
  MooseheadD = d[7] * indirect * latent * delay     # Moosehead (Dover) passage
  GuilfordD = d[8] * indirect * latent * delay      # Guilford Passage
  MattaceunkD = d[9] * indirect * latent * delay    # Mattaceunk (Weldon) passage

  # Make downstream survival probabilities for juveniles
  # Unused. Moved to separate calculations in the 
  # Connecticut River Model. All runs in the Penobscot
  # River to date have used equal performance standards.
  # Will update this model to use the same approach.
  GilmanDj = 1.00                            # Gilman passage
  StillwaterDj = StillwaterD * jReduction    # Stillwater passage
  OronoDj = OronoD * jReduction              # Orono passage
  MilfordDj = MilfordD * jReduction          # Milford passage
  HowlandDj = HowlandD * jReduction          # Howland passage
  WestEnfieldDj = WestEnfieldD * jReduction  # West Enfield passage
  BrownsMillDj = BrownsMillD * jReduction    # Browns Mill passage
  MooseheadDj = MooseheadD * jReduction      # Moosehead (Dover) passage
  GuilfordDj = GuilfordD * jReduction        # Guilford Passage
  MattaceunkDj = MattaceunkD * jReduction    # Mattaceunk (Weldon) passage

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