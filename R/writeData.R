#' @title Write simulation results
#' 
#' @description Internal function used to collect and
#' return all model inputs and relevant model
#' outputs (e.g., population size) from all k iterations 
#' of n years to the temporary environment 
#' in \code{\link{penobscotRiverModel}}()
#' 
#' Not intended to be called directly, but visible 
#' nonetheless.
#' 
#' @return A list of results.
#' 
#' @export
#' 
writeData <- function(){

# DATA WRITE --------------------------------------------------------------
# Post-simulation data manipulation
if (.shadia$useTictoc) {
  tic("data write")
}

# Unlist and stack proportion of repeat spawners in each age for writing
pRepeats = do.call("rbind", lapply(pRepeats, unlist))
colnames(pRepeats) = paste('pRepeat_', colnames(pRepeats), sep = '')

# Unlist and stack age-structured spawning population into a useable output
spawners = do.call("rbind", lapply(spawners, unlist))
colnames(spawners) = paste(colnames(spawners), 'N', sep = '_')

# Rescale population size based on reduction factor at start of script
populationSize = populationSize

# Collect inputs and outputs into a single object for file write
res = data.frame(
  years,
  ptime,
  #OrUp,
  #StUp,
  #GilmUp,
  MdUp,
  HdUp,
  WEnfUp,
  BMillUp,
  MooseUp,
  GuilfUp,
  MattUp,
  OrD,
  StD,
  #GilmD,
  MdD,
  HdD,
  WEnfD,
  BMillD,
  MooseD,
  GuilfD,
  MattD,
  F.inRiver,
  F.commercial,
  F.bycatch,
  indirectM,
  #fallback,
  latentM,
  #juvReduction,
  populationSize,
  #spawners,
  pRepeats,
  LowerPop,
  #OronoPop,
  #StillwaterPop,
  MilfordPop,
  EnfieldPop,
  WeldonPop,
  HowlandPop,
  MoosePop,
  BrownsPop,
  GuilfordPop#,

)

# Collect variables for sensitivity analysis and save them out
# sens = data.frame(
#   pStillUP,
#   pStillD,
#   pPiscUP,
#   S.downstream,
#   S.marine,
#   popStart,
#   p.female,
#   S.prespawnM,
#   S.postspawnM,
#   S.prespawnF,
#   S.postspawnF,
#   S.juvenile,
#   t.stoch,
#   t.RegrInt,
#   t.RegrSlp,
#   b.ArrRegrInt,
#   b.ArrRegrSlp,
#   r.ArrRegrInt,
#   r.ArrRegrSlp,
#   b.Arr,
#   r.Arr,
#   ATUspawn1,
#   ATUspawn2,
#   Dspawn1,
#   Dspawn2,
#   linF,
#   kF,
#   t0F,
#   linM,
#   kM,
#   t0M,
#   lwF.alpha,
#   lwF.beta,
#   lwM.alpha,
#   lwM.beta,
#   b.length,
#   r.length,
#   spawnInt,
#   batchSize,
#   resTime,
#   s.Optim,
#   d.Max,
#   tortuosity,
#   motivation,
#   daily.move,
#   habStoch,
#   scalarVar,
#   scen
# )

# Write the inputs and outputs to a text file that can be read into R
#writeData(filename)

if (.shadia$useTictoc) {
  toc() #("data write")
}

return(#list(		
res	= res#,
#sens = sens
)#)		

}
