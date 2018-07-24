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
pRepeats <- do.call("rbind", lapply(pRepeats, unlist))
colnames(pRepeats) <- paste('pRepeat_', colnames(pRepeats), sep = '')

# Unlist and stack age-structured spawning population into a useable output
spawners <- do.call("rbind", lapply(spawners, unlist))
colnames(spawners) <- paste(colnames(spawners), 'N', sep = '_')

# Rescale population size based on reduction factor at start of script
populationSize <- populationSize

if(river=='penobscot'){
# Collect inputs and outputs into a single object for file write
res <- data.frame(
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
  latentM,
  pRepeats,
  ceiling(populationSize),
  ceiling(LowerPop),
  #OronoPop,
  #StillwaterPop,
  ceiling(MilfordPop),
  ceiling(EnfieldPop),
  ceiling(WeldonPop),
  ceiling(HowlandPop),
  ceiling(MoosePop),
  ceiling(BrownsPop),
  ceiling(GuilfordPop)#,
)

names(res)<-c(
  "year",
  "time",
  #"orono_up",
  #"stillwater_up",
  #"gilman_up",
  "milford_up",
  "howland_up",
  "westenfield_up",
  "brownsmill_up",
  "moosehead_up",
  "guilford_up",
  "weldon_up",
  "orono_down",
  "stillwater_down",
  #"gilman_down",
  "milford_down",
  "howland_down",
  "westenfield_down",
  "brownsmill_down",
  "moosehead_down",
  "guilford_down",
  "weldon_down",
  "inriverF",
  "commercialF",
  "bycatchF",
  "indirect",
  "latent",
  "pRepeat_Age1",
  "pRepeat_Age2",
  "pRepeat_Age3",
  "pRepeat_Age4",
  "pRepeat_Age5",
  "pRepeat_Age6",
  "pRepeat_Age7",
  "pRepeat_Age8",
  "pRepeat_Age9",
  "populationSize",
  "N_pu1A2A",
  #"N_pu1C",
  #"N_pu2C",
  "N_pu3A",
  "N_pu4A",
  "N_pu5A",
  "N_pu1B",
  "N_pu2B",
  "N_pu3B",
  "N_pu4B"
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
}

if(river=='merrimack'){
# Collect inputs and outputs into a single object for file write
res <- data.frame(
  years,
  ptime,
  EssUp,
  PawUp,
  AmosUp,
  HookUp,
  EssD,
  PawD,
  AmosD,
  HookD,
  F.inRiver,
  F.commercial,
  F.bycatch,
  indirectM,
  latentM,
  pRepeats,
  ceiling(populationSize),
  ceiling(popI),
  ceiling(popII),
  ceiling(popIII),
  ceiling(popIV),
  ceiling(popV)
)

names(res)<-c(
  "year",
  "time",
  "EssUp",
  "PawUp",
  "AmosUp",
  "HookUp",
  "EssD",
  "PawD",
  "AmosD",
  "HookD",  
  "inriverF",
  "commercialF",
  "bycatchF",
  "indirect",
  "latent",
  "pRepeat_Age1",
  "pRepeat_Age2",
  "pRepeat_Age3",
  "pRepeat_Age4",
  "pRepeat_Age5",
  "pRepeat_Age6",
  "pRepeat_Age7",
  "pRepeat_Age8",
  "pRepeat_Age9",
  "pRepeat_Age10",  
  "pRepeat_Age11",  
  "populationSize",
  "N_I",
  "N_II",
  "N_III",
  "N_IV",
  "N_V"
)

# Collect variables for sensitivity analysis and save them out
# sens = data.frame(
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
}














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
