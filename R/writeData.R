# writeData.R

# Can probably get rid of this file once
# the package is compiled because it
# will run only in parallel?

# DATA WRITE --------------------------------------------------------------
writeData <- function(){
  
# Post-simulation data manipulation
if (useTictoc) {
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
  OrUp,
  StUp,
  GilmUp,
  MdUp,
  HdUp,
  WEnfUp,
  BMillUp,
  MooseUp,
  GuilfUp,
  MattUp,
  OrD,
  StD,
  GilmD,
  MdD,
  HdD,
  WEnfD,
  BMillD,
  MooseD,
  GuilfD,
  MattD,
  indirectM,
  fallback,
  latentM,
  juvReduction,
  populationSize,
  spawners,
  pRepeats,
  LowerPop,
  OronoPop,
  StillwaterPop,
  MilfordPop,
  EnfieldPop,
  WeldonPop,
  HowlandPop,
  MoosePop,
  BrownsPop,
  GuilfordPop,
  scalarVar,
  scen
)

# Collect variables for sensitivity analysis and save them out
sens = data.frame(
  pStillUP,
  pStillD,
  pPiscUP,
  S.downstream,
  S.marine,
  F.inRiver,
  F.commercial,
  F.bycatch,
  popStart,
  p.female,
  S.prespawnM,
  S.postspawnM,
  S.prespawnF,
  S.postspawnF,
  S.juvenile,
  t.stoch,
  t.RegrInt,
  t.RegrSlp,
  b.ArrRegrInt,
  b.ArrRegrSlp,
  r.ArrRegrInt,
  r.ArrRegrSlp,
  b.Arr,
  r.Arr,
  ATUspawn1,
  ATUspawn2,
  Dspawn1,
  Dspawn2,
  linF,
  kF,
  t0F,
  linM,
  kM,
  t0M,
  lwF.alpha,
  lwF.beta,
  lwM.alpha,
  lwM.beta,
  b.length,
  r.length,
  spawnInt,
  batchSize,
  resTime,
  s.Optim,
  d.Max,
  tortuosity,
  motivation,
  daily.move,
  ptime
)

# Write the inputs and outputs to a text file that can be read into R
#writeData(filename)

if (useTictoc) {
  toc() #("data write")
}

}