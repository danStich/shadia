# setParameters.R

setParameters <- function(){

# control progress indicators and timing
useTictoc <- FALSE
useProgress <- FALSE

# Package install and load
library(plyr)
library(MASS)
library(Rcpp)
library(zoo)
library(methods)

# For profiling and benchmarking
if (useTictoc) {
    #library(profvis)
    library(tictoc)
    #library(microbenchmark)
}

# ---------

# JMS: Parameters to control sampling and data loading
# TRUE == perform loads and sampling
# FALSE == use saved .RData to populate variables

doInnerSampling <- TRUE
doOuterSampling <- TRUE

# ---------

# JMS: set sampling and data load filenames
innerLoopSamplingSource <- 'inner-loop-sampling.R'
innerLoopSamplingRData <- 'inner-loop-sampling.RData'

outerLoopSamplingRData <- 'outer-loop-sampling.RData'
outerLoopSamplingSource <- 'outer-loop-sampling.R'

# ---------

# set the output directory and output filenames
# baseDirectory = getwd()

# directory = paste(baseDirectory,'outfiles',sep='/')
# filename = paste(directory, "pnrProfile", sep = "/")

# verify directory exists
# if (dir.exists(directory)) {
#   print(paste('writing to: ',directory))
# } else {
#   stop("exiting; output directory not found...")
# }


return(list(
useTictoc = useTictoc,
useProgress = useProgress,
doInnerSampling = doInnerSampling,
doOuterSampling = doOuterSampling,
innerLoopSamplingSource = innerLoopSamplingSource,
innerLoopSamplingRData = innerLoopSamplingRData,
outerLoopSamplingRData = outerLoopSamplingRData,
outerLoopSamplingSource = outerLoopSamplingSource
#baseDirectory = baseDirectory,
#directory = directory,
#filename = filename
))
}
