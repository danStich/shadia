#' @title Set test parameters and environment variables
#'
#' @description Internal function for setting testing parameters
#' and environment variables.
#'
#' Not intended to be called directly, but visible
#' for transparency.
#'
#' @return A list of generic parameters for code benchmarking
#' and progress monitoring.
#'
#' @export
#'
setParameters <- function() {

  # control progress indicators and timing
  useTictoc <- FALSE
  useProgress <- FALSE

  # Package load
  # library(plyr)
  # library(MASS)
  # library(Rcpp)
  # library(zoo)
  # library(methods)

  # For profiling and benchmarking
  if (useTictoc) {
    # library(profvis)
    library(tictoc)
    # library(microbenchmark)
  }

  # ---------

  # JMS: Parameters to control sampling and data loading
  # TRUE == perform loads and sampling
  # FALSE == use saved .RData to populate variables

  doInnerSampling <- TRUE
  doOuterSampling <- TRUE

  # ---------

  # JMS: set sampling and data load filenames
  innerLoopSamplingSource <- "inner-loop-sampling.R"
  innerLoopSamplingRData <- "inner-loop-sampling.RData"

  outerLoopSamplingRData <- "outer-loop-sampling.RData"
  outerLoopSamplingSource <- "outer-loop-sampling.R"

  # ---------
  # Return parameters to calling environment
  # as a list.
  return(list(
    useTictoc = useTictoc,
    useProgress = useProgress,
    doInnerSampling = doInnerSampling,
    doOuterSampling = doOuterSampling,
    innerLoopSamplingSource = innerLoopSamplingSource,
    innerLoopSamplingRData = innerLoopSamplingRData,
    outerLoopSamplingRData = outerLoopSamplingRData,
    outerLoopSamplingSource = outerLoopSamplingSource
    # baseDirectory = baseDirectory,
    # directory = directory,
    # filename = filename
  ))
}
