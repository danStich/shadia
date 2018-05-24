#' shadia: American shad dam passage performance standard model
#' 
#' The shadia package provides an interface for running
#' the American shad dam passage performance standard
#' model. It includes a limited number of functions intended to
#' be called directly by the user, and a large number of
#' supporting internal functions that are visible to promote
#' transparency in the model.
#' 
#' @section Functions called directly:
#'   \describe{
#'     \code{\link{penobscotRiverModel}} \cr
#'     \code{\link{addStochList}} \cr
#'     \code{\link{CI}} \cr
#'     \code{\link{invlogit}} \cr
#'     \code{\link{substrRight}}
#'   }
#'   
#' @section Internal functions:
#'  \describe{
#'    \code{\link{annualUpstream}} \cr
#'    \code{\link{additionalEggsProcessing}} \cr
#'    \code{\link{annualUpstream}} \cr
#'    \code{\link{assignFishToRoutes}} \cr
#'    \code{\link{assignFishToRoutes}} \cr
#'    \code{\link{createPUMatrix}} \cr
#'    \code{\link{defineHabitat}} \cr
#'    \code{\link{defineHydroSystem}} \cr
#'    \code{\link{defineOutputVectors}} \cr
#'    \code{\link{definePassageRates}} \cr
#'    \code{\link{downstreamMigration}} \cr
#'    \code{\link{fillOutputVectors}} \cr
#'    \code{\link{fwFishingMort}} \cr
#'    \code{\link{innerLoopSampling}} \cr
#'    \code{\link{nextGeneration}} \cr
#'    \code{\link{outerLoopSampling}} \cr
#'    \code{\link{populationMatrices}} \cr
#'    \code{\link{postSpawnMortality}} \cr
#'    \code{\link{processCohorts}} \cr
#'    \code{\link{processPopulation}} \cr
#'    \code{\link{scalePop}} \cr
#'    \code{\link{setParameters}} \cr
#'    \code{\link{setScalar}} \cr
#'    \code{\link{setUpData}} \cr
#'    \code{\link{startingPop}} \cr
#'    \code{\link{weldonScenarios}} \cr
#'    \code{\link{writeData}} \cr
#'  }
#'  
#' @section Data:
#'   \describe{
#'     
#'   }
#'
#'
#' @docType package
#' 
#' @name shadia
#' 
#' @importFrom graphics abline lines par plot
#' @importFrom stats aggregate nls quantile runif var
#' @importFrom utils write.table
#' @importFrom geosphere daylength
#' @importFrom lubridate year yday
#' @importFrom plyr ddply . summarize summarise
#' @importFrom MASS mvrnorm rnegbin
#' @importFrom zoo na.spline
#' @importFrom fishmethods growth
#' @import Rcpp
#' 
#' @useDynLib shadia
NULL


