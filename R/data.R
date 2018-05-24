#' @title Arrival regression parameters for bucks
#' 
#' @description Bootstrapped regression parameters for
#' a quasibinomial logistic regression used to predict
#' arrival timing of male American shad in the Connecticut
#' River, CT, USA by accumulated thermal units (ATU).
#' 
#' @format A list of 1000 matrices with named dimensions of:
#' \describe{
#'   \item{\code{dim1}}{Intercept, slope}
#'   
#'   \item{\code{dim2}}{Estimate, Std Error, t statistic, and p-value}
#' }
#' 
#' @source Connecticut Department of Energy and Environmental Conservation (2015)
#' 
"arr.B"

#' @title Arrival regression parameters for roes
#' 
#' @description Bootstrapped regression parameters for
#' a quasibinomial logistic regression used to predict
#' arrival timing of female American shad in the Connecticut
#' River, CT, USA by accumulated thermal units (ATU).
#' 
#' @format A list of 1000 matrices with named dimensions
#' \describe{
#'   \item{\code{dim1}}{Intercept, slope}
#'   
#'   \item{\code{dim2}}{Estimate, Std Error, t statistic, and p-value}
#' }
#' 
#' @source Connecticut Department of Energy and Environmental Conservation (2015)
#' 
"arr.B"

#' @title Growth data for Connecticut River American shad
#' 
#' @description Growth data for spawning American shad
#' in the Connecticut River, CT, USA.
#' 
#' @format A list of 1000 matrices with named dimensions
#' \describe{
#'   \item{\code{Sex}}{Fish sex}
#'   
#'   \item{\code{Age}}{Assigned age}
#'   
#'   \item{\code{Length}}{Total length, in cm}
#'   
#'   \item{\code{yearCollected}}{Year during which fish was collected}
#'   
#'   \item{\code{backCalculated}}{Logical indicating whether or not Length observation was back-calculated}
#'   
#'   \item{\code{Mass}}{Mass of fish, in g}
#' }
#' 
#' @source Connecticut Department of Energy and Environmental Conservation (2015)
#' 
"fish"

#' @title Connecticut River temperatures, Stratford, NH, USA
#' 
#' @description Temperature data from Stratford, NH, USA.
#' 
#' @format A data frame with 2919 obs. of 7 variables
#' \describe{
#'   \item{\code{staid}}{USGS Station ID}
#'   
#'   \item{\code{val}}{Temperature in degrees celcius}
#'   
#'   \item{\code{dates}}{Date of measurement}
#'   
#'   \item{\code{qualcode}}{Qualification code}
#'   
#'   \item{\code{year}}{Year of temperature observation}
#'   
#'   \item{\code{day}}{Ordinal date}
#'   
#'   \item{\code{ph}}{Photoperiod, in hours}
#' }
#' 
#' @source U.S. Geological Survey
"tempD"

#' @title Penobscot River temperatures, 
#' Eddington, ME, USA
#' 
#' @description Temperature data for 
#' Penobscot River temperatures, Eddington, ME, USA
#' 
#' @format A data frame with 13265 obs. of 6 variables
#' \describe{
#'   \item{\code{staid}}{USGS Station ID}
#'   
#'   \item{\code{val}}{Temperature in degrees celcius}
#'   
#'   \item{\code{dates}}{Date of temperature observation}
#'   
#'   \item{\code{qualcode}}{Qualification code}
#'   
#'   \item{\code{year}}{Year of temperature observation}
#'   
#'   \item{\code{day}}{Ordinal date}
#' }
#' 
#' @source U.S. Geological Survey
"tempData"
