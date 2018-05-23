#' @title Arrival regression parameters for bucks
#' 
#' @description Bootstrapped regression parameters for
#' a quasibinomial logistic regression used to predict
#' arrival timing of male American shad in the Connecticut
#' River, CT, USA by accumulated thermal units (ATU)
#' 
#' @format A list of 1000 matrices with named dimensions
#' \describe{
#'   \item{dim1}{Intercept, slope}
#'   \item{dim2}{Estimate, Std Error, t statistic, and p-value}
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
#' River, CT, USA by accumulated thermal units (ATU)
#' 
#' @format A list of 1000 matrices with named dimensions
#' \describe{
#'   \item{dim1}{Intercept, slope}
#'   \item{dim2}{Estimate, Std Error, t statistic, and p-value}
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
#'   \item{Sex}{Fish sex}
#'   \item{Age}{Assigned age}
#'   \item{Length}{Total length, in cm}
#'   \item{yearCollected}{Year during which fish was collected}
#'   \item{backCalculated}{Logical indicating whether or not Length observation was back-calculated}
#'   \item{Mass}{Mass of fish, in g}
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
#'   \item{staid}{USGS Station ID}
#'   \item{val}{Temperature in degrees celcius}
#'   \item{dates}{Temperature, in degrees celcius}
#'   \item{qualcode}{Qualification code}
#'   \item{year}{date and time of temperature observation}
#'   \item{day}{Temperature, in degrees celcius}
#'   \item{ph}{Photoperiod, in hours}
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
#'   \item{staid}{USGS Station ID}
#'   \item{val}{Temperature in degrees celcius}
#'   \item{dates}{Temperature, in degrees celcius}
#'   \item{qualcode}{Qualification code}
#'   \item{year}{date and time of temperature observation}
#'   \item{day}{Temperature, in degrees celcius}
#' }
#' 
#' @source U.S. Geological Survey
"tempData"
