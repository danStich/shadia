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
#' Penobscot River, Eddington, ME, USA
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

#' @title South Branch Piscataqoug River near
#'  Goffstown, NH, USA
#' 
#' @description Temperature data for 
#' South Branch Piscataqoug River near
#' Goffstown, NH, USA
#' 
#' @format A data frame with 2194 obs. of 6 variables
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
"tempData_merrimack"

#' @title von Bertalanffy growth parameters for roes
#' 
#' @description Bootstrapped regression parameters for
#' von Bertalanffy growth parameters for models fit to
#' female American shad in the Connecticut
#' River, CT, USA.
#' 
#' @format A list of 10000 matrices with named dimensions
#' \describe{
#'   \item{\code{dim1}}{Sinf, K, t0}
#'   
#'   \item{\code{dim2}}{Estimate, Std Error, t statistic, and p-value}
#' }
#' 
#' @source Connecticut Department of Energy and Environmental Conservation (2015)
#' 
"r.parms"

#' @title von Bertalanffy growth parameters for roes
#' 
#' @description Bootstrapped regression parameters for
#' von Bertalanffy growth parameters for models fit to
#' female American shad in the Merrimack River, USA. Uses
#' data from fish < 3 years of age from the Connecticut
#' River, USA to calibrate growth curves.
#' 
#' @format A list of 10000 matrices with named dimensions
#' \describe{
#'   \item{\code{dim1}}{Sinf, K, t0}
#'   
#'   \item{\code{dim2}}{Estimate, Std Error, t statistic, and p-value}
#' }
#' 
#' @source U.S. Fish and Wildlife Service (2018)
#' 
"r.parms_merrimack"

#' @title von Bertalanffy growth parameters for bucks
#' 
#' @description Bootstrapped regression parameters for
#' von Bertalanffy growth parameters for models fit to
#' male American shad in the Connecticut
#' River, CT, USA.
#' 
#' @format A list of 10000 matrices with named dimensions
#' \describe{
#'   \item{\code{dim1}}{Sinf, K, t0}
#'   
#'   \item{\code{dim2}}{Estimate, Std Error, t statistic, and p-value}
#' }
#' 
#' @source Connecticut Department of Energy and Environmental Conservation (2015)
#' 
"b.parms"

#' @title von Bertalanffy growth parameters for roes
#' 
#' @description Bootstrapped regression parameters for
#' von Bertalanffy growth parameters for models fit to
#' male American shad in the Merrimack River, USA. Uses
#' data from fish < 3 years of age from the Connecticut
#' River, USA to calibrate growth curves.
#' 
#' @format A list of 10000 matrices with named dimensions
#' \describe{
#'   \item{\code{dim1}}{Sinf, K, t0}
#'   
#'   \item{\code{dim2}}{Estimate, Std Error, t statistic, and p-value}
#' }
#' 
#' @source U.S. Fish and Wildlife Service (2018)
#' 
"b.parms_merrimack"

#' @title Connecticut River temperatures,
#' Turner's Falls, MA, USA.
#' 
#' @description Sub-daily temperatures from USGS S.O.
#' Conte Anadromous Fish Lab in Turner's Falls, MA
#' for the 21-year period 1994-2015.
#' 
#' @format A data frame with 167,333 obs. of 3 variables
#' \describe{
#'   \item{\code{val}}{Temperature in degrees celcius}
#'   
#'   \item{\code{year}}{Year of temperature observation}
#'   
#'   \item{\code{day}}{Ordinal date}
#' }
#' 
#' @source T. Castro-Santos, U.S. Geological Survey
"tempData_connecticut"

#' @title Saco River at River Street in Bartlett, NH
#' 
#' @description Temperature data for 
#' the Saco River at Bartlett, NH
#' 
#' @format A data frame with 3631 observations of 6 variables
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
"tempData_saco"
