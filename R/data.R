#' @title Arrival regression parameters for bucks
#'
#' @description Bootstrapped regression parameters for
#' a quasibinomial logistic regression used to predict
#' arrival timing of male American shad in the Connecticut
#' River, CT, USA by accumulated thermal units (ATU).
#'
#' @format A list of 1000 matrices with named dimensions of:
#' \describe{
#'   \code{dim1 }{Intercept, slope}
#'
#'   \code{dim2 }{Estimate, Std Error, t statistic, and p-value}
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
#'   \code{dim1 }{Intercept, slope}
#'
#'   \code{dim2 }{Estimate, Std Error, t statistic, and p-value}
#' }
#'
#' @source Connecticut Department of Energy and Environmental Conservation (2015)
#'
"arr.R"

#' @title Growth data for Connecticut River American shad
#'
#' @description Growth data for spawning American shad
#' in the Connecticut River, CT, USA.
#'
#' @format A dataframe with 19,946 observatoins of 6 variables
#' \describe{
#'   \code{Sex }{Fish sex}
#'
#'   \code{Age }{Assigned age}
#'
#'   \code{Length }{Total length, in cm}
#'
#'   \code{yearCollected }{Year during which fish was collected}
#'
#'   \code{backCalculated }{Logical indicating whether or not Length observation was back-calculated}
#'
#'   \code{Mass }{Mass of fish, in g}
#' }
#'
#' @source Connecticut Department of Energy and Environmental Conservation (2015)
#'
"fish"

#' @title Length and mass of Hudson River American shad
#'
#' @description Length and mass of spawning American shad
#' collected in the Hudson River, USA.
#'
#' @format A dataframe with 26,564 observations of 4 variables
#' \describe{
#'   \code{samp_date }{Date fish was collected}
#'
#'   \code{Length }{Fork length, in mm}
#'
#'   \code{mass }{Mass of fish in grams}
#'
#'   \code{sex }{Sex of fish, `B` is buck (male) and `R`is roe (female)}
#' }
#'
#' @source New York State Department of Environmental Conservation
"hudson_shad"

#' @title Length and mass of Hudson River American shad
#'
#' @description Length and mass of spawning American shad
#' collected in the Hudson River, USA.
#'
#' @format A dataframe with 2,485 observations of 19 variables
#' \describe{
#'   \code{species }{NYSDEC species code}
#'
#'   \code{samp_date }{Date fish was collected}
#'
#'   \code{fl }{Fork length, in mm}
#'
#'   \code{wt }{Mass of fish in grams}
#'
#'   \code{sex }{Sex of fish, `B` is buck (male) and `R`is roe (female)}
#'
#'   \code{age }{Age of fish in years}
#'
#' }
#'
#' @source New York State Department of Environmental Conservation
"hudson_blueback"


#' @title Connecticut River temperatures, Stratford, NH, USA
#'
#' @description Temperature data from Stratford, NH, USA.
#'
#' @format A data frame with 2919 obs. of 7 variables
#' \describe{
#'   \code{staid }{USGS Station ID}
#'
#'   \code{val }{Temperature in degrees celcius}
#'
#'   \code{dates }{Date of measurement}
#'
#'   \code{qualcode }{Qualification code}
#'
#'   \code{year }{Year of temperature observation}
#'
#'   \code{day }{Ordinal date}
#'
#'   \code{ph }{Photoperiod, in hours}
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
#'   \code{staid }{USGS Station ID}
#'
#'   \code{val }{Temperature in degrees celcius}
#'
#'   \code{dates }{Date of temperature observation}
#'
#'   \code{qualcode }{Qualification code}
#'
#'   \code{year }{Year of temperature observation}
#'
#'   \code{day }{Ordinal date}
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
#'   \code{staid }{USGS Station ID}
#'
#'   \code{val }{Temperature in degrees celcius}
#'
#'   \code{dates }{Date of temperature observation}
#'
#'   \code{qualcode }{Qualification code}
#'
#'   \code{year }{Year of temperature observation}
#'
#'   \code{day }{Ordinal date}
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
#'   \code{dim1 }{Sinf, K, t0}
#'
#'   \code{dim2 }{Estimate, Std Error, t statistic, and p-value}
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
#'   \code{dim1 }{Sinf, K, t0}
#'
#'   \code{dim2 }{Estimate, Std Error, t statistic, and p-value}
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
#'   \code{dim1 }{Sinf, K, t0}
#'
#'   \code{dim2 }{Estimate, Std Error, t statistic, and p-value}
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
#'   \code{dim1 }{Sinf, K, t0}
#'
#'   \code{dim2 }{Estimate, Std Error, t statistic, and p-value}
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
#'   \code{val }{Temperature in degrees celcius}
#'
#'   \code{year }{Year of temperature observation}
#'
#'   \code{day }{Ordinal date}
#' }
#'
#' @source T. Castro-Santos, U.S. Geological Survey
"tempData_connecticut"

#' @title Susquehanna River at Danville, PA
#'
#' @description Temperature data for
#' the Susquehanna River at Danville, PA.
#'
#' @format A data frame with 1816 observations of 6 variables
#' \describe{
#'   \code{staid }{USGS Station ID}
#'
#'   \code{dates }{Date of temperature observation}
#'
#'   \code{val }{Temperature in degrees celcius}
#'
#'   \code{day }{Ordinal date}
#'   
#'   \code{year }{Year of temperature observation}
#' }
#'
#' @source U.S. Geological Survey
"tempData_susquehanna"

#' @title Saco River at River Street in Bartlett, NH
#'
#' @description Temperature data for
#' the Saco River at various locations in watershed
#'
#' @format A data frame with 3631 observations of 6 variables
#' \describe{
#'   \code{staid }{Station ID}
#'
#'   \code{dates }{Date of temperature observation}
#'
#'   \code{year }{Year of temperature observation}
#'
#'   \code{day }{Ordinal date}
#'
#'   \code{val }{Temperature in degrees celcius}
#'
#' }
#'
#' @source U.S. Geological Survey, SHEDS Stream Temperature Database
"tempData_saco"

#' @title Kennebec River Temperatures
#'
#' @description Temperature data from
#' various monitoring sites in the
#' Kennebec River, Maine, USA.
#'
#' @format A data frame with 362168 observations of 5 variables
#' \describe{
#'   \code{staid }{Site ID in watershed}
#'
#'   \code{dates }{Date of temperature observation}
#'
#'   \code{year }{Year of temperature observation}
#'
#'   \code{day }{Ordinal date}
#'
#'   \code{val }{Bottom temperature in degrees celcius}
#' }
#'
#' @source Maine Department of Marine Resources, SHEDS Stream Temperature Database
"tempData_kennebec"

#' @title Hudson River Temperatures
#'
#' @description Temperature data from
#' USGS gage 01359139 in the
#' Hudson River, NY, USA.
#'
#' @format A data frame with 4787 observations of 5 variables
#' \describe{
#'   \code{staid }{Site ID in watershed}
#'
#'   \code{val }{Temperature in degrees celcius}
#'
#'   \code{year }{Year of temperature observation}
#'
#'   \code{dates }{Date of temperature observation}
#'
#'   \code{day }{Ordinal date}
#'
#' }
#'
#' @source US Geological Survey
"tempData_hudson"

#' @title Androscoggin River Temperatures
#'
#' @description Temperature data from
#' various monitoring sites in the
#' Androscoggin and Little Androscoggin 
#' rivers, Maine, USA.
#'
#' @format A data frame with 3,008 observations of 5 variables
#' \describe{
#'   \code{staid }{Site ID in watershed}
#'
#'   \code{dates }{Date of temperature observation}
#'
#'   \code{year }{Year of temperature observation}
#'
#'   \code{day }{Ordinal date}
#'
#'   \code{val }{Mean daily bottom temperature in degrees celcius}
#' }
#'
#' @source SHEDS Stream Temperature Database
"tempData_androscoggin"


#' @title Projected sea surface temperatures RCP 4.5
#'
#' @description Projected sea surface temperatures for
#' the Northeast Continental Shelf Large Marine Ecosystem
#' (NELME) under RCP 4.5 scenario for years 1981 - 2099.
#'
#' @format A dataframe with 118 obs. of 5 variables:
#' \describe{
#'   \code{Year }{Year for projection}
#'
#'   \code{Scenario }{Representative climate pathway (RCP) used for projection}
#'
#'   \code{Mean }{Mean projected temperature (C)}
#'
#'   \code{pct5th }{5th percentile for projection}
#'
#'   \code{pct95th }{95th percentile for projection}
#' }
#'
#' @source Integrated Systems Ecology Lab, Gulf Of Maine Research Institute
#'
"rcp45_sst"

#' @title Projected sea surface temperatures RCP 4.5
#'
#' @description Projected sea surface temperatures for
#' the Northeast Continental Shelf Large Marine Ecosystem
#' (NELME) under RCP 8.5 scenario for years 1981 - 2099.
#'
#' @format A dataframe with 118 obs. of 5 variables:
#' \describe{
#'   \code{Year }{Year for projection}
#'
#'   \code{Scenario }{Representative climate pathway (RCP) used for projection}
#'
#'   \code{Mean }{Mean projected temperature (C)}
#'
#'   \code{pct5th }{5th percentile for projection}
#'
#'   \code{pct95th }{95th percentile for projection}
#' }
#'
#' @source Integrated Systems Ecology Lab, Gulf Of Maine Research Institute
#'
"rcp85_sst"

#' @title Projected daily river temperatures (RCP4.5) in CT River, USA
#'
#' @description Daily project temperatures in the Connecticut
#' River, USA for years 1950-2099 under RCP4.5. Three sites
#' are included based on availbility of time-series used to
#' construct the projections.
#'
#' @format A dataframe with 54,747 obs. of 5 variables:
#' \describe{
#'   \code{Date }{Date for projection}
#'
#'   \code{Haddam }{Daily temperature (C) at Haddam}
#'
#'   \code{Thompsonville }{Daily temperature (C) at Thompsonville}
#'
#'   \code{Turners }{Daily temperature (C) at Turners Falls}
#'
#'   \code{avg}{Average daily temperature across sites}
#' }
#'
#' @source Integrated Systems Ecology Lab, Gulf Of Maine Research Institute
#'
"ctr_proj45"

#' @title Projected daily river temperatures (RCP8.5) in CT River, USA
#'
#' @description Daily project temperatures in the Connecticut
#' River, USA for years 1950-2099 under RCP8.5. Three sites
#' are included based on availbility of time-series used to
#' construct the projections.
#'
#' @format A dataframe with 54,747 obs. of 5 variables:
#' \describe{
#'   \code{Date }{Date for projection}
#'
#'   \code{Haddam }{Daily temperature (C) at Haddam}
#'
#'   \code{Thompsonville }{Daily temperature (C) at Thompsonville}
#'
#'   \code{Turners }{Daily temperature (C) at Turners Falls}
#'
#'   \code{avg}{Average daily temperature across sites}
#' }
#'
#' @source Integrated Systems Ecology Lab, Gulf Of Maine Research Institute
#'
"ctr_proj85"


#' @title Projected daily river temperatures (RCP4.5) in
#' Penobscot River, USA
#'
#' @description Daily project temperatures in the Penobscot
#' River, USA for years 1950-2099 under RCP4.5 based on
#' historical temperatures from the USGS gauge at Eddington.
#'
#' @format A dataframe with 54,787 obs. of 5 variables:
#' \describe{
#'   \code{Date }{Date for projection}
#'
#'   \code{Eddington_RCP45_temp }{Daily temperature (C) at Eddington}
#' }
#'
#' @source Integrated Systems Ecology Lab, Gulf Of Maine Research Institute
#'
"pnr_proj45"

#' @title Projected daily river temperatures (RCP8.5) in
#' Penobscot River, USA
#'
#' @description Daily project temperatures in the Penobscot
#' River, USA for years 1950-2099 under RCP8.5 based on
#' historical temperatures from the USGS gauge at Eddington.
#'
#' @format A dataframe with 54,787 obs. of 5 variables:
#' \describe{
#'   \code{Date }{Date for projection}
#'
#'   \code{Eddington_RCP85_temp }{Daily temperature (C) at Eddington}
#' }
#'
#' @source Integrated Systems Ecology Lab, Gulf Of Maine Research Institute
#'
"pnr_proj85"

#' @title Region-system key for von Bertalanffy growth estimates
#'
#' @description A key for matching rivers used in the growth
#' analysis for the 2020 ASMFC American shad stock assessment. This is
#' used to query posterior distributions from von Bertalanffy growth
#' models used to predict fish length at age and incorporate climate
#' impacts on growth and mortality.
#'
#' @format A dataframe with 11 obs. of 3 variables:
#' \describe{
#'   \code{System }{Stock assessment unit}
#'
#'   \code{Region }{Management region based on life-history strategy}
#'
#'   \code{Thompsonville }{Numeric identifier for region}
#' }
#'
#' @source Atlantic States Marine Fisheries Commission
#'
"regions"

#' @title Sex-aggregate von Bertalanffy growth parameters for
#' American shad
#'
#' @description Posterior distributions for parameters of a
#' sex-aggregated von Bertalanffy growth function for American
#' shad in the Northeast Continental Shelf Large Marine Ecosystem.
#'
#' @format A list with 7 elements, each of which corresponds to an
#' estimated parameter in the sex-aggregate model described in Gilligan (In prep).
#'
#' @source Gilligan, E. (In Prep). MS Biology Thesis, SUNY Oneonta.
#'
"vbgf_agg"

#' @title Female von Bertalanffy growth parameters for
#' American shad
#'
#' @description Posterior distributions for parameters of a
#' female-only von Bertalanffy growth function for American
#' shad in the Northeast Continental Shelf Large Marine Ecosystem.
#'
#' @format A list with 7 elements, each of which corresponds to an
#' estimated parameter in the female-only model described in Gilligan (In prep).
#'
#' @source Gilligan, E. (In Prep). MS Biology Thesis, SUNY Oneonta.
#'
"vbgf_f"

#' @title Male von Bertalanffy growth parameters for
#' American shad
#'
#' @description Posterior distributions for parameters of a
#' male-only von Bertalanffy growth function for American
#' shad in the Northeast Continental Shelf Large Marine Ecosystem.
#'
#' @format A list with 7 elements, each of which corresponds to an
#' estimated parameter in the male-only model described in Gilligan (In prep).
#'
#' @source Gilligan, E. (In Prep). MS Biology Thesis, SUNY Oneonta.
#'
"vbgf_m"

#' @title Sex-aggregate von Bertalanffy growth parameters for
#' blueback herring in the Hudson River
#'
#' @description Posterior distributions for parameters of a
#' sex-aggregated von Bertalanffy growth function for blueback herring
#' in the Hudson River, NY, USA.
#'
#' @format A list with 3 elements, each of which corresponds to an
#' estimated parameter in the model (Linf, K, t0).
#'
#' @source New York State Department of Environmental Conservation, unpublished data
#'
"vbgf_hudson_agg"

#' @title Female von Bertalanffy growth parameters for
#' blueback herring in the Hudson River
#'
#' @description Posterior distributions for parameters of a
#' female-only von Bertalanffy growth function for blueback herring
#' in the Hudson River, NY, USA.
#'
#' @format A list with 3 elements, each of which corresponds to an
#' estimated parameter in the model (Linf, K, t0).
#'
#' @source New York State Department of Environmental Conservation, unpublished data
#'
"vbgf_hudson_f"

#' @title Female von Bertalanffy growth parameters for
#' blueback herring in the Hudson River
#'
#' @description Posterior distributions for parameters of a
#' male-only von Bertalanffy growth function for blueback herring
#' in the Hudson River, NY, USA.
#'
#' @format A list with 3 elements, each of which corresponds to an
#' estimated parameter in the model (Linf, K, t0).
#'
#' @source New York State Department of Environmental Conservation, unpublished data
#'
"vbgf_hudson_m"

#' @title Sex-aggregate von Bertalanffy growth parameters for
#' blueback herring in the Kennebec River
#'
#' @description Posterior distributions for parameters of a
#' sex-aggregated von Bertalanffy growth function for blueback herring
#' in the Kennebec River, Maine, USA.
#'
#' @format A list with elements corresponding to estimated parameters
#' of a von Bertalanffy growth function with sex aggregated
#'
#' @source Maine Department of Marine Resources, unpublished
#'
"vbgf_kennebec_bbh_agg"

#' @title Female von Bertalanffy growth parameters for
#' blueback herring in the Hudson River
#'
#' @description Posterior distributions for parameters of a
#' female-only von Bertalanffy growth function for blueback herring
#' in the Kennebec River, Maine, USA.
#'
#' @format A list with elements corresponding to estimated parameters
#' of a von Bertalanffy growth function for females
#'
#' @source Maine Department of Marine Resources, unpublished
#'
"vbgf_kennebec_bbh_f"

#' @title Female von Bertalanffy growth parameters for
#' blueback herring in the Hudson River
#'
#' @description Posterior distributions for parameters of a
#' male-only von Bertalanffy growth function for blueback herring
#' in the Kennebec River, Maine, USA.
#'
#' @format A list with elements corresponding to estimated parameters
#' of a von Bertalanffy growth function for males
#'
#' @source Maine Department of Marine Resources, unpublished
#'
"vbgf_kennebec_bbh_m"

#' @title Length-weight regression coefficients for American shad
#'
#' @description A dataset containing sex-specific regression
#' coefficients of log10length-log10weight relationships
#' for each life-history region (NI = Northern iteroparous,
#' SI = Southern iteroparous, SP = Semelparous). Use to predict
#' weight at age from length derived using region-specific
#' vbgf parameters.
#'
#' @format A data frame with 6 observations of 4 variables:
#' \describe{
#'
#' \code{region}{ Life-history region}
#'
#' \code{sex}{ Fish sex (gender)}
#'
#' \code{alpha}{ Intercept}
#'
#' \code{beta}{ Slope}
#' }
#'
#' @source Atlantic States Marine Fisheries Commission
"length_weight"


