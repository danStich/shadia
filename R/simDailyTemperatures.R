#' @title Simulate (or retrieve) daily temperatures
#'
#' @description Internal function used to simulate daily
#' temperatures in annualSampling()
#'
#' Not intended to be called directly, but visible
#' for the sake of model transparency.
#'
#' @param mu a dataframe with columns `val`, `year`, and `day` created
#' from setUpTemperatureData() within riverModel() functions. Used to
#' simulate new daily temperature observations if `climate == current` in
#' the model call, else temperatures are retrieved from climate projections
#' in models where these are implemented (Connecticut and Penobscot only)
#'
#' @export
#'
simDailyTemperatures <- function(mu, climate, river, current_year, n) {

  # Daily temperatures ------------------------------
  # Use historical temperature data to predict temperature
  # on each day from a multivariate normal distribution

  if (climate == "current") {
    predTemps <- simTemperature(mu)
  }

  # Climate projection scenarios for the Connecticut River
  if (climate == "rcp45") {
    current_year <- lubridate::year(lubridate::now())

    if (river == "connecticut") {
      proj <- ctr_proj45[lubridate::year(ctr_proj45$Date) == (current_year + (n - 1)), ]
    }
    if (river == "penobscot") {
      proj <- pnr_proj45[lubridate::year(pnr_proj45$Date) == (current_year + (n - 1)), ]
      proj$avg <- proj$Eddington_RCP45_temp
    }

    predTemps <- data.frame(
      dates = lubridate::yday(proj[, 1]),
      avg = proj$avg
    )
  }

  if (climate == "rcp85") {
    current_year <- lubridate::year(lubridate::now())

    if (river == "connecticut") {
      proj <- ctr_proj85[lubridate::year(ctr_proj85$Date) == (current_year + (n - 1)), ]
    }
    if (river == "penobscot") {
      proj <- pnr_proj85[lubridate::year(pnr_proj85$Date) == (current_year + (n - 1)), ]
      proj$avg <- proj$Eddington_RCP85_temp
    }

    predTemps <- data.frame(
      dates = lubridate::yday(proj[, 1]),
      avg = proj$avg
    )
  }

  return(predTemps)
}
