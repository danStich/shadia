#' @title Simulate daily temperatures
#' 
#' @description Function used to simulate daily
#' temperatures in each river system using historical gauge
#' data loaded using \code{setupTemperatureData}.
#' 
#' @return A list containing 1) a matrix of projected 
#' temperatures for the number of years specified (predTemps), 
#' and 2) a matrix of accumulated thermal units (ATU) for
#' corresponding days.
#' 
#' @details This function relies on a data set (\code{mu}) containing
#' daily temperatures (\code{val}), year (\code{year}),
#' and ordinal date (\code{day}). Missing, zero, and negative
#' values will throw warnings.
#' 
#' @export
#' 
simTemperature <- function(mu){

# Simulate daily temperatures
# Use historical temperature data to predict temperature
# on each day from a multivariate normal distribution

# Fit a 2nd order polynomial to model log(Temperature)
  mu <- mu[mu$val > 0 , ]
  mu <- mu[!is.na(mu$val) & !is.infinite(mu$val), ]
  tmod <- lm(log(val) ~ day + I(day^2), data = mu)

# Save coefficients and vcv
  tbetas <- coef(tmod)
  tsig <- vcov(tmod)

# Draw new coeffs from mv normal
  tcoeffs <- MASS::mvrnorm(
    1, mu = tbetas, Sigma = tsig, tol=1e-6
    )
  
# Make new days, years, and stochastic noise for
# for prediction
  dates <- seq(1,366,1)
  stoch <- rnorm((length(dates)), 0, 0.05)
  
# Predict temperatures for each day using
# coeffs drawn from mv normal, plus noise
  hindcast <- exp(tcoeffs[1] 
                  + tcoeffs[2]*dates 
                  + tcoeffs[3]*(dates^2) + stoch)
  
  # SANITY CHECK
  #plot(hindcast, type='l', lwd=2)
  
  predTemps = data.frame(
    day = dates,
    val = hindcast
  )

# Return results
  return(predTemps)
  
}
