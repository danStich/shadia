#' @title Simulate daily temperatures
#' 
#' @description Internal function used to simulate daily
#' temperatures in each river system using historical gauge
#' data loaded in \code{setUPData}.
#' 
#' Not currently used in package.
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
simTemperature <- function(){

# Simulate daily temperatures
# Use historical temperature data to predict temperature
# on each day from a multivariate normal distribution

# Fit a 2nd order polynomial to model log(Temperature)
# by day of year after removing values <= 0 in setUpData
  tmod <- lm(log(val) ~ day + I(day^2), data = mu)

# Save coefficients and vcv
  tbetas <- coef(tmod)
  tsig <- vcov(tmod)

# To avoid crazy predictions  
  predTemps <- Inf
  while(((is.infinite(max(predTemps)) | (max(predTemps)>35) | is.nan(max(predTemps))) )){
    
# Try until conditions are met
  try(
    {
    # Draw new coeffs from mv normal
      tcoeffs <- MASS::mvrnorm(
        nYears, mu = tbetas, Sigma = tsig, tol=1e-6
        )
      
    # Make new days, years, and stochastic noise for preds
      dates <- seq(1,366,1)
      Year <- seq(1,nYears,1)
      stoch <- matrix(
        rnorm((length(dates)*length(Year)), 0, 1),
        ncol = nYears, byrow = TRUE)
      
    # Predict temperature each year from mvnormal using
    # `tempC`, an Rcpp fun built with package
      predTemps <- exp(tempC(dates, Year, tcoeffs))
   }
  )
  }

# # Plot for sanity check
#   plot((predTemps[,1] + stoch[,1]),
#        type='l', xlim=c(1,366), ylim=c(0,30),
#        col=rgb(.8,.8,.8,.07)
#        )
#   for(i in 1:ncol(predTemps)){
#     lines(predTemps[,i] + stoch[,i],
#        col=rgb(.1,.1,.1,.5))
#   }
#   lines(apply(predTemps, 1, mean), col='white', lwd=2)
#   box()

# Calculate ATU for each day of simulated temperature data
  predTemps[predTemps<0] <- 0
  newTU <- apply(predTemps, 2, cumsum)

# Return results
  return(list(
    predTemps = predTemps,
    newTU = newTU,
    stoch = stoch
    )
  )
}
