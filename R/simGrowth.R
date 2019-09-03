#' @title Simulate growth parameters
#' 
#' @description Internal function used to simulate 
#' sex-specific length at age from sex-specific
#' von Bertalanffy growth functions (VBGF) incorporating
#' effect of sst on VBGF parameters L-infinity and K.
#' 
#' @return A numeric vector of length 1 containing one sample
#' of simultaneously estimated VBGF parameters (as a random draw
#' from fitted model posteriors).
#' 
#' @details This function relies on model estimated VBGF parameters
#' from the 2020 Atlantic States Marine Fishery Commission 
#' American shad stock assessment. The VBGF was fit using SST as a 
#' continuous predictor of parameters Linf and K in the model. We 
#' use these relationships to predict new values of Linf and K from
#' projected sea surface temperatures with stored posteriors from
#' built-in, sex-specific data sets containing VBGF parameters.
#' 
#' @export
#' 
simGrowth <- function(female=TRUE){

# Load system-region key
  data("regions")
  
# Get number for region from system-region key
  numregion <-  unique(regions$num[regions$Region==region])
  
# Sex-specific VBGF mort estimates ----
# Load vbgf posteriors from built-in R datasets in shadia
  if(female){
    pars <- vbgf_f
  } else {
    pars <- vbgf_m
  }
  
# Define current year for scenarios
  current_year <- lubridate::year(Sys.time()) + (n-1)
  
# Get regional means for vbgf parameters, and combine with
# sst to predict growth parameters conditional on temperature
  
# Extract population-level posteriors from model object
  linfcor <- apply(pars$mu_beta_cor[,1,c(regions$num==numregion)], 1, mean)
  kcor <- apply(pars$mu_beta_cor[,2,c(regions$num==numregion)], 1, mean)
  t0cor <- apply(pars$mu_beta_cor[,3,c(regions$num==numregion)], 1, mean)
  
# Get annual temperature from built-in data sets
# of climate projections (sst)
# Scale using mean and sd of sst in observed data  
  # attr(,"scaled:center")
  # [1] 12.63291
  # attr(,"scaled:scale")
  # [1] 0.6402722

  if(climate=='current' | climate=='rcp85'){  
    X <- rcp85_sst$Mean[rcp85_sst$Year == current_year]
    scaled.X <- as.vector(scale(X, center = 12.63291, scale = 0.6402722))
  }

  if(climate=='rcp45'){  
    X <- rcp45_sst$Mean[rcp45_sst$Year == current_year]
    scaled.X <- as.vector(scale(X, center = 12.63291, scale = 0.6402722))
  }    
  
# Predict VBGF parameters from climate and model estimates
  Linf <- exp(pars$b0_linf + linfcor + pars$bh_linf*scaled.X)
  K <- exp(pars$b0_k + kcor+ pars$bh_k*scaled.X)
  t0 <- exp(pars$b0_t0 + t0cor) - 10
 
  draw <- sample(1:length(Linf), 1)
  
  vbgf_draws <- c(Linf[draw], K[draw], t0[draw])
  
  return(vbgf_draws)
}
