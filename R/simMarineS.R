#' @title Simulate marine survival
#' 
#' @description Internal function used to simulate marine
#' survival from posterior distributions of von Bertalanffy
#' growth function parameters. This function relies on 
#' prediction of climate-informed vbgf parameter estimates
#' using sex-aggregated model fit.
#' 
#' @return A numeric vector of length 1 containing a single simulated
#' value of marine survival sampled randomly from a posterior predictive
#' distribution incorporating projected sea surface temperature 
#' during the current model year.
#' 
#' @details This function relies on model estimated VBGF parameters
#' from the 2020 Atlantic States Marine Fishery Commission 
#' American shad stock assessment. The VBGF was fit using SST as a 
#' continuous predictor of parameters Linf and K in the model. We 
#' use these relationships to predict new values of Linf and K from
#' projected sea surface temperatures with stored posteriors from
#' a sex-aggregated data set.
#' 
#' @export
#' 
simMarineS <- function(){
# Load system-region key
  data("regions")
  
# Get number for region from system-region key
  numregion <-  unique(regions$num[regions$Region==region])
  
# Sex-aggregate VBGF mort estimates ----
# American shad  
  if(species=='shad'){
    
  # Load vbgf posteriors
    data("vbgf_agg")
  
  # Define current year for scenarios
    current_year <- lubridate::year(Sys.time()) + (n-1)
    
  # Get regional means for vbgf parameters, and combine with
  # sst to predict growth parameters conditional on temperature
    
  # Extract population-level posteriors from model object
    linfcor <- apply(vbgf_agg$mu_beta_cor[,1,c(regions$num==numregion)], 1, mean)
    kcor <- apply(vbgf_agg$mu_beta_cor[,2,c(regions$num==numregion)], 1, mean)
    t0cor <- apply(vbgf_agg$mu_beta_cor[,3,c(regions$num==numregion)], 1, mean)
    
  # Get annual temperature from built-in data sets
  # of climate projections (sst)
  # Scale using mean and sd of sst in observed data  
    # attr(,"scaled:center")
    # [1] 12.63291
    # attr(,"scaled:scale")
    # [1] 0.6402722
  
    if(climate=='current'){
      X <- rcp85_sst$Mean[rcp85_sst$Year == lubridate::year(Sys.time())]
      scaled.X <- as.vector(scale(X, center = 12.63291, scale = 0.6402722))    
    }
    
    if(climate=='rcp85'){  
      X <- rcp85_sst$Mean[rcp85_sst$Year == current_year]
      scaled.X <- as.vector(scale(X, center = 12.63291, scale = 0.6402722))
    }
  
    if(climate=='rcp45'){  
      X <- rcp45_sst$Mean[rcp45_sst$Year == current_year]
      scaled.X <- as.vector(scale(X, center = 12.63291, scale = 0.6402722))
    }    
    
  # Predict VBGF parameters from climate and model estimates
    Linf_agg <- exp(vbgf_agg$b0_linf + linfcor + vbgf_agg$bh_linf*scaled.X)
    K_agg <- exp(vbgf_agg$b0_k + kcor + vbgf_agg$bh_k*scaled.X)
    t0_agg <- exp(vbgf_agg$b0_t0 + t0cor) - 10
 
  }
  
  # Blueback herring
    if(species=='blueback'){
    
    # Load vbgf posteriors
      data("vbgf_hudson_agg")    
      
    # Predict VBGF parameters from model estimates
      Linf_agg <- exp(vbgf_hudson_agg$b0_linf)
      K_agg <- exp(vbgf_hudson_agg$b0_k)
      t0_agg <- exp(vbgf_hudson_agg$b0_t0) - 10      
    
    }
  
# Estimate natural mortality (instantaneous) from the posterior
  nM <- 4.118 * K_agg^0.73 * (Linf_agg/10)^-0.33  
  
# Convert to annual rate
  A <- 1 - exp(-nM)
  
# Derive survival from A  
  annual_survival <- 1 - A
  
# Return a random sample from posterior distribution of
# mortality, subtracted from 1 to yield S
  return(sample(annual_survival, 1))
  
}
