#' @title Set up data for simulations
#' 
#' @description Internal functions for manipulations to 
#' empirical data used as inputs, and for fitting of
#' empirical relationships (e.g. discharge regressions).
#' 
#' Not intended to be called directly, but visible 
#' for transparency.
#' 
#' @return A list of generic parameters for code benchmarking
#' and progress monitoring.
#' 
#' @details Have retained data manipulation to retain 
#' integrity of raw data files in built-in data sets and
#' provide transparency in methods.
#' 
#' @export
#' 
setUpTemperatureData <- function(river){
 
  if(river == 'susquehanna'){
    
    # Load the SSR temperature data from
    # years 2012-2014 in the built-in data set
    # and summarize as mean by day and year for
    # mvnorm
    mu <- shadia::tempData_susquehanna %>%
      filter(year >= 2012) %>%
      group_by(day, year) %>%
      summarize( val = mean(val, na.rm = TRUE), .groups = "keep") 
    mu <- data.frame(mu)
    mu <- mu[ , c(3, 2, 1)]
    mu <- na.omit(mu)  
    
  }
  
  return(mu)
   
}


