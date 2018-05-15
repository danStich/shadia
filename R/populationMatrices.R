populationMatrices <- function(){

  # Main-to-piscataquis spawners
  x_1 = expand.grid(puNames[[1]], seq(1, maxAge, 1))
  names(x_1) = c('pus', 'fishAges')
  # Main-to-mainstem spawners
  x_2 = expand.grid(puNames[[2]], seq(1, maxAge, 1))
  names(x_2) = c('pus', 'fishAges')
  # Stillwater-to-piscataquis spawners
  x_3 = expand.grid(puNames[[3]], seq(1, maxAge, 1))
  names(x_3) = c('pus', 'fishAges')
  # Stillwater-to-mainstem spawners
  x_4 = expand.grid(puNames[[4]], seq(1, maxAge, 1))
  names(x_4) = c('pus', 'fishAges')
  # if (useTictoc) toc() # counts in each PU

  # Output list
  return(list(
    x_1 = x_1,
    x_2 = x_2,
    x_3 = x_3,
    x_4 = x_4
  ))    
    
}