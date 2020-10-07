#' @title Get maximum age
#' 
#' @description Get maximum age for population from ASMFC (2020).
#' 
#' @param region Life-history region (one of `Semelparous`, 
#' `Southern Iteroparous` or `Northern Iteroparous`).
#' 
#' @export
getMaxAge = function(region){
  
  if(region == 'Semelparous'){
    maxAge <- 9
  } else {
    maxAge <- 13
  }
  
  return(maxAge)
  
}
