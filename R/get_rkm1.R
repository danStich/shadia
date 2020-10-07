#' @title Get rkm1
#' 
#' @description Assign first RKM in migration model for each river.
#' 
#' @param river One of any rivers implemented in `shadia`.
#' 
#' @export
get_rkm1 = function(river, c_fishAges){
  
  # For Penobscot River:
    if(river=='penobscot'){
      rkm1 <- rep(41, length(c_fishAges))
    }
    # For Merrimack River:
    if(river=='merrimack'){
      rkm1 <- rep(35, length(c_fishAges))
    }
    # For Connecticut River:
    if(river=='connecticut'){
      rkm1 <- rep(90, length(c_fishAges))
    }
    # For Susquehanna River:
    if(river=='susquehanna'){
      rkm1 <- rep(0, length(c_fishAges))
    }
    # For Saco River:
    if(river=='saco'){
      rkm1 <- rep(0, length(c_fishAges))
    }
    # For kennebec River:
    if(river=='kennebec'){
      rkm1 <- rep(30, length(c_fishAges))
    }
    # For Hudson River:
    if(river=='hudson'){
      rkm1 <- rep(145, length(c_fishAges))
    }  

  return(rkm1)
  
}
