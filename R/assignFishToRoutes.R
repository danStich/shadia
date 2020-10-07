#' @title Assign fish to routes
#' 
#' @description River-specific assignment to migration routes for
#' individual fish.
#' 
#' Internal function used inside inner-loop-sampling, but stored in
#' a separate file to make code easier to work with.
#' 
#' @export
#' 
assignFishToRoutes <- function(river, c_fishAges) {
  # Assign upstream and downstream migration routes probabilistically conditional
  # on flow or production potential (upstream) and flow (downstream). 
  
  # Draw upstream migration path based on flow or else proportional production
  # using a random draw from a multinomial distribution with two outcomes  
  
  if(river=='penobscot'){

    upstream_path <- rmultinom(
      n = length(c_fishAges),
      size = 1,
      prob = c(
        pMainUp * pPiscUp,
        pMainUp * pMainstemUp,
        pStillwaterUp * pPiscUp,
        pStillwaterUp * pMainstemUp
      )
    )
    upstream_path[2,][upstream_path[2,] > 0] <- 2
    upstream_path[3,][upstream_path[3,] > 0] <- 3
    upstream_path[4,][upstream_path[4,] > 0] <- 4
    
    # A '1' is Piscataquis, and a '2' is mainstem
    upstream_path <- upstreamPathC(upstream_path)
  }
  
  if(river=='merrimack'){
    upstream_path <- rbinom(length(c_fishAges), 1, pBypassUp) # Bypass
    upstream_path[upstream_path==0] <- 2  
  }
  
  if(river=='connecticut'){
    upstream_path <- rbinom(length(fishAges), 1, pSpillway) # Spillway
    upstream_path[upstream_path==0] <- 2
  }
    
  if(river=='susquehanna'){
  # Upstream path for susquehanna river.
  # Draw migration path based on proportional
  # distribution of habitat in each route used 
  # for upstream migration)
    upstream_path <- rmultinom(
      n = length(c_fishAges),
      size = 1,
      prob = c(
        p_JuniataUp,
        p_WestBranchUp,
        p_ChemungUp,
        p_NorthBranchUp
      )
    )
    upstream_path[2,][upstream_path[2,] > 0] <- 2
    upstream_path[3,][upstream_path[3,] > 0] <- 3
    upstream_path[4,][upstream_path[4,] > 0] <- 4
    
    upstream_path <- upstreamPathC(upstream_path)
  }
  
  # Upstream path for Saco river
    if(river=='saco'){
      upstream_path <- rep(1, length(c_fishAges))
    }
  
  # Upstream path for kennebec river
  if(river=='kennebec'){
    # Mainstem
    upstream_path <- rbinom(length(c_fishAges), 1, (1-p_sebasticook))
    # Sebasticook
    upstream_path[upstream_path==0] <- 2  
  }
      
  # Upstream path for hudson-mohawk rivers
  if(river=='hudson'){
    # Upper Hudson
    upstream_path <- rbinom(length(c_fishAges), 1, (1-pMohawk))
    # Mohawk River
    upstream_path[upstream_path==0] <- 2  
  }     
    
  return(upstream_path)  
  
}   