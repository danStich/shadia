#' @title Create starting population
#' 
#' @description Internal function for creating a starting
#' population of fish for each iteration. 
#' 
#' Not intended to be called directly, but visible 
#' for the sake of model transparency.
#' 
#' @return A list containing the initial population size,
#' spawner-recruitment and repeat-spawning probabilities,
#' and spawning/recruitment pools.
#' 
#' @export
#' 
startingPop <- function(){
  
if(river=='penobscot'){
  # Assign the starting population based on
  # a seed of age-1 fish and application
  # of an ocean survival curve. The population
  # size is scaled to make the models run faster.
  # Output is re-scaled
  
  # Original number of Age 1 individuals in the population
  # JMS: moved to outer-loop-sampling.R
  
  # Original number of individuals (calculated from ocean survival) per age
  for (i in 2:maxAge) {
  assign(paste('Age', i, sep = ''),
         Age1 * (cumprod(oceanSurvival[2:i - 1]))[i - 1],
         envir = .shadia)
  }
    
  # Collect age classes in a vector
  pop <- mget(ls(pat = "^Age", envir = .shadia), envir = .shadia)
  pop <- pop[order(sapply(pop,head,1), decreasing=T)]
  
  # Define probability of recruitment to spawn- based on proportion of spawners
  # in each age class (Bailey and Zydlewski 2013)
  spawnRecruit <- c(0, 0, 0, 0.01, .33, .84, .97, .99, 1.00)
  
  # Initial probalities of repeat spawning- will be derived in annual loop
  pRepeat <- c(0, 0, 0, 0.004, 0.094286, 0.375714, 0.722286, 1.00, 1.00)
  
  # Define spawning population and recruitment pool
  spawningPool <- unlist(pop) * spawnRecruit
  recruitmentPool <- unlist(pop) - unlist(pop) * spawnRecruit
  
  return(list(
    pop = unlist(pop),
    spawnRecruit = spawnRecruit,
    pRepeat = pRepeat,
    spawningPool = spawningPool,
    recruitmentPool = recruitmentPool  
  ))
}

  
if(river=='merrimack'){
  # Assign the starting population based on
  # a seed of age-1 fish and application
  # of an ocean survival curve. The population
  # size is scaled to make the models run faster.
  # Output is re-scaled
  
  # Original number of Age 1 individuals in the population
  # JMS: moved to outer-loop-sampling.R
  
  # Original number of individuals (calculated from ocean survival) per age
  for (i in 2:maxAge) {
  assign(paste('Age', i, sep = ''),
         Age1 * (cumprod(oceanSurvival[2:i - 1]))[i - 1],
         envir = .shadia)
  }
    
  # Collect age classes in a vector
  pop <- mget(ls(pat = "^Age", envir = .shadia), envir = .shadia)
  pop <- pop[order(sapply(pop,head,1), decreasing=T)]
  
  # Define probability of recruitment to spawn- based on proportion of spawners
  # in each age class (Bailey and Zydlewski 2013)
  spawnRecruit <- c(0, 0, 0, 0.02, .23, .48, .64, .71, 1.00, 1.00, 1.00)
  
  # Initial probalities of repeat spawning- will be derived in annual loop
  pRepeat <- c(0, 0, 0, 0.004, 0.094286, 0.375714, 0.722286, 1.00, 1.00, 1.00, 1.00)
  
  # Define spawning population and recruitment pool
  spawningPool <- unlist(pop) * spawnRecruit
  recruitmentPool <- unlist(pop) - unlist(pop) * spawnRecruit
  
  return(list(
    pop = unlist(pop),
    spawnRecruit = spawnRecruit,
    pRepeat = pRepeat,
    spawningPool = spawningPool,
    recruitmentPool = recruitmentPool  
  ))
}  
  
  
if(river=='connecticut'){
  # Assign the starting population based on
  # a seed of age-1 fish and application
  # of an ocean survival curve. The population
  # size is scaled to make the models run faster.
  # Output is re-scaled
  
  # Original number of Age 1 individuals in the population
  # JMS: moved to outer-loop-sampling.R
  
  # Original number of individuals (calculated from ocean survival) per age
  for (i in 2:maxAge) {
  assign(paste('Age', i, sep = ''),
         Age1 * (cumprod(oceanSurvival[2:i - 1]))[i - 1],
         envir = .shadia)
  }
    
  # Collect age classes in a vector
  pop <- mget(ls(pat = "^Age", envir = .shadia), envir = .shadia)
  pop <- pop[order(sapply(pop,head,1), decreasing=T)]
  
  # Define probability of recruitment to spawn- based on proportion of spawners
  # in each age class (Bailey and Zydlewski 2013)
  spawnRecruit <- c(0, 0, 0.01, 0.33, 0.84, 0.97, 0.99, 1.00)
  
  # Initial probalities of repeat spawning- will be derived in annual loop
  pRepeat <- c(0, 0, 0, 0.004, 0.094286, 0.375714, 0.722286, 1.00)
  
  # Define spawning population and recruitment pool
  spawningPool <- unlist(pop) * spawnRecruit
  recruitmentPool <- unlist(pop) - unlist(pop) * spawnRecruit
  
  return(list(
  pop = unlist(pop),
    spawnRecruit = spawnRecruit,
    pRepeat = pRepeat,
    spawningPool = spawningPool,
    recruitmentPool = recruitmentPool  
    ))
}  
  
 
if(river=='susquehanna'){
  # Assign the starting population based on
  # a seed of age-1 fish and application
  # of an ocean survival curve. The population
  # size is scaled to make the models run faster.
  # Output is re-scaled
  
  # Original number of Age 1 individuals in the population
  # JMS: moved to outer-loop-sampling.R
  
  # Original number of individuals (calculated from ocean survival) per age
  for (i in 2:maxAge) {
  assign(paste('Age', i, sep = ''),
         Age1 * (cumprod(oceanSurvival[2:i - 1]))[i - 1],
         envir = .shadia)
  }
    
  # Collect age classes in a vector
  pop <- mget(ls(pat = "^Age", envir = .shadia), envir = .shadia)
  pop <- pop[order(sapply(pop,head,1), decreasing=T)]
  
  # Define probability of recruitment to spawn- based on proportion of spawners
  # in each age class
  spawnRecruit <- c(0, 0, 0.033, 0.341, 0.564, 0.908, 0.974, 0.992, .996, 0.998, 1.00)
  
  # Initial probalities of repeat spawning- will be derived in annual loop
  pRepeat <- c(0, 0, 0, 0.03, 0.11, 0.38, 0.87, 1, 1, 1, 1)
  
  # Define spawning population and recruitment pool
  spawningPool <- unlist(pop) * spawnRecruit
  recruitmentPool <- unlist(pop) - unlist(pop) * spawnRecruit
  
  return(list(
    pop = unlist(pop),
    spawnRecruit = spawnRecruit,
    pRepeat = pRepeat,
    spawningPool = spawningPool,
    recruitmentPool = recruitmentPool  
  ))
}    
 
  
if(river=='saco'){
  # Assign the starting population based on
  # a seed of age-1 fish and application
  # of an ocean survival curve. The population
  # size is scaled to make the models run faster.
  # Output is re-scaled
  
  # Original number of Age 1 individuals in the population
  # JMS: moved to outer-loop-sampling.R
  
  # Original number of individuals (calculated from ocean survival) per age
    for (i in 2:maxAge) {
    assign(paste('Age', i, sep = ''),
           Age1 * (cumprod(oceanSurvival[2:i - 1]))[i - 1],
           envir = .shadia)
    }
    
  # Collect age classes in a vector
    pop <- mget(ls(pat = "^Age", envir = .shadia), envir = .shadia)
    pop <- pop[order(sapply(pop,head,1), decreasing=T)]
  
  # Define probability of recruitment to spawn- based on proportion of spawners
  # in each age class (Bailey and Zydlewski 2013)
   spawnRecruit <- c(0, 0, 0, 0.02, .23, .48, .64, .71, 1.00)
  
  # Initial probalities of repeat spawning- will be derived in annual loop
    pRepeat <- c(0, 0, 0, 0.004, 0.094286, 0.375714, 0.722286, 1.00, 1.00)
  
  # Define spawning population and recruitment pool
    spawningPool <- unlist(pop) * spawnRecruit
    recruitmentPool <- unlist(pop) - unlist(pop) * spawnRecruit
  
  return(list(
    pop = unlist(pop),
    spawnRecruit = spawnRecruit,
    pRepeat = pRepeat,
    spawningPool = spawningPool,
    recruitmentPool = recruitmentPool  
    )
  )
}    
 
     
if(river=='kennebec'){
  # Assign the starting population based on
  # a seed of age-1 fish and application
  # of an ocean survival curve. The population
  # size is scaled to make the models run faster.
  # Output is re-scaled
  
  # Original number of Age 1 individuals in the population
  # JMS: moved to outer-loop-sampling.R
  
  # Original number of individuals (calculated from ocean survival) per age
    for (i in 2:maxAge) {
    assign(paste('Age', i, sep = ''),
           Age1 * (cumprod(oceanSurvival[2:i - 1]))[i - 1],
           envir = .shadia)
    }
    
  # Collect age classes in a vector
    pop <- mget(ls(pat = "^Age", envir = .shadia), envir = .shadia)
    pop <- pop[order(sapply(pop,head,1), decreasing=T)]
  
  # Define probability of recruitment to spawn- based on proportion of spawners
  # in each age class (Bailey and Zydlewski 2013)
   spawnRecruit <- c(0, 0, 0, 0.02, .23, .48, .64, .71, 1.00)
  
  # Initial probalities of repeat spawning- will be derived in annual loop
    pRepeat <- c(0, 0, 0, 0.004, 0.094286, 0.375714, 0.722286, 1.00, 1.00)
  
  # Define spawning population and recruitment pool
    spawningPool <- unlist(pop) * spawnRecruit
    recruitmentPool <- unlist(pop) - unlist(pop) * spawnRecruit
  
  return(list(
    pop = unlist(pop),
    spawnRecruit = spawnRecruit,
    pRepeat = pRepeat,
    spawningPool = spawningPool,
    recruitmentPool = recruitmentPool  
    )
  )
}    
 
if(river=='hudson'){
  # Assign the starting population based on
  # a seed of age-1 fish and application
  # of an ocean survival curve. The population
  # size is scaled to make the models run faster.
  # Output is re-scaled
  
  # Original number of Age 1 individuals in the population
  # JMS: moved to outer-loop-sampling.R
  
  # Original number of individuals (calculated from ocean survival) per age
    for (i in 2:maxAge) {
    assign(paste('Age', i, sep = ''),
           Age1 * (cumprod(oceanSurvival[2:i - 1]))[i - 1],
           envir = .shadia)
    }
    
  # Collect age classes in a vector
    pop <- mget(ls(pat = "^Age", envir = .shadia), envir = .shadia)
    pop <- pop[order(sapply(pop,head,1), decreasing=T)]
  
  # Define probability of recruitment to spawn (NYSDEC, unpublished)
   spawnRecruit <- c(
      0,
      0.000482393,
      0.034249879,
      0.328750603,
      0.816690786,
      0.983839846,
      0.999276411,
      1,
      1,
      1,
      1,
      1,
      1
    )
  
  # Initial probalities of repeat spawning- will be derived in annual loop
    pRepeat <- c(
      0,
      0,
      0,
      0,
      0,
      0.103896104,
      0.285003111,
      0.620954003,
      0.900369004,
      0.984848485,
      1,
      1,
      1
    )
  # Define spawning population and recruitment pool
    spawningPool <- unlist(pop) * spawnRecruit
    recruitmentPool <- unlist(pop) - unlist(pop) * spawnRecruit
  
  return(list(
    pop = unlist(pop),
    spawnRecruit = spawnRecruit,
    pRepeat = pRepeat,
    spawningPool = spawningPool,
    recruitmentPool = recruitmentPool  
    )
  )
}    
   
}