# startingPop()

# Function for creating a starting
# population of fish for simulation

startingPop <- function(){
  
# Assign the starting population based on a seed of age-1 fish and application
# of an ocean survival curve
# The population size is scaled to make the models run faster. Output is re-
# scaled
#scalar = 100 # Done below dynamically now

# Original number of Age 1 individuals in the population
# JMS: moved to outer-loop-sampling.R

# Original number of individuals (calculated from ocean survival) per age
for (i in 2:maxAge) {
assign(paste('Age', i, sep = ''),
       Age1 * (cumprod(oceanSurvival[2:i - 1]))[i - 1])
}
  
# Collect age classes in a vector
pop = mget(ls(pat = "^Age"))

# Define probability of recruitment to spawn- based on proportion of spawners
# in each age class (Bailey and Zydlewski 2013)
spawnRecruit = c(0, 0, 0, 0.01, .33, .84, .97, .99, 1.00)

# Initial probalities of repeat spawning- will be derived in annual loop
pRepeat = c(0, 0, 0, 0.004, 0.094286, 0.375714, 0.722286, 1.00, 1.00)

# Define spawning population and recruitment pool
spawningPool = unlist(pop) * spawnRecruit
recruitmentPool = unlist(pop) - unlist(pop) * spawnRecruit

return(list(
pop = pop,
spawnRecruit = spawnRecruit,
pRepeat = pRepeat,
spawningPool = spawningPool,
recruitmentPool = recruitmentPool  
))


}