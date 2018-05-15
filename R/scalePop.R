#scalePop()

# Function for rescaling components of the population
# based on scalar variable in setScalar()

scalePop <- function() {
    pop = unlist(pop) / scalar
    spawningPool = spawningPool / scalar
    recruitmentPool = recruitmentPool / scalar
 
  return(list(
    pop = pop,
    spawningPool = spawningPool,
    recruitmentPool = recruitmentPool
  ))  
       
}