#weldonScenarios()

# Special function to replace Weldon passage
# depending on when passage is implemented
weldonScenarios <- function(){

  if (n < scenario) {
    upEffs[[2]][5] = 0
    upEffs[[4]][6] = 0
  } else {
    upEffs[[2]][5] = MattaceunkUp
    upEffs[[4]][6] = MattaceunkUp
  }
  
  return(list(upEffs=upEffs))
  
}