#fwFishingMort()

# Assign fw fishing mortality for each PU
# in each migration route

fwFishingMort <- function(){

  inriv = vector(mode = 'list', length = 4)
  inriv[[1]] = c(rep(inRiverF, 3), rep(0, 4))
  inriv[[2]] = c(rep(inRiverF, 4), 0)
  inriv[[3]] = c(rep(inRiverF, 4), rep(0, 4))
  inriv[[4]] = c(rep(inRiverF, 5), 0)
 
  return(list(inriv=inriv))
   
}