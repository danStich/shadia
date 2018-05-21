# Creation of carrying capacity in production units (PU)
defineHabitat <- function(){

habitat = vector(mode = 'list', length = nRoutes)

# NEED TO RE-COMMENT ALL OF THIS!
  OronoHabitat = 1000
  StillwaterHabitat = 10000
  habitat[[1]] = c(
    (22344 + 34868),
    (14339 + 34868),
    (400560 + 26285 + 12746),
    (153461 + 37769 + 15257),
    1053,
    22591,
    14922
  )
  habitat[[2]] = c((22344 + 34868),
                   (14339 + 34868),
                   (400560 + 26285 + 12746),
                   (333196 + 205744),
                   (204336 + 25773)
  )
  habitat[[3]] = c(
    (22344 + 34868),
    (OronoHabitat),
    (StillwaterHabitat),
    (400560 + 26285 + 12746),
    (153461 + 37769 + 15257),
    1053,
    22591,
    14922
  )
  habitat[[4]] = c((22344 + 34868),
                   (OronoHabitat),
                   (StillwaterHabitat),
                   (400560 + 26285 + 12746),
                   (333196 + 205744),
                   (204336 + 25773)
  )
  
return(list(
  OronoHabitat=OronoHabitat,
  StillwaterHabitat=StillwaterHabitat,
  habitat=habitat
  ))  
  
}  