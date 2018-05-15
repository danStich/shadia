#processCohorts()

processCohorts <- function(){

  #if (useTictoc) tic("process Populations")
  #if (useTictoc) tic("process MALES NEW")
  males <- eval(processPopulation(isFemale=FALSE), envir = .GlobalEnv)
  # if (useTictoc) toc()
  #if (useTictoc) tic("process FEMALES NEW")
  females <- eval(processPopulation(isFemale=TRUE), envir = .GlobalEnv)
  #if (useTictoc) toc()
  #if (useTictoc) tic("process EGGS NEW")
  fec_Max <- eval(processPopulation(isFemale=FALSE, isEgg=TRUE), envir = .GlobalEnv)
  #if (useTictoc) toc()
  #if (useTictoc) toc() # process populations  
  
  return(list(  
    males = males,
    females = females,
    fec_Max = fec_Max
  ))   
  
}