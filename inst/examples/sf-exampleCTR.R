# Parallel execution on a local cluster
# -------------------------------------------------------------------------
\dontrun{
  
# R snowfall example

# Load R packages
  library(snowfall)
  library(rlecuyer)
  library(shadia)

# 1. Initialization of snowfall.
# -----  
# Initialize parallel mode using sockets and
# command-line args
sfInit(parallel=TRUE, cpus=3, type="SOCK")

# Display information about nodes and processes
# used by this job. This is entirely optional,
# to demonstrate snowfall methods sfClusterCall()
# and sfCpus().

# Describe the nodes and cpus:
cat(paste0('CPU count: ', sfCpus()), fill=TRUE)

# Count off each process with anonymous function
cat('CPU ids: ', unlist(sfClusterCall(function() Sys.getpid())), fill=TRUE)

# 2. Load data. 
# -----
data('fish')
data('arr.B')
data('arr.R')
data('tempD')
data('tempData')

# 3. Define wrapper function, which can be called in parallel.
#
#   Runs connecticutRiverModel() on each worker
#
#   Here, workerId just contains the identity of the cpu that perfomed
#   the work. We do this only to prove we have used all the specified cpus!
#   Ideally, we will minimize the data sent to (and returned from) the workers.
#
#   Note that constructing and returning a list enables the function to
#   return more than one output.
# -----
wrapper <- function(idx) {

# Get cpu ids  
  workerId <- paste(Sys.info()[['nodename']],
                    Sys.getpid(),
                    sep='-'
                    )

# Run the model
res1 <- connecticutRiverModel(nYears = 40,
                            upstream = list(
                              holyoke = seq(0, 1, 0.10),
                              cabot = seq(0, 1, 0.10),
                              spillway = seq(0, 1, 0.10),
                              gatehouse = seq(0, 1, 0.10),
                              vernon = seq(0, 1, 0.10)
                            ),
                            downstream = list(
                              holyoke = seq(0, 1, 0.10),
                              cabot = seq(0, 1, 0.10),
                              gatehouse = seq(0, 1, 0.10),
                              vernon = seq(0, 1, 0.10)
                            ),
                            pSpillway = 1
        )

# Define the output lists
    retlist <- list(
      worker=workerId,
      res=res1)       
    return(retlist)
}

# 4. Export needed data to workers 
#    load required packages on workers.
# -----
sfLibrary(shadia)

# 5. Start network random number generator 
#    (as "sample" uses random numbers).
# -----
sfClusterSetupRNG()

# 6. Distribute calculation to workers
# -----
niterations <- 10
start <- Sys.time()

# The magic is in snowfall's sfLapply() function,
# which sends wrapper() out to the workers:
result <- sfLapply(1:niterations, wrapper) 

Sys.time()-start

# 7. Stop snowfall
# -----
sfStop()

# 8. Examine the results returned from the cluster:
# -----

# 'result' is a list of lists. Save this:
save(result, file = "snowfall-result.rda")

# Extract results dataframes by string and rbind them
res <- lapply(result, function(x) x[[c('res')]])
resdf <- do.call(rbind, res)

plot(resdf$year, resdf$populationSize)

}