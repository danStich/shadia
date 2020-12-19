# Parallel execution on a local cluster
\dontrun{

# Load R packages
  library(snowfall)
  library(rlecuyer)
  library(shadia)
  library(tidyverse)

# Initialize parallel socket cluster
  sfInit(parallel=TRUE, cpus=7, type="SOCK")

# Define a model run as a function
  model <- function(x) {

  # Run the model
    sim <- connecticutRiverModel(
      nYears = 40,
      upstream = list(
        holyoke = 1,
        cabot = 1,
        spillway = 1,
        gatehouse = 1,
        vernon = 1
        ),
      downstream = list(
        holyoke = 0.80,
        cabot = 0.80,
        gatehouse = 0.80,
        vernon = 0.80
      ),
      downstream_juv = list(
        holyoke = 0.95,
        cabot = 0.95,
        gatehouse = 0.95,
        vernon = 0.95
      ),
      pSpillway = 1
      )
  
  # Output
    return(sim)
}

# Export  libraries or data to workers
  sfLibrary(shadia)

# Distribute calculation to workers
  niterations <- 30

# Use sfLapply() to distribute simulations to workers
# and run the model with these settings in parallel
  result <- sfLapply(1:niterations, model) 
  
# Stop snowfall
  Sys.time() - start  

# Extract user inputs and population metrics
  resdf <- do.call(rbind, result)

# Summarize by year and passage combinations  
  plotter <- resdf %>%
    group_by(timing_holyoke, holyoke_us, holyoke_ds, holyoke_dsj, year) %>%
    summarize(pop = mean(populationSize),
              lci = quantile(populationSize, 0.025),
              uci = quantile(populationSize, 0.975)
              )
  
# Make a plot  
  ggplot(plotter, 
         aes(x = year, y = pop,
             fill = factor(holyoke_us),
             color = factor(holyoke_us)
             )) +
    geom_line(lwd = 1) +
    geom_ribbon(
      aes(x = year, ymin = lci, ymax = uci, color = NULL), alpha = 0.15) +
    xlab("Year") +
    ylab("Millions of spawners") +
    labs(fill = "Upstream passage", color = "Upstream passage") +
    scale_y_continuous(breaks = seq(0,10e7,1e6),
                       labels = format(seq(0, 100, 1), digits=1)) +
    theme_bw() +
    theme(
      axis.title.x = element_text(vjust = -1), 
      axis.title.y = element_text(vjust = 3),
      legend.position = "top"
    ) +
    facet_wrap(~timing_holyoke)  
    

}
