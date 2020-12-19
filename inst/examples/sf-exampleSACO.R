# Parallel execution on a local cluster
\dontrun{

# Load R packages
library(snowfall)
library(rlecuyer)
library(shadia)
library(tidyverse)

# Initialize parallel socket cluster
sfInit(parallel = TRUE, cpus = 7, type = "SOCK")

# Define a model run as a function
model <- function(x) {

  # Run the model
  sim <- sacoRiverModel()

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

# . Abundance at mouth ----
library(tidyverse)
plotter <- resdf %>%
  group_by(year) %>%
  summarize(
    pop = mean(populationSize),
    lci = CI(populationSize)[1],
    uci = CI(populationSize)[2],
    .groups = "keep"
  )

ggplot(plotter, aes(x = year, y = pop)) +
  geom_line(lwd = 1) +
  geom_ribbon(aes(x = year, ymin = lci, ymax = uci, color = NULL)) +
  xlab("Year") +
  ylab("Millions of spawners") +
  scale_y_continuous(
    breaks = seq(0, 10e7, .5e6),
    labels = format(seq(0, 100, 0.5), digits = 2)
  )

}
