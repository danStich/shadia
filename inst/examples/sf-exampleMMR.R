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
  times <- c(1, 3, 7, 20)
  timex <- sample(times, 1)

  upstream <- sample(c(.7, .8, 0.9, 1), 1)
  downstream <- 1

  # Run the model
  sim <- merrimackRiverModel(
    nRuns = 1,
    species = "shad",
    nYears = 50,
    timing = rep(timex, 6),
    upstream = list(
      essex = upstream,
      pawtucketBypass = 1,
      pawtucket = 1,
      amoskeag = 1,
      hookset = 1
    ),
    downstream = list(
      essex = downstream,
      pawtucketBypass = 1,
      pawtucket = 1,
      amoskeag = 1,
      hookset = 1
    ),
    downstream_juv = list(
      essex = downstream,
      pawtucketBypass = 1,
      pawtucket = 1,
      amoskeag = 1,
      hookset = 1
    ),
    pBypassUp = 1,
    pBypassD = 1,
    inRiverF = 0,
    commercialF = 0,
    bycatchF = 0,
    indirect = 1,
    latent = 1,
    watershed = TRUE
  )

  # Output
  return(sim)
}

# Export  libraries or data to workers
sfLibrary(shadia)

# Distribute calculation to workers
niterations <- 200

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
  group_by(year, timing_essex, essex_us) %>%
  summarize(
    pop = mean(populationSize),
    lci = CI(populationSize)[1],
    uci = CI(populationSize)[2],
    .groups = "keep"
  )

ggplot(
  plotter,
  aes(
    x = year, y = pop,
    fill = factor(essex_us),
    color = factor(essex_us)
  )
) +
  geom_line(lwd = 1) +
  geom_ribbon(
    aes(x = year, ymin = lci, ymax = uci, color = NULL),
    alpha = 0.15
  ) +
  xlab("Year") +
  ylab("Millions of spawners") +
  labs(
    fill = "Upstream passage",
    color = "Upstream passage"
  ) +
  scale_y_continuous(
    breaks = seq(0, 10e7, .5e6),
    labels = format(seq(0, 100, 0.5), digits = 2)
  ) +
  theme_bw() +
  theme(
    axis.title.x = element_text(vjust = -1),
    axis.title.y = element_text(vjust = 3),
    legend.position = "top"
  ) +
  facet_wrap(~timing_essex)

}
