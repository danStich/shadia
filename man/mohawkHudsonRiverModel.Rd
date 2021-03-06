% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/00-mohawkHudsonRiverModel.R
\name{mohawkHudsonRiverModel}
\alias{mohawkHudsonRiverModel}
\title{Mohawk-Hudson River Model}
\usage{
mohawkHudsonRiverModel(
  species = "shad",
  pMohawk = 0,
  nRuns = 1,
  nYears = 40,
  n_adults = 10000,
  timing = rep(1, 26),
  upstream = list(federal = 1, C01 = 1, C02 = 1, C03 = 1, C04 = 1, C05 = 1, C06 = 1,
    E02 = 1, E03 = 1, E04 = 1, E05 = 1, E06 = 1, E07 = 1, E08 = 1, E09 = 1, E10 = 1, E11
    = 1, E12 = 1, E13 = 1, E14 = 1, E15 = 1, E16 = 1, E17 = 1, E18 = 1, E19 = 1, E20 = 1),
  downstream = list(federal = 1, C01 = 1, C02 = 1, C03 = 1, C04 = 1, C05 = 1, C06 = 1,
    E02 = 1, E03 = 1, E04 = 1, E05 = 1, E06 = 1, E07 = 1, E08 = 1, E09 = 1, E10 = 1, E11
    = 1, E12 = 1, E13 = 1, E14 = 1, E15 = 1, E16 = 1, E17 = 1, E18 = 1, E19 = 1, E20 = 1),
  downstream_juv = list(federal = 1, C01 = 1, C02 = 1, C03 = 1, C04 = 1, C05 = 1, C06 =
    1, E02 = 1, E03 = 1, E04 = 1, E05 = 1, E06 = 1, E07 = 1, E08 = 1, E09 = 1, E10 = 1,
    E11 = 1, E12 = 1, E13 = 1, E14 = 1, E15 = 1, E16 = 1, E17 = 1, E18 = 1, E19 = 1, E20
    = 1),
  lockMortality = 0,
  inRiverF = 0,
  commercialF = 0,
  bycatchF = 0,
  indirect = 1,
  latent = 1,
  M = NULL,
  watershed = FALSE,
  k_method = "cumulative",
  sensitivity = FALSE,
  spatially_explicit_output = FALSE,
  output_years = NULL,
  output_p_repeat = FALSE
)
}
\arguments{
\item{species}{Species for which the model will be
run. Current options include American \code{'shad'} and
\code{'blueback'} herring.}

\item{nRuns}{The number of times that the
model will be run.}

\item{nYears}{The number of years for which
each run will last. The default is 40 years
to match default FERC license duration.}

\item{n_adults}{Number of starting adults in
population.}

\item{timing}{The amount of time required for
upstream passage by individual fish (in days),
where the default (1) indicates a 24-h dam
passage performance standard and the value is
specified as a proportion of 1 day.}

\item{upstream}{A named list of upstream dam
passage efficiencies at each dam in the
Hudson River and its largest tributary, the
Mohawk River.

Users may specify a single value of upstream
passage at each dam, or a vector of upstream
passage efficiencies at each dam. Note that
passage efficiences passed as vectors are
randomly sampled during each model run
(not each year). Therefore, multiple model runs
are necessary if more than one passage efficiency
is supplied for any dam. As a rough rule of thumb
we advise a minimum of 100 runs per combination of
management parameters (upstream timing and passage,
and downstream survival through dams).}

\item{downstream}{A named list of downstream
dam passage efficiencies at each dam in the
Mohawk and Hudson rivers including
navigational locks.}

\item{downstream_juv}{A named list of downstream
dam passage efficiencies at each dam in the
Mohawk and Hudson rivers for juveniles.}

\item{lockMortality}{Probability that a fish
dies during upstream passage at each lock. A single
value is applied to all locks.}

\item{inRiverF}{Annual, recreational harvest in river.
Parameterized as an annual rate [0, 1].}

\item{commercialF}{Commercial fishery mortality
in marine environment incurred through targeted
fisheries. Parameterized as an annual rate [0, 1].}

\item{bycatchF}{Marine bycatch mortality of
species in non-target fisheries.
Parameterized as an annual rate [0, 1].}

\item{indirect}{Indirect mortality incurred during
freshwater migration as a result of dam-related
impacts (e.g., injury, predation, etc.).}

\item{latent}{Latent mortality incurred during estuary
passage as a result of dam-related impacts (e.g., injury,
delay, etc.).}

\item{M}{Instantaneous natural mortality.}

\item{watershed}{A logical indicating whether or not
to use the same dam passage efficiencies at all dams
for upstream and downstream. If watershed = TRUE, then
the first element in lists `upstream`, `downstream`,
and `downstream_juv` are recycled for all subsequent dams.}

\item{k_method}{Method used to impose carrying capacity. The
default, `cumulative`, assumes that carrying capacity is based on
all available habitat through the most upstream occupied production
units in a given migration route. The alternative, 'discrete' assumes
that carrying capacity is applied within discrete production units
based on the numbers, and was the method used in Stich et al. (2019).}

\item{sensitivity}{Whether to return a dataframe for sensitivity
analysis. The default is set to FALSE for faster run time and smaller
memory load in parallel processing.}

\item{spatially_explicit_output}{Whether to return population size in each production unit.}

\item{output_years}{Whether to return all years (default = `NULL`) or only
final year of each simulation (`"last"`).}

\item{output_p_repeat}{A logical indicating whether to return pRepeat by
age (in years) with the output. The default value is `FALSE` to
limit output size in physical memory.}
}
\value{
Returns a dataframe when sensitivity = FALSE (default).
Returns a list of two named dataframes when sensitivity = TRUE.
The first dataframe (\code{res}) contains user-defined
inputs and available model outputs depending on optional arguments. 
The second dataframe (\code{sens}) contains input variables for 
sensitivity analysis if desired. If run in parallel, returns a list of
lists of dataframes.

The following named columns may be returned in \code{res}:

\itemize{
    \item \code{year} Year of simulation
    \item \code{species} Species used for simulation
    \item \code{pMohawk} Probability of fish using the Mohawk River during upstream migration and spawning.
    \item \code{timing_federal...timing_E20} Passage timing input by user
    \item \code{federal_us...E20_us} User-specified upstream passage efficiencies
    \item \code{federal_ds...E20_ds}  User-specified downstream passage efficiencies
    \item \code{federal_dsj...E20_dsj}  User-specified juvenile downstream passage efficiencies
    \item \code{lockMortality} User-specified mortality during upstream passage of locks
    \item \code{F.inRiver} User-specified recreational fishing mortality
    \item \code{F.commercial} User-specified recreational fishing mortality
    \item \code{F.bycatch} User-specified recreational fishing mortality
    \item \code{indirect} User-specified indirect mortality dams
    \item \code{latent} User-specified latent mortality
    \item \code{pRepeat_Age1...pRepeat_AgeN} Age-specific probability of repeat spawning
    \item \code{N_1a...N21b} Production unit-specific population size after in-river fishery mortality
    \item \code{populationSize} Number of spawners returning to the river
}

The following named columns are returned in \code{sens}:
\itemize{
    \item \code{S.downstream} Downstream survival per kilometer
    \item \code{S.marine} Marine survival as an annual rate
    \item \code{popStart} Starting population size
    \item \code{p.female} Probability of being female
    \item \code{S.prespawnM} Prespawn survival rate for males
    \item \code{S.postspawnM} Postspawn survival rate for males
    \item \code{S.prespawnF} Postspawn survival rate for males
    \item \code{S.postspawnF} Postspawn survival rate for males
    \item \code{S.juvenile} Hatch to out-migrant survival rate
    \item \code{b.Arr} Mean arrival date for males
    \item \code{r.Arr} Mean arrival date for females
    \item \code{ATUspawn1} Accumulated thermal units at initiation of spawn
    \item \code{ATUspawn2} Accumulated thermal units at termination of spawn
    \item \code{Dspawn1} Initial spawning date
    \item \code{Dspawn2} Terminal spawning date
    \item \code{linF} L-infinity parameter from the von Bertalanffy growth function for females
    \item \code{kF} K parameter from the von Bertalanffy growth function for females
    \item \code{t0F} t0 parameter from the von Bertalanffy growth function for females
    \item \code{linM} L-infinity parameter from the von Bertalanffy growth function for males
    \item \code{kM} K parameter from the von Bertalanffy growth function for males
    \item \code{t0M} t0 parameter from the von Bertalanffy growth function for males
    \item \code{b.length} Mean length of males
    \item \code{r.length} Mean length of females
    \item \code{spawnInt} Mean spawning interval
    \item \code{batchSize} Mean batch size
    \item \code{resTime} Mean residence time
    \item \code{s.Optim} Mean optimal ground speed
    \item \code{d.Max} Mean maximum daily movement rate
    \item \code{tortuosity} Path tortuosity parameter
    \item \code{motivation} Seasonal change in fish "motivation" for upstream movement
    \item \code{daily.move} Mean realized daily movement rate
    \item \code{habStoch} Habitat stochasticity
}
}
\description{
Dam passage performance standard model for
Mohawk and Hudson rivers, NY, USA
}
\section{Schematic of production units}{

Production units delineated by dams in the watershed.
Circles are log proportional to carrying capacity in
each unit. Black dots indicate no suitable habitat
in a unit.
}

\section{Warning about serial execution and memory limits}{


Currently, internal functions rely on \code{list2env} to return
lists to a temporary environment created in the
\code{mohawkHudsonRiverModel} function. Consequently, lists
that are exported must be limited in size. Therefore,
users currently need to limit the number of runs per
call (\code{nRuns} argument) to less than 10 or R will
hit memory limits quickly. In reality, serial
execution is prohibitively slow unless implemented
using manual parallel processing (e.g., bash scripting).

In order to achieve a desired number of runs for a given
set of inputs, the recommended approach is to use
parallel execution as demonstrated using the \code{snowfall}
package in the \href{https://shadia-ui.github.io/examples.html}{website examples}.
}

\examples{
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
  sim <- mohawkHudsonRiverModel(nYears = 40, species = "blueback")

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
plotter <- resdf \%>\%
  group_by(year) \%>\%
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
}
