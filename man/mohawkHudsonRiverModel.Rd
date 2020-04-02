% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mohawkHudsonRiverModel.R
\name{mohawkHudsonRiverModel}
\alias{mohawkHudsonRiverModel}
\title{Mohawk-Hudson River Model}
\usage{
mohawkHudsonRiverModel(species = "shad", pMohawk = 0.5, nRuns = 1,
  nYears = 40, timing = rep(1, 26), upstream = list(federal = 1, C01
  = 1, C02 = 1, C03 = 1, C04 = 1, C05 = 1, C06 = 1, E02 = 1, E03 = 1, E04 =
  1, E05 = 1, E06 = 1, E07 = 1, E08 = 1, E09 = 1, E10 = 1, E11 = 1, E12 =
  1, E13 = 1, E14 = 1, E15 = 1, E16 = 1, E17 = 1, E18 = 1, E19 = 1, E20 =
  1), downstream = list(federal = 1, C01 = 1, C02 = 1, C03 = 1, C04 = 1,
  C05 = 1, C06 = 1, E02 = 1, E03 = 1, E04 = 1, E05 = 1, E06 = 1, E07 = 1,
  E08 = 1, E09 = 1, E10 = 1, E11 = 1, E12 = 1, E13 = 1, E14 = 1, E15 = 1,
  E16 = 1, E17 = 1, E18 = 1, E19 = 1, E20 = 1), inRiverF = 0,
  commercialF = 0, bycatchF = 0, indirect = 1, latent = 1,
  watershed = TRUE)
}
\arguments{
\item{nRuns}{The number of times that the
model will be run.}

\item{nYears}{The number of years for which
each run will last. The default is 40 years
to match default FERC license duration.}

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
navigational locks. 

See note in \code{upstream}.}

\item{inRiverF}{Annual, recreational harvest of 
American shad. Parameterized as an annual rate [0, 1].}

\item{commercialF}{Commercial fishery mortality
for American shad in marine environment incurred 
through targeted fisheries. Parameterized as an 
annual rate [0, 1].}

\item{bycatchF}{Marine bycatch mortality of
American shad in non-target fisheries. 
Parameterized as an annual rate [0, 1].}

\item{indirect}{Indirect mortality incurred during
freshwater migration as a result of dam-related
impacts (e.g., injury, predation, etc.).}

\item{latent}{Latent mortality incurred during estuary
passage as a result of dam-related impacts (e.g., injury,
delay, etc.).}

\item{watershed}{A logical indicating whether or not
to use the same dam passage efficiencies at all dams
for upstream and downstream. If \code{watershed = TRUE}, then
the first element in lists \code{upstream} and \code{downstream}
are recycled for all subsequent dams.}
}
\value{
Returns a list of two named dataframes.
The first dataframe (\code{res}) contains user-defined
inputs and available model outputs. The second dataframe
(\code{sens}) contains stochastic model inputs based on
empirical data and/or expert opinion.

If run in parallel, returns a list of lists
of dataframes.

The folowing named columns are returned in \code{res}:
\itemize{
    \item \code{year} Year of simulation
    \item \code{timing_lockwood...timing_burnham} Passage timing input by user
    \item \code{federal_us...E20_us} User-specified upstream passage efficiencies
    \item \code{federal_ds...E20_ds}  User-specified downstream passage efficiencies
    \item \code{pRepeat_Age1...pRepeat_Age9} Age-specific probability of repeat spawning  
    \item \code{populationSize} Total number of adult spawners returning to the river
    \item \code{N_IA...N_XIXB} Production unit-specific population size after in-river fishery mortality
    \item \code{pMohawk} Probability of fish using the Mohawk River during upstream migration and spawning.
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
    \item \code{t.stoch} Temperature stochasticity parameter
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
Runs American shad dam passage performance
standard model for Mohawk and Hudson rivers, NY,
USA.
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
