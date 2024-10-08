% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/07-definePassageRates.R
\name{definePassageRates}
\alias{definePassageRates}
\title{Assign upstream and downstream passage efficiencies}
\usage{
definePassageRates(river)
}
\value{
A list of upstream and downstream dam passage
efficiencies at each dam in the hydro system.
}
\description{
Internal function passing user-defined
arguments from call to \code{\link{penobscotRiverModel}}
and other river models
to named objects used to define passage rates at
dam rkms in the individual-based migration model.

Not intended to be called directly, but visible for
the sake of model transparency.
}
