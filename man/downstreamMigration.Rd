% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/17-downstreamMigration.R
\name{downstreamMigration}
\alias{downstreamMigration}
\title{Downstream migration model}
\usage{
downstreamMigration()
}
\value{
A list containing lists of the numbers of fish from
each cohort reaching consecutive production units, and the
number of fish from each cohort reaching the ocean.
}
\description{
Internal function used to apply a
series of list-based projection operations
to calculate the numbers of males, females, and juveniles
reaching each downstream production unit conditional on
starting production unit, migration route, and age.

Not intended to be called directly, but visible for
model transparency.
}
