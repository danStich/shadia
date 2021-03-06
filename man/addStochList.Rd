% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/13-01-defineFunctions.R
\name{addStochList}
\alias{addStochList}
\title{Add stochasticity to a list}
\usage{
addStochList(x, stoch)
}
\arguments{
\item{x}{A list of numeric vectors.}

\item{stoch}{A numeric vector of length 1.}
}
\value{
The element-wise product of x and stoch.
}
\description{
Add stochastic noise to a list variable.
}
\details{
This function was created to multiply multiple
elements of a list by a stochastic variable that was
drawn from a random number generator. But, more generally
it is just list multiplication with a different name.
}
\examples{

# x <- list(
#   runif(10, 1, 10),
#   runif(10, 10, 20)
# )
#
# addStochList(x, runif(1, 0, 1))
}
