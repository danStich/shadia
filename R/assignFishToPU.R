#' @title Assign fish to routes
#'
#' @description Generically generate individual x_PUS_n,
#' for fish gender and PU number. Not intended to be called
#' directly. Visible for model transparency.
#'
#' @export
#'
# JMS Dec 2017
assignFishToPU <- function(puNum, PUS) {

  # number of km segments in the PU
  iLen <- length(puNames[[puNum]])

  fishPU <- vector(mode = "list", length = (iLen))

  # Now collect the number of fish in the current PU in each route from the PUS
  # dataframe and add them to the correct elements of the empty list

  for (i in 1:iLen) { # number of km segments in the PU
    for (t in 1:maxAge) { # fish ages in years

      conditionA <- PUS$pus == unique(puNames[[puNum]])[i]
      conditionB <- PUS$fishAges == unique(PUS$fishAges)[t]

      fishPU[[i]][t] <- PUS$gender[conditionA & conditionB]
    }
  }
  return(fishPU)
}
