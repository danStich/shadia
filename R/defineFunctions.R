# Define R functions

# Function definitions

#' @title Calculate 95 percent confidence intervals
#' 
#' @description A helper function to calculate 95% confidence
#' intervals on an object.
#' 
#' @param x A numeric vector.
#' 
#' @return Returns the 2.5th and 97.5th quantiles of
#' an object.
#' 
#' @examples 
#' 
#' CI(rnorm(n=1e3, 0, 1))
#' 
#' @export
#' 
CI <- function(x) {
  quantile(x, probs = c(0.025, 0.975))
}

#' @title Substring right
#' 
#' @description Collect characters from the right side of a text string.
#' 
#' @param x A character string or vector of
#' character strings.
#' 
#' @param n The number of characters from the right
#' to collect.
#' 
#' @return A character string of n elements from
#' the right of \code{x}.
#' 
#' @examples 
#' 
#' substrRight("shadia", n)
#' 
#' @export
#' 
substrRight <- function(x, n) {
  substr(x, nchar(x) - n + 1, nchar(x))
}

#' @title Inverse logit
#' 
#' @description A function for back-transformation 
#' of variables from the logit scale.
#' 
#' @param x A numeric vector.
#' 
#' @return A numeric vector on the probability scale [0, 1].
#' 
#' @examples 
#' 
#' x <- rnorm(1e3, 0, 1)
#' invlogit(x)
#' 
#' @export
#' 
invlogit <- function(x) {
  exp(x) / (1 + exp(x))
}

#' @title Add stochasticity to a list
#'
#' @description Add stochastic noise to a list variable.
#' 
#' @param x A list of numeric vectors.
#' 
#' @param stoch A numeric vector of length 1.
#' 
#' @return The element-wise product of x and stoch.
#' 
#' @details This function was created to multiply multiple
#' elements of a list by a stochastic variable that was
#' drawn from a random number generator. But, more generally
#' it is just list multiplication with a different name.
#' 
#' @examples 
#' 
#' x <- list(
#'   first = runif(10, 1, 10)
#'   second = runif(10, 10, 20)
#' )
#' 
#' addStochList(x, runif(1, 0, 1))
#' 
#' @export
#' 
addStochList <- function(x, stoch){
  mapply("*", x, stoch)
  return(x)
}
