#' @title Get maturity schedule
#'
#' @description Get maturity schedule by region.
#'
#' @param region Life-history region (one of `Semelparous`,
#' `Southern Iteroparous` or `Northern Iteroparous`).
#'
#' @export
#'
getMaturity <- function(region) {
  if (region == "Semelparous") {
    spawnRecruit <- c(0, 0, 0.04, 0.19, 0.465, 0.745, 0.96, 1, 1)
  }

  if (region == "Southern Iteroparous") {
    spawnRecruit <- c(0, 0, 0, 0.08, 0.385, 0.715, 0.825, 0.85, 1, 1, 1, 1, 1)
  }

  if (region == "Northern Iteroparous") {
    spawnRecruit <- c(0, 0, 0.015, 0.125, 0.755, 0.845, 0.95, 1, 1, 1, 1, 1, 1)
  }

  return(spawnRecruit)
}
