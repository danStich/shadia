#' @title Get maturity schedule
#'
#' @description Calculate photoperiod by river.
#'
#' @param river One of any rivers implemented in `shadia`.
#'
#' @export
getPhotoperiod <- function(river, day) {

  # Penobscot River:
  if (river == "penobscot") {
    photoPeriod <- daylength(44.39, day)
  }
  # Merrimack River:
  if (river == "merrimack") {
    photoPeriod <- daylength(42.65, day)
  }
  # Connecticut River:
  if (river == "connecticut") {
    photoPeriod <- daylength(42.09, day)
  }
  # Susquehanna River:
  if (river == "susquehanna") {
    photoPeriod <- daylength(40.88, day)
  }
  # Saco River:
  if (river == "saco") {
    photoPeriod <- daylength(43.65, day)
  }
  # Kennebec River:
  if (river == "kennebec") {
    photoPeriod <- daylength(43.65, day)
  }
  # Mohawk and Hudson rivers:
  if (river == "hudson") {
    photoPeriod <- daylength(42.7511, day)
  }

  return(photoPeriod)
}
