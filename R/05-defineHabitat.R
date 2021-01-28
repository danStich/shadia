#' @title Production potential definition
#'
#' @description Internal function used to
#' assign initial values of production potential
#' for American shad in production units of Rivers
#'
#' Not intended to be called directly, but visible
#' for model transparency.
#'
#' @return A list of habitat production potential
#' for production units in the Penobscot River, Maine, USA.
#'
#' @references Maine Department of Marine Resources (MDMR). 2009.
#' Operational plan for the restoration of diadromous fishes
#' to the Penobscot River. Final Report, MDMR,
#' Maine Department of Inland Fish and Wildlife,
#' Augusta, ME.
#'
#' Technical Committee for Anadromous Fishery Management
#' of the Merrimack River Basin. 2010. A plan for the
#' restoration of American shad Merrimack River watershed.
#'
#' @export
#'
defineHabitat <- function(river, nRoutes, species) {
  if (river == "penobscot") {
    habitat <- vector(mode = "list", length = nRoutes)

    OronoHabitat <- 1000
    StillwaterHabitat <- 10000
    habitat[[1]] <- c(
      (22344 + 34868),
      (14339 + 34868),
      (400560 + 26285 + 12746),
      (153461 + 37769 + 15257),
      1053,
      22591,
      14922
    )
    habitat[[2]] <- c(
      (22344 + 34868),
      (14339 + 34868),
      (400560 + 26285 + 12746),
      (333196 + 205744),
      (204336 + 25773)
    )
    habitat[[3]] <- c(
      (22344 + 34868),
      (OronoHabitat),
      (StillwaterHabitat),
      (400560 + 26285 + 12746),
      (153461 + 37769 + 15257),
      1053,
      22591,
      14922
    )
    habitat[[4]] <- c(
      (22344 + 34868),
      (OronoHabitat),
      (StillwaterHabitat),
      (400560 + 26285 + 12746),
      (333196 + 205744),
      (204336 + 25773)
    )

    # Bluebacks moving to 500 fish per ha. Based on
    # G. Wippelhauser pers. comm. and S. Ledwin pers. comm (MEDMR)
    if (species == "blueback") {

      # Multiply by 484 and divide out the original 100 per acre (250/ha)
      habitat <- lapply(habitat, function(x) x * (484 / 100))
    }
  }

  if (river == "merrimack") {
    habitat <- vector(mode = "list", length = nRoutes)

    habitat[[1]] <- c(
      202782,
      92910,
      220532,
      48000,
      182598
    )
    habitat[[2]] <- c(
      202782,
      92910,
      220532,
      48000,
      182598
    )

    # Bluebacks moving to 500 fish per ha. Based on
    # G. Wippelhauser pers. comm. and S. Ledwin pers. comm (MEDMR)
    if (species == "blueback") {

      # Multiply by 484 and divide out the original 100 per acre (250/ha)
      habitat <- lapply(habitat, function(x) x * (484 / 100))
    }
  }


  if (river == "connecticut") {
    # Connecticut River relies on habitat estimated
    # production potential of about 100 fish/acre or
    # 250 fish/ha.
    habitat <- vector(mode = "list", length = nRoutes)

    habitat[[1]] <- c(4825, 1369, 0, 762, 1042) * 250
    habitat[[2]] <- c(4825, 1369, 0, 762, 1042) * 250

    # Bluebacks moving to 500 fish per ha. Based on
    # G. Wippelhauser pers. comm. and S. Ledwin pers. comm (MEDMR)
    if (species == "blueback") {

      # Multiply by 484 and divide out the original 100 per acre (250/ha)
      habitat <- lapply(habitat, function(x) x * (484 / .4) / 250)
    }
  }


  if (river == "susquehanna") {

    # Susquehanna River relies on ha, using same numbers
    # as Connecticut River for the sake of consistency
    habitat <- vector(mode = "list", length = nRoutes)

    habitat[[1]] <- c(0, 0, 0, 1833, 11057, 2018) * 250
    habitat[[2]] <- c(0, 0, 0, 1833, 11057, 1489, 1489, 0) * 250
    habitat[[3]] <- c(0, 0, 0, 1833, 11057, 6664, 104, 0) * 250
    habitat[[4]] <- c(0, 0, 0, 1833, 11057, 6664, 1142, 1147, 0, 0) * 250

    # Bluebacks moving to 500 fish per ha. Based on
    # G. Wippelhauser pers. comm. and S. Ledwin pers. comm (MEDMR)
    if (species == "blueback") {

      # Multiply by 484 and divide out the original 100 per acre (250/ha)
      habitat <- lapply(habitat, function(x) x * (484 / .4) / 250)
    }
  }


  if (river == "saco") {

    # Saco River relies on ha, using same numbers as others
    # for sake of consistency
    habitat <- vector(mode = "list", length = nRoutes)

    habitat[[1]] <- c(268, 5, 178, 97, 57, 238, 157) * 250

    # Bluebacks moving to 500 fish per ha. Based on
    # G. Wippelhauser pers. comm. and S. Ledwin pers. comm (MEDMR)
    if (species == "blueback") {

      # Multiply by 484 and divide out the original 100 per acre (250/ha)
      habitat <- lapply(habitat, function(x) x * (484 / .4) / 250)
    }
  }


  if (river == "kennebec") {
    habitat <- vector(mode = "list", length = nRoutes)

    habitat[[1]] <- c(
      523, # Downstream of Lockwood
      0, # Lockwood to Hydro Kennebec
      211, # Hydro Kennebec to Shawmut
      512, # Shawmut to Weston
      415 # Weston to Abanaki incl lower Sandy
    ) * 250
    habitat[[2]] <- c(
      523, # Downstream of Lockwood
      361, # Confluence to Benton Falls
      119 # Upstream of Benton Falls
    ) * 250

    # Bluebacks moving to 500 fish per ha. Based on
    # G. Wippelhauser pers. comm. and S. Ledwin pers. comm (MEDMR)
    if (species == "blueback") {

      # Multiply by 484 and divide out the original 100 per acre (250/ha)
      habitat <- lapply(habitat, function(x) x * (484 / .4) / 250)
    }
  }


  if (river == "hudson") {
    habitat <- vector(mode = "list", length = nRoutes)

    # Considerations for habitat downstream of Federal Dam:
    # 1_Total starting size of 14436 acres downstream of
    #   Federal Dam (Zydlewski et al., In prep)
    # 2_About 60% of that river length is
    #   brackish or salt water(Kingston, roughly - about 145/246 rkm)
    # 3_An additional 1420 ha of habitat (3583 acres) has been
    #   lost to dredge-and-fill operations (NYSDEC 2010)
    #   https://www.dec.ny.gov/docs/remediation_hudson_pdf/shadrecoveryplan.pdf
    habitat[[1]] <- c(14436 * 0.40, 650, 400, 286, 260, 1500, 275, 36) * 100

    habitat[[2]] <- c(
      # 14436 * 0.40 - 3583, # Change here and above depending on 1,2,3
      14436 * 0.40,
      0, 0, 0, 0, 0, 1903, 1072, 337,
      438, 378, 443, 613, 445,
      182, 388, 237, 150, 345,
      261, 268
    ) * 100

    # # Bluebacks moving to 500 fish per ha? Based on
    # # G. Wippelhauser pers. comm. and S. Ledwin pers. comm (MEDMR)
    # if (species == "blueback") {
    # 
    #   # Multiply by 484 and divide out the original 100 per acre (250/ha)
    #   habitat <- lapply(habitat, function(x) x * (484 / 100))
    # }
  }

  return(habitat)
}
