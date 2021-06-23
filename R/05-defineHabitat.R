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
defineHabitat <- function(river, nRoutes, species, k_method, p_up = 1) {
  if (river == "penobscot") {
    # Group 1: Mainstem to Piscataquis
    # Group 2: Mainstem to mainstem
    # Group 3: Stillwater to Piscataquis
    # Group 4: Stillwater to mainstem    
    # p_up[1]: pStillwaterUp
    # p_up[2]: pPiscUp
    habitat <- vector(mode = "list", length = nRoutes)

    OronoHabitat <- 1000
    StillwaterHabitat <- 10000
    
    habitat[[1]] <- c(
      (22344 + 34868)*(1 - p_up[1])*(p_up[2]),           # Shared with 2, 3, 4
      (14339 + 34868)*(1 - p_up[1])*(p_up[2]),           # Shared with 2, 3, 4
      (400560 + 26285 + 12746)*(1 - p_up[1])*(p_up[2]),  # Shared with 2, 3, 4
      (153461 + 37769 + 15257)*(1 - p_up[1]),  # Shared with 3
      1053*(1 - p_up[1]),                      # Shared with 3
      22591*(1 - p_up[1]),                     # Shared with 3
      14922*(1 - p_up[1])                      # Shared with 3
    )
    habitat[[2]] <- c(
      (22344 + 34868) * (1 - p_up[1]) * (1 - p_up[2]),
      (14339 + 34868) * (1 - p_up[1]) * (1 - p_up[2]),
      (400560 + 26285 + 12746) * (1 - p_up[1]) * (1 - p_up[2]),
      (333196 + 205744) * (1 - p_up[1]),        # Shared with 4
      (204336 + 25773) * (1 - p_up[1])          # Shared with 4
    )
    habitat[[3]] <- c(
      (22344 + 34868) * p_up[1] * p_up[2],          # Shared with 1, 3, 4
      (OronoHabitat) * p_up[1] * p_up[2],           # Shared with 1, 3, 4
      (StillwaterHabitat) * p_up[1] * p_up[2],      # Shared with 1, 3, 4
      (400560 + 26285 + 12746) * p_up[1] * p_up[2], # Shared with 1, 3, 4
      (153461 + 37769 + 15257) * p_up[1] * p_up[2], # Shared with 1
      1053*p_up[1],                      # Shared with 1
      22591*p_up[1],                     # Shared with 1
      14922*p_up[1]                      # Shared with 1
    )
    habitat[[4]] <- c(
      (22344 + 34868) * (p_up[1] * ( 1- p_up[2])),
      (OronoHabitat) * (p_up[1] * ( 1- p_up[2])),
      (StillwaterHabitat) * (p_up[1] * ( 1- p_up[2])),
      (400560 + 26285 + 12746) * (p_up[1] * ( 1- p_up[2])),
      (333196 + 205744) * p_up[1],
      (204336 + 25773) * p_up[1]
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
      202782 * (p_up),
      92910 * (p_up),
      220532 * (p_up),
      48000 * (p_up),
      182598 * (p_up)
    )
    habitat[[2]] <- c(
      202782 * (1 - p_up),
      92910 * (1 - p_up),
      220532 * (1 - p_up),
      48000 * (1 - p_up),
      182598 * (1 - p_up)
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

    habitat[[1]] <- c(4825 * (p_up), 
                      1369* (p_up),
                      0* (p_up), 
                      762* (p_up), 
                      1042* (p_up)) * 250
    habitat[[2]] <- c(4825 * (1 - p_up),
                      1369 * (1 - p_up),
                      0 * (1 - p_up), 
                      762 * (1 - p_up), 
                      1042 * (1 - p_up)) * 250

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

    habitat[[1]] <- c(0, 0, 0,
                      1833 * p_up[1],
                      11057 * p_up[1],
                      2018) * 250 
    habitat[[2]] <- c(0, 0, 0,
                      1833 * p_up[2]/(p_up[2]+p_up[3]+p_up[4]),
                      11057 * p_up[2]/(p_up[2]+p_up[3]+p_up[4]),
                      1489, 
                      1489, 
                      0) * 250
    habitat[[3]] <- c(0, 0, 0,
                      1833 * p_up[3]/(p_up[2]+p_up[3]+p_up[4]),
                      11057 * p_up[3]/(p_up[2]+p_up[3]+p_up[4]),
                      6664 * p_up[3] / (p_up[3] + p_up[4]),
                      104,
                      0) * 250
    habitat[[4]] <- c(0, 0, 0,
                      1833 * p_up[4]/(p_up[2]+p_up[3]+p_up[4]),
                      11057 * p_up[4]/(p_up[2]+p_up[3]+p_up[4]),
                      6664 * p_up[4] / (p_up[3] + p_up[4]),
                      1142,
                      1147,
                      0, 0) * 250
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
      523 * (1 - p_up), # Downstream of Lockwood
      0,                # Lockwood to Hydro Kennebec
      211,              # Hydro Kennebec to Shawmut
      512,              # Shawmut to Weston
      415               # Weston to Abanaki incl lower Sandy
    ) * 250
    habitat[[2]] <- c(
      523 * p_up, # Downstream of Lockwood
      361,        # Confluence to Benton Falls
      119         # Upstream of Benton Falls
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
    habitat[[1]] <- c((14436 * 0.40 - 3583) * (1 - p_up),
                      650, 400, 286, 260, 1500, 275, 36) * 100

    habitat[[2]] <- c(
      # 14436 * 0.40 - 3583, # Change here and above depending on 1,2,3
      (14436 * 0.40 - 3583) * p_up,
      0, 0, 0, 0, 0, 1903, 1072, 337,
      438, 378, 443, 613, 445,
      182, 388, 237, 150, 345,
      261, 268
    ) * 100

    # Bluebacks moving to 500 fish per ha? Based on
    # G. Wippelhauser pers. comm. and S. Ledwin pers. comm (MEDMR)
    ### TESTING NOW
    # if (species == "blueback") {
    # 
    #   # Multiply by 484 and divide out the original 100 per acre (250/ha)
    #   habitat <- lapply(habitat, function(x) x * (484 / 100))
    # }
  }

  
  if (river == "androscoggin") {
    
    habitat <- vector(mode = "list", length = nRoutes)
    # RKM habitat splits for now
    # Split habitat around littlefield (breached)
    # Used CTR Trib value for Little Andro below (G. Wippelhauser, pers comm)
    habitat[[1]] <- c(
      0,        # Downstream of Brunswick
      25738 * (1 - p_up),    # Brunswick to Pejepscot
      17491 * (1 - p_up),    # Pejepscot to Worumbo
      92362 * (1 - p_up),    # Worumbo to Lower Barker, incl. lwr Sabattus
      388,         # Lower to Upper Barker                  
      2509*0.5,    # Upper Barker to Littlefield (breached)
      3980*0.5,    # Littlefield to Hacketts Mills Dam
      6236*0.5,    # Hacketts to Marcal Dam
      4263*0.5,    # Marcal to Welchville
      11736*0.5,   # Welchville to Paris
      4759*0.5     # Paris to Bisco Falls (extent)
    )/10
    habitat[[2]] <- c(
      0,        # Downstream of Brunswick
      25738 * p_up,    # Brunswick to Pejepscot
      17491 * p_up,    # Pejepscot to Worumbo
      92362 * p_up,    # Worumbo to Lower Barker, incl. lwr Sabattus
      4603*0.5, # Farwell Dam to Fortier Dam
      406*0.5   # Fortier to Sabattus Pond Outlet (extent)
    )
    # Bluebacks moving to about 500 fish per ha. Based on
    # G. Wippelhauser and S. Ledwin pers. comm (MEDMR)
    if (species == "blueback") {
      habitat <- lapply(habitat, function(x) x * 968/203/.4)
    }
  }
  
  if(k_method == "cumulative"){
    habitat <- lapply(habitat, cumsum)
  }
  
  return(habitat)
}
