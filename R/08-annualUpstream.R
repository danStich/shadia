#' Annual upstream migration parameters
#'
#' A helper function used to define upstream passage efficiencies
#' and routes based on stochastic variables and user-defined
#' inputs to \code{\link{penobscotRiverModel}}, as well as
#' the probability of using the mainstem Penobscot River
#' during downstream migration. Not intended to
#' be called directly, but visible for model transparency.
#'
#' @param NULL
#'
#' @return A named list of objects used for the annual
#' upstream migration model, including 1) upstream dam
#' passage efficiencies for each of the four possible
#' migration routes (list), 2) probability of using the
#' mainstem Penobscot River around Marsh Island (vector),
#' 3) the probability of using the mainstem Penobscot
#' River during downstream migration (vector), and
#' 4) probability of using the mainstem Penobscot River
#' at the confluence with the Piscataquis River (vector).
#'
#' @export

annualUpstream <- function(river, damRkms) {
  if (river == "penobscot") {
    # Assign efficiencies to the upstream passage groups (pisc or main)
    upEffs <- vector(mode = "list", length = nRoutes)
    upEffs[[1]] <- vector(mode = "numeric", length = length(damRkms[[1]]))
    upEffs[[2]] <- vector(mode = "numeric", length = length(damRkms[[2]]))
    upEffs[[3]] <- vector(mode = "numeric", length = length(damRkms[[3]]))
    upEffs[[4]] <- vector(mode = "numeric", length = length(damRkms[[4]]))

    # Route 1- Mainstem to piscataquis
    upEffs[[1]][1] <- Open
    upEffs[[1]][2] <- Confluence
    upEffs[[1]][3] <- MilfordUp
    upEffs[[1]][4] <- HowlandUp
    upEffs[[1]][5] <- BrownsMillUp
    upEffs[[1]][6] <- MooseheadUp
    upEffs[[1]][7] <- GuilfordUp
    # Route 2- Main-stem to main-stem
    upEffs[[2]][1] <- Open
    upEffs[[2]][2] <- Confluence
    upEffs[[2]][3] <- MilfordUp
    upEffs[[2]][4] <- WestEnfieldUp
    upEffs[[2]][5] <- MattaceunkUp
    # Route 1- Stillwater to piscataquis
    upEffs[[3]][1] <- Open
    upEffs[[3]][2] <- OronoUp
    upEffs[[3]][3] <- StillwaterUp
    upEffs[[3]][4] <- GilmanUp
    upEffs[[3]][5] <- HowlandUp
    upEffs[[3]][6] <- BrownsMillUp
    upEffs[[3]][7] <- MooseheadUp
    upEffs[[3]][8] <- GuilfordUp
    # Route 1- Stillwater to main-stem
    upEffs[[4]][1] <- Open
    upEffs[[4]][2] <- OronoUp
    upEffs[[4]][3] <- StillwaterUp
    upEffs[[4]][4] <- GilmanUp
    upEffs[[4]][5] <- WestEnfieldUp
    upEffs[[4]][6] <- MattaceunkUp

    times <- vector(mode = "list", length = nRoutes)
    times[[1]] <- vector(mode = "numeric", length = length(damRkms[[1]]))
    times[[2]] <- vector(mode = "numeric", length = length(damRkms[[2]]))
    times[[3]] <- vector(mode = "numeric", length = length(damRkms[[3]]))
    times[[4]] <- vector(mode = "numeric", length = length(damRkms[[4]]))

    # Route 1- Mainstem to piscataquis
    times[[1]][1] <- 1
    times[[1]][2] <- 1
    times[[1]][3] <- timely[[1]]
    times[[1]][4] <- timely[[2]]
    times[[1]][5] <- timely[[4]]
    times[[1]][6] <- timely[[5]]
    times[[1]][7] <- timely[[6]]
    # Route 2- Main-stem to main-stem
    times[[2]][1] <- 1
    times[[2]][2] <- 1
    times[[2]][3] <- timely[[1]]
    times[[2]][4] <- timely[[3]]
    times[[2]][5] <- timely[[7]]
    # Route 1- Stillwater to piscataquis
    times[[3]][1] <- 1
    times[[3]][2] <- 1
    times[[3]][3] <- 1
    times[[3]][4] <- 1
    times[[3]][5] <- timely[[2]]
    times[[3]][6] <- timely[[4]]
    times[[3]][7] <- timely[[5]]
    times[[3]][8] <- timely[[6]]
    # Route 1- Stillwater to main-stem
    times[[4]][1] <- 1
    times[[4]][2] <- 1
    times[[4]][3] <- 1
    times[[4]][4] <- 1
    times[[4]][5] <- timely[[3]]
    times[[4]][6] <- timely[[7]]

    return(list(
      times = times,
      upEffs = upEffs
    ))
  }

  if (river == "merrimack") {
    # Assign efficiencies to the upstream passage groups (pisc or main)
    upEffs <- vector(mode = "list", length = nRoutes)

    # Route 1- Bypass at Pawtucket
    upEffs[[1]][1] <- EssexUp
    upEffs[[1]][2] <- PawtucketBypassUp
    upEffs[[1]][3] <- AmoskeagUp
    upEffs[[1]][4] <- HooksetUp
    # Route 2- Mainstem at Pawtucket
    upEffs[[2]][1] <- EssexUp
    upEffs[[2]][2] <- PawtucketUp
    upEffs[[2]][3] <- AmoskeagUp
    upEffs[[2]][4] <- HooksetUp

    # Passage times
    times <- vector(mode = "list", length = nRoutes)

    # Route 1- Bypass at Pawtucket
    times[[1]][1] <- timely[[1]]
    times[[1]][2] <- timely[[2]]
    times[[1]][3] <- timely[[4]]
    times[[1]][4] <- timely[[5]]
    # Route 2- Mainstem at Pawtucket
    times[[2]][1] <- timely[[1]]
    times[[2]][2] <- timely[[3]]
    times[[2]][3] <- timely[[4]]
    times[[2]][4] <- timely[[5]]

    # Output list
    return(list(
      times = times,
      upEffs = upEffs
    ))
  }

  if (river == "connecticut") {
    # Assign efficiencies to the upstream passage groups (pisc or main)
    upEffs <- vector(mode = "list", length = nRoutes)

    # Route 1- Canal
    upEffs[[1]][1] <- HolyokeUp
    upEffs[[1]][2] <- CabotUp
    upEffs[[1]][3] <- GatehouseUp
    upEffs[[1]][4] <- VernonUp

    # Route 2- Spillway
    upEffs[[2]][1] <- HolyokeUp
    upEffs[[2]][2] <- SpillwayUp
    upEffs[[2]][3] <- GatehouseUp
    upEffs[[2]][4] <- VernonUp

    # Upstream passage times
    times <- vector(mode = "list", length = nRoutes)

    # Route 1- Canal route
    times[[1]][1] <- timely[[1]]
    times[[1]][2] <- timely[[2]]
    times[[1]][3] <- timely[[4]]
    times[[1]][4] <- timely[[5]]

    # Route 2- Spillway route
    times[[2]][1] <- timely[[1]]
    times[[2]][2] <- timely[[3]]
    times[[2]][3] <- timely[[4]]
    times[[2]][4] <- timely[[5]]

    # Output list
    return(list(
      times = times,
      upEffs = upEffs
    ))
  }

  if (river == "susquehanna") {
    # Assign efficiencies to the upstream passage groups
    upEffs <- vector(mode = "list", length = nRoutes)
    upEffs[[1]] <- vector(mode = "numeric", length = length(damRkms[[1]]))
    upEffs[[2]] <- vector(mode = "numeric", length = length(damRkms[[2]]))
    upEffs[[3]] <- vector(mode = "numeric", length = length(damRkms[[3]]))
    upEffs[[4]] <- vector(mode = "numeric", length = length(damRkms[[4]]))

    # Route 1- Juniata River
    upEffs[[1]][1] <- ConowingoUp
    upEffs[[1]][2] <- HoltwoodUp
    upEffs[[1]][3] <- SafeHarborUp
    upEffs[[1]][4] <- YorkHavenUp
    upEffs[[1]][5] <- junConfluenceUp
    # Route 2- West Branch Susquehanna
    upEffs[[2]][1] <- ConowingoUp
    upEffs[[2]][2] <- HoltwoodUp
    upEffs[[2]][3] <- SafeHarborUp
    upEffs[[2]][4] <- YorkHavenUp
    upEffs[[2]][5] <- SunburyUp
    upEffs[[2]][6] <- WilliamsportUp
    upEffs[[2]][7] <- LockHavenUp
    # Route 3- Chemung River
    upEffs[[3]][1] <- ConowingoUp
    upEffs[[3]][2] <- HoltwoodUp
    upEffs[[3]][3] <- SafeHarborUp
    upEffs[[3]][4] <- YorkHavenUp
    upEffs[[3]][5] <- SunburyUp
    upEffs[[3]][6] <- NyUp
    upEffs[[3]][7] <- ChaseHibbardUp
    # Route 4- North Branch
    upEffs[[4]][1] <- ConowingoUp
    upEffs[[4]][2] <- HoltwoodUp
    upEffs[[4]][3] <- SafeHarborUp
    upEffs[[4]][4] <- YorkHavenUp
    upEffs[[4]][5] <- SunburyUp
    upEffs[[4]][6] <- NyUp
    upEffs[[4]][7] <- RockBottomUp
    upEffs[[4]][8] <- UnadillaReachUp
    upEffs[[4]][9] <- ColliersvilleUp

    # Dam passage times by route
    times <- vector(mode = "list", length = nRoutes)
    times[[1]] <- vector(mode = "numeric", length = length(damRkms[[1]]))
    times[[2]] <- vector(mode = "numeric", length = length(damRkms[[2]]))
    times[[3]] <- vector(mode = "numeric", length = length(damRkms[[3]]))
    times[[4]] <- vector(mode = "numeric", length = length(damRkms[[4]]))

    # Route 1- Juniata River
    times[[1]][1] <- timely[[1]]
    times[[1]][2] <- timely[[2]]
    times[[1]][3] <- timely[[3]]
    times[[1]][4] <- timely[[4]]
    times[[1]][5] <- 1
    # Route 2- West Branch Susquehanna
    times[[2]][1] <- timely[[1]]
    times[[2]][2] <- timely[[2]]
    times[[2]][3] <- timely[[3]]
    times[[2]][4] <- timely[[4]]
    times[[2]][5] <- timely[[5]]
    times[[2]][6] <- timely[[6]]
    times[[2]][7] <- timely[[7]]
    # Route 3- Chemung River
    times[[3]][1] <- timely[[1]]
    times[[3]][2] <- timely[[2]]
    times[[3]][3] <- timely[[3]]
    times[[3]][4] <- timely[[4]]
    times[[3]][5] <- timely[[5]]
    times[[3]][6] <- 1
    times[[3]][7] <- timely[[8]]
    # Route 4- North Branch
    times[[4]][1] <- timely[[1]]
    times[[4]][2] <- timely[[2]]
    times[[4]][3] <- timely[[3]]
    times[[4]][4] <- timely[[4]]
    times[[4]][5] <- timely[[5]]
    times[[4]][6] <- 1
    times[[4]][7] <- timely[[9]]
    times[[4]][8] <- 1
    times[[4]][9] <- timely[[10]]

    return(list(
      times = times,
      upEffs = upEffs
    ))
  }

  if (river == "saco") {
    # Assign efficiencies to the upstream passage groups (pisc or main)
    upEffs <- vector(mode = "list", length = nRoutes)
    upEffs[[1]] <- vector(mode = "numeric", length = length(damRkms[[1]]))

    # Route 1
    upEffs[[1]][1] <- cataractUp
    upEffs[[1]][2] <- springUp
    upEffs[[1]][3] <- skeltonUp
    upEffs[[1]][4] <- barmillsUp
    upEffs[[1]][5] <- buxtonUp
    upEffs[[1]][6] <- bonnyUp

    # Passage times
    times <- vector(mode = "list", length = nRoutes)

    # Route 1
    times[[1]][1] <- timely[[1]]
    times[[1]][2] <- timely[[2]]
    times[[1]][3] <- timely[[3]]
    times[[1]][4] <- timely[[4]]
    times[[1]][5] <- timely[[5]]
    times[[1]][6] <- timely[[6]]

    # Output list
    return(list(
      times = times,
      upEffs = upEffs
    ))
  }

  if (river == "kennebec") {
    # Assign efficiencies to the upstream passage groups (pisc or main)
    upEffs <- vector(mode = "list", length = nRoutes)

    # Route 1- Mainstem
    upEffs[[1]][1] <- lockwoodUp
    upEffs[[1]][2] <- hydrokennUp
    upEffs[[1]][3] <- shawmutUp
    upEffs[[1]][4] <- westonUp

    # Route 2- Sebasticook
    upEffs[[2]][1] <- bentonUp
    upEffs[[2]][2] <- burnhamUp

    # Upstream passage times
    times <- vector(mode = "list", length = nRoutes)

    # Route 1- Mainstem
    times[[1]][1] <- timely[[1]]
    times[[1]][2] <- timely[[2]]
    times[[1]][3] <- timely[[3]]
    times[[1]][4] <- timely[[4]]

    # Route 2- Sebasticook
    times[[2]][1] <- timely[[5]]
    times[[2]][2] <- timely[[6]]

    # Output list
    return(list(
      times = times,
      upEffs = upEffs
    ))
  }

  if (river == "hudson") {
    # Assign efficiencies to the upstream passage groups (pisc or main)
    upEffs <- vector(mode = "list", length = nRoutes)

    upEffs[[1]][1] <- federalUp
    for (i in 1:length(grep("C", names(upstream)))) {
      upEffs[[1]][i + 1] <- unlist(mget(paste0(names(upstream)[i + 1], "Up"), .shadia))
    }
    upEffs[[2]][1] <- federalUp
    for (i in 1:length(grep("E", names(upstream)))) {
      upEffs[[2]][i + 1] <- unlist(mget(paste0(names(upstream)[i + 7], "Up"), .shadia))
    }

    # Upstream passage times
    times <- vector(mode = "list", length = nRoutes)

    # Route 1- Upper Hudson
    times[[1]][1] <- timely[[1]]
    for (i in 1:length(grep("C", names(upstream)))) {
      times[[1]][i + 1] <- timely[[i + 1]]
    }

    # Route 2- Mohawk River
    times[[2]][1] <- timely[[1]]
    for (i in 1:length(grep("E", names(upstream)))) {
      times[[2]][i + 1] <- timely[[i + 7]]
    }

    # Output list
    return(list(
      times = times,
      upEffs = upEffs
    ))
  }

  if (river == "androscoggin") {
    # Assign efficiencies to the upstream passage groups
    # (main or sabattus)
    upEffs <- vector(mode = "list", length = nRoutes)
    
    # Route 1- Mainstem
    upEffs[[1]][1] <- brunswickUp
    upEffs[[1]][2] <- pejebscotUp
    upEffs[[1]][3] <- worumboUp
    upEffs[[1]][4] <- lbarkerUp
    upEffs[[1]][5] <- ubarkerUp
    upEffs[[1]][6] <- littlefieldUp
    upEffs[[1]][7] <- hackettUp   
    upEffs[[1]][8] <- marcalUp
    upEffs[[1]][9] <- welchvilleUp
    upEffs[[1]][10] <- parisUp       
    
    # Route 2- Sabattus
    upEffs[[2]][1] <- brunswickUp
    upEffs[[2]][2] <- pejebscotUp
    upEffs[[2]][3] <- worumboUp    
    upEffs[[2]][4] <- farwellUp
    upEffs[[2]][5] <- fortierUp

    # Upstream passage times
    times <- vector(mode = "list", length = nRoutes)

    # Route 1- Mainstem
    times[[1]][1] <- timely[[1]]
    times[[1]][2] <- timely[[2]]
    times[[1]][3] <- timely[[3]]
    times[[1]][4] <- timely[[4]]
    times[[1]][5] <- timely[[5]]
    times[[1]][6] <- timely[[6]]
    times[[1]][7] <- timely[[7]]
    times[[1]][8] <- timely[[8]]    
    times[[1]][9] <- timely[[9]]
    times[[1]][10] <- timely[[10]]

    # Route 2- Sabattus
    times[[2]][1] <- timely[[1]]
    times[[2]][2] <- timely[[2]]
    times[[2]][3] <- timely[[3]]
    times[[2]][4] <- timely[[11]]
    times[[2]][5] <- timely[[12]]

    # Output list
    return(list(
      times = times,
      upEffs = upEffs
    ))
  }  
  
}
