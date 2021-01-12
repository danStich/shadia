#' @title Write simulation results
#'
#' @description Internal function used to collect and
#' return all model inputs and relevant model
#' outputs (e.g., population size) from all k iterations
#' of n years to the temporary environment
#' in \code{\link{penobscotRiverModel}},
#' \code{\link{merrimackRiverModel}}, and
#' \code{\link{connecticutRiverModel}}.
#'
#' Not intended to be called directly, but visible
#' nonetheless.
#'
#' @return A list of results.
#'
#' @export
#'
writeData <- function() {

  # DATA WRITE ----

  # Unlist and stack proportion of repeat spawners in each age for writing
  pRepeats <- do.call("rbind", lapply(pRepeats, unlist))
  colnames(pRepeats) <- paste("pRepeat_", seq(1, maxAge, 1), sep = "")

  # Unlist and stack upstream passage times
  times <- do.call("rbind", lapply(ptime, unlist))
  colnames(times) <- paste("timing_", 1:length(timely), sep = "")

  # Unlist and stack age-structured spawning population into a useable output
  spawners <- do.call("rbind", lapply(spawningPool, unlist))
  colnames(spawners) <- paste(colnames(spawners), "N", sep = "_")

  # Rescale population size based on reduction factor at start of script
  populationSize <- populationSize

  if (river == "penobscot") {
    # Collect inputs and outputs into a single object for file write
    res <- data.frame(
      years,
      species,
      climate = climate_scen,
      pStillUp = pStillwaterUp,
      pStillD = pStillwaterUp,
      pPiscUp = pPiscUp,
      times,
      pDraws,
      dDraws,
      djDraws,
      F.inRiver,
      F.commercial,
      F.bycatch,
      indirectM,
      latentM,
      pRepeats,
      ceiling(LowerPop),
      ceiling(MilfordPop),
      ceiling(EnfieldPop),
      ceiling(WeldonPop),
      ceiling(HowlandPop),
      ceiling(MoosePop),
      ceiling(BrownsPop),
      ceiling(GuilfordPop),
      ceiling(populationSize)
    )

    names(res) <- c(
      "year",
      "species",
      "climate",
      "pStillUp",
      "pStillD",
      "pPiscUp",
      paste0("timing_", names(upstream)),
      paste0(names(upstream), "_us"),
      paste0(names(downstream), "_ds"),
      paste0(names(downstream_juv), "_dsj"),
      "inriverF",
      "commercialF",
      "bycatchF",
      "indirect",
      "latent",
      colnames(pRepeats),
      "N_pu1A2A",
      "N_pu3A",
      "N_pu4A",
      "N_pu5A",
      "N_pu1B",
      "N_pu2B",
      "N_pu3B",
      "N_pu4B",
      "populationSize"
    )
  }

  if (river == "merrimack") {
    # Collect inputs and outputs into a single object for file write
    res <- data.frame(
      years,
      species,
      pBypassUS,
      pBypassDS,
      times,
      pDraws,
      dDraws,
      djDraws,
      F.inRiver,
      F.commercial,
      F.bycatch,
      indirectM,
      latentM,
      pRepeats,
      ceiling(popI),
      ceiling(popII),
      ceiling(popIII),
      ceiling(popIV),
      ceiling(popV),
      ceiling(populationSize)
    )

    names(res) <- c(
      "year",
      "species",
      "pBypassUp",
      "pBypassD",
      paste0("timing_", names(upstream)),
      paste0(names(upstream), "_us"),
      paste0(names(downstream), "_ds"),
      paste0(names(downstream_juv), "_dsj"),
      "inriverF",
      "commercialF",
      "bycatchF",
      "indirect",
      "latent",
      colnames(pRepeats),
      "N_I",
      "N_II",
      "N_III",
      "N_IV",
      "N_V",
      "populationSize"
    )
  }

  if (river == "connecticut") {
    # Collect inputs and outputs into a single object for file write
    res <- data.frame(
      years,
      species,
      climate = climate_scen,
      pSpill,
      NorthFieldV,
      NorthFieldT,
      NorthFieldVa,
      NorthFieldTa,
      times,
      pDraws,
      dDraws,
      djDraws,
      F.inRiver,
      F.commercial,
      F.bycatch,
      indirectM,
      latentM,
      pRepeats,
      ceiling(popI),
      ceiling(popII),
      ceiling(popIII),
      ceiling(popIV),
      ceiling(popV),
      ceiling(populationSize)
    )

    names(res) <- c(
      "year",
      "species",
      "climate",
      "pSpillway",
      "NorthFieldV",
      "NorthFieldT",
      "NorthFieldVa",
      "NorthFieldTa",
      paste0("timing_", names(upstream)),
      paste0(names(upstream), "_us"),
      paste0(names(downstream), "_ds"),
      paste0(names(downstream_juv), "_dsj"),
      "inriverF",
      "commercialF",
      "bycatchF",
      "indirect",
      "latent",
      colnames(pRepeats),
      "N_I",
      "N_II",
      "N_III",
      "N_IV",
      "N_V",
      "populationSize"
    )
  }

  if (river == "susquehanna") {
    # Collect inputs and outputs into a single object for file write
    res <- data.frame(
      years = years,
      species,
      pJuniataUp,
      pWestBranchUp,
      pChemungUp,
      times,
      pDraws,
      dDraws,
      djDraws,
      F.inRiver = F.inRiver,
      F.commercial = F.commercial,
      F.bycatch = F.bycatch,
      indirectM = indirectM,
      latentM = latentM,
      pRepeats,
      LowPop = LowPop,
      ConPop = ConPop,
      HolPop = HolPop,
      SafPop = SafPop,
      YorPop = YorPop,
      SunPop = SunPop,
      JunPop = JunPop,
      WesPop = WesPop,
      WilPop = WilPop,
      LocPop = LocPop,
      ChePop = ChePop,
      ChaPop = ChaPop,
      NorPop = NorPop,
      RocPop = RocPop,
      UnaPop = UnaPop,
      ColPop = ColPop,
      populationSize = populationSize
    )


    names(res) <- c(
      "year",
      "species",
      "pJuniataUp",
      "pWestBranchUp",
      "pChemungUp",
      paste0("timing_", names(upstream)),
      paste0(names(upstream), "_us"),
      paste0(names(downstream), "_ds"),
      paste0(names(downstream_juv), "_dsj"),
      "inriverF",
      "commercialF",
      "bycatchF",
      "indirect",
      "latent",
      paste0("pRepeat_Age", seq(1, maxAge)),
      "N_1A",
      "N_2A",
      "N_3A",
      "N_4A",
      "N_5A",
      "N_6A",
      "N_1B",
      "N_1C",
      "N_2C",
      "N_3C",
      "N_1D",
      "N_2D",
      "N_7A",
      "N_8A",
      "N_9A",
      "N_10A",
      "populationSize"
    )
  }

  if (river == "saco") {
    # Collect inputs and outputs into a single object for file write
    res <- data.frame(
      years,
      species,
      times,
      pDraws,
      dDraws,
      djDraws,
      F.inRiver,
      F.commercial,
      F.bycatch,
      indirectM,
      latentM,
      pRepeats,
      ceiling(popI),
      ceiling(popII),
      ceiling(popIII),
      ceiling(popIV),
      ceiling(popV),
      ceiling(popVI),
      ceiling(popVII),
      ceiling(populationSize)
    )

    names(res) <- c(
      "year",
      "species",
      paste0("timing_", names(upstream)),
      paste0(names(upstream), "_us"),
      paste0(names(downstream), "_ds"),
      paste0(names(downstream_juv), "_dsj"),
      "inriverF",
      "commercialF",
      "bycatchF",
      "indirect",
      "latent",
      colnames(pRepeats),
      "N_I",
      "N_II",
      "N_III",
      "N_IV",
      "N_V",
      "N_VI",
      "N_VII",
      "populationSize"
    )
  }

  if (river == "kennebec") {
    # Collect inputs and outputs into a single object for file write
    res <- data.frame(
      years = years,
      species,
      pSebasticook = sebasticook,
      times,
      pDraws,
      dDraws,
      djDraws,
      F.inRiver,
      F.commercial,
      F.bycatch,
      indirectM,
      latentM,
      pRepeats,
      ceiling(pop1a),
      ceiling(pop2a),
      ceiling(pop3a),
      ceiling(pop4a),
      ceiling(pop5a),
      ceiling(pop1b),
      ceiling(pop2b),
      ceiling(populationSize)
    )

    names(res) <- c(
      "year",
      "species",
      "pSebasticook",
      paste0("timing_", names(upstream)),
      paste0(names(upstream), "_us"),
      paste0(names(downstream), "_ds"),
      paste0(names(downstream_juv), "_dsj"),
      "inriverF",
      "commercialF",
      "bycatchF",
      "indirect",
      "latent",
      colnames(pRepeats),
      "N_IA",
      "N_IIA",
      "N_IIIA",
      "N_IVA",
      "N_VA",
      "N_IB",
      "N_IIB",
      "populationSize"
    )
  }

  if (river == "hudson") {
    if(watershed == FALSE){
    # Collect inputs and outputs into a single object for file write
    res <- data.frame(
      years,
      species,
      pMohawk = pMohawk,
      times,
      pDraws,
      dDraws,
      djDraws,
      upstreamMortality,
      F.inRiver,
      F.commercial,
      F.bycatch,
      indirectM,
      latentM,
      pRepeats,
      ceiling(pop01a),
      ceiling(pop02a),
      ceiling(pop03a),
      ceiling(pop04a),
      ceiling(pop05a),
      ceiling(pop06a),
      ceiling(pop07a),
      ceiling(pop08a),
      ceiling(pop02b),
      ceiling(pop03b),
      ceiling(pop04b),
      ceiling(pop05b),
      ceiling(pop06b),
      ceiling(pop07b),
      ceiling(pop08b),
      ceiling(pop09b),
      ceiling(pop10b),
      ceiling(pop11b),
      ceiling(pop12b),
      ceiling(pop13b),
      ceiling(pop14b),
      ceiling(pop15b),
      ceiling(pop16b),
      ceiling(pop17b),
      ceiling(pop18b),
      ceiling(pop19b),
      ceiling(pop20b),
      ceiling(pop21b),
      ceiling(populationSize)
    )

    names(res) <- c(
      "year",
      "species",
      "pMohawk",
      paste0("timing_", names(upstream)),
      paste0(names(upstream), "_us"),
      paste0(names(downstream), "_ds"),
      paste0(names(downstream_juv), "_dsj"),
      "lockMortality",
      "inriverF",
      "commercialF",
      "bycatchF",
      "indirect",
      "latent",
      paste0("pRepeat_Age", seq(1, maxAge)),
      "N_1a",
      "N_2a",
      "N_3a",
      "N_4a",
      "N_5a",
      "N_6a",
      "N_7a",
      "N_8a",
      "N_2b",
      "N_3b",
      "N_4b",
      "N_5b",
      "N_6b",
      "N_7b",
      "N_8b",
      "N_9b",
      "N_10b",
      "N_11b",
      "N_12b",
      "N_13b",
      "N_14b",
      "N_15b",
      "N_16b",
      "N_17b",
      "N_18b",
      "N_19b",
      "N_20b",
      "N_21b",
      "populationSize"
    )
    }
    
    if(watershed == TRUE){
    # Collect inputs and outputs into a single object for file write
    res <- data.frame(
      years,
      species,
      pMohawk = pMohawk,
      times[1],
      pDraws[1],
      dDraws[1],
      djDraws[1],
      upstreamMortality,
      F.inRiver,
      F.commercial,
      F.bycatch,
      indirectM,
      latentM,
      pRepeats,
      ceiling(pop01a),
      ceiling(pop02a),
      ceiling(pop03a),
      ceiling(pop04a),
      ceiling(pop05a),
      ceiling(pop06a),
      ceiling(pop07a),
      ceiling(pop08a),
      ceiling(pop02b),
      ceiling(pop03b),
      ceiling(pop04b),
      ceiling(pop05b),
      ceiling(pop06b),
      ceiling(pop07b),
      ceiling(pop08b),
      ceiling(pop09b),
      ceiling(pop10b),
      ceiling(pop11b),
      ceiling(pop12b),
      ceiling(pop13b),
      ceiling(pop14b),
      ceiling(pop15b),
      ceiling(pop16b),
      ceiling(pop17b),
      ceiling(pop18b),
      ceiling(pop19b),
      ceiling(pop20b),
      ceiling(pop21b),
      ceiling(populationSize)
    )

    names(res) <- c(
      "year",
      "species",
      "pMohawk",
      "timing",
      "upstream",
      "downstream",
      "downstream_juv",
      "lockMortality",
      "inriverF",
      "commercialF",
      "bycatchF",
      "indirect",
      "latent",
      paste0("pRepeat_Age", seq(1, maxAge)),
      "N_1a",
      "N_2a",
      "N_3a",
      "N_4a",
      "N_5a",
      "N_6a",
      "N_7a",
      "N_8a",
      "N_2b",
      "N_3b",
      "N_4b",
      "N_5b",
      "N_6b",
      "N_7b",
      "N_8b",
      "N_9b",
      "N_10b",
      "N_11b",
      "N_12b",
      "N_13b",
      "N_14b",
      "N_15b",
      "N_16b",
      "N_17b",
      "N_18b",
      "N_19b",
      "N_20b",
      "N_21b",
      "populationSize"
    )
    }    
    
    
  }

  # Collect variables for sensitivity analysis and save them out
  sens <- data.frame(
    S.downstream,
    S.marine,
    popStart,
    p.female,
    S.prespawnM,
    S.postspawnM,
    S.prespawnF,
    S.postspawnF,
    S.juvenile,
    b.Arr,
    r.Arr,
    ATUspawn1,
    ATUspawn2,
    Dspawn1,
    Dspawn2,
    linF,
    kF,
    t0F,
    linM,
    kM,
    t0M,
    b.length,
    r.length,
    spawnInt,
    batchSize,
    RAF,
    s.Optim,
    d.Max,
    tortuosity,
    motivation,
    daily.move,
    habStoch
  )

  # Output write
  if (sensitivity == TRUE) {
    return(list(
      res = res,
      sens = sens
    ))
  }

  if (sensitivity == FALSE) {
    return(res)
  }
}
