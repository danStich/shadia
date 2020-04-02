#' @title Downstream migration model
#' 
#' @description Internal function used to apply a 
#' series of list-based projection operations
#' to calculate the numbers of males, females, and juveniles
#' reaching each downstream production unit conditional on
#' starting production unit, migration route, and age.
#' 
#' Not intended to be called directly, but visible for
#' model transparency.
#' 
#' @return A list containing lists of the numbers of fish from
#' each cohort reaching consecutive production units, and the 
#' number of fish from each cohort reaching the ocean.
#' 
#' @export
#' 
downstreamMigration <- function(){

if(river=='penobscot'){
# Derive downstream passage efficiencies for each group of spawners in each PU.
# This uses the starting position of the fish, and then incorporates cumulative
# dam passage efficiencies from the starting PU to the Bay- including the
# adjustment for proportional passage through the Stillwater Branch that was
# described in Holbrook et al. (2011) and Stich et al. (2015).
# Piscataquis out-migrants that used the main-stem around Marsh Island
sPU_Pisc <- c()
sPU_Pisc[[1]] <- OpenD * (downstreamS ^ puRkm[[1]][1])
sPU_Pisc[[2]] <- (OpenD ^ 2) * (downstreamS ^ puRkm[[1]][2])
sPU_Pisc[[3]] <- sPU_Pisc[[2]] *
  (((1 - pStillwaterD) * MilfordD) + (pStillwaterD * StillwaterD * OronoD *
                                        GilmanD)) *
  (downstreamS ^ puRkm[[1]][3])
sPU_Pisc[[4]] <- sPU_Pisc[[3]]	* HowlandD * (downstreamS ^ puRkm[[1]][4])
sPU_Pisc[[5]] <- sPU_Pisc[[4]] * BrownsMillD * (downstreamS ^ puRkm[[1]][5])
sPU_Pisc[[6]] <- sPU_Pisc[[5]]	* MooseheadD * (downstreamS ^ puRkm[[1]][6])
sPU_Pisc[[7]] <- sPU_Pisc[[6]] * GuilfordD * (downstreamS ^ puRkm[[1]][7])
# Mainstem out-migrants	that used the main-stem around Marsh Island
sPU_Main <- c()
sPU_Main[[1]] <- OpenD * (downstreamS ^ puRkm[[2]][1])
sPU_Main[[2]] <- (OpenD ^ 2) * (downstreamS ^ puRkm[[2]][2])
sPU_Main[[3]] <- sPU_Pisc[[2]] * (((1 - pStillwaterD) * MilfordD) +
                                   (pStillwaterD * StillwaterD * OronoD * GilmanD)) * (downstreamS ^
                                                                                         puRkm[[2]][3])
sPU_Main[[4]] <- sPU_Main[[3]] * WestEnfieldD * (downstreamS ^ puRkm[[2]][4])
sPU_Main[[5]] <- sPU_Main[[4]] * MattaceunkD * (downstreamS ^ puRkm[[2]][5])
# Piscataquis out-migrants that used the Stillwater Branch around Marsh Island
sPU_PiscStill <- c()
sPU_PiscStill[[1]] <- OpenD * (downstreamS ^ puRkm[[3]][1])
sPU_PiscStill[[2]] <- (OpenD ^ 2) * (downstreamS ^ puRkm[[3]][2])
sPU_PiscStill[[3]] <- sPU_PiscStill[[2]] *
  (((1 - pStillwaterD) * MilfordD) + (pStillwaterD * StillwaterD * OronoD)) *
  (downstreamS ^ puRkm[[3]][3])
sPU_PiscStill[[4]] <- sPU_PiscStill[[3]] * (((1 - pStillwaterD) * 1.00) +
                                             (pStillwaterD * GilmanD)) * (downstreamS ^ puRkm[[3]][4])
sPU_PiscStill[[5]] <- sPU_PiscStill[[4]]	* HowlandD *
  (downstreamS ^ puRkm[[3]][5])
sPU_PiscStill[[6]] <- sPU_PiscStill[[5]] * BrownsMillD *
  (downstreamS ^ puRkm[[3]][6])
sPU_PiscStill[[7]] <- sPU_PiscStill[[6]]	* MooseheadD *
  (downstreamS ^ puRkm[[3]][7])
sPU_PiscStill[[8]] <- sPU_PiscStill[[7]] * GuilfordD *
  (downstreamS ^ puRkm[[3]][8])
# Mainstem out-migrants that used the Stillwater Branch around Marsh Island
sPU_MainStill <- c()
sPU_MainStill[[1]] <- OpenD * (downstreamS ^ puRkm[[4]][1])
sPU_MainStill[[2]] <- (OpenD ^ 2) * (downstreamS ^ puRkm[[4]][2])
sPU_MainStill[[3]] <- sPU_MainStill[[2]] *
  (((1 - pStillwaterD) * MilfordD) + (pStillwaterD * StillwaterD * OronoD)) *
  (downstreamS ^ puRkm[[4]][3])
sPU_MainStill[[4]] <- sPU_MainStill[[3]] * (((1 - pStillwaterD) * 1.00) +
                                             (pStillwaterD * GilmanD)) * (downstreamS ^ puRkm[[4]][4])
sPU_MainStill[[5]] <- sPU_MainStill[[4]]  * WestEnfieldD *
  (downstreamS ^ puRkm[[4]][5])
sPU_MainStill[[6]] <- sPU_MainStill[[5]] * MattaceunkD *
  (downstreamS ^ puRkm[[4]][6])

# Derive downstream passage efficiencies for each group of juveniles in each PU.
# This uses the starting position of the fish, and then incorporates cumulative
# dam passage efficiencies from the starting PU to the Bay- including the
# adjustment for proportional passage through the Stillwater Branch that was
# described in Holbrook et al. (2011) and Stich et al. (2015).
# Piscataquis out-migrants that used the main-stem around Marsh Island
sPUj_Pisc <- c()
sPUj_Pisc[[1]] <- OpenD * (downstreamS ^ puRkm[[1]][1])
sPUj_Pisc[[2]] <- (OpenD ^ 2) * (downstreamS ^ puRkm[[1]][2])
sPUj_Pisc[[3]] <- sPUj_Pisc[[2]] *
  (((1 - pStillwaterD) * MilfordDj) +
     (pStillwaterD * StillwaterD * OronoDj * GilmanDj)) * (downstreamS ^
                                                             puRkm[[1]][3])
sPUj_Pisc[[4]] <- sPUj_Pisc[[3]]	* HowlandDj * (downstreamS ^ puRkm[[1]][4])
sPUj_Pisc[[5]] <- sPUj_Pisc[[4]] * BrownsMillDj * (downstreamS ^ puRkm[[1]][5])
sPUj_Pisc[[6]] <- sPUj_Pisc[[5]]	* MooseheadDj * (downstreamS ^ puRkm[[1]][6])
sPUj_Pisc[[7]] <- sPUj_Pisc[[6]] * GuilfordDj * (downstreamS ^ puRkm[[1]][7])
# Mainstem out-migrants	that used the main-stem around Marsh Island
sPUj_Main <- c()
sPUj_Main[[1]] <- OpenD * (downstreamS ^ puRkm[[2]][1])
sPUj_Main[[2]] <- (OpenD ^ 2) * (downstreamS ^ puRkm[[2]][2])
sPUj_Main[[3]] <- sPUj_Pisc[[2]] * (((1 - pStillwaterD) * MilfordDj) +
                                     (pStillwaterD * StillwaterD * OronoDj * GilmanDj)) * (downstreamS ^
                                                                                             puRkm[[2]][3])
sPUj_Main[[4]] <- sPUj_Main[[3]] * WestEnfieldDj * (downstreamS ^ puRkm[[2]][4])
sPUj_Main[[5]] <- sPUj_Main[[4]] * MattaceunkDj * (downstreamS ^ puRkm[[2]][5])
# Piscataquis out-migrants that used the Stillwater Branch around Marsh Island
sPUj_PiscStill = c()
sPUj_PiscStill[[1]] <- OpenD * (downstreamS ^ puRkm[[3]][1])
sPUj_PiscStill[[2]] <- (OpenD ^ 2) * (downstreamS ^ puRkm[[3]][2])
sPUj_PiscStill[[3]] <- sPUj_PiscStill[[2]] *
  (((1 - pStillwaterD) * MilfordDj) + (pStillwaterD * StillwaterDj * OronoDj)) *
  (downstreamS ^ puRkm[[3]][3])
sPUj_PiscStill[[4]] <- sPUj_PiscStill[[3]] * (((1 - pStillwaterD) * 1.00) +
                                               (pStillwaterD * GilmanDj)) * (downstreamS ^ puRkm[[3]][4])
sPUj_PiscStill[[5]] <- sPUj_PiscStill[[4]]	* HowlandDj *
  (downstreamS ^ puRkm[[3]][5])
sPUj_PiscStill[[6]] <- sPUj_PiscStill[[5]] * BrownsMillDj *
  (downstreamS ^ puRkm[[3]][6])
sPUj_PiscStill[[7]] <- sPUj_PiscStill[[6]]	* MooseheadDj *
  (downstreamS ^ puRkm[[3]][7])
sPUj_PiscStill[[8]] <- sPUj_PiscStill[[7]] * GuilfordDj *
  (downstreamS ^ puRkm[[3]][8])
# Mainstem out-migrants that used the Stillwater Branch around Marsh Island
sPUj_MainStill <- c()
sPUj_MainStill[[1]] <- OpenD * (downstreamS ^ puRkm[[4]][1])
sPUj_MainStill[[2]] <- (OpenD ^ 2) * (downstreamS ^ puRkm[[4]][2])
sPUj_MainStill[[3]] <- sPUj_MainStill[[2]] *
  (((1 - pStillwaterD) * MilfordDj) + (pStillwaterD * StillwaterDj * OronoDj)) *
  (downstreamS ^ puRkm[[4]][3])
sPUj_MainStill[[4]] <- sPUj_MainStill[[3]] * (((1 - pStillwaterD) * 1.00) +
                                               (pStillwaterD * GilmanDj)) * (downstreamS ^ puRkm[[4]][4])
sPUj_MainStill[[5]] <- sPUj_MainStill[[4]]  * WestEnfieldDj *
  (downstreamS ^ puRkm[[4]][5])
sPUj_MainStill[[6]] <- sPUj_MainStill[[5]] * MattaceunkDj *
  (downstreamS ^ puRkm[[4]][6])

# Calculate number of males reaching the mouth of the river after spawn from
# each PU from each upstream migration route
malesOut <- vector(mode = 'list', length = length(males))
# Mainstem-to-piscataquis spawners
for (i in 1:length(sPU_Pisc)) {
  malesOut[[1]][[i]] <- mapply("*", males[[1]], sPU_Pisc)[, i]
}
# Mainstem-to-mainstem spawners
for (i in 1:length(sPU_Main)) {
  malesOut[[2]][[i]] <- mapply("*", males[[2]], sPU_Main)[, i]
}
# Stillwater-to-piscataquis spawners
for (i in 1:length(sPU_PiscStill)) {
  malesOut[[3]][[i]] <- mapply("*", males[[3]], sPU_PiscStill)[, i]
}
# Stillwater-to-mainstem spawners
for (i in 1:length(sPU_MainStill)) {
  malesOut[[4]][[i]] <- mapply("*", males[[4]], sPU_MainStill)[, i]
}

# Sum number of males in each age from all PUs reaching river mouth
malesOut <- apply(data.frame(malesOut[[1]]), 1, sum) +
  apply(data.frame(malesOut[[2]]), 1, sum) +
  apply(data.frame(malesOut[[3]]), 1, sum) +
  apply(data.frame(malesOut[[4]]), 1, sum)

# Calculate number of females reaching the mouth of the river after spawn from
# each PU in each route
femalesOut <- vector(mode = 'list', length = length(females))
# Mainstem-to-piscataquis spawners
for (i in 1:length(sPU_Pisc)) {
  femalesOut[[1]][[i]] <- mapply("*", females[[1]], sPU_Pisc)[, i]
}
# Mainstem-to-mainstem spawners
for (i in 1:length(sPU_Main)) {
  femalesOut[[2]][[i]] <- mapply("*", females[[2]], sPU_Main)[, i]
}
# Stillwater-to-piscataquis spawners
for (i in 1:length(sPU_PiscStill)) {
  femalesOut[[3]][[i]] <- mapply("*", females[[3]], sPU_PiscStill)[, i]
}
# Stillwater-to-mainstem spawners
for (i in 1:length(sPU_MainStill)) {
  femalesOut[[4]][[i]] <- mapply("*", females[[4]], sPU_MainStill)[, i]
}

# Sum number of females in each age from all PUs reaching river mouth
femalesOut <- apply(data.frame(femalesOut[[1]]), 1, sum) +
  apply(data.frame(femalesOut[[2]]), 1, sum) +
  apply(data.frame(femalesOut[[3]]), 1, sum) +
  apply(data.frame(femalesOut[[4]]), 1, sum)

# Calculate the number of recruits reaching the ocean
# DOES NOT INCLUDE ELEVATED MORTALITY FOR JUVENILES YET, ALTHOUGH I
# THINK THOSE DATA ARE AVAILABLE
recruitsOut <- vector(mode = 'list', length = length(recruits))
# Mainstem-to-piscataquis spawners
for (i in 1:length(sPUj_Pisc)) {
  recruitsOut[[1]][[i]] <- mapply("*", recruits[[1]][[i]], sPUj_Pisc[i])
}
# Mainstem-to-mainstem spawners
for (i in 1:length(sPUj_Main)) {
  recruitsOut[[2]][[i]] <- mapply("*", recruits[[2]][[i]], sPUj_Main[i])
}
# Stillwater-to-piscataquis spawners
for (i in 1:length(sPUj_PiscStill)) {
  recruitsOut[[3]][[i]] <- mapply("*", recruits[[3]][[i]], sPUj_PiscStill[i])
}
# Stillwater-to-mainstem spawners
for (i in 1:length(sPUj_MainStill)) {
  recruitsOut[[4]][[i]] <- mapply("*", recruits[[4]][[i]], sPUj_MainStill[i])
}

# Sum number of recruits in each age from all PUs reaching river mouth
recruitsOut <- sum(unlist(recruitsOut))

# Sum total number of out migrants
outMigrants <- femalesOut + malesOut

return(
  list(
    sPU_Pisc = sPU_Pisc,
    sPU_Main = sPU_Main,
    sPU_PiscStill = sPU_PiscStill,
    sPU_MainStill = sPU_MainStill,
    sPUj_Pisc = sPUj_Pisc,
    sPUj_Main = sPUj_Main,
    sPUj_PiscStill = sPUj_PiscStill,
    sPUj_MainStill = sPUj_MainStill,
    malesOut = malesOut,
    femalesOut = femalesOut,
    recruitsOut = recruitsOut,
    outMigrants = outMigrants
  )
)

}

if(river=='merrimack'){
  # Derive downstream passage efficiencies for each group of spawners in each PU.
  # This uses the starting position of the fish, and then incorporates cumulative
  # dam passage efficiencies from the starting PU to the ocean.
  # Mainstem at Pawtucket
    sPU <- c()
    sPU[[1]] <- downstreamS ^ puRkm[[1]][1]
    sPU[[2]] <- sPU[[1]] * EssexD * (downstreamS ^ puRkm[[1]][2])
    sPU[[3]] <- sPU[[2]] * (((1-pBypassD)*PawtucketD) + pBypassD*PawtucketBypassD) *
      (downstreamS ^ puRkm[[1]][3])
    sPU[[4]] <- sPU[[3]] * AmoskeagD * (downstreamS ^ puRkm[[1]][4])
    sPU[[5]] <- sPU[[4]] * HooksetD * (downstreamS ^ puRkm[[1]][5])
    
    # Bypass at Pawtucket
    sPU_bp <- c()
    sPU_bp[[1]] <- downstreamS ^ puRkm[[2]][1]
    sPU_bp[[2]] <- sPU_bp[[1]] * EssexD * (downstreamS ^ puRkm[[2]][2])
    sPU_bp[[3]] <- sPU_bp[[2]] * (((1-pBypassD)*PawtucketD) + pBypassD*PawtucketBypassD) *
      (downstreamS ^ puRkm[[1]][3])
    sPU_bp[[4]] <- sPU_bp[[3]] * AmoskeagD * (downstreamS ^ puRkm[[2]][4])
    sPU_bp[[5]] <- sPU_bp[[4]] * HooksetD * (downstreamS ^ puRkm[[2]][5])
  
  # Derive downstream passage efficiencies for each group of juveniles in each PU.
  # This uses the starting position of the fish, and then incorporates cumulative
  # dam passage efficiencies from the starting PU to the ocean.
  # Mainstem at Pawtucket
    sPUj <- c()
    sPUj[[1]] <- downstreamS ^ puRkm[[1]][1]
    sPUj[[2]] <- sPUj[[1]] * EssexDj * (downstreamS ^ puRkm[[1]][2])
    sPUj[[3]] <- sPUj[[2]] * (((1-pBypassD)*PawtucketDj) + pBypassD*PawtucketBypassDj) *
      (downstreamS ^ puRkm[[1]][3])
    sPUj[[4]] <- sPUj[[3]] * AmoskeagDj * (downstreamS ^ puRkm[[1]][4])
    sPUj[[5]] <- sPUj[[4]] * HooksetDj * (downstreamS ^ puRkm[[1]][5])
    
    # Bypass at Pawtucket
    sPU_bpj <- c()
    sPU_bpj[[1]] <- downstreamS ^ puRkm[[2]][1]
    sPU_bpj[[2]] <- sPU_bpj[[1]] * EssexDj * (downstreamS ^ puRkm[[2]][2])
    sPU_bpj[[3]] <- sPU_bpj[[2]] * (((1-pBypassD)*PawtucketDj) + pBypassD*PawtucketBypassDj) *
      (downstreamS ^ puRkm[[1]][3])
    sPU_bpj[[4]] <- sPU_bpj[[3]] * AmoskeagDj * (downstreamS ^ puRkm[[2]][4])
    sPU_bpj[[5]] <- sPU_bpj[[4]] * HooksetDj * (downstreamS ^ puRkm[[2]][5])
  
  # Calculate number of males reaching the mouth of the river after spawn from
  # each PU
    malesOut <- vector(mode = 'list', length = length(males))
    for (i in 1:length(sPU)) {
      malesOut[[1]][[i]] <- mapply("*", males[[1]], sPU)[, i]
      malesOut[[2]][[i]] <- mapply("*", males[[2]], sPU_bp)[, i]    
    }
    # Sum number of males in each age from all PUs reaching river mouth
      malesOut <- apply(data.frame(malesOut[[1]]), 1, sum) +
        apply(data.frame(malesOut[[2]]), 1, sum)
      
  # Calculate number of females reaching the mouth of the river after spawn from
  # each PU
    femalesOut <- vector(mode = 'list', length = length(females))
    for (i in 1:length(sPU)) {
      femalesOut[[1]][[i]] <- mapply("*", females[[1]], sPU)[, i]
      femalesOut[[2]][[i]] <- mapply("*", females[[2]], sPU_bp)[, i]    
    }
    # Sum number of females in each age from all PUs reaching river mouth
      femalesOut <- apply(data.frame(femalesOut[[1]]), 1, sum) +
        apply(data.frame(femalesOut[[2]]), 1, sum)
      
  # Calculate the number of recruits reaching the ocean
    recruitsOut <- vector(mode = 'list', length = length(recruits))
    # Mainstem-to-piscataquis spawners
    for (i in 1:length(sPUj)) {
      recruitsOut[[1]][[i]] <- mapply("*", recruits[[1]][[i]], sPUj[i])
      recruitsOut[[2]][[i]] <- mapply("*", recruits[[2]][[i]], sPU_bpj[i])
    }
    # Sum number of recruits in each age from all PUs reaching river mouth
    recruitsOut <- sum(unlist(recruitsOut))
  
  # Sum total number of out migrants
  outMigrants <- femalesOut + malesOut
  
  return(
    list(
      sPU = sPU,
      sPj = sPUj,
      malesOut = malesOut,
      femalesOut = femalesOut,
      recruitsOut = recruitsOut,
      outMigrants = outMigrants
    )
  )  
}
    
if(river=='connecticut'){
  # Derive downstream passage efficiencies for each group of spawners in each PU.
  # This uses the starting position of the fish, and then incorporates cumulative
  # dam passage efficiencies from the starting PU to the ocean.
  
  # In connecticut river, need to add loss for take at males[[1]][[4]]northfield mountain
  # This occurs for adults and juveniles in the turners falls head pond
  # and in the vernon dam head pond. We use independent rates for adults
  # and for juveniles in each of the reaches.
  
  # Juveniles - these are summed into a single route already 
  # for applying carrying capacity, so we just do that one:
    recruits[[1]][[4]] <- recruits[[1]][[4]]*northfield$turnersJ
    recruits[[1]][[5]]  <- recruits[[1]][[5]]*northfield$vernonJ 
  
  # Males, both routes:
    males[[1]][[4]] <- males[[1]][[4]]*northfield$turnersA
    males[[1]][[5]] <- males[[1]][[5]]*northfield$vernonA   
    males[[2]][[4]] <- males[[2]][[4]]*northfield$turnersA
    males[[2]][[5]] <- males[[2]][[5]]*northfield$vernonA     
  
  # Females, both routes:
    females[[1]][[4]] <- females[[1]][[4]]*northfield$turnersA
    females[[1]][[5]] <- females[[1]][[5]]*northfield$vernonA   
    females[[2]][[4]] <- females[[2]][[4]]*northfield$turnersA
    females[[2]][[5]] <- females[[2]][[5]]*northfield$vernonA  
  
  # In the connecticut river, all downstream fish passage
  # is designed to occur through gatehouse, so we can get
  # away with one migration route here.
    sPU <- c()
    sPU[[1]] <- downstreamS ^ puRkm[[1]][1]
    sPU[[2]] <- sPU[[1]] * HolyokeD * (downstreamS ^ puRkm[[1]][2])
    sPU[[3]] <- sPU[[2]] * CabotD * (downstreamS ^ puRkm[[1]][3])
    sPU[[4]] <- sPU[[3]] * GatehouseD * (downstreamS ^ puRkm[[1]][4])
    sPU[[5]] <- sPU[[4]] * VernonD * (downstreamS ^ puRkm[[1]][5])
    
  # Derive downstream passage efficiencies for each group of juveniles in each PU.
  # This uses the starting position of the fish, and then incorporates cumulative
  # dam passage efficiencies from the starting PU to the ocean.
    sPUj <- c()
    sPUj[[1]] <- downstreamS ^ puRkm[[1]][1]
    sPUj[[2]] <- sPUj[[1]] * HolyokeD * (downstreamS ^ puRkm[[1]][2])
    sPUj[[3]] <- sPUj[[2]] * CabotD * (downstreamS ^ puRkm[[1]][3])
    sPUj[[4]] <- sPUj[[3]] * GatehouseD * (downstreamS ^ puRkm[[1]][4])
    sPUj[[5]] <- sPUj[[4]] * VernonD * (downstreamS ^ puRkm[[1]][5])

  # Calculate number of males reaching the mouth of the river after spawn from
  # each PU
    malesOut <- vector(mode = 'list', length = length(males))
    for (i in 1:length(sPU)) {
      malesOut[[1]][[i]] <- mapply("*", males[[1]], sPU)[, i]
      malesOut[[2]][[i]] <- mapply("*", males[[2]], sPU)[, i]
    }
    
    # Sum number of males in each age from all PUs reaching river mouth
      malesOut <- apply(data.frame(malesOut[[1]]), 1, sum) +
        apply(data.frame(malesOut[[2]]), 1, sum)
  
  # Calculate number of females reaching the mouth of the river after spawn from
  # each PU
    femalesOut <- vector(mode = 'list', length = length(females))
    for (i in 1:length(sPU)) {
      femalesOut[[1]][[i]] <- mapply("*", females[[1]], sPU)[, i]
      femalesOut[[2]][[i]] <- mapply("*", females[[2]], sPU)[, i]
    }
    
    # Sum number of females in each age from all PUs reaching river mouth
      femalesOut <- apply(data.frame(femalesOut[[1]]), 1, sum) +
        apply(data.frame(femalesOut[[2]]), 1, sum)
    
  # Calculate the number of recruits reaching the ocean
    recruitsOut <- vector(mode = 'list', length = length(recruits))
    # Mainstem-to-piscataquis spawners
      for (i in 1:length(sPUj)) {
        recruitsOut[[1]][[i]] <- mapply("*", recruits[[1]][[i]], sPUj[i])
      }
      # Sum number of recruits in each age from all PUs reaching river mouth
      recruitsOut <- sum(unlist(recruitsOut))
  
      # Sum total number of out migrants
      outMigrants <- femalesOut + malesOut
    
  return(
    list(
      sPU = sPU,
      sPj = sPUj,
      malesOut = malesOut,
      femalesOut = femalesOut,
      recruitsOut = recruitsOut,
      outMigrants = outMigrants
    )
  )  
}  

if(river=='susquehanna'){
  # Derive downstream passage efficiencies for each group of spawners in each PU.
  # This uses the starting position of the fish, and then incorporates cumulative
  # dam passage efficiencies from the starting PU to the ocean.
  sPU_jun <- vector(mode='list', length=nPU[1])
  sPU_jun[[1]] <- downstreamS ^ puRkm[[1]][1]
  sPU_jun[[2]] <- sPU_jun[[1]] * ConowingoD * (downstreamS ^ puRkm[[1]][2])
  sPU_jun[[3]] <- sPU_jun[[2]] * HoltwoodD * (downstreamS ^ puRkm[[1]][3])
  sPU_jun[[4]] <- sPU_jun[[3]] * SafeHarborD * (downstreamS ^ puRkm[[1]][4])
  sPU_jun[[5]] <- sPU_jun[[4]] * YorkHavenD * (downstreamS ^ puRkm[[1]][5])
  sPU_jun[[6]] <- sPU_jun[[5]] * junConfluenceD * (downstreamS ^ puRkm[[1]][6])
  
  sPU_wes <- vector(mode='list', length=nPU[2])
  sPU_wes[[1]] <- downstreamS ^ puRkm[[1]][1]
  sPU_wes[[2]] <- sPU_wes[[1]] * ConowingoD * (downstreamS ^ puRkm[[2]][2])
  sPU_wes[[3]] <- sPU_wes[[2]] * HoltwoodD * (downstreamS ^ puRkm[[2]][3])
  sPU_wes[[4]] <- sPU_wes[[3]] * SafeHarborD * (downstreamS ^ puRkm[[2]][4])
  sPU_wes[[5]] <- sPU_wes[[4]] * YorkHavenD * (downstreamS ^ puRkm[[2]][5])
  sPU_wes[[6]] <- sPU_wes[[5]] * SunburyD * (downstreamS ^ puRkm[[2]][6])
  sPU_wes[[7]] <- sPU_wes[[6]] * WilliamsportD * (downstreamS ^ puRkm[[2]][7])
  sPU_wes[[8]] <- sPU_wes[[7]] * LockHavenD * (downstreamS ^ puRkm[[2]][8])
  
  sPU_che <- vector(mode='list', length=nPU[3])
  sPU_che[[1]] <- downstreamS ^ puRkm[[3]][1]
  sPU_che[[2]] <- sPU_che[[1]] * ConowingoD * (downstreamS ^ puRkm[[3]][2])
  sPU_che[[3]] <- sPU_che[[2]] * HoltwoodD * (downstreamS ^ puRkm[[3]][3])
  sPU_che[[4]] <- sPU_che[[3]] * SafeHarborD * (downstreamS ^ puRkm[[3]][4])
  sPU_che[[5]] <- sPU_che[[4]] * YorkHavenD * (downstreamS ^ puRkm[[3]][5])
  sPU_che[[6]] <- sPU_che[[5]] * SunburyD * (downstreamS ^ puRkm[[3]][6])
  sPU_che[[7]] <- sPU_che[[6]] * NyD * (downstreamS ^ puRkm[[3]][7])
  sPU_che[[8]] <- sPU_che[[7]] * ChaseHibbardD * (downstreamS ^ puRkm[[3]][8])
  
  sPU_nor <- vector(mode='list', length=nPU[4])
  sPU_nor[[1]] <- downstreamS ^ puRkm[[1]][1]
  sPU_nor[[2]] <- sPU_nor[[1]] * ConowingoD * (downstreamS ^ puRkm[[4]][2])
  sPU_nor[[3]] <- sPU_nor[[2]] * HoltwoodD * (downstreamS ^ puRkm[[4]][3])
  sPU_nor[[4]] <- sPU_nor[[3]] * SafeHarborD * (downstreamS ^ puRkm[[4]][4])
  sPU_nor[[5]] <- sPU_nor[[4]] * YorkHavenD * (downstreamS ^ puRkm[[4]][5])
  sPU_nor[[6]] <- sPU_nor[[5]] * SunburyD * (downstreamS ^ puRkm[[4]][6])
  sPU_nor[[7]] <- sPU_nor[[6]] * NyD * (downstreamS ^ puRkm[[4]][7])
  sPU_nor[[8]] <- sPU_nor[[7]] * RockBottomD * (downstreamS ^ puRkm[[4]][8])
  sPU_nor[[9]] <- sPU_nor[[8]] * UnadillaReachD * (downstreamS ^ puRkm[[4]][9])
  sPU_nor[[10]] <- sPU_nor[[9]] * ColliersvilleD * (downstreamS ^ puRkm[[4]][10])

  
  # Derive downstream passage efficiencies for each group of juveniles in each PU.
  # This uses the starting position of the fish, and then incorporates cumulative
  # dam passage efficiencies from the starting PU to the ocean.
  sPU_junj <- vector(mode='list', length=nPU[1])
  sPU_junj[[1]] <- downstreamS ^ puRkm[[1]][1]
  sPU_junj[[2]] <- sPU_junj[[1]] * ConowingoDj * (downstreamS ^ puRkm[[1]][2])
  sPU_junj[[3]] <- sPU_junj[[2]] * HoltwoodDj * (downstreamS ^ puRkm[[1]][3])
  sPU_junj[[4]] <- sPU_junj[[3]] * SafeHarborDj * (downstreamS ^ puRkm[[1]][4])
  sPU_junj[[5]] <- sPU_junj[[4]] * YorkHavenDj * (downstreamS ^ puRkm[[1]][5])
  sPU_junj[[6]] <- sPU_junj[[5]] * junConfluenceDj * (downstreamS ^ puRkm[[1]][6])
  
  sPU_wesj <- vector(mode='list', length=nPU[2])
  sPU_wesj[[1]] <- downstreamS ^ puRkm[[1]][1]
  sPU_wesj[[2]] <- sPU_wesj[[1]] * ConowingoDj * (downstreamS ^ puRkm[[2]][2])
  sPU_wesj[[3]] <- sPU_wesj[[2]] * HoltwoodDj * (downstreamS ^ puRkm[[2]][3])
  sPU_wesj[[4]] <- sPU_wesj[[3]] * SafeHarborDj * (downstreamS ^ puRkm[[2]][4])
  sPU_wesj[[5]] <- sPU_wesj[[4]] * YorkHavenDj * (downstreamS ^ puRkm[[2]][5])
  sPU_wesj[[6]] <- sPU_wesj[[5]] * SunburyDj * (downstreamS ^ puRkm[[2]][6])
  sPU_wesj[[7]] <- sPU_wesj[[6]] * WilliamsportDj * (downstreamS ^ puRkm[[2]][7])
  sPU_wesj[[8]] <- sPU_wesj[[7]] * LockHavenDj * (downstreamS ^ puRkm[[2]][8])
  
  sPU_chej <- vector(mode='list', length=nPU[3])
  sPU_chej[[1]] <- downstreamS ^ puRkm[[1]][1]
  sPU_chej[[2]] <- sPU_chej[[1]] * ConowingoDj * (downstreamS ^ puRkm[[3]][2])
  sPU_chej[[3]] <- sPU_chej[[2]] * HoltwoodDj * (downstreamS ^ puRkm[[3]][3])
  sPU_chej[[4]] <- sPU_chej[[3]] * SafeHarborDj * (downstreamS ^ puRkm[[3]][4])
  sPU_chej[[5]] <- sPU_chej[[4]] * YorkHavenDj * (downstreamS ^ puRkm[[3]][5])
  sPU_chej[[6]] <- sPU_chej[[5]] * SunburyDj * (downstreamS ^ puRkm[[3]][6])
  sPU_chej[[7]] <- sPU_chej[[6]] * NyDj * (downstreamS ^ puRkm[[3]][7])
  sPU_chej[[8]] <- sPU_chej[[7]] * ChaseHibbardDj * (downstreamS ^ puRkm[[3]][8])
  
  sPU_norj <- vector(mode='list', length=nPU[4])
  sPU_norj[[1]] <- downstreamS ^ puRkm[[1]][1]
  sPU_norj[[2]] <- sPU_norj[[1]] * ConowingoDj * (downstreamS ^ puRkm[[4]][2])
  sPU_norj[[3]] <- sPU_norj[[2]] * HoltwoodDj * (downstreamS ^ puRkm[[4]][3])
  sPU_norj[[4]] <- sPU_norj[[3]] * SafeHarborDj * (downstreamS ^ puRkm[[4]][4])
  sPU_norj[[5]] <- sPU_norj[[4]] * YorkHavenDj * (downstreamS ^ puRkm[[4]][5])
  sPU_norj[[6]] <- sPU_norj[[5]] * SunburyDj * (downstreamS ^ puRkm[[4]][6])
  sPU_norj[[7]] <- sPU_norj[[6]] * NyDj * (downstreamS ^ puRkm[[4]][7])
  sPU_norj[[8]] <- sPU_norj[[7]] * RockBottomDj * (downstreamS ^ puRkm[[4]][8])
  sPU_norj[[9]] <- sPU_norj[[8]] * UnadillaReachDj * (downstreamS ^ puRkm[[4]][9])
  sPU_norj[[10]] <- sPU_norj[[9]] * ColliersvilleDj * (downstreamS ^ puRkm[[4]][10])

# Calculate number of males reaching the mouth of the river after spawn from
# each PU from each upstream migration route
  malesOut <- vector(mode = 'list', length = length(males))
  # Juniata
  for (i in 1:length(sPU_jun)) {
    malesOut[[1]][[i]] <- mapply("*", males[[1]], sPU_jun)[, i]
  }
  # West Branch
  for (i in 1:length(sPU_wes)) {
    malesOut[[2]][[i]] <- mapply("*", males[[2]], sPU_wes)[, i]
  }
  # Chemung
  for (i in 1:length(sPU_che)) {
    malesOut[[3]][[i]] <- mapply("*", males[[3]], sPU_che)[, i]
  }
  # North Branch
  for (i in 1:length(sPU_nor)) {
    malesOut[[4]][[i]] <- mapply("*", males[[4]], sPU_nor)[, i]
  }
  # Sum number of males in each age from all PUs reaching river mouth
  malesOut <- apply(data.frame(malesOut[[1]]), 1, sum) +
    apply(data.frame(malesOut[[2]]), 1, sum) +
    apply(data.frame(malesOut[[3]]), 1, sum) +
    apply(data.frame(malesOut[[4]]), 1, sum)

# Calculate number of females reaching the mouth of the river after spawn from
# each PU
  femalesOut <- vector(mode = 'list', length = length(females))
  # Juniata
  for (i in 1:length(sPU_jun)) {
    femalesOut[[1]][[i]] <- mapply("*", females[[1]], sPU_jun)[, i]
  }
  # West Branch
  for (i in 1:length(sPU_wes)) {
    femalesOut[[2]][[i]] <- mapply("*", females[[2]], sPU_wes)[, i]
  }
  # Chemung
  for (i in 1:length(sPU_che)) {
    femalesOut[[3]][[i]] <- mapply("*", females[[3]], sPU_che)[, i]
  }
  # North Brance
  for (i in 1:length(sPU_nor)) {
    femalesOut[[4]][[i]] <- mapply("*", females[[4]], sPU_nor)[, i]
  }
  # Sum number of females in each age from all PUs reaching river mouth
  femalesOut <- apply(data.frame(femalesOut[[1]]), 1, sum) +
    apply(data.frame(femalesOut[[2]]), 1, sum) +
    apply(data.frame(femalesOut[[3]]), 1, sum) +
    apply(data.frame(femalesOut[[4]]), 1, sum)
  
# Calculate the number of recruits reaching the ocean
  recruitsOut <- vector(mode = 'list', length = length(recruits))
  # Juniata
  for (i in 1:length(sPU_junj)) {
    recruitsOut[[1]][[i]] <- mapply("*", recruits[[1]][[i]], sPU_junj[i])
  }
  # West Branch
  for (i in 1:length(sPU_wesj)) {
    recruitsOut[[2]][[i]] <- mapply("*", recruits[[2]][[i]], sPU_wesj[i])
  }
  # Chemung
  for (i in 1:length(sPU_chej)) {
    recruitsOut[[3]][[i]] <- mapply("*", recruits[[3]][[i]], sPU_chej[i])
  }
  # North Branch
  for (i in 1:length(sPU_norj)) {
    recruitsOut[[4]][[i]] <- mapply("*", recruits[[4]][[i]], sPU_norj[i])
  }

  # Sum number of recruits in each age from all PUs reaching river mouth
  recruitsOut <- sum(unlist(recruitsOut))
  
# Sum total number of out migrants
  outMigrants <- femalesOut + malesOut

return(
  list(
    sPU_jun = sPU_jun,
    sPU_wes = sPU_wes,
    sPU_che = sPU_che,
    sPU_nor = sPU_nor,
    sPU_junj = sPU_junj,
    sPU_wesj = sPU_wesj,
    sPU_chej = sPU_chej,
    sPU_norj = sPU_norj,
    malesOut = malesOut,
    femalesOut = femalesOut,
    recruitsOut = recruitsOut,
    outMigrants = outMigrants
  )
)  
}

if(river=='saco'){
# Derive downstream passage efficiencies for each group of spawners in each PU.
# This uses the starting position of the fish, and then incorporates cumulative
# dam passage efficiencies from the starting PU to the ocean.
# Mainstem at Pawtucket
  sPU <- c()
  sPU[[1]] <- downstreamS ^ puRkm[[1]][1]
  sPU[[2]] <- sPU[[1]] * cataractD * (downstreamS ^ puRkm[[1]][2])
  sPU[[3]] <- sPU[[2]] * springD * (downstreamS ^ puRkm[[1]][3])
  sPU[[4]] <- sPU[[3]] * skeltonD * (downstreamS ^ puRkm[[1]][4])
  sPU[[5]] <- sPU[[4]] * barmillsD * (downstreamS ^ puRkm[[1]][5])
  sPU[[6]] <- sPU[[5]] * buxtonD * (downstreamS ^ puRkm[[1]][6])
  sPU[[7]] <- sPU[[6]] * bonnyD * (downstreamS ^ puRkm[[1]][7])

# Derive downstream passage efficiencies for each group of juveniles in each PU.
# This uses the starting position of the fish, and then incorporates cumulative
# dam passage efficiencies from the starting PU to the ocean.
# Mainstem at Pawtucket
  sPUj <- c()
  sPUj[[1]] <- downstreamS ^ puRkm[[1]][1]
  sPUj[[2]] <- sPUj[[1]] * cataractDj * (downstreamS ^ puRkm[[1]][2])
  sPUj[[3]] <- sPUj[[2]] * springDj * (downstreamS ^ puRkm[[1]][3])
  sPUj[[4]] <- sPUj[[3]] * skeltonDj * (downstreamS ^ puRkm[[1]][4])
  sPUj[[5]] <- sPUj[[4]] * barmillsDj * (downstreamS ^ puRkm[[1]][5])
  sPUj[[6]] <- sPUj[[5]] * buxtonDj * (downstreamS ^ puRkm[[1]][6])
  sPUj[[7]] <- sPUj[[6]] * bonnyDj * (downstreamS ^ puRkm[[1]][7])
  
# Calculate number of males reaching the mouth of the river after spawn from
# each PU
  malesOut <- vector(mode = 'list', length = length(males))
  for (i in 1:length(sPU)) {
    malesOut[[1]][[i]] <- mapply("*", males[[1]], sPU)[, i]
  }
  # Sum number of males in each age from all PUs reaching river mouth
    malesOut <- apply(data.frame(malesOut[[1]]), 1, sum)
    
# Calculate number of females reaching the mouth of the river after spawn from
# each PU
  femalesOut <- vector(mode = 'list', length = length(females))
  for (i in 1:length(sPU)) {
    femalesOut[[1]][[i]] <- mapply("*", females[[1]], sPU)[, i]
  }
  # Sum number of females in each age from all PUs reaching river mouth
    femalesOut <- apply(data.frame(femalesOut[[1]]), 1, sum)
    
# Calculate the number of recruits reaching the ocean
  recruitsOut <- vector(mode = 'list', length = length(recruits))
  # Mainstem-to-piscataquis spawners
  for (i in 1:length(sPUj)) {
    recruitsOut[[1]][[i]] <- mapply("*", recruits[[1]][[i]], sPUj[i])
  }
  # Sum number of recruits in each age from all PUs reaching river mouth
  recruitsOut <- sum(unlist(recruitsOut))

# Sum total number of out migrants
  outMigrants <- femalesOut + malesOut

return(
  list(
    sPU = sPU,
    sPj = sPUj,
    malesOut = malesOut,
    femalesOut = femalesOut,
    recruitsOut = recruitsOut,
    outMigrants = outMigrants
  )
)  
}
  
if(river=='kennebec'){
  # Derive downstream passage efficiencies for each group of spawners in each PU.
  # This uses the starting position of the fish, and then incorporates cumulative
  # dam passage efficiencies from the starting PU to the ocean.
  # Mainstem
    sPU <- c()
    sPU[[1]] <- downstreamS ^ puRkm[[1]][1]
    sPU[[2]] <- sPU[[1]] * lockwoodD * (downstreamS ^ puRkm[[1]][2])
    sPU[[3]] <- sPU[[2]] * hydrokennD * (downstreamS ^ puRkm[[1]][3])
    sPU[[4]] <- sPU[[3]] * shawmutD * (downstreamS ^ puRkm[[1]][4])
    sPU[[5]] <- sPU[[4]] * westonD * (downstreamS ^ puRkm[[1]][5])
    
    # Sebasticook
    sPU_seb <- c()
    sPU_seb[[1]] <- downstreamS ^ puRkm[[2]][1]
    sPU_seb[[2]] <- sPU_seb[[1]] * bentonD * (downstreamS ^ puRkm[[2]][2])
    sPU_seb[[3]] <- sPU_seb[[2]] * burnhamD * (downstreamS ^ puRkm[[2]][3])
  
  # Derive downstream passage efficiencies for each group of juveniles in each PU.
  # This uses the starting position of the fish, and then incorporates cumulative
  # dam passage efficiencies from the starting PU to the ocean.
  # Mainstem
    sPUj <- c()
    sPUj[[1]] <- downstreamS ^ puRkm[[1]][1]
    sPUj[[2]] <- sPUj[[1]] * lockwoodD * (downstreamS ^ puRkm[[1]][2])
    sPUj[[3]] <- sPUj[[2]] * hydrokennD * (downstreamS ^ puRkm[[1]][3])
    sPUj[[4]] <- sPUj[[3]] * shawmutD * (downstreamS ^ puRkm[[1]][4])
    sPUj[[5]] <- sPUj[[4]] * westonD * (downstreamS ^ puRkm[[1]][5])
    
    # Bypass at Pawtucket
    sPU_sebj <- c()
    sPU_sebj[[1]] <- downstreamS ^ puRkm[[2]][1]
    sPU_sebj[[2]] <- sPU_sebj[[1]] * bentonD * (downstreamS ^ puRkm[[2]][2])
    sPU_sebj[[3]] <- sPU_sebj[[2]] * burnhamD * (downstreamS ^ puRkm[[2]][3])
    
  # Calculate number of males reaching the mouth of the river after spawn from
  # each PU
    malesOut <- vector(mode = 'list', length = length(males))
    for (i in 1:length(sPU)) {
      malesOut[[1]][[i]] <- mapply("*", males[[1]], sPU)[, i]
    }
    for (i in 1:length(sPU_seb)) {
      malesOut[[2]][[i]] <- mapply("*", males[[2]], sPU_seb)[, i]
    }    
    # Sum number of males in each age from all PUs reaching river mouth
      malesOut <- apply(data.frame(malesOut[[1]]), 1, sum) +
        apply(data.frame(malesOut[[2]]), 1, sum)
      
  # Calculate number of females reaching the mouth of the river after spawn from
  # each PU
    femalesOut <- vector(mode = 'list', length = length(females))
    for (i in 1:length(sPU)) {
      femalesOut[[1]][[i]] <- mapply("*", females[[1]], sPU)[, i]
    }
    for (i in 1:length(sPU_seb)) {
      femalesOut[[2]][[i]] <- mapply("*", females[[2]], sPU_seb)[, i]
    }    
    # Sum number of females in each age from all PUs reaching river mouth
      femalesOut <- apply(data.frame(femalesOut[[1]]), 1, sum) +
        apply(data.frame(femalesOut[[2]]), 1, sum)
      
  # Calculate the number of recruits reaching the ocean
    recruitsOut <- vector(mode = 'list', length = length(recruits))
    # Mainstem
    recruitsOut <- vector(mode = 'list', length = length(recruits))
    for (i in 1:length(sPU)) {
      recruitsOut[[1]][[i]] <- mapply("*", recruits[[1]], sPUj)
    }
    # Sebasticook
    for (i in 1:length(sPU_seb)) {
      recruitsOut[[2]][[i]] <- mapply("*", recruits[[2]], sPU_sebj)
    }    
    # Sum number of recruits in each age from all PUs reaching river mouth
    recruitsOut <- sum(unlist(recruitsOut))
  
  # Sum total number of out migrants
  outMigrants <- femalesOut + malesOut
  
  return(
    list(
      sPU = sPU,
      sPj = sPUj,
      sPU_seb = sPU_seb,
      sPU_sebj = sPU_sebj,      
      malesOut = malesOut,
      femalesOut = femalesOut,
      recruitsOut = recruitsOut,
      outMigrants = outMigrants
    )
  )  
}

if(river=='hudson'){
  # Derive downstream passage efficiencies for each group of spawners in each PU.
  # This uses the starting position of the fish, and then incorporates cumulative
  # dam passage efficiencies from the starting PU to the ocean.
  
  # Upper Hudson
    sPU <- c()
    sPU[[1]] <- downstreamS ^ puRkm[[1]][1]
    sPU[[2]] <- sPU[[1]] * federalD * (downstreamS ^ puRkm[[1]][2])
    sPU[[3]] <- sPU[[2]] * C01D * (downstreamS ^ puRkm[[1]][3])
    sPU[[4]] <- sPU[[3]] * C02D * (downstreamS ^ puRkm[[1]][4])
    sPU[[5]] <- sPU[[4]] * C03D * (downstreamS ^ puRkm[[1]][5])
    sPU[[6]] <- sPU[[5]] * C04D * (downstreamS ^ puRkm[[1]][6])
    sPU[[7]] <- sPU[[6]] * C05D * (downstreamS ^ puRkm[[1]][7])
    sPU[[8]] <- sPU[[7]] * C06D * (downstreamS ^ puRkm[[1]][8])
        
  # Mohawk
    sPU_moh <- c()
    sPU_moh[[1]] <- downstreamS ^ puRkm[[2]][1]
    sPU_moh[[2]] <- sPU_moh[[1]] * federalD * (downstreamS ^ puRkm[[2]][2])
    sPU_moh[[3]] <- sPU_moh[[2]] * E02D * (downstreamS ^ puRkm[[2]][3])
    sPU_moh[[4]] <- sPU_moh[[3]] * E03D * (downstreamS ^ puRkm[[2]][4])
    sPU_moh[[5]] <- sPU_moh[[4]] * E04D * (downstreamS ^ puRkm[[2]][5])
    sPU_moh[[6]] <- sPU_moh[[5]] * E05D * (downstreamS ^ puRkm[[2]][6])
    sPU_moh[[7]] <- sPU_moh[[6]] * E06D * (downstreamS ^ puRkm[[2]][7])
    sPU_moh[[8]] <- sPU_moh[[7]] * E07D * (downstreamS ^ puRkm[[2]][8])
    sPU_moh[[9]] <- sPU_moh[[8]] * E08D * (downstreamS ^ puRkm[[2]][9])
    sPU_moh[[10]] <- sPU_moh[[9]] * E09D * (downstreamS ^ puRkm[[2]][10])
    sPU_moh[[11]] <- sPU_moh[[10]] * E10D * (downstreamS ^ puRkm[[2]][11])
    sPU_moh[[12]] <- sPU_moh[[11]] * E11D * (downstreamS ^ puRkm[[2]][12])
    sPU_moh[[13]] <- sPU_moh[[12]] * E12D * (downstreamS ^ puRkm[[2]][13])
    sPU_moh[[14]] <- sPU_moh[[13]] * E13D * (downstreamS ^ puRkm[[2]][14])    
    sPU_moh[[15]] <- sPU_moh[[14]] * E14D * (downstreamS ^ puRkm[[2]][15])
    sPU_moh[[16]] <- sPU_moh[[15]] * E15D * (downstreamS ^ puRkm[[2]][16])        
    sPU_moh[[17]] <- sPU_moh[[16]] * E16D * (downstreamS ^ puRkm[[2]][17])        
    sPU_moh[[18]] <- sPU_moh[[17]] * E17D * (downstreamS ^ puRkm[[2]][18])    
    sPU_moh[[19]] <- sPU_moh[[18]] * E18D * (downstreamS ^ puRkm[[2]][19])
    sPU_moh[[20]] <- sPU_moh[[19]] * E19D * (downstreamS ^ puRkm[[2]][20])
    sPU_moh[[21]] <- sPU_moh[[20]] * E20D * (downstreamS ^ puRkm[[2]][21])
    
  # Derive downstream passage efficiencies for each group of juveniles in each PU.
  # This uses the starting position of the fish, and then incorporates cumulative
  # dam passage efficiencies from the starting PU to the ocean.
  # Upper Hudson
    sPUj <- c()
    sPUj[[1]] <- downstreamS ^ puRkm[[1]][1]
    sPUj[[2]] <- sPUj[[1]] * federalDj * (downstreamS ^ puRkm[[1]][2])
    sPUj[[3]] <- sPUj[[2]] * C01Dj * (downstreamS ^ puRkm[[1]][3])
    sPUj[[4]] <- sPUj[[3]] * C02Dj * (downstreamS ^ puRkm[[1]][4])
    sPUj[[5]] <- sPUj[[4]] * C03Dj * (downstreamS ^ puRkm[[1]][5])
    sPUj[[6]] <- sPUj[[5]] * C04Dj * (downstreamS ^ puRkm[[1]][6])
    sPUj[[7]] <- sPUj[[6]] * C05Dj * (downstreamS ^ puRkm[[1]][7])
    sPUj[[8]] <- sPUj[[7]] * C06Dj * (downstreamS ^ puRkm[[1]][8])
    
    # Bypass at Pawtucket
    sPU_mohj <- c()
    sPU_mohj[[1]] <- downstreamS ^ puRkm[[2]][1]
    sPU_mohj[[2]] <- sPU_mohj[[1]] * federalDj * (downstreamS ^ puRkm[[2]][2])
    sPU_mohj[[3]] <- sPU_mohj[[2]] * E02Dj * (downstreamS ^ puRkm[[2]][3])
    sPU_mohj[[4]] <- sPU_mohj[[3]] * E03Dj * (downstreamS ^ puRkm[[2]][4])
    sPU_mohj[[5]] <- sPU_mohj[[4]] * E04Dj * (downstreamS ^ puRkm[[2]][5])
    sPU_mohj[[6]] <- sPU_mohj[[5]] * E05Dj * (downstreamS ^ puRkm[[2]][6])
    sPU_mohj[[7]] <- sPU_mohj[[6]] * E06Dj * (downstreamS ^ puRkm[[2]][7])
    sPU_mohj[[8]] <- sPU_mohj[[7]] * E07Dj * (downstreamS ^ puRkm[[2]][8])
    sPU_mohj[[9]] <- sPU_mohj[[8]] * E08Dj * (downstreamS ^ puRkm[[2]][9])
    sPU_mohj[[10]] <- sPU_mohj[[9]] * E09Dj * (downstreamS ^ puRkm[[2]][10])
    sPU_mohj[[11]] <- sPU_mohj[[10]] * E10Dj * (downstreamS ^ puRkm[[2]][11])
    sPU_mohj[[12]] <- sPU_mohj[[11]] * E11Dj * (downstreamS ^ puRkm[[2]][12])
    sPU_mohj[[13]] <- sPU_mohj[[12]] * E12Dj * (downstreamS ^ puRkm[[2]][13])
    sPU_mohj[[14]] <- sPU_mohj[[13]] * E13Dj * (downstreamS ^ puRkm[[2]][14])    
    sPU_mohj[[15]] <- sPU_mohj[[14]] * E14Dj * (downstreamS ^ puRkm[[2]][15])
    sPU_mohj[[16]] <- sPU_mohj[[15]] * E15Dj * (downstreamS ^ puRkm[[2]][16])        
    sPU_mohj[[17]] <- sPU_mohj[[16]] * E16Dj * (downstreamS ^ puRkm[[2]][17])        
    sPU_mohj[[18]] <- sPU_mohj[[17]] * E17Dj * (downstreamS ^ puRkm[[2]][18])        
    sPU_mohj[[19]] <- sPU_mohj[[18]] * E18Dj * (downstreamS ^ puRkm[[2]][19])
    sPU_mohj[[20]] <- sPU_mohj[[19]] * E19Dj * (downstreamS ^ puRkm[[2]][20])
    sPU_mohj[[21]] <- sPU_mohj[[20]] * E20Dj * (downstreamS ^ puRkm[[2]][21])
    
  # Calculate number of males reaching the mouth of the river after spawn from
  # each PU
    malesOut <- vector(mode = 'list', length = length(males))
    for (i in 1:length(sPU)) {
      malesOut[[1]][[i]] <- mapply("*", males[[1]], sPU)[, i]
    }
    for (i in 1:length(sPU_moh)) {
      malesOut[[2]][[i]] <- mapply("*", males[[2]], sPU_moh)[, i]
    }    
    # Sum number of males in each age from all PUs reaching river mouth
      malesOut <- apply(data.frame(malesOut[[1]]), 1, sum) +
        apply(data.frame(malesOut[[2]]), 1, sum)
      
  # Calculate number of females reaching the mouth of the river after spawn from
  # each PU
    femalesOut <- vector(mode = 'list', length = length(females))
    for (i in 1:length(sPU)) {
      femalesOut[[1]][[i]] <- mapply("*", females[[1]], sPU)[, i]
    }
    for (i in 1:length(sPU_moh)) {
      femalesOut[[2]][[i]] <- mapply("*", females[[2]], sPU_moh)[, i]
    }    
    # Sum number of females in each age from all PUs reaching river mouth
      femalesOut <- apply(data.frame(femalesOut[[1]]), 1, sum) +
        apply(data.frame(femalesOut[[2]]), 1, sum)
      
  # Calculate the number of recruits reaching the ocean
    recruitsOut <- vector(mode = 'list', length = length(recruits))
    # Upper Hudson
    recruitsOut <- vector(mode = 'list', length = length(recruits))
    for (i in 1:length(sPU)) {
      recruitsOut[[1]][[i]] <- mapply("*", recruits[[1]], sPUj)
    }
    # Mohawk River
    for (i in 1:length(sPU_moh)) {
      recruitsOut[[2]][[i]] <- mapply("*", recruits[[2]], sPU_mohj)
    }    
    # Sum number of recruits in each age from all PUs reaching river mouth
    recruitsOut <- sum(unlist(recruitsOut))
  
  # Sum total number of out migrants
  outMigrants <- femalesOut + malesOut
  
  return(
    list(
      sPU = sPU,
      sPj = sPUj,
      sPU_moh = sPU_moh,
      sPU_mohj = sPU_mohj,      
      malesOut = malesOut,
      femalesOut = femalesOut,
      recruitsOut = recruitsOut,
      outMigrants = outMigrants
    )
  )  
}  
    
}