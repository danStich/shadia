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
sPU <- c()
sPU[[1]] <- downstreamS ^ puRkm[[1]][1]
sPU[[2]] <- sPU[[1]] * EssexD * (downstreamS ^ puRkm[[1]][2])
sPU[[3]] <- sPU[[2]] * PawtucketD * (downstreamS ^ puRkm[[1]][3])
sPU[[4]] <- sPU[[3]] * AmoskeagD * (downstreamS ^ puRkm[[1]][4])
sPU[[5]] <- sPU[[4]] * HooksetD * (downstreamS ^ puRkm[[1]][5])

# Derive downstream passage efficiencies for each group of juveniles in each PU.
# This uses the starting position of the fish, and then incorporates cumulative
# dam passage efficiencies from the starting PU to the ocean.
sPUj <- c()
sPUj[[1]] <- downstreamS ^ puRkm[[1]][1]
sPUj[[2]] <- sPUj[[1]] * EssexDj * (downstreamS ^ puRkm[[1]][2])
sPUj[[3]] <- sPUj[[2]] * PawtucketDj * (downstreamS ^ puRkm[[1]][3])
sPUj[[4]] <- sPUj[[3]] * AmoskeagDj * (downstreamS ^ puRkm[[1]][4])
sPUj[[5]] <- sPUj[[4]] * HooksetDj * (downstreamS ^ puRkm[[1]][5])

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
}