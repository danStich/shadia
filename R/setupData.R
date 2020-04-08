#' @title Set up data for simulations
#' 
#' @description Internal functions for manipulations to 
#' empirical data used as inputs, and for fitting of
#' empirical relationships (e.g. discharge regressions).
#' 
#' Not intended to be called directly, but visible 
#' for transparency.
#' 
#' @return A list of generic parameters for code benchmarking
#' and progress monitoring.
#' 
#' @details Have retained data manipulation to retain 
#' integrity of raw data files in built-in data sets and
#' provide transparency in methods.
#' 
#' @export
#' 
setUpData <- function(){

  
if(river=='penobscot'){
# Maximum age for fish in this population
maxAge <- 9

# Fish age and growth data from Connecticut River -----
# Using built-in data set `fish`
names(fish) <- c('sex', 'age', 'fl', 'year', 'backCalculated', 'mass')
fish <- fish[fish$fl > 10, ]

# Make a dataframe of age and growth data just for females
roes <- fish[fish$sex == 'R',]

# Make an dataframe of age and growth data just for males
bucks <- fish[fish$sex == 'B',]

# Correct errors in fish mass & make a separate df for l-w regression due to
# missing fish masses
fish$mass <- as.numeric(fish$mass)
fishw <- fish[!is.na(fish$mass) & fish$mass != 0,]

# Log transform and data cleaning for l-w regressions
# Bucks
b.l <- log(fishw$fl[fishw$sex == 'B'])
b.w <- log(fishw$mass[fishw$sex == 'B'])
buck.lw <- na.omit(data.frame(b.l, b.w))
buck.lw <- buck.lw[is.finite(buck.lw[, 2]),]

# Roes
r.l <- log(fishw$fl[fishw$sex == 'R'])
r.w <- log(fishw$mass[fishw$sex == 'R'])
roe.lw <- na.omit(data.frame(r.l, r.w))
roe.lw <- roe.lw[is.finite(roe.lw[, 2]),]

# Temperature data for Penobscot River -----
# Load the pnr temperature data from
# years 2008-2013 in the built-in dataset
tempData2 <- tempData[tempData$year > 2007 & tempData$year < 2014 ,]

# Summarize the temperature by day across years
mu <- ddply(tempData2,
           .(day, year),
           summarize,
           val = mean(val, na.rm = TRUE))

# Change the orders of the column to match original data
mu <- mu[, c(3, 2, 1)]
mu <- na.omit(mu)

# Read in temperature data for NH (tempD)
# This is now done automatically with package 
# data sets. 
### NEED TO RENAME THESE TEMPERATURE FILES !!!! ###
# Summarize the temperature by day across years
hmu <- ddply(tempD,
            .(day, year),
            summarize,
            val = mean(val, na.rm = TRUE))
# Change the orders of the columns to match original data
hmu <- hmu[, c(3, 2, 1)]
hmu <- na.omit(hmu)

# Make a regression relating temperature in the PNR to temperature in the CTR
pnr <- mu[paste(mu$year, mu$day) %in%
                      paste(hmu$year, hmu$day),]
ctr <- hmu[paste(hmu$year, hmu$day) %in% 
                       paste(mu$year, mu$day),]
# Predict temperature
calMod <- summary(lm(pnr$val ~ ctr$val))$coefficients
hmu$val <- calMod[1, 1] + calMod[2, 1] * hmu$val

return(list(
maxAge = maxAge,
fish = fish,
roes = roes,
bucks = bucks,
b.l = b.l,
b.w = b.w,
buck.lw = buck.lw,
buck.lw = buck.lw,
r.l = r.l,
r.w = r.w,
roe.lw = roe.lw,
roe.lw = roe.lw,
tempData2 = tempData2,
mu = mu,
pnr = pnr,
ctr = ctr,
calMod = calMod,
hmu = hmu
))
}
  
  
if(river=='merrimack'){
# Maximum age for fish in this population
maxAge <- 11

# Fish age and growth data from Merrimack River -----
# Using built-in data set `fish` because there are no
# weight data from Merrimack River
names(fish) <- c('sex', 'age', 'fl', 'year', 'backCalculated', 'mass')
fish <- fish[fish$fl > 10, ]

# Make a dataframe of age and growth data just for females
roes <- fish[fish$sex == 'R',]

# Make an dataframe of age and growth data just for males
bucks <- fish[fish$sex == 'B',]

# Correct errors in fish mass & make a separate df for l-w regression due to
# missing fish masses
fish$mass <- as.numeric(fish$mass)
fishw <- fish[!is.na(fish$mass) & fish$mass != 0,]

# Log transform and data cleaning for l-w regressions
# Bucks
b.l <- log(fishw$fl[fishw$sex == 'B'])
b.w <- log(fishw$mass[fishw$sex == 'B'])
buck.lw <- na.omit(data.frame(b.l, b.w))
buck.lw <- buck.lw[is.finite(buck.lw[, 2]),]

# Roes
r.l <- log(fishw$fl[fishw$sex == 'R'])
r.w <- log(fishw$mass[fishw$sex == 'R'])
roe.lw <- na.omit(data.frame(r.l, r.w))
roe.lw <- roe.lw[is.finite(roe.lw[, 2]),]

# Temperature data for Merrimack River -----
# Load the MMR temperature data from
# years 2008-2013 in the built-in dataset
tempData_merrimack2 <- tempData_merrimack#[tempData_merrimack$year > 2007 & tempData_merrimack$year < 2014 ,]

# Summarize the temperature by day across years
mu <- ddply(tempData_merrimack2,
           .(day, year),
           summarize,
           val = mean(val, na.rm = TRUE))

# Change the orders of the column to match original data
mu <- mu[, c(3, 2, 1)]
mu <- na.omit(mu)

# Read in temperature data for Connecticut River in NH (tempD)
### NEED TO RENAME THESE TEMPERATURE FILES !!!! ###
# Summarize the temperature by day across years
hmu <- ddply(tempD,
            .(day, year),
            summarize,
            val = mean(val, na.rm = TRUE))
# Change the orders of the columns to match original data
hmu <- hmu[, c(3, 2, 1)]
hmu <- na.omit(hmu)

# Make a regression relating temperature in the 
# Merrimack to temperature in the Connecticut
mmr <- mu[paste(mu$year, mu$day) %in%
                      paste(hmu$year, hmu$day),]
ctr <- hmu[paste(hmu$year, hmu$day) %in% 
                       paste(mu$year, mu$day),]
# Predict temperature
calMod <- summary(lm(mmr$val ~ ctr$val))$coefficients
hmu$val <- calMod[1, 1] + calMod[2, 1] * hmu$val

return(list(
  maxAge = maxAge,
  fish = fish,
  roes = roes,
  bucks = bucks,
  b.l = b.l,
  b.w = b.w,
  buck.lw = buck.lw,
  buck.lw = buck.lw,
  r.l = r.l,
  r.w = r.w,
  roe.lw = roe.lw,
  roe.lw = roe.lw,
  tempData_merrimack2 = tempData_merrimack2,
  mu = mu,
  mmr = mmr,
  ctr = ctr,
  calMod = calMod,
  hmu = hmu
))
}  

  
if(river=='connecticut'){
# Fish age and growth data from Connecticut River -----  
# Maximum age for fish in this population
  maxAge <- 8

# Using built-in data set `fish`
  names(fish) <- c('sex', 'age', 'fl', 'year', 'backCalculated', 'mass')
  fish <- fish[fish$fl > 10, ]

# Make a dataframe of age and growth data just for females
  roes <- fish[fish$sex == 'R',]

# Make an dataframe of age and growth data just for males
  bucks <- fish[fish$sex == 'B',]

# Correct errors in fish mass & make a separate df for l-w regression due to
# missing fish masses
  fish$mass <- as.numeric(fish$mass)
  fishw <- fish[!is.na(fish$mass) & fish$mass != 0,]

# Log transform and data cleaning for l-w regressions
# Bucks
  b.l <- log(fishw$fl[fishw$sex == 'B'])
  b.w <- log(fishw$mass[fishw$sex == 'B'])
  buck.lw <- na.omit(data.frame(b.l, b.w))
  buck.lw <- buck.lw[is.finite(buck.lw[, 2]),]

# Roes
  r.l <- log(fishw$fl[fishw$sex == 'R'])
  r.w <- log(fishw$mass[fishw$sex == 'R'])
  roe.lw <- na.omit(data.frame(r.l, r.w))
  roe.lw <- roe.lw[is.finite(roe.lw[, 2]),]

# Temperature data for Connecticut River -----
# Load temperature data for Turner's Falls
# during the past 20 years from built-in
# data set
  tf_temp = tempData_connecticut
  day1 = 60
  day2 = 250
  mu_plot = ddply(tf_temp, .(day, year), summarize, val=mean(val, na.rm=TRUE))
  mu = na.omit(mu_plot)
  mu = mu[ , c(3, 2, 1)]
  counts = c()
  for(i in 1:length(unique(mu[ , 2]))){
    counts[i]=nrow(mu[mu[,2]==unique(mu[,2])[i], ])
  }
  years = unique(mu[,2])
  mu = mu[mu[ , 2] %in% years[counts>=350], ]
  mu_plot = mu_plot[mu_plot[ , 2] %in% years[counts>=350], ]

# Load temperature data for Connecticut
# River in New Hampshire, USA. Estimate
# daily averages for each day in each year.
  hmu <- ddply(tempD,
              .(day, year),
              summarize,
              val = mean(val, na.rm = TRUE))
# Change the orders of the columns to
# match original data
  hmu <- hmu[, c(3, 2, 1)]
  hmu <- na.omit(hmu)

return(list(
  maxAge = maxAge,
  fish = fish,
  roes = roes,
  bucks = bucks,
  b.l = b.l,
  b.w = b.w,
  buck.lw = buck.lw,
  buck.lw = buck.lw,
  r.l = r.l,
  r.w = r.w,
  roe.lw = roe.lw,
  roe.lw = roe.lw,
  mu = mu,
  mu = mu,
  mu_plot = mu_plot,
  hmu = hmu
  ))
}


if(river=='susquehanna'){
# Maximum age for fish in this population
maxAge <- 11

# Fish data from Connecticut River -----
# Using built-in data set `fish` because we do not have
# mass for Susquehanna in current data sets
names(fish) <- c('sex', 'age', 'fl', 'year', 'backCalculated', 'mass')
fish <- fish[fish$fl > 10, ]

# Make a dataframe of age and growth data just for females
roes <- fish[fish$sex == 'R',]

# Make an dataframe of age and growth data just for males
bucks <- fish[fish$sex == 'B',]

# Correct errors in fish mass & make a separate df for l-w regression due to
# missing fish masses
fish$mass <- as.numeric(fish$mass)
fishw <- fish[!is.na(fish$mass) & fish$mass != 0,]

# Log transform and data cleaning for l-w regressions
# Bucks
b.l <- log(fishw$fl[fishw$sex == 'B'])
b.w <- log(fishw$mass[fishw$sex == 'B'])
buck.lw <- na.omit(data.frame(b.l, b.w))
buck.lw <- buck.lw[is.finite(buck.lw[, 2]),]

# Roes
r.l <- log(fishw$fl[fishw$sex == 'R'])
r.w <- log(fishw$mass[fishw$sex == 'R'])
roe.lw <- na.omit(data.frame(r.l, r.w))
roe.lw <- roe.lw[is.finite(roe.lw[, 2]),]

# Temperature data for Susquehanna River -----
# Load the SSR temperature data from
# years 2008-2013 in the built-in dataset
tempData_susquehanna2 <- tempData_susquehanna[tempData_susquehanna$year>=2012,]

# Summarize the temperature by day across years
mu <- ddply(tempData_susquehanna2,
           .(day, year),
           summarize,
           val = mean(val, na.rm = TRUE))

# Change the orders of the column to match original data
mu <- mu[, c(3, 2, 1)]
mu <- na.omit(mu)
# For testing arbitrary, invariant temp increases/decreases
#mu$val <- mu$val - 1

# Read in temperature data for Connecticut River in NH (tempD)
### NEED TO RENAME THESE TEMPERATURE FILES !!!! ###
# Summarize the temperature by day across years
hmu <- ddply(tempD,
            .(day, year),
            summarize,
            val = mean(val, na.rm = TRUE))
# Change the orders of the columns to match original data
hmu <- hmu[, c(3, 2, 1)]
hmu <- na.omit(hmu)

# Make a regression relating temperature in the 
# Susquehanna to temperature in the Connecticut
ssr <- mu[paste(mu$year, mu$day) %in%
                      paste(hmu$year, hmu$day),]
ctr <- hmu[paste(hmu$year, hmu$day) %in% 
                       paste(mu$year, mu$day),]
# Predict temperature
calMod <- summary(lm(ssr$val ~ ctr$val))$coefficients
hmu$val <- calMod[1, 1] + calMod[2, 1] * hmu$val

return(list(
  maxAge = maxAge,
  fish = fish,
  roes = roes,
  bucks = bucks,
  b.l = b.l,
  b.w = b.w,
  buck.lw = buck.lw,
  buck.lw = buck.lw,
  r.l = r.l,
  r.w = r.w,
  roe.lw = roe.lw,
  roe.lw = roe.lw,
  tempData_susquehanna2 = tempData_susquehanna2,
  mu = mu,
  ssr = ssr,
  ctr = ctr,
  calMod = calMod,
  hmu = hmu
))
}  

  
if(river=='saco'){
# Maximum age for fish in this population
maxAge <- 9

# Fish age and growth data from Connecticut River -----
# Using built-in data set `fish`
names(fish) <- c('sex', 'age', 'fl', 'year', 'backCalculated', 'mass')
fish <- fish[fish$fl > 10, ]

# Make a dataframe of age and growth data just for females
roes <- fish[fish$sex == 'R',]

# Make an dataframe of age and growth data just for males
bucks <- fish[fish$sex == 'B',]

# Correct errors in fish mass & make a separate df for l-w regression due to
# missing fish masses
fish$mass <- as.numeric(fish$mass)
fishw <- fish[!is.na(fish$mass) & fish$mass != 0,]

# Log transform and data cleaning for l-w regressions
# Bucks
b.l <- log(fishw$fl[fishw$sex == 'B'])
b.w <- log(fishw$mass[fishw$sex == 'B'])
buck.lw <- na.omit(data.frame(b.l, b.w))
buck.lw <- buck.lw[is.finite(buck.lw[, 2]),]

# Roes
r.l <- log(fishw$fl[fishw$sex == 'R'])
r.w <- log(fishw$mass[fishw$sex == 'R'])
roe.lw <- na.omit(data.frame(r.l, r.w))
roe.lw <- roe.lw[is.finite(roe.lw[, 2]),]

# Temperature data for Saco River -----
# Load the temperature data from Saco River
tempData_saco2 <- tempData_saco


# Only a few years have good enough data for simulating
# temperatures.
tempData_saco2 <- subset(
  tempData_saco2,
  subset= year %in% c(2010, 2011, 2013)
  )

# Summarize the temperature by day across years
mu <- ddply(tempData_saco2,
           .(day, year),
           summarize,
           val = mean(val, na.rm = TRUE))

# Change the orders of the column to match original data
mu <- mu[, c(3, 2, 1)]
mu <- na.omit(mu)

# Read in temperature data for Connecticut River in NH (tempD)
# Summarize the temperature by day across years
hmu <- ddply(tempD,
            .(day, year),
            summarize,
            val = mean(val, na.rm = TRUE))
# Change the orders of the columns to match original data
hmu <- hmu[, c(3, 2, 1)]
hmu <- na.omit(hmu)

# Make a regression relating temperature in the 
# Saco to temperature in the Connecticut
scr <- mu[paste(mu$year, mu$day) %in%
                      paste(hmu$year, hmu$day),]
ctr <- hmu[paste(hmu$year, hmu$day) %in% 
                       paste(mu$year, mu$day),]
# Predict temperature
calMod <- summary(lm(scr$val ~ ctr$val))$coefficients
hmu$val <- calMod[1, 1] + calMod[2, 1] * hmu$val

# SANITY CHECK FOR TEMPERATURE DATA
# sort(unique(hmu$year))
# YEAR = 2011
# plot(hmu$day[hmu$year==YEAR], 
#      hmu$val[hmu$year==YEAR],
#      type = 'l', 
#      xlim=c(0, 366)
#      )

return(list(
  maxAge = maxAge,
  fish = fish,
  roes = roes,
  bucks = bucks,
  b.l = b.l,
  b.w = b.w,
  buck.lw = buck.lw,
  buck.lw = buck.lw,
  r.l = r.l,
  r.w = r.w,
  roe.lw = roe.lw,
  roe.lw = roe.lw,
  tempData_saco2 = tempData_saco2,
  mu = mu,
  scr = scr,
  ctr = ctr,
  calMod = calMod,
  hmu = hmu
))

}

  
if(river=='kennebec'){
# Maximum age for fish in this population
maxAge <- 9

# Fish length-weight regression from Connecticut River -----
# Using built-in data set `fish`
names(fish) <- c('sex', 'age', 'fl', 'year', 'backCalculated', 'mass')
fish <- fish[fish$fl > 10, ]

# Make a dataframe of age and growth data just for females
roes <- fish[fish$sex == 'R',]

# Make an dataframe of age and growth data just for males
bucks <- fish[fish$sex == 'B',]

# Correct errors in fish mass & make a separate df for l-w regression due to
# missing fish masses
fish$mass <- as.numeric(fish$mass)
fishw <- fish[!is.na(fish$mass) & fish$mass != 0,]

# Log transform and data cleaning for l-w regressions
# Bucks
b.l <- log(fishw$fl[fishw$sex == 'B'])
b.w <- log(fishw$mass[fishw$sex == 'B'])
buck.lw <- na.omit(data.frame(b.l, b.w))
buck.lw <- buck.lw[is.finite(buck.lw[, 2]),]

# Roes
r.l <- log(fishw$fl[fishw$sex == 'R'])
r.w <- log(fishw$mass[fishw$sex == 'R'])
roe.lw <- na.omit(data.frame(r.l, r.w))
roe.lw <- roe.lw[is.finite(roe.lw[, 2]),]

# Temperature data for Kennebec River -----
# Load the temperature data from Saco River
# Summarize the temperature by day across years
mu <- ddply(tempData_kennebec,
           .(day, year),
           summarize,
           val = mean(val, na.rm = TRUE))

mu <- na.omit(mu)

# Read in temperature data for Connecticut River in NH (tempD)
# Summarize the temperature by day across years
hmu <- ddply(tempD,
            .(day, year),
            summarize,
            val = mean(val, na.rm = TRUE))
# Change the orders of the columns to match original data
hmu <- hmu[, c(3, 2, 1)]
hmu <- na.omit(hmu)

# Make a regression relating temperature in the 
# Kennebec to temperature in the Connecticut
kbr <- mu[paste(mu$year, mu$day) %in%
                      paste(hmu$year, hmu$day),]
ctr <- hmu[paste(hmu$year, hmu$day) %in% 
                       paste(mu$year, mu$day),]
# Predict temperature
calMod <- summary(lm(kbr$val ~ ctr$val))$coefficients
hmu$val <- calMod[1, 1] + calMod[2, 1] * hmu$val

# SANITY CHECK FOR TEMPERATURE DATA
# sort(unique(hmu$year))
# YEAR = 2008
# plot(hmu$day[hmu$year==YEAR],
#      hmu$val[hmu$year==YEAR],
#      type = 'l',
#      xlim=c(0, 366)
#      )

return(list(
  maxAge = maxAge,
  fish = fish,
  roes = roes,
  bucks = bucks,
  b.l = b.l,
  b.w = b.w,
  buck.lw = buck.lw,
  buck.lw = buck.lw,
  r.l = r.l,
  r.w = r.w,
  roe.lw = roe.lw,
  roe.lw = roe.lw,
  mu = mu,
  kbr = kbr,
  ctr = ctr,
  calMod = calMod,
  hmu = hmu
))

}

if(river=='hudson'){
  # Maximum age for fish in this population
  maxAge <- 13
  
  # Length-at-age data from the Hudson River -----
  
  if(species == 'shad'){
    # Use built-in data set from the Hudson River
      
    # Log transform and data cleaning for l-w regressions
    # Bucks
    b.l <- log(hudson_shad$fl[hudson_shad$sex == 'B'])
    b.w <- log(hudson_shad$mass[hudson_shad$sex == 'B'])
    buck.lw <- na.omit(data.frame(b.l, b.w))
    buck.lw <- buck.lw[is.finite(buck.lw[, 2]),]
    
    # Roes
    r.l <- log(hudson_shad$fl[hudson_shad$sex == 'R'])
    r.w <- log(hudson_shad$mass[hudson_shad$sex == 'R'])
    roe.lw <- na.omit(data.frame(r.l, r.w))
    roe.lw <- roe.lw[is.finite(roe.lw[, 2]),]
  }
  
  # Temperature data for Merrimack River -----
  # Load the MHR temperature data from
  # years 2007-2020 in the built-in dataset
  tempData_hudson2 <- tempData_hudson
  
  # Summarize the temperature by day across years
  mu <- ddply(tempData_hudson2,
             .(day, year),
             summarize,
             val = mean(val, na.rm = TRUE))
  
  # Change the orders of the column to match original data
  mu <- mu[, c(3, 2, 1)]
  mu <- na.omit(mu)
  
  # Read in temperature data for Connecticut River in NH (tempD)
  # Summarize the temperature by day across years
  hmu <- ddply(tempD,
              .(day, year),
              summarize,
              val = mean(val, na.rm = TRUE))
  # Change the orders of the columns to match original data
  hmu <- hmu[, c(3, 2, 1)]
  hmu <- na.omit(hmu)
  
  # Make a regression relating temperature in the 
  # Hudson to temperature in the Connecticut
  mhr <- mu[paste(mu$year, mu$day) %in%
                        paste(hmu$year, hmu$day),]
  ctr <- hmu[paste(hmu$year, hmu$day) %in% 
                         paste(mu$year, mu$day),]
  # Predict temperature
  calMod <- summary(lm(mhr$val ~ ctr$val))$coefficients
  hmu$val <- calMod[1, 1] + calMod[2, 1] * hmu$val
  
  # SANITY CHECK FOR TEMPERATURE DATA
  # sort(unique(hmu$year))
  # YEAR = 2008
  # plot(hmu$day[hmu$year==YEAR],
  #      hmu$val[hmu$year==YEAR],
  #      type = 'l',
  #      xlim=c(0, 366)
  #      )
  
  return(list(
    maxAge = maxAge,
    b.l = b.l,
    b.w = b.w,
    buck.lw = buck.lw,
    buck.lw = buck.lw,
    r.l = r.l,
    r.w = r.w,
    roe.lw = roe.lw,
    roe.lw = roe.lw,
    tempData_hudson2 = tempData_hudson2,
    mu = mu,
    mhr = mhr,
    ctr = ctr,
    calMod = calMod,
    hmu = hmu
  ))
}      

}  










# Relate temperature to arrival in river using glm -----

#####
# This relationship was bootstrapped and regression coeffs
# added as built-in data sets arr.B and arr.R, but the 
# analysis is retained in comments below for transparency in
# methods
#####


# Read in the cpue data and do manipulation
#cpue = read.csv("datasets/cpue.csv")
# Fix the date column format
# data('cpue')
# cpue$Date <- as.POSIXct(as.character(cpue$Date), format = "%m/%d/%Y")
# # Make year a character vector
# cpue$year <- as.character(cpue$year)
# # Create a column for ordinal date
# cpue$day <- yday(cpue$Date)
# # Calculate a cpue column for each sex
# cpue$Rcpue <- cpue$Rcount / cpue$nFishers
# cpue$Bcpue <- cpue$Bcount / cpue$nFishers
# 
# # Calculate total catch by date
# ldat <- vector(mode = 'list', length = length(unique(cpue$year)))
# for (i in 1:length(unique(cpue$year))) {
#   dat = cpue[cpue$year == unique(cpue$year)[i],]
#   for (t in 1:nrow(dat)) {
#     # Calculate cumulative totals for bucks and roes
#     dat$Rtot = cumsum(dat$Rcount)
#     dat$Btot = cumsum(dat$Bcount)
#     dat$AnnTotR = sum(dat$Rcount)
#     dat$AnnTotB = sum(dat$Bcount)
#     # Calculate cfd for bucks and roes
#     dat$Rcdf[t] = dat$Rtot[t] / dat$Rtot[nrow(dat)]
#     dat$Bcdf[t] = dat$Btot[t] / dat$Btot[nrow(dat)]
#   }
#   ldat[[i]] = dat
# }
# 
# # Now put the dataframe back together
# cpue <- do.call(rbind, ldat)
# # Get proportional harvest each day
# cpue$Rprob <- cpue$Rcount / cpue$AnnTotR
# # Add a column for year to the catch data
# cpue$year <- year(cpue$Date)
# # Now add temperature to cpue data
# test <- merge(cpue, mu, by = c('year', 'day'), all.x = TRUE)
# 
# # Run the regression model to estimate probability of catch on a given day
# Rmod <- glm(Rcdf ~ val,
#            data = test,
#            na.action = na.exclude,
#            family = 'quasibinomial')
# Bmod <- glm(Bcdf ~ val,
#            data = test,
#            na.action = na.exclude,
#            family = 'quasibinomial')
# # Collect model coefficients to use in drawing entry date for each fish
# res.R <- summary(Rmod)$coefficients
# res.B <- summary(Bmod)$coefficients

# # Model probability of catch each day
# # moved to defineFunctions.R
# # JMS
# 
