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

# Maximum age for fish in this population
maxAge <- 9

# Fish age and growth data from Connecticut River -----
# Using built-in data set `fish`
names(fish) = c('sex', 'age', 'fl', 'year', 'backCalculated', 'mass')
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

# Read in temperature data for Hartford CT (tempD)
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
