# setupData.R

setUpData <- function(){

# if (!exists('yday')) source('defineFunctions.R')
# defineFunctions() # should load automatically

# Maximum age for fish in this population
maxAge <<- 9

# FISH AGE & GROWTH DATA FROM CTDEEP --------------------------------------
# Read in the data
#read.csv('datasets/ctr_fish.csv')
fish$fl[fish$fl < 10] = fish$fl[fish$fl < 10] * 10 # Transcription errors
fish$fl[fish$fl < 10] = fish$fl[fish$fl < 10] * 10 # Transcription errors
names(fish) = c('sex', 'age', 'fl', 'year', 'backCalculated', 'mass')

# Make a dataframe of age and growth data just for females
roes <<- fish[fish$sex == 'R',]

# Make an dataframe of age and growth data just for males
bucks <<- fish[fish$sex == 'B',]
bucks$fl[bucks$fl < 10] = bucks$fl[bucks$fl < 10] * 10 # More transcription errors

# Correct errors in fish mass & make a separate df for l-w regression due to
# missing fish masses
fish$mass <- as.numeric(fish$mass)
fishw <- fish[!is.na(fish$mass) & fish$mass != 0,]
fishw$mass[fishw$mass < 500] <- fishw$mass[fishw$mass < 500] * 10

# Log transform and data cleaning for l-w regressions
# Bucks
b.l <<- log(fishw$fl[fishw$sex == 'B'])
b.w <<- log(fishw$mass[fishw$sex == 'B'])
buck.lw <<- na.omit(data.frame(b.l, b.w))
buck.lw <<- buck.lw[is.finite(buck.lw[, 2]),]

# Roes
r.l <<- log(fishw$fl[fishw$sex == 'R'])
r.w <<- log(fishw$mass[fishw$sex == 'R'])
roe.lw <<- na.omit(data.frame(r.l, r.w))
roe.lw <<- roe.lw[is.finite(roe.lw[, 2]),]

# TEMPERATURE DATA FOR PENOBSCOT RIVER ------------------------------------
# Load the pnr temperature data

# Should be done automatically
#load("datasets/tempData.rda")

# JMS: limit size of the object first,
# then do operations on it
tempData2 <<- tempData[tempData$year > 2007 & tempData$year < 2014 ,]

# Summarize the temperature by day across years
mu <<- ddply(tempData2,
           .(day, year),
           summarize,
           val = mean(val, na.rm = TRUE))

# Change the orders of the column to match original data
mu <<- mu[, c(3, 2, 1)]
mu <<- na.omit(mu)

# Read in temperature data for Hartford CT
# This is now done automatically with package 
# data sets. 
### NEED TO RENAME THESE ###
#data('tempD')
# Summarize the temperature by day across years
hmu <<- ddply(tempD,
            .(day, year),
            summarize,
            val = mean(val, na.rm = TRUE))
# Change the orders of the columns to match original data
hmu <<- hmu[, c(3, 2, 1)]
hmu <<- na.omit(hmu)

# Make a regression relating temperature in the PNR to temperature in the CTR
pnr <<- mu[paste(mu$year, mu$day) %in% paste(hmu$year, hmu$day),]
ctr <<- hmu[paste(hmu$year, hmu$day) %in% paste(mu$year, mu$day),]
# Predict temperature
calMod <<- summary(lm(pnr$val ~ ctr$val))$coefficients
hmu$val <<- calMod[1, 1] + calMod[2, 1] * hmu$val

# RELATE ARRIVAL DATE TO TEMPERATURE WITH GLM BASED ON HARVEST ------------
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
# ldat <<- vector(mode = 'list', length = length(unique(cpue$year)))
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
# test <<- merge(cpue, mu, by = c('year', 'day'), all.x = TRUE)
# # Create new sequence of days for predictions
# newDay <<- seq(0, 30, 1)
# 
# # Model probability of catch each day
# # moved to defineFunctions.R
# # JMS
# 
# # Run the regression model to estimate probability of catch on a given day
# Rmod <<- glm(Rcdf ~ val,
#            data = test,
#            na.action = na.exclude,
#            family = 'quasibinomial')
# Bmod <<- glm(Bcdf ~ val,
#            data = test,
#            na.action = na.exclude,
#            family = 'quasibinomial')
# # Collect model coefficients to use in drawing entry date for each fish
# res.R <<- summary(Rmod)$coefficients
# res.B <<- summary(Bmod)$coefficients

}