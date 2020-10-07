# library(tidyverse)
# library(geosphere)
# library(lubridate)
# 
# cpue <- read.csv(file.choose())
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
#     dat$Rcdf[t] = dat$Rtot[t] / max(dat$Rtot)
#     dat$Bcdf[t] = dat$Btot[t] / max(dat$Btot)
#   }
#   ldat[[i]] = dat
# }
# 
# # Now put the dataframe back together
# cpue <- do.call(rbind, ldat)
# # Get proportional harvest each day
# # cpue$Rprob <- cpue$Rcount / cpue$AnnTotR
# # Add a column for year to the catch data
# cpue$year <- year(cpue$Date)
# # Now add temperature to cpue data
# mu <- tempData_connecticut[,c(ncol(tempData_connecticut),1,2)]
# test <<- merge(cpue, mu, by = c('year', 'day'), all.x = TRUE)
# 
# # Create new sequence of days for predictions
# newDay <<- seq(0, 30, 1)
# 
# # Model probability of catch each day
# # moved to defineFunctions.R
# # JMS
# 
# # Bootstrapping regression coefficients
# # so these can be stored as R objects
# # to avoid locking issues in package
# # implementation.
# n = 1000
# arr.R <- vector(mode = 'list', length=n)
# arr.B <- vector(mode = 'list', length=n)
# 
# for(i in 1:1000){
# 
# boots <- test[sample(1:nrow(test), nrow(test)-30), ]
# 
# # Run the regression model to estimate probability of catch on a given day
# Rmod <- glm(Rcdf ~ val,
#            data = boots,
#            na.action = na.exclude,
#            family = 'quasibinomial')
# Bmod <- glm(Bcdf ~ val,
#            data = boots,
#            na.action = na.exclude,
#            family = 'quasibinomial')
# # Collect model coefficients to use in drawing entry date for each fish
# arr.R[[i]] <- summary(Rmod)$coefficients
# arr.B[[i]] <- summary(Bmod)$coefficients
# }
# 
# save(arr.R, file='./data/arr.R.rda')
# save(arr.B, file='./data/arr.B.rda')
