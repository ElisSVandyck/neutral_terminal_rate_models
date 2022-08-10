#------------------------------------------------------------------------------#
# File:        prepare.rstar.data.us.R
#
# Description: This file (1) compiles and (2) prepares the data used in
#              HLW for the US.
#------------------------------------------------------------------------------#
rm(list = ls())
source("utilities.R")

# Load time series library
if (!require("tis")) {install.packages("tis"); library('tis')}

#------------------------------------------------------------------------------#
# Get Raw Data
#------------------------------------------------------------------------------#

# Set the start and end dates of the data used in the estimation
data.start <- c(1960,1)
data.end   <- c(2022,2)

# Import data using the function getFRED() in utilities.R
# If the connection does not work, try the old URL:
# "https://research.stlouisfed.org/fred2/data/NAME.txt"
# NOTE: The getFRED() function requires the wget command line utility;
#       users can also manually download the text files.

# gdp             <- getFRED(paste0('https://fred.stlouisfed.org/',
#                                      'data/GDPC1.txt'))
# 
# price.index     <- getFRED(paste0('https://fred.stlouisfed.org/',
#                                      'data/PCEPILFE.txt'))
# 
# ny.discount     <- getFRED(paste0('https://fred.stlouisfed.org/',
#                                      'data/INTDSRUSM193N.txt'))
# 
# fed.funds       <- getFRED(paste0('https://fred.stlouisfed.org/',
#                                      'data/FEDFUNDS.txt'))

gdp             <- getFRED(paste0('rawData/GDPC1.txt'))
#price.index     <- getFRED(paste0('rawData/JCXFE.txt'))    # For the non-public update after the first GDP release, use the native quarterly series
price.index     <- getFRED(paste0('rawData/PCEPILFE.txt')) # For the public update after the second GDP release, use the native monthly series
ny.discount     <- getFRED(paste0('rawData/INTDSRUSM193N.txt'))
fed.funds       <- getFRED(paste0('rawData/FEDFUNDS.txt'))

#------------------------------------------------------------------------------#
# Prepare Data
#------------------------------------------------------------------------------#

# Take log of real GDP
gdp.log <- log(gdp)

# Create an annualized inflation series using the price index
inflation <- 400*log(price.index/Lag(price.index, k=1))

# Inflation expectations measure: 4-quarter moving average of past inflation
inflation.expectations <- (inflation + Lag(inflation, k=1) + Lag(inflation, k=2) + Lag(inflation, k=3))/4

# Express interest rate data on a 365-day basis
ny.discount.eff <- 100*((1+ny.discount/36000)^365 -1)
fed.funds.eff   <- 100*((1+fed.funds/36000)^365 -1)

# NY Fed discount rate is used prior to 1965; thereafter, use the effective federal funds rate
interest <- mergeSeries(window(ny.discount.eff, end = c(1964,4)),window(fed.funds.eff, start = c(1965,1)))

#------------------------------------------------------------------------------#
# Output Data
#------------------------------------------------------------------------------#
data.out <- window(cbind(gdp.log, inflation, inflation.expectations, interest),start = data.start, end = data.end)
write.table(data.out,file = 'inputData/rstar.data.us.csv', sep = ',',
            col.names = TRUE, quote = FALSE, na = '.', row.names = FALSE)
