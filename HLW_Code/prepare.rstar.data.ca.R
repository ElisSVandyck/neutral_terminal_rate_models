#------------------------------------------------------------------------------#
# File:        prepare.rstar.data.ca.R
#
# Description: This file (1) compiles and (2) prepares the data used in
#              HLW for Canada.
#------------------------------------------------------------------------------#
rm(list = ls())
source("utilities.R")

# Load time series library
if (!require("tis")) {install.packages("tis"); library("tis")}

# Set up seasonal adjustment
if (!require("seasonal")) {install.packages("seasonal"); library('seasonal')}
#Sys.setenv(X13_PATH = "X13")

#------------------------------------------------------------------------------#
# Get Raw Data and Create Monthly Series (if applicable)
#
# Note: Code is set up such that each CANSIM series is in its own CSV with time as rows
#------------------------------------------------------------------------------#

# Set the start and end dates of the data used in the estimation
data.start <- c(1960,1)
data.end   <- c(2019,2)

# Import GDP data 
# PRIOR STEPS:
#     1. Download data from the International Financial Statistics (IFS) Database on the IMF website:
#        Series: "Gross Domestic Product, Real, Seasonally adjusted, Index"
#     2. Save data as a CSV and specify the file name in gdp.file
gdp.file <- "rawData/canada_gdp_ifs.csv"
gdp.data <- read.table(gdp.file, skip = 1, header = TRUE, sep = ',', stringsAsFactors = FALSE)
gdp      <- tis(gdp.data$Canada, start = data.start, tif = 'quarterly')

# Import daily bank rate data
# PRIOR STEPS:
#     1. Download data from the Statistics Canada (CANSIM) website:
#     Series: v122530 
#     2. Save data as a CSV and specify the file name in bank.rate.file
bank.rate.file <- "rawData/cansim_bank_rate_v122530.csv"
bank.rate.data <- read.table(bank.rate.file, skip = 2, header = TRUE, sep = ',', stringsAsFactors = FALSE)
bank.rate.m    <- tis(bank.rate.data$v122530, start = data.start, tif='monthly')

# Import daily target rate data and take EOP monthly values
# PRIOR STEPS:
#     1. Download data from the Statistics Canada (CANSIM) website:
#     Series: v39079        
#     2. Save data as a CSV and specify the file name in target.rate.file
target.rate.start <- c(2001,1)
target.rate.file  <- "rawData/cansim_target_rate_v39079.csv"
target.rate.data  <- read.table(target.rate.file, skip = 2, header = TRUE, sep = ',', stringsAsFactors = FALSE)
target.rate.data$Daily <- as.Date(target.rate.data$Daily, "%m/%d/%Y") #as.Date(target.rate.data$Daily, "%b %d %Y")

# Format data as time series with daily frequency
if (!require("xts")) {install.packages("xts"); library("xts")}
target.rate.d <- as.xts(target.rate.data$v39079, order.by = target.rate.data$Daily, frequency = 365)
# Remove data with zero values (these indicate non-business days)
target.rate.d <- target.rate.d[!(target.rate.d==0.00)]
# Aggregate data to monthly frequency by taking end-of-period values
target.rate.m <- apply.monthly(target.rate.d, tail, n=1)
detach("package:xts")
detach("package:zoo")
# Format data as a tis time series with monthly frequency
target.rate.m <- as.tis(data.frame(target.rate.m)$target.rate.m, start = target.rate.start, tif = 'monthly')

# Import monthly core CPI and CPI data (two series - the second CPI series will be used to extend data back)
# PRIOR STEPS:
#     1. Download data from the Statistics Canada (CANSIM) website:
#     Series: v41690926, v41690914, v41690973 (respectively)
#     2. Save data as a CSV and specify the file name in core.cpi.file, cpi.file, cpi.back.file
# NOTES: Series v41690914 begins in Jan. 1992 and will be used as the CPI series thereafter;
#        Growth rates from series v41690973 are used to extend the CPI series back to 1959.
#        Series v41690914 and v41690926 are already seasonally adjusted but v41690973 is not,
#        so we seasonally adjust it.
core.cpi.start <- c(1984,1)
cpi.start      <- c(1992,1)
cpi.back.start <- c(1959,1)

#core.cpi.file  <- "rawData/cansim_core_cpi_v41690926.csv"
core.cpi.file  <- "rawData/cansim_core_cpi_v112593705.csv"
cpi.file       <- "rawData/cansim_cpi_v41690914.csv"
cpi.back.file  <- "rawData/cansim_cpi_back_v41690973.csv"

core.cpi.data  <- read.table(core.cpi.file, skip = 2, header = TRUE, sep = ',', stringsAsFactors = FALSE)
cpi.data       <- read.table(cpi.file, skip = 2, header = TRUE, sep = ',', stringsAsFactors = FALSE)
cpi.back.data  <- read.table(cpi.back.file, skip = 2, header = TRUE, sep = ',', stringsAsFactors = FALSE)

# Format data as time series with monthly frequency
#core.cpi.m     <- tis(core.cpi.data$v41690926, start = core.cpi.start, tif = 'monthly')
core.cpi.m     <- tis(core.cpi.data$v112593705, start = core.cpi.start, tif = 'monthly')
cpi.m          <- tis(cpi.data$v41690914, start = cpi.start, tif = 'monthly')
cpi.back.m.nsa <- tis(cpi.back.data$v41690973, start = cpi.back.start, tif = 'monthly')

# Seasonally adjust CPI Growth Rate data (other two series are already SA)
cpi.back.m <- final(seas(as.ts(naWindow(cpi.back.m.nsa),freq=12)))
cpi.back.m <- as.tis(cpi.back.m,start=cpi.back.start,tif='monthly') 


#------------------------------------------------------------------------------#
# Prepare Data
#------------------------------------------------------------------------------#

# Take log of real GDP
gdp.log <- log(gdp)

# CPI series: cpi.m is used from Jan 1992 to present and extended back to
# Jan 1959 using growth rates with splice() function in utilities.R
cpi.series.m <- splice(cpi.back.m, cpi.m, cpi.start, freq = 'monthly')

# Aggregate core CPI and CPI series to quarterly frequency by taking the average
core.cpi.q <- aggregate(core.cpi.m, nf = 4, FUN = mean)
cpi.q      <- aggregate(cpi.series.m, nf = 4, FUN = mean)

# Create annualized core inflation and inflation series using the price indices
core.inflation.q <- 400*log(core.cpi.q/Lag(core.cpi.q, k=1))
inflation.q      <- 400*log(cpi.q/Lag(cpi.q, k=1))

# Final inflation series: CPI series is used prior to 1984q2;
# thereafter, use core CPI series
inflation <- mergeSeries(window(inflation.q, end = core.cpi.start),window(core.inflation.q, start = shiftQuarter(core.cpi.start,1)))

# Inflation expectations measure: 4-quarter moving average of past inflation
inflation.expectations <- (inflation + Lag(inflation, k=1) + Lag(inflation, k=2) + Lag(inflation, k=3))/4

# Bank rate is used prior to May 2001; thereafter, use the target rate
interest.m <- mergeSeries(window(bank.rate.m, end = c(2001,4)),window(target.rate.m, start = c(2001,5)))

# Aggregate interest rate data to quarterly frequency by taking the average
interest.q <- aggregate(interest.m, nf = 4, FUN = mean)

# Express interest rate data on a 365-day basis
interest <- 100*((1+interest.q/36000)^365 -1)

#------------------------------------------------------------------------------#
# Output Data
#------------------------------------------------------------------------------#
data.out <- window(cbind(gdp.log, inflation, inflation.expectations, interest),start = data.start, end = data.end)
write.table(data.out,file = 'inputData/rstar.data.ca.csv', sep = ',',
            col.names = TRUE, quote = FALSE, na = '.', row.names = FALSE)

