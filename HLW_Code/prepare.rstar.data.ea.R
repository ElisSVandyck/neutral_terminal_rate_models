#------------------------------------------------------------------------------#
# File:        prepare.rstar.data.ea.R
#
# Description: This file (1) compiles and (2) prepares the data to be used in
#              HLW for the Euro Area.
#------------------------------------------------------------------------------#
rm(list = ls())
source("utilities.R")

# Load time series library
if (!require("tis")) {install.packages("tis"); library('tis')}

# Set up seasonal adjustment
if (!require("seasonal")) {install.packages("seasonal"); library('seasonal')}
#Sys.setenv(X13_PATH = "X13")

#------------------------------------------------------------------------------#
# Get Raw Data
#------------------------------------------------------------------------------#

# Set the start and end dates of the data used in the estimation
data.start <- c(1971,1)
data.end   <- c(2019,2)

# Identify the date at which the core CPI series begins (Mnemonic: HEX)
core.cpi.start <- c(1987,4)

# Import Area Wide Model data
# PRIOR STEPS:
#     1. Download data from the Euro Area Business Cycle Network website
#     2. Save data as a CSV and specify the file name in awm.file
# NOTE: We seasonally adjust data rather than using HEXSA, HICPSA because of availability
awm.file <- 'rawData/area_wide_model_ea.csv'
awm.data <- read.table(awm.file,header=TRUE,sep=',')[,c('DATE','YER','STN','HEX','HICP')]

awm.start    <- c(as.numeric(substr(awm.data[1,'DATE'],1,4)),
                  as.numeric(substr(awm.data[1,'DATE'],6,7)))

gdp          <- tis(awm.data$YER, start = awm.start, tif = 'quarterly')

cpi.nsa      <- tis(awm.data$HICP, start = awm.start, tif = 'quarterly')
core.cpi.nsa <- tis(awm.data$HEX, start = awm.start, tif = 'quarterly')
core.cpi.nsa <- window(core.cpi.nsa, start = core.cpi.start)
interest.q   <- tis(awm.data$STN, start = awm.start, tif = 'quarterly')

# Import ECB SDW Series
# PRIOR STEPS:
#     1. Download the following series from the ECB's Statistical Data Warehouse:
#        - GDP:       MNA.Q.Y.I8.W2.S1.S1.B.B1GQ._Z._Z._Z.EUR.LR.N 
#        - Core HICP: ICP.M.U2.N.XE0000.4.INX
#        - Interest:  FM.Q.U2.EUR.RT.MM.EURIBOR3MD_.HSTA
#     2. Save data as CSV files and specify names in variable.file
#     3. Ensure data is sorted with dates ascending
#     4. Specify start dates and splice dates where applicable
gdp.file      <- 'rawData/ea_gdp.csv'
gdp.data      <- read.table(gdp.file,header=FALSE,sep=',',skip=5)
gdp.start.ecb <- c(1995,1)

gdp.ecb       <- tis(gdp.data$V2, start = gdp.start.ecb, tif = 'quarterly')

gdp.spliced   <- splice(gdp, gdp.ecb, gdp.start.ecb, "Quarterly")

core.cpi.file <- 'rawData/ea_hicp.csv'
core.cpi.data <- read.table(core.cpi.file,header=FALSE,sep=',',skip=5)
core.cpi.start.ecb   <- c(1996,1)
core.cpi.splice.date <- c(2018,1)

core.cpi.nsa.m.ecb <- tis(core.cpi.data$V2, start = core.cpi.start.ecb, tif = 'monthly')

interest.file <- 'rawData/ea_nominal_rate.csv'
interest.data <- read.table(interest.file,header=FALSE,sep=',',skip=5)
interest.start.ecb   <- c(1994,1)
interest.splice.date <- c(2018,1)

interest.q.ecb <- tis(interest.data$V2, start = interest.start.ecb, tif = 'quarterly')

interest.spliced <- mergeSeries(window(interest.q, end = shiftQuarter(interest.splice.date,-1)),
                                window(interest.q.ecb, start = interest.splice.date))

#------------------------------------------------------------------------------#
# Prepare Data
#------------------------------------------------------------------------------#

# Take log of real GDP
gdp.log <- log(gdp.spliced)

# Aggregate ECB core CPI data to quarterly from monthly frequency
core.cpi.nsa.ecb <- convert(core.cpi.nsa.m.ecb, tif = 'quarterly', observed = 'averaged')

# Splice ECB core CPI data with AWM core CPI data in 2015q4
core.cpi.nsa.spliced <- splice(core.cpi.nsa, core.cpi.nsa.ecb, shiftQuarter(core.cpi.splice.date, -1), "Quarterly")

# Seasonally adjust CPI and core CPI data and re-format as tis series
cpi <- final(seas(as.ts(naWindow(cpi.nsa),freq=4)))
cpi <- as.tis(cpi,start=awm.start,tif='quarterly')

core.cpi <- final(seas(as.ts(naWindow(core.cpi.nsa.spliced),freq=4)))
core.cpi <- as.tis(core.cpi,start=core.cpi.start,tif='quarterly')

# Create annualized core inflation and inflation series using the price indices
core.inflation.q <- 400*log(core.cpi/Lag(core.cpi, k=1))
inflation.q      <- 400*log(cpi/Lag(cpi, k=1))

# Final inflation series: CPI series is used prior to 1988q1;
# thereafter, use core CPI series
inflation <- mergeSeries(window(inflation.q, end = core.cpi.start),window(core.inflation.q, start = shiftQuarter(core.cpi.start,1)))

# Inflation expectations measure: 4-quarter moving average of past inflation
inflation.expectations <- (inflation + Lag(inflation, k=1) + Lag(inflation, k=2) + Lag(inflation, k=3))/4

# Express interest rate data on a 365-day basis
interest <- 100*((1+interest.spliced/36000)^365 -1)

#------------------------------------------------------------------------------#
# Output Data
#------------------------------------------------------------------------------#
data.out <- window(cbind(gdp.log, inflation, inflation.expectations, interest),start = data.start, end = data.end)
write.table(data.out,file = 'inputData/rstar.data.ea.csv', sep = ',',
            col.names = TRUE, quote = FALSE, na = '.', row.names = FALSE)
