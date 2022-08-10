#------------------------------------------------------------------------------#
# File:        prepare.rstar.data.uk.R
#
# Description: This file (1) compiles and (2) prepares the data used in
#              HLW for the UK.
#------------------------------------------------------------------------------#
rm(list = ls())
source("utilities.R")

# Load time series library
if (!require("tis")) {install.packages("tis"); library('tis')}

# Set up seasonal adjustment
if (!require("seasonal")) {install.packages("seasonal"); library('seasonal')}
#Sys.setenv(X13_PATH = "X13")

#------------------------------------------------------------------------------#
# Get Raw Data and Prepare Daily Bank Rate Series
#------------------------------------------------------------------------------#

# Set the start and end dates of the data
data.start <- c(1960,1)
data.end   <- c(2019,2)

# Import GDP data
# PRIOR STEPS:
#     1. Download data from the ONS website:
#        Series: ABMI: "Gross Domestic Product: chained volume measures: Seasonally adjusted (Millions of pounds)
#     2. Save data as a CSV and specify the file name in gdp.file
gdp.file  <- "rawData/uk_gdp_ons_abmi.csv"
gdp.data  <- read.table(gdp.file, skip = 7, header = FALSE, sep = ',', stringsAsFactors = FALSE) 
gdp       <- tis(gdp.data$V2, start = data.start, tif='quarterly')

# Import core CPI and CPI data
# PRIOR STEPS:
#     1. Download data from the OECD website:
#        Source: Consumer Prices (MEI); Series: Consumer prices - all items; Consumer prices - all items non-food, non-energy
#     2. Save both data series in one CSV and specify the file name in cpi.file
core.cpi.start <- c(1970,1)
cpi.start      <- c(1959,1)

cpi.file     <- "rawData/uk_price_indices.csv"
cpi.data     <- read.table(cpi.file, skip = 0, header = TRUE, sep = ',', stringsAsFactors = FALSE)
core.cpi.nsa <- tis(cpi.data$core_cpi, start = cpi.start, tif = 'quarterly')
core.cpi.nsa <- window(core.cpi.nsa, start = core.cpi.start)
cpi.nsa      <- tis(cpi.data$cpi, start = cpi.start, tif = 'quarterly')

# Import bank rate data
# PRIOR STEPS:
#     1. Download data from the BOE website:
#        Bank of England Statistical Interactive Database - official Bank Rate history
#     2. Manually edit the historical since 1694 series 
#            a. Include only 1960 to present, but enter 11/20/1958 change as 1/1/1960 so series has starting value
#            b. Populate rows with years. Remove sub-headers and rows of spaces
#               In Excel, select area and choose F5, "Special", "Blanks", Delete
#            c. Code assume the setup has 4 columns: "year","day","month","rate"
bank.rate.file      <- "rawData/uk_boe_bank_rate_changes.csv"
bank.rate.data      <- read.table(bank.rate.file, skip = 0, header = TRUE, sep = ',', stringsAsFactors = FALSE)
bank.rate.data$date <- as.Date(paste(bank.rate.data$month,bank.rate.data$day,bank.rate.data$year), "%b %d %Y")
bank.rate.data      <- subset(bank.rate.data, select = c("date","rate"))

# Converting changes in bank rate to daily series:
#    - bank.rate.data contains changes in the BOE bank rate (dates are included only if a change occurs)
#    - bank.rate.changes.d is a daily time series with NA values inserted for dates on which a change does not occur
#    - bank.rate.d is a daily series created by carrying forward the last value when a value is NA
date.seq            <- seq(from = bank.rate.data$date[1], to = as.Date(ti(data.end,tif='quarterly')), by = '1 day')
if (!require("xts")) {install.packages("xts"); library("xts")} # Time series library
bank.rate.changes.d <- data.frame(bank.rate = with(bank.rate.data, rate[match(date.seq, date)]))
bank.rate.d         <- as.xts(bank.rate.changes.d, order.by = date.seq, frequency = 365)
bank.rate.d         <- na.locf(bank.rate.d)
detach("package:xts") # Remove packages to avoid masking base functions
detach("package:zoo")
bank.rate.d         <- tis(bank.rate.d$bank.rate, end = ti(data.end, tif='quarterly'), tif = 'daily')[,1]

#------------------------------------------------------------------------------#
# Prepare data
#------------------------------------------------------------------------------#

# Take log of real GDP
gdp.log <- log(gdp)

# Seasonally adjust CPI and core CPI data and re-format as tis series
cpi <- final(seas(as.ts(naWindow(cpi.nsa),freq=4)))
cpi <- as.tis(cpi,start=cpi.start,tif='quarterly')

core.cpi <- final(seas(as.ts(naWindow(core.cpi.nsa),freq=4)))
core.cpi <- as.tis(core.cpi,start=core.cpi.start,tif='quarterly')

# Create annualized core inflation and inflation series using the price indices
core.inflation.q <- 400*log(core.cpi/Lag(core.cpi, k=1))
inflation.q      <- 400*log(cpi/Lag(cpi, k=1))

# Final inflation series: CPI series is used prior to 1970q2;
# thereafter, use core CPI series
inflation <- mergeSeries(window(inflation.q, end = core.cpi.start),window(core.inflation.q, start = shiftQuarter(core.cpi.start,1)))

# Inflation expectations measure: 4-quarter moving average of past inflation
inflation.expectations <- (inflation + Lag(inflation, k=1) + Lag(inflation, k=2) + Lag(inflation, k=3))/4

# Aggregate bank rate data to quarterly frequency by taking the average
interest.q <- convert(bank.rate.d, tif = 'quarterly', observed = 'averaged')

# Express interest rate data on a 365-day basis
interest <- 100*((1+interest.q/36000)^365 -1)

#------------------------------------------------------------------------------#
# Output Data
#------------------------------------------------------------------------------#
data.out <- window(cbind(gdp.log, inflation, inflation.expectations, interest),start = data.start, end = data.end)
write.table(data.out,file = 'inputData/rstar.data.uk.csv', sep = ',',
            col.names = TRUE, quote = FALSE, na = '.', row.names = FALSE)
