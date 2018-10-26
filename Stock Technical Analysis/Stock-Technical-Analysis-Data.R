# 1. Stock Technical Analysis Data

# 1. Stock Technical Analysis

# 1.1. Load R packages
library("TTR")
library("quantmod")
library("PerformanceAnalytics")

# 1.2. Set working directory
# getwd()
# setwd("C:/.../Stock Technical Analysis with R")

# 1.3. Get data
getSymbols("AAPL", src = "google")

# 1.5. Delimit data range
aapl <- window(AAPL['2014-10-01::2015-09-30'])

# 1.4. Technical Analysis Charts
lineChart(aapl)
barChart(aapl)
candleChart(aapl)