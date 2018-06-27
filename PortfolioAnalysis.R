# Packages ####
library(quantmod)
library(zoo)
library(xts)
library(PerformanceAnalytics)

# Clear plots and environment
rm(list = ls())
dev.off(dev.list()["RStudioGD"])

# Variables ####
instruments <- c("TSLA", "SPY", "TLT", "GLD")
start_date <- "2000-01-01"
suppressMessages((
  getSymbols(
    instruments,
    src = "yahoo",
    auto.assign = TRUE,
    from = start_date
  )
))

portfolio <- cbind(TSLA$TSLA.Close, SPY$SPY.Close, TLT$TLT.Close,GLD$GLD.Close)
d_portfolio <- diff(portfolio)
# Stocks to Bonds ####
Risk_Premium <- TLT$TLT.Close/SPY$SPY.Close

plot.zoo(Risk_Premium)
abline(h=1)