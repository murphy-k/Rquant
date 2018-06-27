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

portfolio <-
  cbind(TSLA$TSLA.Close, SPY$SPY.Close, TLT$TLT.Close, GLD$GLD.Close)
d_portfolio <- diff(portfolio)
# Stocks to Bonds ####
Risk_Premium <- TLT$TLT.Close / SPY$SPY.Close

plot.zoo(Risk_Premium)
abline(h = 1)

# Example Portfolio ####
values <- c(38396, 595, 21525, 55850, 9545, 5900)
names <- c("cash", "CTL", "COST", "DTO", "HSY", "TUES")
weights <- values / sum(values)

portfolio <- as.data.frame(cbind(values, weights), row.names = names)
summary(portfolio)
barplot(
  portfolio$weights,
  main = "Portfolio Weights",
  ylab = "Percent of Portfolio",
  xlab = "Instrument",
  names.arg = names
)
