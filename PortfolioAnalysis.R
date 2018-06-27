# Packages ####
library(quantmod)
library(zoo)
library(xts)
library(PerformanceAnalytics)

# Clear plots and environment
rm(list = ls())
dev.off(dev.list()["RStudioGD"])
Sys.setenv(TZ='UTC')

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
names <- c("CTL", "COST", "DTO", "HSY", "TUES")
qty <- c(31.787, 102.249, 1000, 101.443, 2000)
quotes <- getQuote(names, src = "yahoo")
dollar_value <- qty * quotes$Last
weights <- round((dollar_value/sum(dollar_value)*100),2)
portfolio <- as.data.frame(cbind(dollar_value, weights), row.names = names)

summary(portfolio)
barplot(
  portfolio$weights,
  main = "Portfolio Weights",
  ylab = "Percent of Portfolio",
  xlab = "Instrument",
  names.arg = names
)
