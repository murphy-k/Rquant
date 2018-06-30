# Packages ####
library(quantmod)
library(PortfolioAnalytics)
# Clear plots and environment
rm(list = ls())
dev.off(dev.list()["RStudioGD"])
Sys.setenv(TZ='UTC')

# Variables ####
tickers <- c("CTL", "COST", "DTO", "HSY", "TUES")
start_date <- "2000-01-01"
suppressMessages((
  getSymbols(
    tickers,
    src = "yahoo",
    auto.assign = TRUE,
    from = start_date
  )
))

# Example Portfolio ####
qty <- c(31.787, 102.249, 1000, 101.443, 2000)
quotes <- getQuote(tickers, src = "yahoo")
dollar_values <- qty * quotes$Last
weights <- round((dollar_values/sum(dollar_values)*100),2)
portfolio <- as.data.frame(cbind(dollar_values, weights), row.tickers = tickers)

summary(portfolio)
barplot(
  portfolio$weights,
  main = "Portfolio Weights",
  ylab = "Percent of Portfolio",
  xlab = "Instrument",
  names.arg = tickers)
cat("Total Value of Equities =  $", (sum(dollar_values)))
