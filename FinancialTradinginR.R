# Packages ####
library(quantstrat)

rm(list = ls())
dev.off(dev.list()["RStudioGD"])

# Environment ####
initdate <- "1999-01-01"
from <- "2003-01-01"
to <- "2015-12-31"
tradesize <- 100000
initeq <- 100000
Sys.setenv(TZ = "UTC") # Set the timezone to UTC
currency("USD") # Set the currency to USD
getSymbols(
  "SPY",
  from = "2000-01-01",
  to = Sys.Date(),
  src =  "yahoo",
  adjust =  TRUE
)
stock("SPY", currency = "USD")

# Define the names of your strategy, portfolio and account
strategy.st <- "firststrat"
portfolio.st <- "firststrat"
account.st <- "firststrat"
# Remove the existing strategy if it exists
rm.strat(strategy.st)
# Initialize the portfolio
initPortf(
  portfolio.st,
  symbols = "SPY",
  initDate = initdate,
  currency = "USD"
)
# Initialize the account
initAcct(
  account.st,
  portfolios = portfolio.st,
  initDate = initdate,
  currency = "USD",
  initEq = initeq
)
# Initialize the orders
initOrders(portfolio.st, initDate = initdate)
# Store the strategy
strategy(strategy.st, store = TRUE)


# Visual ####
plot(Cl(SPY))
lines(DVO(HLC(SPY), navg = 2, percentlookback = 126))
lines(SMA(Cl(SPY), n = 200), col = "red") # Add a 200-day SMA using lines()

# Create a 200-day SMA
spy_sma <- SMA(Cl(SPY), n = 200)
# Create an RSI with a 3-day lookback period
spy_rsi <- RSI(Cl(SPY), n = 3)
# Indicators ####
# Add a 200-day SMA indicator to strategy.st
add.indicator(
  strategy = strategy.st,
  # Add the SMA function
  name = "SMA",
  # Create a lookback period
  arguments = list(x = quote(Cl(mktdata)), n = 200),
  # Label your indicator SMA200
  label = "SMA200"
)
# Add a 50-day SMA indicator to strategy.st
add.indicator(
  strategy = strategy.st,
  # Add the SMA function
  name = "SMA",
  # Create a lookback period
  arguments = list(x = quote(Cl(mktdata)), n = 50),
  # Label your indicator SMA50
  label = "SMA50"
)
# Add an RSI 3 indicator to strategy.st
add.indicator(
  strategy = strategy.st,
  # Add the RSI 3 function
  name = "RSI",
  # Create a lookback period
  arguments = list(x = quote(Cl(mktdata)), n = 3),
  # Label your indicator RSI_3
  label = "RSI_3"
)
# Write the RSI_avg function
RSI_avg <- function(price, n1, n2) {
  # RSI 1 takes an input of the price and n1
  rsi_1 <- RSI(price = price, n = n1)
  # RSI 2 takes an input of the price and n2
  rsi_2 <- RSI(price = price, n = n2)
  # RSI_avg is the average of rsi_1 and rsi_2
  RSI_avg <- (rsi_1 + rsi_2) / 2
  # Your output of RSI_avg needs a column name of RSI_avg
  colnames(RSI_avg) <- "RSI_avg"
  return(RSI_avg)
}
# Add this function as RSI_3_4 to your strategy with n1 = 3 and n2 = 4
add.indicator(
  strategy.st,
  name = "RSI_avg",
  arguments = list(
    price = quote(Cl(mktdata)),
    n1 = 3,
    n2 = 4
  ),
  label = "RSI_3_4"
)
# Declare the DVO function
DVO <- function(HLC,
                navg = 2,
                percentlookback = 126) {
  # Compute the ratio between closing prices to the average of high and low
  ratio <- Cl(HLC) / ((Hi(HLC) + Lo(HLC)) / 2)
  # Smooth out the ratio outputs using a moving average
  avgratio <- SMA(ratio, n = navg)
  # Convert ratio into a 0-100 value using runPercentRank()
  out <-
    runPercentRank(avgratio, n = percentlookback, exact.multiplier = 1) * 100
  colnames(out) <- "DVO"
  return(out)
}
# Add the DVO indicator to your strategy
add.indicator(
  strategy = strategy.st,
  name = "DVO",
  arguments = list(
    HLC = quote(HLC(mktdata)),
    navg = 2,
    percentlookback = 126
  ),
  label = "DVO_2_126"
)
test <- cbind(SPY$SPY.Close, DVO(SPY, navg = 2, percentlookback = 126))
spy_dvo <- DVO(HLC = HLC(SPY),navg = 2,percentlookback = 126)
summary(spy_dvo)
