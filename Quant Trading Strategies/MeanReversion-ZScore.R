# Trading Strategy: Mean-Reversion Statistical Arbitrage
# Technical Indicators: Z-Score
# Optimization/Walk Forward Analysis: No/No

# 1. Packages ####
library("quantstrat")
library("tseries")
library("roll")
library("pracma")
rm(list = ls())
dev.off(dev.list()["RStudioGD"])

# 2. Setup ####
# 2.1. Initial Settings
init.portf <- '2007-12-31'
start.date <- '2008-01-01'
end.date <- Sys.Date()
Sys.setenv(TZ = "UTC")
init.equity <- 100000
enable_stops <- TRUE
period <- 11
buythreshold <- -1.5
sellthreshold <- 1.5
position_size <- 100
txn_fee <- -6
initial_stop <- 0.025
trailing_stop <- 0.025
# 2.2. Data Downloading
getSymbols(
  Symbols = "AAPL",
  src = "yahoo",
  from = start.date,
  to = end.date,
  index.class = "POSIXct",
  adjust = T
)

# 2.3. Initialize Currency
currency(primary_id = "USD")

# 2.4.Initialize Stock Instrument
stock(primary_id = "AAPL",
      currency = "USD",
      multiplier = 1)

# 3. Details ####

# Mean-Reversion Statistical Arbitrage Strategy
# Stationary Time Series Tests: ADF, KPSS, Hurst Exponent
# Buy Rules = Buy when Z-Score < -1.5 Treshold
# Sell Rules = Sell when Z-Score > +1.5 Treshold

# 3.1. Stationary Time Series Tests

# Augmented Dickey-Fuller Test
# p-value > 0.05 = not stationary

# Kwiatkowski-Phillips-Schmidt-Shin Test
# p-value < 0.05 = not stationary

# Hurst Exponent
# H = 0.5 (Random Walk)
# 0.5 < H < 1 (Trending)
# 0 < H < 0.5 (Mean Reverting)

# 3.1.1. Level Time Series
adf.test(Ad(AAPL))
kpss.test(Ad(AAPL))
hurstexp(Ad(AAPL))

# 3.1.2. Differentiated Time Series
diffx <- diff(log(Ad(AAPL)), lag = 1)
diffx <- diffx[complete.cases(diffx)]
adf.test(diffx)
kpss.test(diffx)
hurstexp(diffx)

# 3.2. Z-Score Function and Calculation

# 3.2.1. Z-Score Function
zscore.fun <- function(x, n) {
  roll_scale(x, width = n)
}

# 3.2.2. Z-Score Calculation
zscore <- zscore.fun(diff(log(Cl(AAPL)), lag = 1), n = period)
plot.zoo(
  zscore,
  xlab = "Date",
  ylab = "Z-Score",
  main = "AAPL Z-Score",
  type = "h"
)
abline(h = sellthreshold, col = "red")
abline(h = buythreshold, col = "green")
abline(h = last(zscore$AAPL.Close), col = "blue")
# 4. Initialization ####
# 4.1. Strategy Name
mean3.strat <- "MeanStrat3"
# 4.2. Clear Strategy Data
rm.strat(mean3.strat)
# 4.3. Strategy Object
strategy(name = mean3.strat, store = TRUE)
# 4.4. Completed Strategy Object
summary(getStrategy(mean3.strat))

# 5. Definitions ####
# 5.1. Add Strategy Indicator
# 5.1.1. Add Z-Score Indicator
add.indicator(
  strategy = mean3.strat,
  name = "zscore.fun",
  arguments = list(x = quote(diff(log(
    Cl(mktdata)
  ), lag = 1)), n = period),
  label = "zscore"
)

# 5.2. Signals ####

# 5.2.1. Add Z-Score Buying Signal
add.signal(
  strategy = mean3.strat,
  name = "sigThreshold",
  arguments = list(
    threshold = buythreshold,
    column = "zscore",
    relationship = "lt"
  ),
  label = "BuySignal"
)
# 5.2.2. Add Z-Score Selling Signal
add.signal(
  strategy = mean3.strat,
  name = "sigThreshold",
  arguments = list(
    threshold = sellthreshold,
    column = "zscore",
    relationship = "gt"
  ),
  label = "SellSignal"
)

# 5.3. Rules ####

# 5.3.1. Add Enter Rule
add.rule(
  strategy = mean3.strat,
  name = 'ruleSignal',
  arguments = list(
    sigcol = "BuySignal",
    sigval = TRUE,
    orderqty = position_size,
    ordertype = 'market',
    orderside = 'long'
  ),
  type = 'enter',
  label = "EnterRule",
  enabled = T
)
# Stop-Loss and Trailing-Stop Rules
add.rule(
  strategy = mean3.strat,
  name = 'ruleSignal',
  arguments = list(
    sigcol = "BuySignal",
    sigval = TRUE,
    orderqty = 'all',
    ordertype = 'stoplimit',
    threshold = initial_stop,
    orderside = 'long'
  ),
  type = 'chain',
  label = "StopLoss",
  parent = "EnterRule",
  enabled = enable_stops
)
add.rule(
  strategy = mean3.strat,
  name = 'ruleSignal',
  arguments = list(
    sigcol = "BuySignal",
    sigval = TRUE,
    orderqty = 'all',
    ordertype = 'stoptrailing',
    threshold = trailing_stop,
    orderside = 'long'
  ),
  type = 'chain',
  label = "TrailingStop",
  parent = "EnterRule",
  enabled = enable_stops
)

# 5.3.2. Add Exit Rule
add.rule(
  strategy = mean3.strat,
  name = 'ruleSignal',
  arguments = list(
    sigcol = "SellSignal",
    sigval = TRUE,
    orderqty = 'all',
    ordertype = 'market',
    orderside = 'long',
    TxnFees = txn_fee
  ),
  type = 'exit',
  label = "ExitRule",
  enabled = T
)

# 5.4. Completed Strategy Object
summary(getStrategy(mean3.strat))

# 6. Portfolio Initialization ####

# 6.1. Portfolio Names
mean3.portf <- "MeanPort3"

# 6.2. Clear Portfolio Data
rm.strat(mean3.portf)

# 6.3. Initialize Portfolio Object
initPortf(name = mean3.portf,
          symbols = "AAPL",
          initDate = init.portf)

# 6.2. Initialize Account Object
initAcct(
  name = mean3.strat,
  portfolios = mean3.portf,
  initDate = init.portf,
  initEq = init.equity
)

# 6.3. Initialize Orders Object
initOrders(portfolio = mean3.portf, initDate = init.portf)

# 7. Application ####

# 7.1. Strategy Application to Market Data
applyStrategy(strategy = mean3.strat, portfolios = mean3.portf)

# 7.2 Strategy Updating
# Specific Order Must be Followed

# 7.2.1. Update Portfolio
updatePortf(Portfolio = mean3.portf)

# 7.2.2. Update Account
updateAcct(name = mean3.strat)

# 7.2.3. Update Equity
updateEndEq(Account = mean3.strat)

# 8. Reporting ####

# 8.1. Strategy Trading Statistics

# 8.1.1. Strategy General Trade Statistics
mean3.stats <- t(tradeStats(Portfolios = mean3.portf))
View(mean3.stats)

# 8.1.2. Strategy Per Trade Statistics
mean3.perstats <- perTradeStats(Portfolio = mean3.portf)
View(mean3.perstats)

# 8.1.3. Strategy Order Book
mean3.book <- getOrderBook(portfolio = mean3.portf)
mean3.book

# 8.1.4. Strategy Position Chart
chart.theme <- chart_theme()
chart.theme$col$dn.col <- 'white'
chart.theme$col$dn.border <- 'lightgray'
chart.theme$col$up.border <- 'lightgray'
chart.Posn(Portfolio = mean3.portf,
           Symbol = "AAPL",
           theme = chart.theme)

# 8.1.5. Strategy Equity Curve
mean3.acct <- getAccount(Account = mean3.strat)
mean3.equity <- mean3.acct$summary$End.Eq
plot(mean3.equity, main = "Mean3 Strategy Equity Curve")

# 8.1.6. Strategy Performance Chart
mean3.ret <- Return.calculate(mean3.equity, method = "log")
bh.ret <- Return.calculate(AAPL$AAPL.Adjusted, method = "log")
mean3.comp <- cbind(mean3.ret, bh.ret)
charts.PerformanceSummary(mean3.comp, main = "Mean3 Strategy Performance")
table.AnnualizedReturns(mean3.comp)

# 8.2. Strategy Risk Management

# 8.2.1. Strategy Maximum Adverse Excursion Chart
chart.ME(
  Portfolio = mean3.portf,
  Symbol = 'AAPL',
  type = 'MAE',
  scale = 'percent'
)

# 8.2.2. Strategy Maximum Favorable Excursion Chart
chart.ME(
  Portfolio = mean3.portf,
  Symbol = 'AAPL',
  type = 'MFE',
  scale = 'percent'
)

# 8.2.3. Strategy Maximum Portfolio Position
mean3.kelly <- KellyRatio(mean3.ret, method = "half")
mean3.kelly
