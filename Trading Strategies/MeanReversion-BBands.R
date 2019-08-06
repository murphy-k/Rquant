# Trading Strategy: Mean-Reversion "Bollinger Bands"
# Technical Indicators: BBands
# Optimization/Walk Forward Analysis: No/No

# 1. Packages ####
library("quantstrat")
rm(list = ls())
dev.off(dev.list()["RStudioGD"])
# 2. Setup ####

# 2.1. Initial Settings
init.portf <- '2007-12-31'
start.date <- '2008-01-01'
end.date <- Sys.Date()
Sys.setenv(TZ = "UTC")
init.equity <- 100000
enable_stops <- FALSE
period <- 20
sd <- 2.0
position_size <- 100
txn_fee <- -6

# 2.2. Data Downloading
getSymbols(
  Symbols = "SPY",
  src = "yahoo",
  from = start.date,
  to = end.date,
  index.class = "POSIXct",
  adjust = T
)

# 2.3. Initialize Currency
currency(primary_id = "USD")

# 2.4.Initialize Stock Instrument
stock(primary_id = "SPY",
      currency = "USD",
      multiplier = 1)

# 3. Details ####

# Mean-Reversion Strategy
# Buy Rules = Buy when Close < Lower Band
# Sell Rules = Sell when Close > Upper Band
lineChart(SPY)
addBBands(n = period, sd = sd)

# 4. Initialization ####

# 4.1. Strategy Name
mean1.strat <- "MeanStrat1"

# 4.2. Clear Strategy Data
rm.strat(mean1.strat)

# 4.3. Strategy Object
strategy(name = mean1.strat, store = TRUE)

# 4.4. Completed Strategy Object
summary(getStrategy(mean1.strat))

# 5. Definitions ####

# 5.1. Add Strategy Indicator

# 5.1.1. Add BBands
add.indicator(
  strategy = mean1.strat,
  name = "BBands",
  arguments = list(HLC = quote(HLC(mktdata)), n = 20, sd = 2),
  label = 'BBands'
)

# 5.2. Signals ####

# 5.2.1. Add Buying Signal
add.signal(
  strategy = mean1.strat,
  name = "sigCrossover",
  arguments = list(columns = c("Close", "dn"), relationship = "lt"),
  label = "BuySignal"
)
# 5.2.2. Add Selling Signal
add.signal(
  strategy = mean1.strat,
  name = "sigCrossover",
  arguments = list(columns = c("Close", "up"), relationship = "gt"),
  label = "SellSignal"
)

# 5.3. Rules ####

# 5.3.1. Add Enter Rule
add.rule(
  strategy = mean1.strat,
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
  strategy = mean1.strat,
  name = 'ruleSignal',
  arguments = list(
    sigcol = "BuySignal",
    sigval = TRUE,
    orderqty = 'all',
    ordertype = 'stoplimit',
    threshold = 0.05,
    orderside = 'long'
  ),
  type = 'chain',
  label = "StopLoss",
  parent = "EnterRule",
  enabled = enable_stops
)
add.rule(
  strategy = mean1.strat,
  name = 'ruleSignal',
  arguments = list(
    sigcol = "BuySignal",
    sigval = TRUE,
    orderqty = 'all',
    ordertype = 'stoptrailing',
    threshold = 0.07,
    orderside = 'long'
  ),
  type = 'chain',
  label = "TrailingStop",
  parent = "EnterRule",
  enabled = enable_stops
)

# 5.3.2. Add Exit Rule
add.rule(
  strategy = mean1.strat,
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
summary(getStrategy(mean1.strat))

# 6. Portfolio Initialization ####

# 6.1. Portfolio Names
mean1.portf <- "MeanPort1"

# 6.2. Clear Portfolio Data
rm.strat(mean1.portf)

# 6.3. Initialize Portfolio Object
initPortf(name = mean1.portf,
          symbols = "SPY",
          initDate = init.portf)

# 6.2. Initialize Account Object
initAcct(
  name = mean1.strat,
  portfolios = mean1.portf,
  initDate = init.portf,
  initEq = init.equity
)

# 6.3. Initialize Orders Object
initOrders(portfolio = mean1.portf, initDate = init.portf)

# 7. Application ####

# 7.1. Strategy Application to Market Data
applyStrategy(strategy = mean1.strat, portfolios = mean1.portf)

# 7.2 Strategy Updating
# Specific Order Must be Followed

# 7.2.1. Update Portfolio
updatePortf(Portfolio = mean1.portf)

# 7.2.2. Update Account
updateAcct(name = mean1.strat)

# 7.2.3. Update Equity
updateEndEq(Account = mean1.strat)

# 8. Reporting ####

# 8.1. Strategy Trading Statistics

# 8.1.1. Strategy General Trade Statistics
mean1.stats <- t(tradeStats(Portfolios = mean1.portf))
View(mean1.stats)

# 8.1.2. Strategy Per Trade Statistics
mean1.perstats <- perTradeStats(Portfolio = mean1.portf)
View(mean1.perstats)

# 8.1.3. Strategy Order Book
mean1.book <- getOrderBook(portfolio = mean1.portf)
mean1.book

# 8.1.4. Strategy Position Chart
chart.theme <- chart_theme()
chart.theme$col$dn.col <- 'white'
chart.theme$col$dn.border <- 'lightgray'
chart.theme$col$up.border <- 'lightgray'
chart.Posn(Portfolio = mean1.portf,
           Symbol = "SPY",
           theme = chart.theme)
add_BBands(n = 20, sd = 2, maType = "SMA")

# 8.1.5. Strategy Equity Curve
mean1.acct <- getAccount(Account = mean1.strat)
mean1.equity <- mean1.acct$summary$End.Eq
plot(mean1.equity, main = "Mean1 Strategy Equity Curve")

# 8.1.6. Strategy Performance Chart
mean1.ret <- Return.calculate(mean1.equity, method = "log")
bh.ret <- Return.calculate(SPY[, 4], method = "log")
mean1.comp <- cbind(mean1.ret, bh.ret)
charts.PerformanceSummary(mean1.comp, main = "Mean1 Strategy Performance")
table.AnnualizedReturns(mean1.comp)

# 8.2. Strategy Risk Management

# 8.2.1. Strategy Maximum Adverse Excursion Chart
chart.ME(
  Portfolio = mean1.portf,
  Symbol = 'SPY',
  type = 'MAE',
  scale = 'percent'
)

# 8.2.2. Strategy Maximum Favorable Excursion Chart
chart.ME(
  Portfolio = mean1.portf,
  Symbol = 'SPY',
  type = 'MFE',
  scale = 'percent'
)

# 8.2.3. Strategy Maximum Portfolio Position
mean1.kelly <- KellyRatio(mean1.ret, method = "half")
mean1.kelly
