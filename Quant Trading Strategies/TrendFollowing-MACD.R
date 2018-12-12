# Trading Strategy: Trend-Following Momentum
# Technical Indicators: MACD
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
init.equity <- 10000
enable_stops <- TRUE
fastema <- 2
slowema <- 22
signal <- 6
position_size <- 100
txn_fee <- -6

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

# Trend-Following Momentum Strategy
# Buy Rules = Buy when MACD > MACD Signal
# Sell Rules = Sell when MACD < MACD Signal
barChart(AAPL$AAPL.Adjusted)
addMACD(fast = fastema,
        slow = slowema,
        signal = signal)

# 4. Initialization ####

# 4.1. Strategy Name
trend2.strat <- "TrendStrat2"

# 4.2. Clear Strategy Data
rm.strat(trend2.strat)

# 4.3. Strategy Object
strategy(name = trend2.strat, store = TRUE)

# 4.4. Completed Strategy Object
summary(getStrategy(trend2.strat))

# 5. Definitions ####
# 5.1. Add Strategy Indicator
add.indicator(
  strategy = trend2.strat,
  name = "MACD",
  arguments = list(
    x = quote(Ad(mktdata)),
    nFast = fastema,
    nSlow = slowema,
    nSig = signal
  ),
  label = "MACD"
)

# 5.2. Signals ####

# 5.2.1. Add Buying Signal
add.signal(
  strategy = trend2.strat,
  name = "sigCrossover",
  arguments = list(
    columns = c("macd", "signal"),
    relationship = "gt"
  ),
  label = "BuySignal"
)
# 5.2.2. Add Selling Signal
add.signal(
  strategy = trend2.strat,
  name = "sigCrossover",
  arguments = list(
    columns = c("macd", "signal"),
    relationship = "lt"
  ),
  label = "SellSignal"
)

# 5.3. Rules ####

# 5.3.1. Add Enter Rule
add.rule(
  strategy = trend2.strat,
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
  strategy = trend2.strat,
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
  strategy = trend2.strat,
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
  strategy = trend2.strat,
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
summary(getStrategy(trend2.strat))

# 6. Portfolio Initialization ####

# 6.1. Portfolio Names
trend2.portf <- "TrendPort2"

# 6.2. Clear Portfolio Data
rm.strat(trend2.portf)

# 6.3. Initialize Portfolio Object
initPortf(name = trend2.portf,
          symbols = "AAPL",
          initDate = init.portf)

# 6.2. Initialize Account Object
initAcct(
  name = trend2.strat,
  portfolios = trend2.portf,
  initDate = init.portf,
  initEq = init.equity
)

# 6.3. Initialize Orders Object
initOrders(portfolio = trend2.portf, initDate = init.portf)

# 7. Application ####

# 7.1. Strategy Application to Market Data
applyStrategy(strategy = trend2.strat, portfolios = trend2.portf)

# 7.2 Strategy Updating
# Specific Order Must be Followed

# 7.2.1. Update Portfolio
updatePortf(Portfolio = trend2.portf)

# 7.2.2. Update Account
updateAcct(name = trend2.strat)

# 7.2.3. Update Equity
updateEndEq(Account = trend2.strat)

# 8. Reporting ####

# 8.1. Strategy Trading Statistics

# 8.1.1. Strategy General Trade Statistics
trend2.stats <- t(tradeStats(Portfolios = trend2.portf))
View(trend2.stats)

# 8.1.2. Strategy Per Trade Statistics
trend2.perstats <- perTradeStats(Portfolio = trend2.portf)
View(trend2.perstats)

# 8.1.3. Strategy Order Book
trend2.book <- getOrderBook(portfolio = trend2.portf)
trend2.book

# 8.1.4. Strategy Position Chart
chart.theme <- chart_theme()
chart.theme$col$dn.col <- 'white'
chart.theme$col$dn.border <- 'lightgray'
chart.theme$col$up.border <- 'lightgray'
chart.Posn(Portfolio = trend2.portf,
           Symbol = "AAPL",
           theme = chart.theme)
add_MACD(
  fast = fastema,
  slow = slowema,
  signal = signal,
  maType = "EMA"
)

# 8.1.5. Strategy Equity Curve
trend2.acct <- getAccount(Account = trend2.strat)
trend2.equity <- trend2.acct$summary$End.Eq
plot(trend2.equity, main = "Trend2 Strategy Equity Curve")

# 8.1.6. Strategy Performance Chart
trend2.ret <- Return.calculate(trend2.equity, method = "log")
bh.ret <- Return.calculate(AAPL[, 6], method = "log")
trend2.comp <- cbind(trend2.ret, bh.ret)
charts.PerformanceSummary(trend2.comp, main = "Trend2 Strategy Performance")
table.AnnualizedReturns(trend2.comp)

# 8.2. Strategy Risk Management

# 8.2.1. Strategy Maximum Adverse Excursion Chart
chart.ME(
  Portfolio = trend2.portf,
  Symbol = 'AAPL',
  type = 'MAE',
  scale = 'percent'
)

# 8.2.2. Strategy Maximum Favorable Excursion Chart
chart.ME(
  Portfolio = trend2.portf,
  Symbol = 'AAPL',
  type = 'MFE',
  scale = 'percent'
)

# 8.2.3. Strategy Maximum Portfolio Position
trend2.kelly <- KellyRatio(trend2.ret, method = "half")
trend2.kelly
