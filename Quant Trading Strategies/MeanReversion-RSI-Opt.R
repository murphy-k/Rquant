# Trading Strategy: Mean-Reversion "Relative-Strength Index"
# Technical Indicators: RSI
# Optimization/Walk Forward Analysis: Yes/No

# 1. Packages ####
library("quantstrat")
rm(list = ls())
dev.off(dev.list()["RStudioGD"])

# 2. Setup ####
# 2.1. Initial Settings
init.portf <- '2013-12-31'
start.date <- '2014-01-01'
end.date <- '2016-01-01'
Sys.setenv(TZ = "UTC")
init.equity <- 10000
enable_stops <- TRUE
period_params <- list(n = c(2:14))
buythreshold_params <- list(threshold = c(20,30))
sellthreshold_params <- list(threshold = c(70,80))
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
# Mean-Reversion Relative-Strength Strategy
# Buy Rules = Buy when RSI < +30 Treshold
# Sell Rules = Sell when RSI > +70 Treshold
barChart(AAPL)
addRSI(n = 9)

# 4. Initialization ####
# 4.1. Strategy Name
opt.mean2.strat <- "OptMeanStrat2"
# 4.2. Clear Strategy Data
rm.strat(opt.mean2.strat)
# 4.3. Strategy Object
strategy(name = opt.mean2.strat, store = TRUE)
# 4.4. Completed Strategy Object
summary(getStrategy(opt.mean2.strat))

# 5. Definitions ####
# 5.1. Add Strategy Indicator
add.indicator(
  strategy = opt.mean2.strat,
  name = "RSI",
  arguments = list(price = quote(getPrice(mktdata))),
  label = 'RSI'
)
# 5.2. Signals ####
# 5.2.1. Add Buying Signal
add.signal(
  strategy = opt.mean2.strat,
  name = "sigThreshold",
  arguments = list(column = "RSI", relationship = "lt"),
  label = "BuySignal"
)
# 5.2.2. Add Selling Signal
add.signal(
  strategy = opt.mean2.strat,
  name = "sigThreshold",
  arguments = list(column = "RSI", relationship = "gt"),
  label = "SellSignal"
)

# 5.3. Rules ####
# 5.3.1. Add Enter Rule
add.rule(
  strategy = opt.mean2.strat,
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
  strategy = opt.mean2.strat,
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
  strategy = opt.mean2.strat,
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
  strategy = opt.mean2.strat,
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

# 5.4. Parameters ####
# Number of Periods
add.distribution(
  strategy = opt.mean2.strat,
  paramset.label = 'OptMeanPar2',
  component.type = 'indicator',
  component.label = 'RSI',
  variable = period_params,
  label = 'n'
)
# Buy Signal Threshold
add.distribution(
  strategy = opt.mean2.strat,
  paramset.label = 'OptMeanPar2',
  component.type = 'signal',
  component.label = 'BuySignal',
  variable = buythreshold_params,
  label = 'BuyThreshold'
)
# Sell Signal Threshold
add.distribution(
  strategy = opt.mean2.strat,
  paramset.label = 'OptMeanPar2',
  component.type = 'signal',
  component.label = 'SellSignal',
  variable = sellthreshold_params,
  label = 'SellThreshold'
)
# 5.5. Completed Strategy Object
summary(getStrategy(opt.mean2.strat))

# 6. Portfolio Initialization ####
# 6.1. Portfolio Names
opt.mean2.portf <- "OptMeanPort2"
# 6.2. Clear Portfolio Data
rm.strat(opt.mean2.portf)
# 6.3. Initialize Portfolio Object
initPortf(name = opt.mean2.portf,
          symbols = "AAPL",
          initDate = init.portf)

# 6.2. Initialize Account Object
initAcct(
  name = opt.mean2.strat,
  portfolios = opt.mean2.portf,
  initDate = init.portf,
  initEq = init.equity
)

# 6.3. Initialize Orders Object
initOrders(portfolio = opt.mean2.portf, initDate = init.portf)

# 7. Optimization ####

# 7.1. Strategy Optimization Results
opt.mean2.results <-
  apply.paramset(
    strategy.st = opt.mean2.strat,
    paramset.label = 'OptMeanPar2',
    portfolio.st = opt.mean2.portf,
    account.st = opt.mean2.strat,
    nsamples = 0,
    verbose = TRUE
  )

# 7.2. Strategy Optimization Trading Statistics

# 7.2.1. Strategy Optimization General Trade Statistics
all.mean2.stats <- opt.mean2.results$tradeStats
View(all.mean2.stats)
# 7.2.2. Strategy Optimization Net Trading PL
plot(
  x = all.mean2.stats$Portfolio,
  y = all.mean2.stats$Net.Trading.PL,
  main = "Mean2 Optimization Net Trading PL",
  xlab = "Portfolio",
  ylab = "Net.Trading.PL"
)

# 7.2.3. Strategy Optimization Maximum Drawdown
plot(
  x = all.mean2.stats$Portfolio,
  y = all.mean2.stats$Max.Drawdown,
  main = "Mean2 Optimization Maximum Drawdown",
  xlab = "Portfolio",
  ylab = "Max.Drawdown"
)

# 7.2.4. Strategy Optimization Profit to Maximum Drawdown
plot(
  x = all.mean2.stats$Portfolio,
  y = all.mean2.stats$Profit.To.Max.Draw,
  main = "Mean2 Optimization Profit to Maximum Drawdown",
  xlab = "Portfolio",
  ylab = "Profit.To.Max.Draw"
)
which.max(all.mean2.stats$Profit.To.Max.Draw)
