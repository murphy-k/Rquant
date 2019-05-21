# Trading Strategy: Trend-Following
# Technical Indicators: Fast and Slow SMA
# Optimization/Walk Forward Analysis: Yes/No

# 1. Pacakges ####
library("quantstrat")
rm(list = ls())
dev.off(dev.list()["RStudioGD"])

# 2. Setup ####
# 2.1. Initial Settings
init.portf <- '2017-12-31'
start.date <- '2018-01-01'
end.date <- Sys.Date()
Sys.setenv(TZ = "UTC")
init.equity <- 10000
enable_stops <- FALSE
fast_sma_params <- list(n = c(2:9))
slow_sma_params <- list(n = c(10:50))
position_size <- 100
txn_fee <- -6
initial_stop <- 0.05
trailing_stop <- 0.07

# 2.2. Data Downloading
getSymbols(
  Symbols = "SPY",
  src = "yahoo",
  from = start.date,
  to = end.date,
  index.class = "POSIXct",
  adjust = TRUE,
  auto.assign = TRUE
)
# 2.3. Initialize Currency
currency(primary_id = "USD")

# 2.4.Initialize Stock Instrument
stock(primary_id = "SPY",
      currency = "USD",
      multiplier = 1)



# 3. Details ####
# Trend-Following Strategy
# Buy Rules = Buy when fast SMA > slow SMA
# Sell Rules = Sell when fast SMA < slow SMA

# 4. Initialization ####
# 4.1. Strategy Name
opt.trend1.strat <- "OptTrendStrat1"

# 4.2. Clear Strategy Data
rm.strat(opt.trend1.strat)

# 4.3. Strategy Object
strategy(name = opt.trend1.strat, store = TRUE)

# 4.4. Completed Strategy Object
summary(getStrategy(opt.trend1.strat))

# 5. Definitions ####

# 5.1. Add Strategy Indicator

# 5.1.1. Add Fast SMA
add.indicator(
  strategy = opt.trend1.strat,
  name = "SMA",
  arguments = list(x = quote(Cl(mktdata))),
  label = "FastSMA"
)
# 5.1.2. Add Slow SMA
add.indicator(
  strategy = opt.trend1.strat,
  name = "SMA",
  arguments = list(x = quote(Cl(mktdata))),
  label = "SlowSMA"
)

# 5.2. Signals ####

# 5.2.1. Add Buying Signal
add.signal(
  strategy = opt.trend1.strat,
  name = "sigCrossover",
  arguments = list(
    columns = c("FastSMA", "SlowSMA"),
    relationship = "gt"
  ),
  label = "BuySignal"
)
# 5.2.2. Add Selling Signal
add.signal(
  strategy = opt.trend1.strat,
  name = "sigCrossover",
  arguments = list(
    columns = c("FastSMA", "SlowSMA"),
    relationship = "lt"
  ),
  label = "SellSignal"
)

# 5.3. Rules ####

# 5.3.1. Add Enter Rule
add.rule(
  strategy = opt.trend1.strat,
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
  strategy = opt.trend1.strat,
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
  strategy = opt.trend1.strat,
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
  strategy = opt.trend1.strat,
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
# 5.4.1. Add SMA Parameters Combinations
# Fast SMA
add.distribution(
  strategy = opt.trend1.strat,
  paramset.label = 'OptTrendPar1',
  component.type = 'indicator',
  component.label = 'FastSMA',
  variable = fast_sma_params,
  label = 'nFastSMA'
)
# Slow SMA
add.distribution(
  strategy = opt.trend1.strat,
  paramset.label = 'OptTrendPar1',
  component.type = 'indicator',
  component.label = 'SlowSMA',
  variable = slow_sma_params,
  label = 'nSlowSMA'
)

# 5.4. Completed Strategy Object
summary(getStrategy(opt.trend1.strat))

# 6. Portfolio Initialization ####

# 6.1. Portfolio Names
opt.trend1.portf <- "OptTrendPort1"

# 6.2. Clear Portfolio Data
rm.strat(opt.trend1.portf)

# 6.3. Initialize Portfolio Object
initPortf(name = opt.trend1.portf,
          symbols = "SPY",
          initDate = init.portf)

# 6.4. Initialize Account Object
initAcct(
  name = opt.trend1.strat,
  portfolios = opt.trend1.portf,
  initDate = init.portf,
  initEq = init.equity
)

# 6.5. Initialize Orders Object
initOrders(portfolio = opt.trend1.portf, initDate = init.portf)

# 7. Optimization ####

# 7.1. Strategy Optimization Results
opt.trend1.results <-
  apply.paramset(
    strategy.st = opt.trend1.strat,
    paramset.label = 'OptTrendPar1',
    portfolio.st = opt.trend1.portf,
    account.st = opt.trend1.strat,
    nsamples = 0,
    verbose = TRUE
  )

# 7.2. Strategy Optimization Trading Statistics

# 7.2.1. Strategy Optimization General Trade Statistics
all.trend1.stats <- opt.trend1.results$tradeStats
View(all.trend1.stats)

# 7.2.2. Strategy Optimization Net Trading PL
plot(
  x = all.trend1.stats$Portfolio,
  y = all.trend1.stats$Net.Trading.PL,
  main = "Trend1 Optimization Net Trading PL",
  xlab = "Portfolio",
  ylab = "Net.Trading.PL"
)

# 7.2.3. Strategy Optimization Maximum Drawdown
plot(
  x = all.trend1.stats$Portfolio,
  y = all.trend1.stats$Max.Drawdown,
  main = "Trend1 Optimization Maximum Drawdown",
  xlab = "Portfolio",
  ylab = "Max.Drawdown"
)

# 7.2.4. Strategy Optimization Profit to Maximum Drawdown
plot(
  x = all.trend1.stats$Portfolio,
  y = all.trend1.stats$Profit.To.Max.Draw,
  main = "Trend1 Optimization Profit to Maximum Drawdown",
  xlab = "Portfolio",
  ylab = "Profit.To.Max.Draw"
)
which.max(all.trend1.stats$Profit.To.Max.Draw)
