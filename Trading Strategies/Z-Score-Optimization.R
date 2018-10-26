# Trading Strategy: Mean-Reversion Statistical Arbitrage
# Technical Indicators: Z-Score
# Optimization

# 1. Load R packages

library("quantstrat")
library("tseries")
library("roll")
library("pracma")

# Initial Settings
init.portf <- '1998-12-31'
start.date <- '1999-01-01'
end.date <- '2018-04-13'
Sys.setenv(TZ = "UTC")
# Set-up initial equity, position sizing and stops on/off.
init.equity <- 25000
position_size <- 100
enable_stops <- TRUE
# Z-Score calculation parameters - will simulate all combinations.
n_vector <- c(10, 15, 20)
sdBuy_vector <- c(-2.0, -1.5, -1.75)
sdSell_vector <- c(2.0, 1.5, 1.75)

# Data Downloading
getSymbols(
  Symbols = "HSY",
  src = "yahoo",
  from = start.date,
  to = end.date,
  index.class = "POSIXct",
  adjust = T
)

# Initialize currency & stock instrument handlers
currency(primary_id = "USD")
stock(primary_id = "HSY",
      currency = "USD",
      multiplier = 1)


# Visualize the number of rows in our HSY data frame to accurately format
# the 2-period sma filter. (Not used - YET)

str(HSY)
HSY_filtered <- (SMA(x = HSY[1:4850, 6], n = 2))
str(HSY_filtered)
plot(HSY[, 6])
plot(HSY_filtered[, 1], type = "l")

# Stationarity Time Series Tests
# ADF Test       | p-value > 0.05 = not stationary
# KPSS Test      | p-value < 0.05 = not stationary
# Hurst Exponent | H = 0.5 (Random Walk)
#                | 0.5 < H < 1 (Trending)
#                | 0 < H < 0.5 (Mean Reverting)

# Run the Time Series Stationarity Tests
adf.test(Cl(HSY))
kpss.test(Cl(HSY))
hurstexp(Cl(HSY))

# Run the Differentiated Time Series Stationarity Tests
diffx <- diff(log(Cl(HSY)), lag = 1)
diffx <- diffx[complete.cases(diffx)]
adf.test(diffx)
kpss.test(diffx)
hurstexp(diffx[1:316])

# z-score function and calculation

# z-score function definition
zscore.fun <- function(x, n) {
  roll_scale(x, width = n)
}

# Visualize z-score on our given data set.
zscore <- zscore.fun(diff(log(Cl(HSY)), lag = 1), n = 20)
plot(zscore)
abline(h = 2.0, col = 2)
abline(h = -2.0, col = 3)

# Strategy Initialization
#   Strategy Name
opt.mean3.strat <- "OptMeanStrat3"
#   Clear Strategy Data
rm.strat(opt.mean3.strat)
#   Strategy Object
strategy(name = opt.mean3.strat, store = TRUE)
#   Completed Strategy Object
summary(getStrategy(opt.mean3.strat))

# Strategy Definition
#   Add Strategy Indicator(s)
#     Add Z-Score Indicator
add.indicator(
  strategy = opt.mean3.strat,
  name = "zscore.fun",
  arguments = list(x = quote(diff(log(
    Cl(mktdata)
  ), lag = 1))),
  label = "zscore"
)

# Add Strategy Signals
#   Add Z-Score Buying Signal
add.signal(
  strategy = opt.mean3.strat,
  name = "sigThreshold",
  arguments = list(column = "zscore", relationship = "lt"),
  label = "BuySignal"
)
#   Add Z-Score Selling Signal
add.signal(
  strategy = opt.mean3.strat,
  name = "sigThreshold",
  arguments = list(column = "zscore", relationship = "gt"),
  label = "SellSignal"
)

#Add Strategy Rules
#   Add Enter Rule
add.rule(
  strategy = opt.mean3.strat,
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
#   Stop-Loss and Trailing-Stop Rules (Set to on/off in initial settings)
add.rule(
  strategy = opt.mean3.strat,
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
  strategy = opt.mean3.strat,
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

#   Add Exit Rule
add.rule(
  strategy = opt.mean3.strat,
  name = 'ruleSignal',
  arguments = list(
    sigcol = "SellSignal",
    sigval = TRUE,
    orderqty = 'all',
    ordertype = 'market',
    orderside = 'long',
    TxnFees = -6
  ),
  type = 'exit',
  label = "ExitRule",
  enabled = T
)

# Add ZScore Parameters Combinations
#   calculate number of periods from vector
add.distribution(
  strategy = opt.mean3.strat,
  paramset.label = 'OptMeanPar3',
  component.type = 'indicator',
  component.label = 'zscore',
  variable = list(n = n_vector),
  label = 'n'
)
#   calculate the std.dev buy signal vector
add.distribution(
  strategy = opt.mean3.strat,
  paramset.label = 'OptMeanPar3',
  component.type = 'signal',
  component.label = 'BuySignal',
  variable = list(threshold = sdBuy_vector),
  label = 'sdBuy'
)
#   calculate the std.dev sell signal vector
add.distribution(
  strategy = opt.mean3.strat,
  paramset.label = 'OptMeanPar3',
  component.type = 'signal',
  component.label = 'SellSignal',
  variable = list(threshold = sdSell_vector),
  label = 'sdSell'
)

# Completed Strategy Object
summary(getStrategy(opt.mean3.strat))

# Portfolio Initialization
#   Portfolio Name
opt.mean3.portf <- "OptMeanPort3"
#   Clear Portfolio Data
rm.strat(opt.mean3.portf)
#   Initialize Portfolio Object
initPortf(name = opt.mean3.portf,
          symbols = "HSY",
          initDate = init.portf)

#Initialize Account Object
initAcct(
  name = opt.mean3.strat,
  portfolios = opt.mean3.portf,
  initDate = init.portf,
  initEq = init.equity
)
# Initialize Orders Object
initOrders(portfolio = opt.mean3.portf, initDate = init.portf)

# Strategy Results
opt.mean3.results <-
  apply.paramset(
    strategy.st = opt.mean3.strat,
    paramset.label = 'OptMeanPar3',
    portfolio.st = opt.mean3.portf,
    account.st = opt.mean3.strat,
    nsamples = 0,
    verbose = TRUE
  )

# Strategy Results General Trade Statistics
all.mean3.stats <- opt.mean3.results$tradeStats
View(t(all.mean3.stats))
#   Strategy Optimization Net Trading PL
plot(
  x = all.mean3.stats$Portfolio,
  y = all.mean3.stats$Net.Trading.PL,
  main = "Mean3 Optimization Net Trading PL",
  xlab = "Portfolio",
  ylab = "Net.Trading.PL"
)
#   Strategy Optimization Maximum Drawdown
plot(
  x = all.mean3.stats$Portfolio,
  y = all.mean3.stats$Max.Drawdown,
  main = "Mean3 Optimization Maximum Drawdown",
  xlab = "Portfolio",
  ylab = "Max.Drawdown"
)
#   Strategy Optimization Profit to Maximum Drawdown
plot(
  x = all.mean3.stats$Portfolio,
  y = all.mean3.stats$Profit.To.Max.Draw,
  main = "Mean3 Optimization Profit to Maximum Drawdown",
  xlab = "Portfolio",
  ylab = "Profit.To.Max.Draw"
)
