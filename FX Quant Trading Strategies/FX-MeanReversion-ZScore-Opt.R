# Trading Strategy: Mean-Reversion Statistical Arbitrage
# Technical Indicators: Z-Score
# Optimization/Walk Forward Analysis: Yes/No

# 1. Packages ####
library("quantstrat")
library("tseries")
library("roll")
library("pracma")
library("quantmod")
library("lubridate")
rm(list = ls())
dev.off(dev.list()["RStudioGD"])

# 1.0 Download FX Data ####
fxhistoricaldata <- function(Symbol, timeframe, download = FALSE)
{
  # setup temp folder
  temp.folder <- paste(getwd(), 'temp', sep = '/')
  dir.create(temp.folder, F)
  filename <-
    paste(temp.folder,
          '/',
          "fxhistoricaldata_",
          Symbol ,
          "_" ,
          timeframe,
          ".csv",
          sep = '')
  
  if (download) {
    downloadfile <-
      paste(
        "http://api.fxhistoricaldata.com/indicators?instruments=" ,
        Symbol ,
        "&expression=open,high,low,close&item_count=10000&format=csv&timeframe=",
        timeframe,
        sep = ''
      )
    download.file(downloadfile, filename,  mode = 'wb')
  }
  tempdf <- read.csv(filename)
  colnames(tempdf) <-
    c("Curr", "Date", "Open", "High", "Low", "Close")
  tempdf <- tempdf[c("Date", "Open", "High", "Low", "Close")]
  tempdf$Date <- ymd_hms(tempdf$Date)
  out <-  xts(tempdf[, -1], order.by = tempdf[, 1])
  
  return(out)
}
# 2.0  View FX data ####
timeframe <- "hour"
EURUSD <- fxhistoricaldata("EUR_USD", timeframe, download = TRUE)
str(EURUSD)
periodicity(EURUSD)


# Z-Score Backtest ####
init.portf <- start(EURUSD) - 100000
start.date <- start(EURUSD)
Sys.setenv(TZ = "UTC")
init.equity <- 100000
enable_stops <- FALSE
period_params <- list(n = c(10:30))
buythreshold_params <-  list(threshold = c(-1.5, -1.75, -2.00))
sellthreshold_params <- list(threshold = c(1.5, 1.75, 2.00))
position_size <- 10000
txn_fee <- -0.00
initial_stop <- 0.005
trailing_stop <- 0.005

# 2.3. Initialize Currency
currency(primary_id = "USD")

# 2.4.Initialize Stock Instrument
stock(primary_id = "EURUSD",
      currency = "USD",
      multiplier = 1)

# 3. Details ####
# Mean-Reversion Statistical Arbitrage Strategy
# Stationary Time Series Tests: ADF, KPSS, Hurst Exponent

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
adf.test((EURUSD[,4]))
kpss.test((EURUSD[,4]))
hurstexp((EURUSD[,4]))

# 3.1.2. Differentiated Time Series
diffx <- diff(log((EURUSD[,4])), lag = 1)
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
zscore <-
  zscore.fun((EURUSD[,4]),
             n = sum((period_params$n) / (length(period_params$n))))
plot.zoo(
  x = EURUSD[,4],
  type = "l",
  xlab = "Date",
  ylab = "Price",
  main = "EURUSD"
)

plot.zoo(
  x = zscore,
  type = "h",
  xlab = "Date",
  ylab = c("Z-Score ", sum((period_params$n) / (
    length(period_params$n)
  ))),
  main = "EURUSD"
)
abline(h = 0, col = "black")
abline(h = 2, col = "green")
abline(h = -2, col = "red")

# 4. Initialization ####
# 4.1. Strategy Name
opt.mean3.strat <- "OptMeanStrat3"
# 4.2. Clear Strategy Data
rm.strat(opt.mean3.strat)
# 4.3. Strategy Object
strategy(name = opt.mean3.strat, store = TRUE)
# 4.4. Completed Strategy Object
summary(getStrategy(opt.mean3.strat))

# 5. Definitions ####
# 5.1. Add Strategy Indicator
# 5.1.1. Add Z-Score Indicator
add.indicator(
  strategy = opt.mean3.strat,
  name = "zscore.fun",
  arguments = list(x = quote(diff(log(
    (mktdata)
  ), lag = 1))),
  label = "zscore"
)

# 5.2. Signals ####

# 5.2.1. Add Z-Score Buying Signal
add.signal(
  strategy = opt.mean3.strat,
  name = "sigThreshold",
  arguments = list(column = "zscore", relationship = "lt"),
  label = "BuySignal"
)
# 5.2.2. Add Z-Score Selling Signal
add.signal(
  strategy = opt.mean3.strat,
  name = "sigThreshold",
  arguments = list(column = "zscore", relationship = "gt"),
  label = "SellSignal"
)

# 5.3. Rules ####

# 5.3.1. Add Enter Rule
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
# Stop-Loss and Trailing-Stop Rules
add.rule(
  strategy = opt.mean3.strat,
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
  strategy = opt.mean3.strat,
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
  strategy = opt.mean3.strat,
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

# 5.4 Parameters ####

# Number of Periods Indicator Calculation
add.distribution(
  strategy = opt.mean3.strat,
  paramset.label = 'OptMeanPar3',
  component.type = 'indicator',
  component.label = 'zscore',
  variable = period_params,
  label = 'n'
)
# Number of Standard Deviations Buy Signal Threshold
add.distribution(
  strategy = opt.mean3.strat,
  paramset.label = 'OptMeanPar3',
  component.type = 'signal',
  component.label = 'BuySignal',
  variable = buythreshold_params,
  label = 'sdBuy'
)
# Number of Standard Deviations Sell Signal Threshold
add.distribution(
  strategy = opt.mean3.strat,
  paramset.label = 'OptMeanPar3',
  component.type = 'signal',
  component.label = 'SellSignal',
  variable = sellthreshold_params,
  label = 'sdSell'
)


# 5.5. Completed Strategy Object
summary(getStrategy(opt.mean3.strat))

# 6. Portfolio Initialization ####
# 6.1. Portfolio Names
opt.mean3.portf <- "OptMeanPort3"
# 6.2. Clear Portfolio Data
rm.strat(opt.mean3.portf)
# 6.3. Initialize Portfolio Object
initPortf(name = opt.mean3.portf,
          symbols = "EURUSD",
          initDate = init.portf)
# 6.2. Initialize Account Object
initAcct(
  name = opt.mean3.strat,
  portfolios = opt.mean3.portf,
  initDate = init.portf,
  initEq = init.equity
)
# 6.3. Initialize Orders Object
initOrders(portfolio = opt.mean3.portf, initDate = init.portf)

# 7. Optimization ####
# 7.1. Strategy Optimization Results
opt.mean3.results <-
  apply.paramset(
    strategy.st = opt.mean3.strat,
    paramset.label = 'OptMeanPar3',
    portfolio.st = opt.mean3.portf,
    account.st = opt.mean3.strat,
    nsamples = 0,
    verbose = TRUE
  )

# 7.2. Strategy Optimization Trading Statistics

# 7.2.1. Strategy Optimization General Trade Statistics
all.mean3.stats <- opt.mean3.results$tradeStats
View(all.mean3.stats)

# 7.2.2. Strategy Optimization Net Trading PL
plot(
  x = all.mean3.stats$Portfolio,
  y = all.mean3.stats$Net.Trading.PL,
  main = "Mean3 Optimization Net Trading PL",
  xlab = "Portfolio",
  ylab = "Net.Trading.PL"
)

# 7.2.3. Strategy Optimization Maximum Drawdown
plot(
  x = all.mean3.stats$Portfolio,
  y = all.mean3.stats$Max.Drawdown,
  main = "Mean3 Optimization Maximum Drawdown",
  xlab = "Portfolio",
  ylab = "Max.Drawdown"
)

# 7.2.4. Strategy Optimization Profit to Maximum Drawdown
plot(
  x = all.mean3.stats$Portfolio,
  y = all.mean3.stats$Profit.To.Max.Draw,
  main = "Mean3 Optimization Profit to Maximum Drawdown",
  xlab = "Portfolio",
  ylab = "Profit.To.Max.Draw"
)

plot(
  x = all.mean3.stats$Portfolio,
  y = all.mean3.stats$Num.Trades,
  main = "Mean3 Optimization Number of Trades",
  xlab = "Portfolio",
  ylab = "Num.Trades"
)

all.mean3.stats$NTdPTMD <-
  (all.mean3.stats$Num.Trades / all.mean3.stats$Profit.To.Max.Draw)
