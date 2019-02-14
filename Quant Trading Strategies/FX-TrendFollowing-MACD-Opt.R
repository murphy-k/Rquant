# Trading Strategy: Trend-Following Momentum
# Technical Indicators: MACD
# Optimization/Walk Forward Analysis: Yes/No
library(quantmod)
library(lubridate)
library(quantstrat)
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

# MACD Optimization ####
init.portf <- start(EURUSD) - 100000
start.date <- start(EURUSD)
end.date <- Sys.Date()
Sys.setenv(TZ = "UTC")
init.equity <- 100000
enable_stops <- FALSE
fastema_params <- list(nFast = c(2, 4, 6, 8, 10, 12, 14, 16, 18))
slowema_params <- list(nSlow = c(20, 22, 24, 26, 28, 30, 32, 34, 36))
signal_params <- list(nSig = c(6, 9, 12))
position_size <- 10000
txn_fee <- -0.00
initial_stop <- 0.0015
trailing_stop <- 0.0015


# 2.3. Initialize Currency
currency(primary_id = "USD")

# 2.4.Initialize Stock Instrument
stock(primary_id = "EURUSD",
      currency = "USD",
      multiplier = 1)

# 3. Details ####
# Trend-Following Strategy
# Buy Rules = Buy when MACD line > signal line
# Sell Rules = Sell when MACD line < signal line
barChart(EURUSD, theme = "white")
addMACD(fast = 12,
        slow = 26,
        signal = 9)

# 4. Initialization ####
# 4.1. Strategy Name
opt.trend2.strat <- "OptTrendStrat2"

# 4.2. Clear Strategy Data
rm.strat(opt.trend2.strat)

# 4.3. Strategy Object
strategy(name = opt.trend2.strat, store = TRUE)

# 4.4. Completed Strategy Object
summary(getStrategy(opt.trend2.strat))

# 5. Definitions ####
# 5.1. Add Strategy Indicator
add.indicator(
  strategy = opt.trend2.strat,
  name = "MACD",
  arguments = list(x = quote(Cl(mktdata))),
  label = "MACD"
)

# 5.2. Signals ####

# 5.2.1. Add Buying Signal
add.signal(
  strategy = opt.trend2.strat,
  name = "sigCrossover",
  arguments = list(
    columns = c("macd", "signal"),
    relationship = "gt"
  ),
  label = "BuySignal"
)
# 5.2.2. Add Selling Signal
add.signal(
  strategy = opt.trend2.strat,
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
  strategy = opt.trend2.strat,
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
  strategy = opt.trend2.strat,
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
  strategy = opt.trend2.strat,
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
  strategy = opt.trend2.strat,
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
# Fast EMA
add.distribution(
  strategy = opt.trend2.strat,
  paramset.label = 'OptTrendPar2',
  component.type = 'indicator',
  component.label = 'MACD',
  variable = fastema_params,
  label = 'nFastEMA'
)
# Slow EMA
add.distribution(
  strategy = opt.trend2.strat,
  paramset.label = 'OptTrendPar2',
  component.type = 'indicator',
  component.label = 'MACD',
  variable = slowema_params,
  label = 'nSlowEMA'
)
# Signal EMA
add.distribution(
  strategy = opt.trend2.strat,
  paramset.label = 'OptTrendPar2',
  component.type = 'indicator',
  component.label = 'MACD',
  variable = signal_params,
  label = 'nSigEMA'
)

# 5.4. Completed Strategy Object
summary(getStrategy(opt.trend2.strat))

# 6. Portfolio Initialization ####

# 6.1. Portfolio Names
opt.trend2.portf <- "OptTrendPort2"

# 6.2. Clear Portfolio Data
rm.strat(opt.trend2.portf)

# 6.3. Initialize Portfolio Object
initPortf(name = opt.trend2.portf,
          symbols = "EURUSD",
          initDate = init.portf)

# 6.4. Initialize Account Object
initAcct(
  name = opt.trend2.strat,
  portfolios = opt.trend2.portf,
  initDate = init.portf,
  initEq = init.equity
)

# 6.5. Initialize Orders Object
initOrders(portfolio = opt.trend2.portf, initDate = init.portf)

# 7. Optimization ####

# 7.1. Strategy Optimization Results
opt.trend2.results <-
  apply.paramset(
    strategy.st = opt.trend2.strat,
    paramset.label = 'OptTrendPar2',
    portfolio.st = opt.trend2.portf,
    account.st = opt.trend2.strat,
    nsamples = 0,
    verbose = TRUE
  )

# 7.2. Strategy Optimization Trading Statistics

# 7.2.1. Strategy Optimization General Trade Statistics
all.trend2.stats <- opt.trend2.results$tradeStats
View(all.trend2.stats)

# 7.2.2. Strategy Optimization Net Trading PL
plot(
  x = all.trend2.stats$Portfolio,
  y = all.trend2.stats$Net.Trading.PL,
  main = "Trend2 Optimization Net Trading PL",
  xlab = "Portfolio",
  ylab = "Net.Trading.PL"
)

# 7.2.3. Strategy Optimization Maximum Drawdown
plot(
  x = all.trend2.stats$Portfolio,
  y = all.trend2.stats$Max.Drawdown,
  main = "Trend2 Optimization Maximum Drawdown",
  xlab = "Portfolio",
  ylab = "Max.Drawdown"
)

# 7.2.4. Strategy Optimization Profit to Maximum Drawdown
plot(
  x = all.trend2.stats$Portfolio,
  y = all.trend2.stats$Profit.To.Max.Draw,
  main = "Trend2 Optimization Profit to Maximum Drawdown",
  xlab = "Portfolio",
  ylab = "Profit.To.Max.Draw"
)
which.max(all.trend1.stats$Profit.To.Max.Draw)