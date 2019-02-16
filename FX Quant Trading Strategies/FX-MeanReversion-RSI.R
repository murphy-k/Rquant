# Trading Strategy: Mean-Reversion "Relative-Strength Index"
# Technical Indicators: RSI
# Optimization/Walk Forward Analysis: No/No

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

# RSI Backtest ####
init.portf <- start(EURUSD) - 10000
start.date <- start(EURUSD)
Sys.setenv(TZ = "UTC")
init.equity <- 100000
enable_stops <- TRUE
period <- 10
buythreshold <- 30
sellthreshold <- 70
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
# Mean-Reversion Relative-Strength Strategy
# Buy Rules = Buy when RSI < +30 Treshold
# Sell Rules = Sell when RSI > +70 Treshold
barChart(EURUSD)
addRSI(n = period)

# 4. Initialization ####
# 4.1. Strategy Name
mean2.strat <- "MeanStrat2"
# 4.2. Clear Strategy Data
rm.strat(mean2.strat)
# 4.3. Strategy Object
strategy(name = mean2.strat, store = TRUE)
# 4.4. Completed Strategy Object
summary(getStrategy(mean2.strat))

# 5. Definitions ####
# 5.1. Add Strategy Indicator
add.indicator(
  strategy = mean2.strat,
  name = "RSI",
  arguments = list(
    price = quote(getPrice(mktdata)),
    n = period
  ),
  label = 'RSI'
)

# 5.2. Signals ####
# 5.2.1. Add Buying Signal
add.signal(
  strategy = mean2.strat,
  name = "sigThreshold",
  arguments = list(
    threshold = buythreshold,
    column = "RSI",
    relationship = "lt"
  ),
  label = "BuySignal"
)
# 5.2.2. Add Selling Signal
add.signal(
  strategy = mean2.strat,
  name = "sigThreshold",
  arguments = list(
    threshold = sellthreshold,
    column = "RSI",
    relationship = "gt"
  ),
  label = "SellSignal"
)

# 5.3. Rules ####
# 5.3.1. Add Enter Rule
add.rule(
  strategy = mean2.strat,
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
  strategy = mean2.strat,
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
  strategy = mean2.strat,
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
  strategy = mean2.strat,
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
summary(getStrategy(mean2.strat))

# 6. Portfolio Initialization ####

# 6.1. Portfolio Names
mean2.portf <- "MeanPort2"

# 6.2. Clear Portfolio Data
rm.strat(mean2.portf)

# 6.3. Initialize Portfolio Object
initPortf(name = mean2.portf,
          symbols = "EURUSD",
          initDate = init.portf)

# 6.2. Initialize Account Object
initAcct(
  name = mean2.strat,
  portfolios = mean2.portf,
  initDate = init.portf,
  initEq = init.equity
)

# 6.3. Initialize Orders Object
initOrders(portfolio = mean2.portf, initDate = init.portf)

# 7. Application ####

# 7.1. Strategy Application to Market Data
applyStrategy(strategy = mean2.strat, portfolios = mean2.portf)

# 7.2 Strategy Updating
# Specific Order Must be Followed
# 7.2.1. Update Portfolio
updatePortf(Portfolio = mean2.portf)

# 7.2.2. Update Account
updateAcct(name = mean2.strat)

# 7.2.3. Update Equity
updateEndEq(Account = mean2.strat)

# 8. Reporting ####
# 8.1. Strategy Trading Statistics
# 8.1.1. Strategy General Trade Statistics
mean2.stats <- t(tradeStats(Portfolios = mean2.portf))
View(mean2.stats)

# 8.1.2. Strategy Per Trade Statistics
mean2.perstats <- perTradeStats(Portfolio = mean2.portf)
View(mean2.perstats)

# 8.1.3. Strategy Order Book
mean2.book <- getOrderBook(portfolio = mean2.portf)
mean2.book

# 8.1.4. Strategy Position Chart
chart.theme <- chart_theme()
chart.theme$col$dn.col <- 'white'
chart.theme$col$dn.border <- 'lightgray'
chart.theme$col$up.border <- 'lightgray'
chart.Posn(Portfolio = mean2.portf,
           Symbol = "EURUSD",
           theme = chart.theme)
add_RSI(n = period)

# 8.1.5. Strategy Equity Curve
mean2.acct <- getAccount(Account = mean2.strat)
mean2.equity <- mean2.acct$summary$End.Eq
plot(mean2.equity, main = "Mean2 Strategy Equity Curve")

# 8.1.6. Strategy Performance Chart
mean2.ret <- Return.calculate(mean2.equity, method = "log")
bh.ret <-
  Return.calculate(get("EURUSD")[, 4], method = "log")
mean2.comp <- cbind(mean2.ret, bh.ret)
charts.PerformanceSummary(mean2.comp, main = "Mean2 Strategy Performance")
table.AnnualizedReturns(mean2.comp)

# 8.2. Strategy Risk Management

# 8.2.1. Strategy Maximum Adverse Excursion Chart
chart.ME(
  Portfolio = mean2.portf,
  Symbol = "EURUSD"
  ,
  type = 'MAE',
  scale = 'percent'
)

# 8.2.2. Strategy Maximum Favorable Excursion Chart
chart.ME(
  Portfolio = mean2.portf,
  Symbol = "EURUSD",
  type = 'MFE',
  scale = 'percent'
)

# 8.2.3. Strategy Maximum Portfolio Position
mean2.kelly <- KellyRatio(mean2.ret, method = "half")
mean2.kelly
