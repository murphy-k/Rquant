# Trading Strategy: Trend-Following
# Technical Indicators: Fast and Slow SMA
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
EURUSD <- fxhistoricaldata('EUR_USD', 'hour', download = TRUE)
EURUSD$Adjusted <- EURUSD$Close
str(EURUSD)

# SMA Backtest ####
init.portf <- "2018-10-20"
start.date <- "2018-10-21"
end.date <- Sys.Date()
Sys.setenv(TZ = "UTC")
init.equity <- 100000
enable_stops <- FALSE
fastLength <- 2
slowLength <- 11
position_size <- 10000
txn_fee <- -0.00
initial_stop <- 0.0005
trailing_stop <- 0.0005
fastLength <- 8
slowLength <- 12
position_size <- 10000
txn_fee <- -1.00
initial_stop <- 0.05
trailing_stop <- 0.05

# 2.3. Initialize Currency
currency(primary_id = "USD")

# 2.4.Initialize Stock Instrument
stock(primary_id = "EURUSD",
      currency = "USD",
      multiplier = 1)

# 3. Details ####

# Trend-Following Strategy
# Buy Rule = Buy when Fast SMA > Slow SMA,
# Sell Rule = Sell when Fast SMA < Slow SMA
chartSeries(EURUSD, theme ="white", subset = "2019-01-01::")
addSMA(n = fastLength, col = "black")
addSMA(n = slowLength, col = "blue")

# 4. Initialization ####

# 4.1. Strategy Name
trend1.strat <- "TrendStrat1"

# 4.2. Clear Strategy Data
rm.strat(trend1.strat)

# 4.3. Strategy Object
strategy(name = trend1.strat, store = TRUE)

# 4.4. Completed Strategy Object
summary(getStrategy(trend1.strat))

# 5. Definitions ####

# 5.1. Add Strategy Indicator

# 5.1.1. Add Fast SMA
add.indicator(
  strategy = trend1.strat,
  name = "SMA",
  arguments = list(x = quote(Ad(mktdata)), n = fastLength),
  label = "FastSMA"
)
# 5.1.2. Add Slow SMA
add.indicator(
  strategy = trend1.strat,
  name = "SMA",
  arguments = list(x = quote(Ad(mktdata)), n = slowLength),
  label = "SlowSMA"
)

# 5.2. Signals ####

# 5.2.1. Add Buying Signal
add.signal(
  strategy = trend1.strat,
  name = "sigCrossover",
  arguments = list(
    columns = c("FastSMA", "SlowSMA"),
    relationship = "gt"
  ),
  label = "BuySignal"
)
# 5.2.2. Add Selling Signal
add.signal(
  strategy = trend1.strat,
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
  strategy = trend1.strat,
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
  strategy = trend1.strat,
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
  strategy = trend1.strat,
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
  strategy = trend1.strat,
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
summary(getStrategy(trend1.strat))

# 6. Portfolio Initialization ####

# 6.1. Portfolio Names
trend1.portf <- "TrendPort1"

# 6.2. Clear Portfolio Data
rm.strat(trend1.portf)

# 6.3. Initialize Portfolio Object
initPortf(name = trend1.portf,
          symbols = "EURUSD",
          initDate = init.portf)

# 6.4. Initialize Account Object
initAcct(
  name = trend1.strat,
  portfolios = trend1.portf,
  initDate = init.portf,
  initEq = init.equity
)

# 6.5. Initialize Orders Object
initOrders(portfolio = trend1.portf, initDate = init.portf)

# 7. Application ####

# 7.1. Strategy Application to Market Data
applyStrategy(strategy = trend1.strat, portfolios = trend1.portf)

# 7.2 Strategy Updating
# Specific Order Must be Followed

# 7.2.1. Update Portfolio
updatePortf(Portfolio = trend1.portf)

# 7.2.2. Update Account
updateAcct(name = trend1.strat)

# 7.2.3. Update Equity
updateEndEq(Account = trend1.strat)

# 8. Reporting ####
round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  
  df[,nums] <- round(df[,nums], digits = digits)
  
  (df)
}
# 8.1. Strategy Trading Statistics

# 8.1.1. Strategy General Trade Statistics
trend1.stats <- round_df((tradeStats(Portfolios = trend1.portf)))
View(t(trend1.stats))
trend1.stats <- round_df(t(tradeStats(Portfolios = trend1.portf)))
View(trend1.stats)

# 8.1.2. Strategy Per Trade Statistics
trend1.perstats <- round_df(perTradeStats(Portfolio = trend1.portf))
View(trend1.perstats)

# 8.1.3. Strategy Order Book
trend1.book <- getOrderBook(portfolio = trend1.portf)
trend1.book

# 8.1.4. Strategy Position Chart
chart.theme <- chart_theme()
chart.theme$col$dn.col <- 'white'
chart.theme$col$dn.border <- 'lightgray'
chart.theme$col$up.border <- 'lightgray'
chart.Posn(Portfolio = trend1.portf,
           Symbol = "EURUSD",
           theme = chart.theme,Dates = "2019-01-10::")
add_SMA(n = fastLength)
add_SMA(n = slowLength, col = "darkblue")

# 8.1.5. Strategy Equity Curve
trend1.acct <- getAccount(Account = trend1.strat)
trend1.equity <- trend1.acct$summary$End.Eq
plot(trend1.equity, main = "Trend1 Strategy Equity Curve")

# 8.1.6. Strategy Performance Chart
trend1.ret <- Return.calculate(trend1.equity, method = "log")
bh.ret <- Return.calculate(get("EURUSD")[, 4], method = "log")
trend1.comp <- cbind(trend1.ret, bh.ret)
charts.PerformanceSummary(trend1.comp, main = "Trend1 Strategy Performance")
table.AnnualizedReturns(trend1.comp)

# 8.2. Strategy Risk Management

# 8.2.1. Strategy Maximum Adverse Excursion Chart
chart.ME(
  Portfolio = trend1.portf,
  Symbol = "EURUSD",
  type = 'MAE',
  scale = 'percent'
)

# 8.2.2. Strategy Maximum Favorable Excursion Chart
chart.ME(
  Portfolio = trend1.portf,
  Symbol = "EURUSD",
  type = 'MFE',
  scale = 'percent'
)

# 8.2.3. Strategy Maximum Portfolio Position
trend1.kelly <- KellyRatio(trend1.ret, method = "half")
trend1.kelly
