library("quantstrat")

rm(list = ls())
dev.off(dev.list()["RStudioGD"])

# Settings ####
init.portf <- '2007-12-31'
start.date <- '2008-01-01'
end.date <- Sys.Date()
Sys.setenv(TZ = "UTC")
init.equity <- 100000
position_size <- 100
enable_stops <- TRUE
init_stop <- 0.15
trailing_stop <- 0.15
symbol <- "SPY"

getSymbols(
  Symbols = symbol,
  src = "yahoo",
  from = start.date,
  to = end.date,
  index.class = "POSIXct",
  adjust = TRUE
)


currency(primary_id = "USD")
stock(primary_id = symbol,
      currency = "USD",
      multiplier = 1)


chartSeries(get(symbol))
addRSI(n = 14)

# Strategy ####

RSI.strat <- "Mean-Reversion (RSI)"
rm.strat(RSI.strat)
strategy(name = RSI.strat, store = TRUE)
summary(getStrategy(RSI.strat))

# Indicator ####
add.indicator(
  strategy = RSI.strat,
  name = "RSI",
  arguments = list(
    price = quote(getPrice(mktdata)),
    n = 14,
    maType = 'EMA'
  ),
  label = 'RSI'
)

# Signals ####
# Buy Signal
add.signal(
  strategy = RSI.strat,
  name = "sigThreshold",
  arguments = list(
    threshold = 30,
    column = "RSI",
    relationship = "lt"
  ),
  label = "BuySignal"
)
# Sell Signal
add.signal(
  strategy = RSI.strat,
  name = "sigThreshold",
  arguments = list(
    threshold = 70,
    column = "RSI",
    relationship = "gt"
  ),
  label = "SellSignal"
)

# Rules ####

# Add Entry Rule
add.rule(
  strategy = RSI.strat,
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
  enabled = TRUE
)
# Stop-Loss and Trailing-Stop Rules
add.rule(
  strategy = RSI.strat,
  name = 'ruleSignal',
  arguments = list(
    sigcol = "BuySignal",
    sigval = TRUE,
    orderqty = 'all',
    ordertype = 'stoplimit',
    threshold = init_stop,
    orderside = 'long'
  ),
  type = 'chain',
  label = "StopLoss",
  parent = "EnterRule",
  enabled = enable_stops
)
add.rule(
  strategy = RSI.strat,
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

# Add Exit Rule
add.rule(
  strategy = RSI.strat,
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
  enabled = TRUE
)

# Completed Strategy Object
summary(getStrategy(RSI.strat))

# Portfolio ####
RSI.portf <- "RSI Strategy Long Portfolio"
rm.strat(RSI.portf)
initPortf(name = RSI.portf,
          symbols = symbol,
          initDate = init.portf)

# Account ####
initAcct(
  name = RSI.strat,
  portfolios = RSI.portf,
  initDate = init.portf,
  initEq = init.equity
)
# Orders ####
initOrders(portfolio = RSI.portf, initDate = init.portf)

# Application ####
applyStrategy(strategy = RSI.strat, portfolios = RSI.portf)

# Updating ####
updatePortf(Portfolio = RSI.portf)
updateAcct(name = RSI.strat)
updateEndEq(Account = RSI.strat)

# Reporting ####
# Trade Statistics
RSI.stats <- t(tradeStats(Portfolios = RSI.portf))
View(RSI.stats)

# Per Trade Statistics
RSI.perstats <- perTradeStats(Portfolio = RSI.portf)
View(RSI.perstats)

# Order Book
RSI.book <- getOrderBook(portfolio = RSI.portf)
str(RSI.book)

# Position Chart
chart.theme <- chart_theme()
chart.theme$col$dn.col <- 'white'
chart.theme$col$dn.border <- 'lightgray'
chart.theme$col$up.border <- 'lightgray'
chart.Posn(Portfolio = RSI.portf,
           Symbol = symbol,
           theme = chart.theme)
add_RSI(n = 14, maType = "EMA")

# Equity Curve
RSI.acct <- getAccount(Account = RSI.strat)
RSI.equity <- RSI.acct$summary$End.Eq
plot(RSI.equity, main = "RSI Strategy Equity Curve")

# Strategy Performance 
RSI.ret <- Return.calculate(RSI.equity, method = "discrete")
buyhold.ret <- Return.calculate(get(symbol)[, 4], method = "discrete")
RSI.comp <- cbind(RSI.ret, buyhold.ret)
charts.PerformanceSummary(RSI.comp, main = "RSI Strategy Performance")
table.AnnualizedReturns(RSI.comp)

# Risk Mgmt ####
# Maximum Adverse Excursion
chart.ME(
  Portfolio = RSI.portf,
  Symbol = symbol,
  type = 'MAE',
  scale = 'percent'
)

# Maximum Favorable Excursion 
chart.ME(
  Portfolio = RSI.portf,
  Symbol = symbol,
  type = 'MFE',
  scale = 'percent'
)

# Maximum Portfolio Position (Leverage)
RSI.kelly <- KellyRatio(RSI.ret, method = "half")
RSI.kelly
