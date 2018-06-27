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
fast_sma <- 5
slow_sma <- 50
init_stop <- 0.15
trailing_stop <- 0.15

# Symbol Download
getSymbols(
  Symbols = "NOV",
  src = "yahoo",
  from = start.date,
  to = end.date,
  index.class = "POSIXct",
  adjust = T,
  auto.assign = TRUE
)
# Currency Setup
currency(primary_id = "USD")

# Stock Instrument Setup
stock(primary_id = "NOV",
      currency = "USD",
      multiplier = 1)


# Visualization
chartSeries(NOV, type = "candlesticks")
addSMA(n = fast_sma, col = "red")
addSMA(n = slow_sma, col = "blue")

# Trading Strategy
SMAx2.strat <- "SMAx2_Strategy"
rm.strat(SMAx2.strat) # clear strategy data
strategy(name = SMAx2.strat, store = TRUE) # strategy object
summary(getStrategy(SMAx2.strat)) # completed strategy object

# Define Strategy
# Add Fast SMA
add.indicator(
  strategy = SMAx2.strat,
  name = "SMA",
  arguments = list(x = quote(Cl(mktdata)), n = fast_sma),
  label = "FastSMA"
)
# Add Slow SMA
add.indicator(
  strategy = SMAx2.strat,
  name = "SMA",
  arguments = list(x = quote(Cl(mktdata)), n = slow_sma),
  label = "SlowSMA"
)

# Buying Signal
add.signal(
  strategy = SMAx2.strat,
  name = "sigCrossover",
  arguments = list(
    columns = c("FastSMA", "SlowSMA"),
    relationship = "gt"
  ),
  label = "BuySignal"
)
# Selling Signal
add.signal(
  strategy = SMAx2.strat,
  name = "sigCrossover",
  arguments = list(
    columns = c("FastSMA", "SlowSMA"),
    relationship = "lt"
  ),
  label = "SellSignal"
)

# Entry Rule
add.rule(
  strategy = SMAx2.strat,
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
  strategy = SMAx2.strat,
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
  enabled = TRUE
)
add.rule(
  strategy = SMAx2.strat,
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
  enabled = TRUE
)

# Add Exit Rule
add.rule(
  strategy = SMAx2.strat,
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

# Completed Strategy Object
summary(getStrategy(SMAx2.strat))

# Portfolio
StrategyPortfolio.portf <- "Strategy Portfolio"
rm.strat(StrategyPortfolio.portf)
initPortf(name = StrategyPortfolio.portf,
          symbols = "NOV",
          initDate = init.portf)
initAcct(
  name = SMAx2.strat,
  portfolios = StrategyPortfolio.portf,
  initDate = init.portf,
  initEq = init.equity
)


initOrders(portfolio = StrategyPortfolio.portf, initDate = init.portf)

applyStrategy(strategy = SMAx2.strat, portfolios = StrategyPortfolio.portf)
updatePortf(Portfolio = StrategyPortfolio.portf)
updateAcct(name = SMAx2.strat)
updateEndEq(Account = SMAx2.strat)

# Strategy Reporting
SMAx2Strategy.stats <-
  t(tradeStats(Portfolios = StrategyPortfolio.portf))
View(SMAx2Strategy.stats)

# Per Trade Stats
SMAx2Strategy.perstats <-
  perTradeStats(Portfolio = StrategyPortfolio.portf)
View(SMAx2Strategy.perstats)

#  Strategy Order Book
SMAx2Strategy.book <-
  getOrderBook(portfolio = StrategyPortfolio.portf)
SMAx2Strategy.book

# Strategy Position Chart
chart.theme <- chart_theme()
chart.theme$col$dn.col <- 'white'
chart.theme$col$dn.border <- 'lightgray'
chart.theme$col$up.border <- 'lightgray'
chart.Posn(Portfolio = StrategyPortfolio.portf,
           Symbol = "NOV",
           theme = chart.theme)
add_SMA(n = fast_sma)
add_SMA(n = slow_sma, col = "darkblue")

# Equity Curve
SMAx2.acct <- getAccount(Account = SMAx2.strat)
SMAx2.equity <- SMAx2.acct$summary$End.Eq
plot(SMAx2.equity, main = "Fast SMA vs. Slow SMA Equity Curve")

# Strategy Performance Chart
SMAx2Strat.ret <-
  Return.calculate(SMAx2.equity, method = "discrete")
buy_hold.ret <- Return.calculate(NOV[, 4], method = "discrete")
Strat.comp <- cbind(SMAx2Strat.ret, buy_hold.ret)
charts.PerformanceSummary(Strat.comp,
                          main = "Fast SMA vs. Slow SMA Performance")
table.AnnualizedReturns(Strat.comp)

# 8.2. Strategy Risk Management

# 8.2.1. Strategy Maximum Adverse Excursion Chart
chart.ME(
  Portfolio = StrategyPortfolio.portf,
  Symbol = 'NOV',
  type = 'MAE',
  scale = 'percent'
)

# 8.2.2. Strategy Maximum Favorable Excursion Chart
chart.ME(
  Portfolio = StrategyPortfolio.portf,
  Symbol = 'NOV',
  type = 'MFE',
  scale = 'percent'
)

# 8.2.3. Strategy Maximum Portfolio Position
SMAx2.kelly <- KellyRatio(SMAx2Strat.ret, method = "half")
SMAx2.kelly