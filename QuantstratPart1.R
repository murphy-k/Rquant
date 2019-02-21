# Going to be running through the "Nuts and Bolts of Quantstrat - using
# Ilya Kipnis' IKTrading package. First thing I want to do is download and
# install the required packages.
# require(devtools)
# install_github("IlyaKipnis/IKTrading")
rm(list = ls())

# Packages ####
library(IKTrading)
library(quantstrat)
library(PerformanceAnalytics)
library(TTR)

# Settings ####
init_date <- "2017-12-31"
from_date <- "2018-01-01"
to_date <- as.character(Sys.Date())
currency <- ('USD')
Sys.setenv(TZ = "UTC")
options(scipen = 999)

# Data ####
symbols <- c(
  "XLB",
  #SPDR Materials sector
  "XLE",
  #SPDR Energy sector
  "XLF",
  #SPDR Financial sector
  "XLP",
  #SPDR Consumer staples sector
  "XLI",
  #SPDR Industrial sector
  "XLU",
  #SPDR Utilities sector
  "XLV",
  #SPDR Healthcare sector
  "XLK",
  #SPDR Tech sector
  "XLY",
  #SPDR Consumer discretionary sector
  "RWR",
  #SPDR Dow Jones REIT ETF
  "EWJ",
  #iShares Japan
  "EWG",
  #iShares Germany
  "EWU",
  #iShares UK
  "EWC",
  #iShares Canada
  "EWY",
  #iShares South Korea
  "EWA",
  #iShares Australia
  "EWH",
  #iShares Hong Kong
  "EWS",
  #iShares Singapore
  "IYZ",
  #iShares U.S. Telecom
  "EZU",
  #iShares MSCI EMU ETF
  "IYR",
  #iShares U.S. Real Estate
  "EWT",
  #iShares Taiwan
  "EWZ",
  #iShares Brazil
  "EFA",
  #iShares EAFE
  "IGE",
  #iShares North American Natural Resources
  "EPP",
  #iShares Pacific Ex Japan
  "LQD",
  #iShares Investment Grade Corporate Bonds
  "SHY",
  #iShares 1-3 year TBonds
  "IEF",
  #iShares 3-7 year TBonds
  "TLT"
) #iShares 20+ year Bonds

if (!"XLB" %in% ls()) {
  suppressMessages(getSymbols(
    symbols,
    from = from_date,
    to = to_date,
    src = "yahoo",
    adjust = TRUE,
  ))
}
stock(symbols, currency = 'USD', multiplier = 1)

# Equity and Sizing ####
trade_size <- 1000
init_eq <- trade_size * length(symbols)
strategy.st <- portfolio.st <- account.st <- "DollarVsATRos"

rm.strat(strategy.st)

initPortf(
  portfolio.st,
  symbols = symbols,
  initDate = init_date,
  currency = 'USD'
)
initAcct(
  account.st,
  portfolios = portfolio.st,
  initDate = init_date,
  currency = 'USD',
  initEq = init_eq
)

initOrders(portfolio.st, initDate = initDate)
strategy(strategy.st, store = TRUE)

# Parameters ####
pct_atr <- 0.02
period <- 10
atr_order <- TRUE

nRSI <- 2
buy_threshold <- 20
sell_threshold <- 80
nSMA <- 200

# Indicators ####
add.indicator(
  strategy.st,
  name = "lagATR",
  arguments = list(HLC = quote(HLC(mktdata)), n = period),
  label = "atrX"
)

add.indicator(
  strategy.st,
  name = "RSI",
  arguments = list(price = quote(Cl(mktdata)), n = nRSI),
  label = "rsi"
)
add.indicator(
  strategy.st,
  name = "SMA",
  arguments = list(x = quote(Cl(mktdata)), n = nSMA),
  label = "sma"
)

