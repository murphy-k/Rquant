# Rolling Correlations in R
# 1.0 Packages
library(quantmod)
library(dplyr)
library(dygraphs)
library(magrittr)
# 1.1 Data ####
ticker <-
  c("XLY",
    "XLP",
    "XLE",
    "XLF",
    "XLV",
    "XLI",
    "XLB",
    "XLK",
    "XLU",
    "SPY")
sector <-
  c(
    "Consumer Discretionary",
    "Consumer Staples",
    "Energy",
    "Financials",
    "Health Care",
    "Industrials",
    "Materials",
    "Information Technology",
    "Utilities",
    "SP500"
  )

etf_ticker_sector <- data_frame(ticker, sector)
etf_ticker_sector

etf_weekly_returns <- function(ticker) {
  symbols <- getSymbols(ticker, auto.assign = TRUE, warnings = FALSE)
  etf_prices <-
    do.call(merge, lapply(symbols, function(x)
      Cl(get(x))))
  etf_returns <- do.call(merge, lapply(etf_prices,
                                       function(x)
                                         periodReturn(x, period = 'weekly', type = 'log')))
  #Change the column names to the sector names from our dataframe above.
  colnames(etf_returns) <- etf_ticker_sector$sector
  etf_returns
}
etf_returns <-
  etf_weekly_returns(etf_ticker_sector$ticker) %>% round(digits = 4)
head(etf_returns)

sector_index_correlation <- function(x, window) {
  merged_xts <- merge(x, etf_returns$'Index')
  merged_xts$rolling_test <- rollapply(merged_xts, window,
                                       function(x)
                                         cor(x[, 1], x[, 2], use = "pairwise.complete.obs"),
                                       by.column = FALSE)
  names(merged_xts) <-
    c("Sector Returns", "SPY Returns", "Sector/SPY Correlation")
  merged_xts
}
sector_index_correlation(etf_returns$`Consumer Discretionary`)
