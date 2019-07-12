library(quantmod)
library(rvest)
library(tidyverse)
library(tidyquant)
library(janitor)

today <- Sys.Date()
# date = today %m+% years(-3)
date = "2019-01-01"
# pass SP500 ticker ^GSPC to tq_get function
SP500 = tq_get("^GSPC", from = date)
SP500 %>%
  head()

# We need to create a vector of all companies in the SP500, iterate over this
# vector and call tq_get() on each element in the vector, returning a dataframe
# for each ticker, then combine all data frames into one dataframe. The current
# SP500 stocks are located here:
# https://en.wikipedia.org/wiki/List_of_S%26P_500_companies

# get the URL for the wikipedia page with all SP500 symbols
url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"

# use that URL to scrape the SP500 table using rvest
tickers <- url %>%
  # read the HTML from the webpage
  read_html() %>%
  # one way to get table
  #html_nodes(xpath='//*[@id="mw-content-text"]/div/table[1]') %>%
  # easier way to get table
  html_nodes(xpath = '//*[@id="constituents"]') %>%
  html_table()
#create a vector of tickers
sp500tickers <- tickers[[1]]
sp500tickers = sp500tickers %>% mutate(Symbol = case_when(
  Symbol == "BRK.B" ~ "BRK-B",
  Symbol == "BF.B" ~ "BF-B",
  TRUE ~ as.character(Symbol)
))
get_symbols = function(ticker = "AAPL") {
  df = tq_get(ticker, from = date) %>%
    mutate(symbol = rep(ticker, length(date)))
}

tickers_df = map(sp500tickers$Symbol, get_symbols) %>%
  bind_rows()

#####

getSymbols(
  sp500_symbols,
  src = "yahoo",
  from = start_date,
  auto.assign = TRUE,
  warnings = FALSE
)
get_symbols = function(ticker = "AAPL") {
  df = tq_get(ticker, from = date) %>%
    mutate(symbol = rep(ticker, length(date)))
}

tickers_df = map(sp500tickers$Symbol, get_symbols) %>%
  bind_rows()

startDate <- "2018-06-01"
endDate <- Sys.Date()
XOP_ts <- window(x = XOP$XOP.Close,
                 start = startDate,
                 end = endDate)
SPY_ts <- window(x = SPY$SPY.Close,
                 start = startDate,
                 end = endDate)
USO_ts <- window(x = USO$USO.Close,
                 start = startDate,
                 end = endDate)
XLE_ts <- window(x = XLE$XLE.Close,
                 start = startDate,
                 end = endDate)
df <- cbind(XOP_ts, SPY_ts, USO_ts, XLE_ts)
chart.RollingCorrelation(
  Ra = df[, c(2:4)],
  Rb = df[, 1],
  width = 20,
  #colorset = tol6qualitative,
  legend.loc = "bottomright",
  main = "Rolling Correlations",
  type = "l"
)
