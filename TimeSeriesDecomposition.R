#### Time Series Decomposition
library(forecast)
library(ggplot2)
library(quantmod)

start_date = "2018-01-01"
symbol <- "MSFT"

x <- getSymbols(symbol, from = start_date, auto.assign = FALSE)

x_ts <- ts(x[,4], frequency = 52, start = 2018-01-01)
x_de <- decompose(x_ts)
autoplot(x_de)

