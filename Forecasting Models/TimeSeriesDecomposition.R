#### Time Series Decomposition
library(forecast)
library(ggplot2)
library(quantmod)
library(timeSeries)
rm(list = ls())

getSymbols("MSFT", from = "2019-01-01")
MSFT_ts <- ts(MSFT[, 4], frequency = 20)
MSFT_de <- decompose(MSFT_ts, type = "multiplicative")
autoplot(MSFT_de)

getSymbols("TOTALSA", src = "FRED")

sales_ts <- ts(TOTALSA, frequency = 12, start = 1976 - 01 - 01)
sales_de <- decompose(sales_ts,type = "multiplicative")
autoplot(sales_de,
         main = "Total Vehicle Sales Time-Series Decomposition")

USED <- getSymbols("MRTSSM44112USN", src="FRED", auto.assign = FALSE)
USED_ts <- ts(USED, frequency = 12, start = 1992-01-01)
USED_de <- decompose(USED_ts, type = "multiplicative")
autoplot(USED_de, 
         main="Retail Sales: Used Car Dealers")

snaive(USED_ts) %>%
  autoplot(main = "Retail Sales: Used Car Dealers - Seasonal Naive Forecast")

arima_fit <- auto.arima(USED_ts)
autoplot(forecast(arima_fit))

autoplot(USED_ts) +
  autolayer(naive(USED_ts, h=24),
            series="Naïve", PI=FALSE) +
  autolayer(snaive(USED_ts, h=24),
            series="Seasonal naïve", PI=FALSE) +
  ggtitle("Forecasts for monthly used car sales") +
  xlab("Year") + ylab("Sales") +
  guides(colour=guide_legend(title="Forecast"))
