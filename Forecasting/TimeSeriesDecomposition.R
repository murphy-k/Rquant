#### Time Series Decomposition
library(forecast)
library(ggplot2)
library(quantmod)
library(timeSeries)
library(seasonal)
rm(list = ls())

getSymbols("SPY", from = "2018-01-01")
SPY_ts <- ts(SPY[, 4], frequency = 20)
SPY_de <- decompose(SPY_ts, type = "multiplicative")
autoplot(SPY_de, main = "SPY Time Series Decomposition")
# Work on x11 decomposition
seas(x = SPY_ts,xreg = x11="")

# Car Sales Decomposition
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
