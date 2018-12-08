# Packages ####
library(fpp2)
library(quantmod)
library(forecast)
# Clear environment & plots ####

rm(list = ls())
dev.off(dev.list()["RStudioGD"])

# Variables Handling ####
getSymbols("ULTA",
           src = "yahoo",
           auto.assign = TRUE,
           from = "2018-01-01")
ULTA_Close <- Ad(ULTA)
d_ULTA_Close <- diff(ULTA_Close)

# Create a ts object
ULTA_ts <-
  ts(ULTA, start = head(index(ULTA), 1), end = tail(index(ULTA), 1))

# Data Visualization ####
chart_Series(ULTA_Close)
gglagplot(ULTA_Close)
ggAcf(ULTA_Close)

autoplot(d_ULTA_Close)
ggAcf(d_ULTA_Close)

autoplot(ULTA_ts[, 6], facets = TRUE)
ggAcf(ULTA_ts[, 6], facets = TRUE)
ggAcf(diff(ULTA_ts[, 6]))

# Ljung-Box Test ####
# White Noise describes purely random data. "Ljung-Box" test can confirm the
# randomness of a series.
# p-value > 0.05 = suggests NO difference from white noise
# p-value < 0.05 = suggests difference from white noise

options(scipen = 999)
Box.test(d_ULTA_Close, lag = 10, type = "Ljung")
Box.test(ULTA_Close, lag = 10, type = "Ljung")

# Naive Forecast ####
# A forecast is the mean or median of 'simulated' futures of a time series.
# The simplest forecast to use is the most recent observation; this is called a
# naive forecast and is a useful benchmark for other forecasting methods.

# Using the pipe function
ULTA_Close %>% naive(h = 10) -> naive_fc
autoplot(naive_fc)
checkresiduals(naive_fc)

# Subset for Training/Testing ####
length(ULTA_ts[,6]) - 20
train1_ULTA_ts <- subset(ULTA_ts[, 6], start = 0,  end = (length(ULTA_ts[,6])))
autoplot(train1_ULTA_ts)

# Model Training ####
train_naive_fc <- naive(train1_ULTA_ts, h = 20)
train_mean_fc <- meanf(train1_ULTA_ts, h = 20)
train_ses_fc <- ses(train1_ULTA_ts, h = 20)

autoplot(train_naive_fc) +
  autolayer(ULTA_ts[, 6], color = "black")
autoplot(train_mean_fc) +
  autolayer(ULTA_ts[, 6], color = "black")
autoplot(train_ses_fc) +
  autolayer(ULTA_ts[, 6])

# Model Evaluation ####
accuracy(train_naive_fc, ULTA_ts[, 6])["Test set", c("MAPE", "RMSE")]
accuracy(train_mean_fc, ULTA_ts[, 6])["Test set", c("MAPE", "RMSE")]
accuracy(train_ses_fc, ULTA_ts[, 6])["Test set", c("MAPE", "RMSE")]