# Packages ####
library(fpp2)
library(quantmod)
library(forecast)
# Clear environment & plots ####

rm(list = ls())
dev.off(dev.list()["RStudioGD"])

# Variables Handling ####
getSymbols("TEVA",
           src = "yahoo",
           auto.assign = TRUE,
           from = "2018-01-01")
TEVA_Close <- TEVA$TEVA.Close
d_TEVA_Close <- diff(TEVA_Close)

# Create a ts object
TEVA_ts <-
  ts(TEVA, start = head(index(TEVA), 1), end = tail(index(TEVA), 1))

# Data Visualization ####
chart_Series(TEVA_Close)
gglagplot(TEVA_Close)
ggAcf(TEVA_Close)

autoplot(d_TEVA_Close)
ggAcf(d_TEVA_Close)

autoplot(TEVA_ts[, 6], facets = TRUE)
ggAcf(TEVA_ts[, 6], facets = TRUE)
ggAcf(diff(TEVA_ts[, 6]))

# Ljung-Box Test ####
# White Noise describes purely random data. "Ljung-Box" test can confirm the
# randomness of a series.
# p-value > 0.05 = suggests NO difference from white noise
# p-value < 0.05 = suggests difference from white noise

options(scipen = 999)
Box.test(d_TEVA_Close, lag = 10, type = "Ljung")
Box.test(TEVA_Close, lag = 10, type = "Ljung")

# Naive Forecast ####
# A forecast is the mean or median of 'simulated' futures of a time series.
# The simplest forecast to use is the most recent observation; this is called a
# naive forecast and is a useful benchmark for other forecasting methods.

# Using the pipe function
TEVA_Close %>% naive(h = 10) -> naive_fc
autoplot(naive_fc)
checkresiduals(naive_fc)

# Subset for Training/Testing ####
length(TEVA_ts[,6]) - 20
train1_TEVA_ts <- subset(TEVA_ts[, 6], start = 0,  end = (length(TEVA_ts[,6])))
autoplot(train1_TEVA_ts)

# Model Training ####
train_naive_fc <- naive(train1_TEVA_ts, h = 20)
train_mean_fc <- meanf(train1_TEVA_ts, h = 20)
train_ses_fc <- ses(train1_TEVA_ts, h = 20)

autoplot(train_naive_fc) +
  autolayer(TEVA_ts[, 6], color = "black")
autoplot(train_mean_fc) +
  autolayer(TEVA_ts[, 6], color = "black")
autoplot(train_ses_fc) +
  autolayer(TEVA_ts[, 6])

# Model Evaluation ####
accuracy(train_naive_fc, TEVA_ts[, 6])["Test set", c("MAPE", "RMSE")]
accuracy(train_mean_fc, TEVA_ts[, 6])["Test set", c("MAPE", "RMSE")]
accuracy(train_ses_fc, TEVA_ts[, 6])["Test set", c("MAPE", "RMSE")]
