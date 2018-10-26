# Packages ####
library(fpp2)
library(quantmod)
library(forecast)
# Clear environment & plots ####

rm(list = ls())
dev.off(dev.list()["RStudioGD"])

# Variables Handling ####
getSymbols("LGORF",
           src = "yahoo",
           auto.assign = TRUE,
           from = "2018-01-01")
LGORF_Close <- LGORF$LGORF.Close
d_LGORF_Close <- diff(LGORF_Close)

# Create a ts object
LGORF_ts <-
  ts(LGORF, start = head(index(LGORF), 1), end = tail(index(LGORF), 1))

# Data Visualization ####
chart_Series(LGORF_Close)
gglagplot(LGORF_Close)
ggAcf(LGORF_Close)

autoplot(d_LGORF_Close)
ggAcf(d_LGORF_Close)

autoplot(LGORF_ts[, 6], facets = TRUE)
ggAcf(LGORF_ts[, 6], facets = TRUE)
ggAcf(diff(LGORF_ts[, 6]))

# Ljung-Box Test ####
# White Noise describes purely random data. "Ljung-Box" test can confirm the
# randomness of a series.
# p-value > 0.05 = suggests NO difference from white noise
# p-value < 0.05 = suggests difference from white noise

options(scipen = 999)
Box.test(d_LGORF_Close, lag = 10, type = "Ljung")
Box.test(LGORF_Close, lag = 10, type = "Ljung")

# Naive Forecast ####
# A forecast is the mean or median of 'simulated' futures of a time series.
# The simplest forecast to use is the most recent observation; this is called a
# naive forecast and is a useful benchmark for other forecasting methods.

# Using the pipe function
LGORF_Close %>% naive(h = 10) -> naive_fc
autoplot(naive_fc)
checkresiduals(naive_fc)

# Subset for Training/Testing ####
length(LGORF_ts[,6]) - 20
train1_LGORF_ts <- subset(LGORF_ts[, 6], start = 0,  end = (length(LGORF_ts[,6])))
autoplot(train1_LGORF_ts)

# Model Training ####
train_naive_fc <- naive(train1_LGORF_ts, h = 20)
train_mean_fc <- meanf(train1_LGORF_ts, h = 20)
train_ses_fc <- ses(train1_LGORF_ts, h = 20)

autoplot(train_naive_fc) +
  autolayer(LGORF_ts[, 6], color = "black")
autoplot(train_mean_fc) +
  autolayer(LGORF_ts[, 6], color = "black")
autoplot(train_ses_fc) +
  autolayer(LGORF_ts[, 6])

# Model Evaluation ####
accuracy(train_naive_fc, LGORF_ts[, 6])["Test set", c("MAPE", "RMSE")]
accuracy(train_mean_fc, LGORF_ts[, 6])["Test set", c("MAPE", "RMSE")]
accuracy(train_ses_fc, LGORF_ts[, 6])["Test set", c("MAPE", "RMSE")]
