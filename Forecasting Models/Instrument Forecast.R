# Packages ####
library(fpp2)
library(quantmod)
library(forecast)

# Clear environment & plots ####
rm(list = ls())
dev.off(dev.list()["RStudioGD"])

# Variables Handling ####
getSymbols("XOP",
           src = "yahoo",
           auto.assign = TRUE,
           from = "2018-01-01")
x <- Ad(XOP)
d_x <- diff(x, na.trim=TRUE)

# Create a ts object
x_ts <-
  ts(XOP, start = head(index(XOP), 1), end = tail(index(XOP), 1))

# Data Visualization ####
chart_Series(x)
gglagplot(x)
ggAcf(x)

autoplot(d_x)
ggAcf(d_x)

autoplot(x_ts[, 6], facets = TRUE)
ggAcf(x_ts[, 6], facets = TRUE)
ggAcf(diff(x_ts[, 6]))

# Ljung-Box Test ####
# White Noise describes purely random data. "Ljung-Box" test can confirm the
# randomness of a series.
# p-value > 0.05 = suggests NO difference from white noise
# p-value < 0.05 = suggests difference from white noise

Box.test(d_x, lag = 10, type = "Ljung")
Box.test(x, lag = 10, type = "Ljung")

# Naive Forecast ####
# A forecast is the mean or median of 'simulated' futures of a time series.
# The simplest forecast to use is the most recent observation; this is called a
# naive forecast and is a useful benchmark for other forecasting methods.

# Using the pipe function
x %>% naive(h = 30) -> naive_fc
autoplot(naive_fc)
checkresiduals(naive_fc)

# Subset for Training/Testing ####
length(x_ts[,6]) - 20
train1_x_ts <- subset(x_ts[, 6], start = 0,  end = (length(x_ts[,6])))
autoplot(train1_x_ts)

# Model Training ####
train_naive_fc <- naive(train1_x_ts, h = 20)
train_mean_fc <- meanf(train1_x_ts, h = 20)
train_ses_fc <- ses(train1_x_ts, h = 20)

autoplot(train_naive_fc) +
  autolayer(x_ts[, 6], color = "black")
autoplot(train_mean_fc) +
  autolayer(x_ts[, 6], color = "black")
autoplot(train_ses_fc) +
  autolayer(x_ts[, 6])

# Model Evaluation ####
accuracy(train_naive_fc, x_ts[, 6])["Test set", c("MAPE", "RMSE")]
accuracy(train_mean_fc, x_ts[, 6])["Test set", c("MAPE", "RMSE")]
accuracy(train_ses_fc, x_ts[, 6])["Test set", c("MAPE", "RMSE")]
