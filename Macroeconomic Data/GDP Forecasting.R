# Packages ####
library(fpp2)
library(quantmod)
library(forecast)
# Clear environment & plots ####

rm(list = ls())
dev.off(dev.list()["RStudioGD"])

# Variables Handling ####
getSymbols("GDPC1",
           src = "FRED",
           auto.assign = TRUE)
GDP_Growth <-
  getSymbols("A191RL1Q225SBEA", src = "FRED", auto.assign = FALSE)
# Data Visualization ####
chart_Series(GDP_Growth)

chart_Series(GDPC1)
gglagplot(GDPC1)
ggAcf(GDPC1, lag.max = 100)

pctchg_GDP <- ROC(GDPC1, n = 1, type = "continuous")
chart_Series(pctchg_GDP)

GDP_Growth_ts <- as.ts(GDP_Growth)
# Ljung-Box Test ####
# White Noise describes purely random data. "Ljung-Box" test can confirm the
# randomness of a series.
# p-value > 0.05 = suggests NO difference from white noise
# p-value < 0.05 = suggests difference from white noise

options(scipen = 999)
Box.test(GDP_Growth, lag = 10, type = "Ljung")
Box.test(GDPC1, lag = 10, type = "Ljung")

# Naive Forecast ####
# A forecast is the mean or median of 'simulated' futures of a time series.
# The simplest forecast to use is the most recent observation; this is called a
# naive forecast and is a useful benchmark for other forecasting methods.

# Using the pipe function
GDP_Growth_ts %>% naive(h = 10) -> naive_fc
autoplot(naive_fc)
checkresiduals(naive_fc)

# Subset for Training/Testing ####
length(GDP_Growth_ts)
length(GDP_Growth_ts) - (length(GDP_Growth_ts)*0.8)
train1_GDP_Growth_ts <- subset(GDP_Growth_ts, start = 0,  end = 228)
autoplot(train1_GDP_Growth_ts)

# Model Training ####
train_naive_fc <- naive(train1_GDP_Growth_ts, h = 57)
train_mean_fc <- meanf(train1_GDP_Growth_ts, h = 57)
train_ses_fc <- ses(train1_GDP_Growth_ts, h = 57)

autoplot(train_naive_fc) +
  autolayer(GDP_Growth_ts, color = "black")
autoplot(train_mean_fc) +
  autolayer(GDP_Growth_ts, color = "black")
autoplot(train_ses_fc) +
  autolayer(GDP_Growth_ts)

# Model Evaluation ####
accuracy(train_naive_fc, GDP_Growth_ts)["Test set", c("MAPE", "RMSE")]
accuracy(train_mean_fc, GDP_Growth_ts)["Test set", c("MAPE", "RMSE")]
accuracy(train_ses_fc, GDP_Growth_ts)["Test set", c("MAPE", "RMSE")]
