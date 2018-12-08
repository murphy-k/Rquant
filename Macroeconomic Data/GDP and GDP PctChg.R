# Packages ####
library(fpp2)
library(quantmod)
library(forecast)
rm(list = ls())
dev.off(dev.list()["RStudioGD"])

# Variables Handling ####
getSymbols("GDPC1",
           src = "FRED",
           auto.assign = TRUE)
# Data Visualization ####
chart_Series(GDPC1)
gglagplot(GDPC1)
ggAcf(GDPC1, lag.max = 100)

pctchg_GDP <- ROC(GDPC1, n = 1, type = "continuous")
pctchg_GDP_ts <- as.ts(pctchg_GDP)
chart_Series(pctchg_GDP)

# Ljung-Box Test ####
# White Noise describes purely random data. "Ljung-Box" test can confirm the
# randomness of a series.
# p-value > 0.05 = suggests NO difference from white noise
# p-value < 0.05 = suggests difference from white noise
options(scipen = 999)
Box.test(pctchg_GDP, lag = 10, type = "Ljung")
Box.test(GDPC1, lag = 10, type = "Ljung")

# Naive Forecast ####
# A forecast is the mean or median of 'simulated' futures of a time series.
# The simplest forecast to use is the most recent observation; this is called a
# naive forecast and is a useful benchmark for other forecasting methods.

# Using the pipe function
pctchg_GDP_ts %>% na.trim() %>% naive(h = 10) -> naive_fc
autoplot(naive_fc)
checkresiduals(naive_fc)

# Subset for Training/Testing ####
length(pctchg_GDP_ts)
train1_pctchg_GDP_ts <-
  subset(pctchg_GDP_ts, end = length(pctchg_GDP_ts) * 0.8)
autoplot(train1_pctchg_GDP_ts)
h = length(pctchg_GDP_ts) - length(train1_pctchg_GDP_ts)
# Model Training ####
train_naive_fc <- naive(train1_pctchg_GDP_ts, h = h) 
train_mean_fc <- meanf(train1_pctchg_GDP_ts, h = h)
train_ses_fc <- ses(train1_pctchg_GDP_ts, h = h)

autoplot(train_naive_fc) +
  autolayer(pctchg_GDP_ts, color = "black")
autoplot(train_mean_fc) +
  autolayer(pctchg_GDP_ts, color = "black")
autoplot(train_ses_fc) +
  autolayer(pctchg_GDP_ts)

# Model Evaluation ####
accuracy(train_naive_fc, pctchg_GDP_ts)["Test set", c("MAPE", "RMSE")]
accuracy(train_mean_fc, pctchg_GDP_ts)["Test set", c("MAPE", "RMSE")]
accuracy(train_ses_fc, pctchg_GDP_ts)["Test set", c("MAPE", "RMSE")]
