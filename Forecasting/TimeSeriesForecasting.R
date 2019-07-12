# Time Series Forecasting
# Pseudo OOS
# Real Gross Domestic Product (FRED: 'GDPC1')

library(forecast)
library(quantmod)
library(tseries)
library(ggplot2)

# 1.0. Data ####
RGDP <- getSymbols("GDPC1", src = "FRED", auto.assign = FALSE)
RGDP_ts <- as.ts(RGDP)

# 1.1. Split into a training range and a test range
h <- 12
RGDP_train <- window(RGDP_ts, start = 1, end = length(RGDP_ts) - h)
RGDP_test <- window(RGDP_ts, start = length(RGDP_ts) - h + 1)

# 1.2. View entire series
RGDP_ts %>% autoplot()

# 1.3. View training range + test range layer
RGDP_train %>% autoplot() + autolayer(RGDP_test)

# 2.0. Forecasting Models ####
# 2.1. Aritmetic Mean
mean <- meanf(RGDP_train, h = h)
autoplot(mean,
         main = "Arithmetic Mean Method",
         ylab = "Level",
         xlab = "Time") + autolayer(RGDP_test)

# 2.2. Naive or Random Walk Method
rw1 <- naive(RGDP_train, h = h)
autoplot(rw1,
         main = "Naive Method",
         ylab = "Level",
         xlab = "Time") + autolayer(RGDP_test)
rw2 <- rwf(RGDP_train, h = h)
autoplot(rw2,
         main = "Random Walk Method",
         ylab = "Level",
         xlab = "Time") + autolayer(RGDP_test)

# 2.3. Seasonal Random Walk Method
srw <- snaive(RGDP_train, h = h)
autoplot(srw,
         main = "Seasonal Random Walk Method",
         ylab = "Level",
         xlab = "Time") + autolayer(RGDP_test)

# 2.4. Random Walk with Drift Method
rwd <- rwf(RGDP_train, drift = T, h = h)
autoplot(rwd,
         main = "Random Walk with Drift Method",
         ylab = "Level",
         xlab = "Time") + autolayer(RGDP_test)
