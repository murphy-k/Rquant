# 4. Autoregressive Integrated Moving Average Models

# 1.1. Load R packages
library("forecast")
library("tseries")

# 1.3. Read .CSV file
x <- read.csv("GDP_CPATAX.csv", header = T)
plot(
  x,
  type = "l",
  main = "Corporate Profits / GDP",
  ylab = "Level",
  xlab = "Day",
  type = "b"
)

# 1.4. Delimit training range
xt <- window(x[, 2], start = 2600, end = 2701)
plot(
  xt,
  type = "l",
  main = "Daily BTCUSD Prices Training Range",
  ylab = "Level",
  xlab = "Day"
)

# 1.5. Delimit forecasting test range
xf <- window(x[, 2], start = 2702)
plot(
  xf,
  type = "l",
  main = "Daily BTCUSD Prices Forecasting Range",
  ylab = "Level",
  xlab = "Day"
)


#########

# 2.1. Arithmetic Mean
mean <- meanf(xt, h = 22)
plot(mean,
     main = "Arithmetic Mean Method",
     ylab = "Level",
     xlab = "Day")
lines(x)

# 2.2. Naive or Random Walk Method
rw1 <- naive(xt, h = 22)
rw2 <- rwf(xt, h = 22)
plot(rw1,
     main = "Naive or Random Walk Method 1",
     ylab = "Level",
     xlab = "Day")
plot(rw2,
     main = "Naive or Random Walk Method 2",
     ylab = "Level",
     xlab = "Day")
lines(x)

# 2.3. Seasonal Random Walk Method
srw <- snaive(xt, h = 22)
plot(srw,
     main = "Seasonal Naive Method",
     ylab = "Level",
     xlab = "Day")
lines(x)

# 2.4. Random Walk with Drift Method
rwd <- rwf(xt, drift = T, h = 22)
plot(rwd,
     main = "Random Walk with Drift Method",
     ylab = "Level",
     xlab = "Day")
lines(x)

# 2.5. Forecasting Accuracy
# Training set = Data from 1 to 252 (1/oct/2014 - 29/sep/2015)
# Test set = Data form 253 to 274 (1/oct/2015 - 30/oct/2015)
accuracy(mean, xf)
accuracy(rw1, xf)
accuracy(srw, xf)
accuracy(rwd, xf)

##########

# 3.1. Simple Moving Average SMA
sma5 <- ma(xt, 5)
sma20 <- ma(xt, 20)
sma2 <- ma(xt, 2)
plot(
  sma5,
  main = "Simple Moving Average SMA",
  ylab = "Level",
  xlab = "Day",
  col = 4
)
lines(sma20, col = 3)
lines(x)
plot(
  forecast(sma5,
           h = 22),
  main = "Simple Moving Average SMA",
  ylab = "Level",
  xlab = "Day",
  col = 4
)
lines(x)
accuracy(sma5, xf)

# 3.2. Brown's Simple Exponential Smoothing ETS(A,N,N)
brown1 <- ses(xt, h = 22)
brown2 <- ets(xt, model = "ANN", damped = F)
# Chart
plot(brown1,
     main = "Brown's Simple Exponential Smoothing ETS(A,N,N)",
     ylab = "Level",
     xlab = "Day")
lines(x)
# Smoothing Parameters
brown2

# 3.3. Holt's Linear Trend Method ETS(A,A,N)
holt1 <- holt(xt, h = 22)
holt2 <- ets(xt, model = "AAN", damped = F)
# Chart
plot(holt1,
     main = "Holt's Linear Trend Method ETS(A,A,N)",
     ylab = "Level",
     xlab = "Day")
lines(x)
# Smoothing Parameters
holt2

# 3.4. Exponential Trend Method ETS(A,M,N)
exp1 <- holt(xt, h = 22, exponential = T)
exp2 <- ets(xt, model = "AMN", damped = F)
# Chart
plot(exp1,
     main = "Exponential Trend Method ETS(A,M,N)",
     ylab = "Level",
     xlab = "Day")
lines(x)
## Smoothing Parameters
exp2
## ets model combination not supported

# 3.5. Gardner's Additive Damped Trend Method ETS(A,Ad,N)
gardner1 <- holt(xt, h = 22, damped = T)
gardner2 <- ets(xt, model = "AAN", damped = T)
# Chart
plot(gardner1,
     main = "Gardner's Additive Damped Trend Method ETS(A,Ad,N)",
     ylab = "Level",
     xlab = "Day")
lines(x)
# Smoothing Parameters
gardner2

# 3.6. Taylor's Multiplicative Damped Trend Method ETS(A,Md,N)
taylor1 <- holt(xt,
                h = 22,
                exponential = T,
                damped = T)
taylor2 <- ets(xt, model = "AMN", damped = T)
# Chart
plot(taylor1,
     main = "Taylor's Multiplicative Damped Trend Method ETS(A,Md,N)",
     ylab = "Level",
     xlab = "Day")
lines(x)
# Smoothing Parameters
taylor2
## ets model combination not supported

# 3.7. Holt-Winters Additive Method ETS(A,A,A)
hwa1 <- hw(xt, h = 22, seasonal = "additive")
hwa2 <- ets(xt, model = "AAA", damped = F)
# Chart
plot(hwa1,
     main = "Holt-Winters Additive Method ETS(A,A,A)",
     ylab = "Level",
     xlab = "Day")
lines(x)
# Smoothing Parameters
hwa2
## Ets error = Non-seasonal data

# 3.8. Holt-Winters Multiplicative Method ETS(A,A,M)
hwm1 <- hw(xt, h = 22, seasonal = "multiplicative")
hwm2 <- ets(xt, model = "AAM", damped = F)
# Chart
plot(hwm1,
     main = "Holt-Winters Multiplicative Method ETS(A,A,M)",
     ylab = "Level",
     xlab = "Day")
lines(x)
# Smoothing Parameters
hwm2
## Ets error = Non-seasonal data

# 3.9. Holt-Winters Damped Method ETS(A,Ad,M)
hwd1 <- hw(xt,
           h = 22,
           seasonal = "multiplicative",
           damped = T)
hwd2 <- ets(xt, model = "AAM", damped = T)
# Chart
plot(hwd1,
     main = "Holt-Winters Damped Method ETS(A,Ad,M)",
     ylab = "Level",
     xlab = "Day")
lines(x)
# Smoothing Parameters
hwd2
## Ets error= Non-seasonal data

# 3.10. Method Selection
# Best fitting training set
# Training set = Data from 1 to 252 (1/oct/2014 - 29/sep/2015)
sbest <- ets(xt)
sbest
# Decomposition of ETS method
plot(ets(xt))

# 3.11. Method Forecasting Accuracy
# Training set = Data from 1 to 252 (1/oct/2014 - 29/sep/2015)
# Test set = Data form 253 to 274 (1/oct/2015 - 30/oct/2015)
accuracy(forecast(sma5, h = 22), xf)
accuracy(forecast(sma20, h = 22), xf)
accuracy(brown1, xf)
accuracy(holt1, xf)
accuracy(exp1, xf)
accuracy(gardner1, xf)
accuracy(taylor1, xf)
accuracy(forecast(sbest, h = 22), xf)

##########

# 4.1. Level Stationarity
# Normal and Partial Autocorrelation Functions ACF & PACF
acf(xt)
pacf(xt)
# Augmented Dickey-Fuller Test ADF
adf.test(xt, alternative = "stationary")
# Kwiatkowski-Phillips-Schmidt-Shin Test KPSS
kpss.test(xt)

# 4.2. First Difference Stationarity
plot(
  x,
  type = "l",
  main = "Daily GLD Stock Prices 10/2014-10/2015",
  ylab = "Level",
  xlab = "Day"
)
plot(
  diff(x[, 2]),
  type = "l",
  main = "Daily GLD Stock Prices Returns 10/2014-10/2015",
  ylab = "Returns",
  xlab = "Day"
)
plot(
  diff(log(x[, 2])),
  type = "l",
  main = "Daily GLD Stock Prices Log Returns 10/2014-10/2015",
  ylab = "Log Returns",
  xlab = "Day"
)
# Normal and Partial Autocorrelation Functions ACF & PACF
acf(diff(xt))
pacf(diff(xt))
# Augmented Dickey-Fuller Test ADF
adf.test(diff(xt), alternative = "stationary")
# Kwiatkowski-Phillips-Schmidt-Shin Test KPSS
kpss.test(diff(xt))

# 4.3. ARIMA Models Specification
# Normal and Partial Autocorrelation Functions ACF & PACF
acf(xt)
pacf(xt)
acf(diff(xt))
pacf(diff(xt))

# 4.4. Random Walk Model ARIMA(0,1,0) without constant
arw1 <- rwf(xt, h = 22)
arw2 <- Arima(xt, order = c(0, 1, 0))
grw1 <- rwf(log(xt), h = 22)
grw2 <- Arima(log(xt), order = c(0, 1, 0))
# Chart
plot(arw1,
     main = "Random Walk Model ARIMA(0,1,0) without constant",
     ylab = "Level",
     xlab = "Day")
lines(x)
# ARIMA Coefficients
summary(arw2)

# 4.5. Random Walk with Drift Model ARIMA(0,1,0) with constant
arwd1 <- Arima(xt, order = c(0, 1, 0), include.mean = T)
arwd2 <- Arima(diff(xt), order = c(0, 0, 0), include.mean = T)
grwd1 <- rwf(log(xt), drift = T, h = 22)
grwd2 <- Arima(log(xt), order = c(0, 1, 0), include.mean = T)
# Chart
plot(forecast(arwd1, h = 22),
     main = "Random Walk Model with Drift ARIMA(0,1,0) with constant",
     ylab = "Level",
     xlab = "Day")
lines(x)
# ARIMA Coefficients
summary(arwd2)

# 4.6. First Order Autoregressive ARIMA(1,0,0) with constant
ar <- Arima(xt, order = c(1, 0, 0), include.mean = T)
# Chart
plot(forecast(ar, h = 22),
     main = "First Order Autoregressive ARIMA(1,0,0) with constant",
     ylab = "Level",
     xlab = "Day")
lines(x)
# Coefficients
summary(ar)

# 4.7. Differentiated First Order Autoregressive ARIMA(1,1,0) with constant
dar1a <- Arima(xt, order = c(1, 1, 0), include.mean = T)
dar1b <- Arima(diff(xt), order = c(1, 0, 0), include.mean = T)
# Charts
plot(forecast(dar1a, h = 22),
     main = "Differentiated First Order Autoregressive ARIMA(1,0,0) with constant",
     ylab = "Level",
     xlab = "Day")
lines(x)
# ARIMA Coefficients
summary(dar1b)

# 4.8. Brown's Simple Exponential Smoothing ARIMA(0,1,1) without constant
abrown <- Arima(xt, order = c(0, 1, 1))
# Chart
plot(
  forecast(abrown, h = 22),
  main = "Brown's Simple Exponential Smoothing ARIMA(0,1,1) without constant",
  ylab = "Level",
  xlab = "Day"
)
lines(x)
# Coefficients
summary(abrown)

# 4.9. Simple Exponential Smoothing  with Growth ARIMA(0,1,1) with constant
abrownga <- Arima(xt, order = c(0, 1, 1), include.mean = T)
abrowngb <- Arima(diff(xt), order = c(0, 0, 1), include.mean = T)
# Charts
plot(
  forecast(abrownga, h = 22),
  main = "Simple Exponential Smoothing  with Growth ARIMA(0,1,1) with constant",
  ylab = "Level",
  xlab = "Day"
)
lines(x)

# ARIMA Coefficients
summary(abrowngb)

# 4.10. Holt's Linear Trend ARIMA(0,2,1) with constant and ARIMA (0,2,2) without constant
aholt1 <- Arima(xt, order = c(0, 2, 1), include.mean = T)
aholt2 <- Arima(diff(diff(xt)),
                order = c(0, 0, 1),
                include.mean = T)
aholt3 <- Arima(xt, order = c(0, 2, 2))
# Charts
plot(
  forecast(aholt1, h = 22),
  main = "Holt's Linear Trend ARIMA(0,2,1) with constant",
  ylab = "Level",
  xlab = "Day"
)
plot(
  forecast(aholt3, h = 22),
  main = "Holt's Linear Trend ARIMA(0,2,2) without constant",
  ylab = "Level",
  xlab = "Day"
)
lines(x)
# ARIMA Coefficients
summary(aholt2)
summary(aholt3)

# 4.11. Gardner's Additive Damped Trend ARIMA(1,1,2) without constant
agardner <- Arima(xt, order = c(1, 1, 2))
# Chart
plot(
  forecast(agardner, h = 22),
  main = "Gardner's Additive Damped Trend ARIMA(1,1,2) without constant",
  ylab = "Level",
  xlab = "Day"
)
lines(x)
# Coefficients
summary(agardner)

# 4.12. Seasonal Random Walk with Drift ARIMA(0,0,0)x(0,1,0)m with constant
srwd1 <- Arima(
  xt,
  order = c(0, 0, 0),
  seasonal = c(0, 1, 0),
  include.mean = T
)
srwd2 <-
  Arima(
    diff(xt, lag = 5),
    order = c(0, 0, 0),
    seasonal = c(0, 0, 0),
    include.mean = T
  )
# Chart
plot(forecast(srwd1, h = 22),
     main = "Seasonal Random Walk with Drift ARIMA(0,0,0)x(0,1,0)m with constant",
     ylab = "Level",
     xlab = "Day")
lines(x)
# ARIMA Coefficients
summary(srwd2)

# 4.13. Seasonal Random Trend ARIMA(0,1,0)x(0,1,0)m without constant
srt <- Arima(xt, order = c(0, 1, 0),
             seasonal = c(0, 1, 0))
# Chart
plot(forecast(srt, h = 22),
     main = "Seasonal Random Trend ARIMA(0,1,0)x(0,1,0)m without constant",
     ylab = "Level",
     xlab = "Day")
lines(x)
# Coefficients
summary(srt)

# 4.14. General Seasonal Model ARIMA(0,1,1)x(0,1,1)m without constant
gseas <- Arima(xt, order = c(0, 1, 1),
               seasonal = c(0, 1, 1))
# Chart
plot(forecast(gseas, h = 22),
     main = "General Seasonal Model ARIMA(0,1,1)x(0,1,1)m without constant",
     ylab = "Level",
     xlab = "Day")
lines(x)
# Coefficients
summary(gseas)

# 4.15. General First Order Autoregressive Seasonal Model ARIMA(1,0,1)x(0,1,1)m with constant
gsar1a <- Arima(
  xt,
  order = c(1, 0, 1),
  seasonal = c(0, 1, 1),
  include.mean = T
)
gsar1b <-
  Arima(
    diff(xt, lag = 5),
    order = c(1, 0, 1),
    seasonal = c(0, 0, 1),
    include.mean = T
  )
# Charts
plot(
  forecast(gsar1a, h = 22),
  main = "General First Order Autoregressive Seasonal Model ARIMA(1,0,1)x(0,1,1)m with constant",
  ylab = "Level",
  xlab = "Day"
)
lines(x)
# ARIMA Coefficients
summary(gsar1b)

# 4.16. Seasonally Differentiated First Order Autoregressive ARIMA(1,0,0)x(0,1,0)m with constant
sdar1a <- Arima(
  xt,
  order = c(1, 0, 0),
  seasonal = c(0, 1, 0),
  include.mean = T
)
sdar1b <-
  Arima(
    diff(xt, lag = 5),
    order = c(1, 0, 0),
    seasonal = c(0, 0, 0),
    include.mean = T
  )
# Charts
plot(
  forecast(sdar1a, h = 22),
  main = "Seasonally Differentiated First Order Autoregressive ARIMA(1,0,0)x(0,1,0)m with constant",
  ylab = "Level",
  xlab = "Day"
)
lines(x)
# ARIMA Coefficients
summary(sdar1b)

# 4.17. Holt-Winters Additive Seasonality ARIMA (0,1,6)X(0,1,0)5 without constant
ahwa <- Arima(xt, order = c(0, 1, 6), seasonal = c(0, 1, 0))
# Chart
plot(forecast(ahwa, h = 22),
     main = "Holt-Winters Additive Seasonality ARIMA (0,1,6)X(0,1,0)5 without constant",
     ylab = "Level",
     xlab = "Day")
lines(x)
# ARIMA Coefficients
summary(ahwa)

# 4.18. Model Selection
# Best fitting training set
# Training set = Data from 1 to 252 (1/oct/2014 - 29/sep/2015)
abest <- auto.arima(xt)
abest

# 4.19. Model Forecasting Accuracy
# Training set = Data from 1 to 252 (1/oct/2014 - 29/sep/2015)
# Test set = Data form 253 to 274 (1/oct/2015 - 30/oct/2015)
accuracy(forecast(arw1, h = 22), xf)
accuracy(forecast(ar, h = 22), xf)
accuracy(forecast(dar1a, h = 22), xf)
accuracy(forecast(abrown, h = 22), xf)
accuracy(forecast(abrownga, h = 22), xf)
accuracy(forecast(aholt1, h = 22), xf)
accuracy(forecast(agardner, h = 22), xf)
accuracy(forecast(srwd1, h = 22), xf)
accuracy(forecast(srt, h = 22), xf)
accuracy(forecast(gseas, h = 22), xf)
accuracy(forecast(gsar1a, h = 22), xf)
accuracy(forecast(sdar1a, h = 22), xf)
accuracy(forecast(ahwa, h = 22), xf)
accuracy(forecast(abest, h = 22), xf)

# 4.20. Residuals White Noise
# Best fitting and forecasting model
abest
# Normal and Partial Autocorrelation Functions ACF & PACF
acf(residuals(abest))
pacf(residuals(abest))
# Ljung.Box Autocorrelation Test
Box.test(residuals(abest), lag = 10, type = "Ljung")

# 4.21. Coefficients Statistical Significance
# [-2.0322,2.0322] not statistical significant with 95% confidence.

##########