# Forecasting Models
rm(list = ls())
dev.off(dev.list()["RStudioGD"])

# 1.0 Packages ####
library(forecast)
library(tseries)
library(quantmod)

# 1.1 Data ####
getSymbols(c("CPATAX", "GDP"), src = "FRED", auto.assign = TRUE)
plot(CPATAX,
     type = "l",
     main = "Corporate Profits")
plot(GDP, type = "l", main = "GDP")

# 1.2 Build 'x' (CPATAX /GDP) ####
x <- as.ts(CPATAX / GDP)
plot(x, type = "l", main = "Corporate Profits as a % of GDP")

# 1.3 Delimit training range ####
xt <- subset(x, end = length(x) * 0.80)
plot(xt,
     type = "l",
     main = "Training Range")

# 1.4 Delimit forecasting test range ####
xf <- subset(x, start = length(xt))
plot(xf,
     type = "l",
     main = "Forecasting Range")

# 2.0 Training Models ####
# 2.0.1 Define forecast length (difference between training window and full
#       series)
h = (length(x) - length(xt))

# 2.1. Arithmetic Mean
mean <- meanf(xt, h = h)
plot(mean,
     main = "Arithmetic Mean Method")
lines(x)

# 2.2. Naive or Random Walk Method
rw1 <- naive(xt, h = h)
rw2 <- rwf(xt, h = h)
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
srw <- snaive(xt, h = h)
plot(srw,
     main = "Seasonal Naive Method",
     ylab = "Level",
     xlab = "Day")
lines(x)

# 2.4. Random Walk with Drift Method
rwd <- rwf(xt, drift = T, h = h)
plot(rwd,
     main = "Random Walk with Drift Method",
     ylab = "Level",
     xlab = "Day")
lines(x)

# 2.5. Forecasting Accuracy
accuracy(mean, xf)
accuracy(rw1, xf)
accuracy(srw, xf)
accuracy(rwd, xf)


# 3.0 Non-Simple Forecasting Methods ####
# 3.1 Brown's Simple Exponential Smoothing ETS(A,N,N)
brown1 <- ses(xt, h = h)
plot(brown1,
     main = "Brown's Simple Exponential Smoothing ETS(A,N,N)",
     ylab = "Level",
     xlab = "Day")
lines(x)
# Smoothing Parameters
brown1$model

# 3.2 Holt's Linear Trend Method ETS(A,A,N)
holt1 <- holt(xt, h = h)
plot(holt1,
     main = "Holt's Linear Trend Method ETS(A,A,N)",
     ylab = "Level",
     xlab = "Day")
lines(x)
# Smoothing Parameters
holt1$model

# 3.3 Exponential Trend Method ETS(A,M,N)
exp1 <- holt(xt, h = h, exponential = T)
plot(exp1,
     main = "Exponential Trend Method ETS(A,M,N)",
     ylab = "Level",
     xlab = "Day")
lines(x)
## Smoothing Parameters
exp1$model

# 3.4 Gardner's Additive Damped Trend Method ETS(A,Ad,N)
gardner1 <- holt(xt, h = h, damped = T)
plot(gardner1,
     main = "Gardner's Additive Damped Trend Method ETS(A,Ad,N)",
     ylab = "Level",
     xlab = "Day")
lines(x)
# Smoothing Parameters
gardner1$model

# 3.5 Taylor's Multiplicative Damped Trend Method ETS(A,Md,N)
taylor1 <- holt(xt,
                h = h,
                exponential = T,
                damped = T)
plot(taylor1,
     main = "Taylor's Multiplicative Damped Trend Method ETS(A,Md,N)",
     ylab = "Level",
     xlab = "Day")
lines(x)
# Smoothing Parameters
taylor1$model

# 3.6 Holt-Winters Additive Method ETS(A,A,A)
hwa1 <- hw(xt, h = h, seasonal = "additive")
hwa2 <- ets(xt, model = "AAA", damped = F)
plot(hwa1,
     main = "Holt-Winters Additive Method ETS(A,A,A)",
     ylab = "Level",
     xlab = "Day")
lines(x)
# Smoothing Parameters
hwa2

# 3.7 Holt-Winters Multiplicative Method ETS(A,A,M)
hwm1 <- hw(xt, h = h, seasonal = "multiplicative")
hwm2 <- ets(xt, model = "AAM", damped = F)
plot(hwm1,
     main = "Holt-Winters Multiplicative Method ETS(A,A,M)",
     ylab = "Level",
     xlab = "Day")
lines(x)
# Smoothing Parameters
hwm2


# 3.8 Holt-Winters Damped Method ETS(A,Ad,M)
hwd1 <- hw(xt,
           h = h,
           seasonal = "multiplicative",
           damped = T)
hwd2 <- ets(xt, model = "AAM", damped = T)
plot(hwd1,
     main = "Holt-Winters Damped Method ETS(A,Ad,M)",
     ylab = "Level",
     xlab = "Day")
lines(x)
# Smoothing Parameters
hwd2


# 3.9 Method Selection
# Best fitting training set
sbest <- ets(xt)
sbest
# Decomposition of ETS method
plot(ets(xt))

# 3.10 Method Forecasting Accuracy
accuracy(brown1, xf)
accuracy(holt1, xf)
accuracy(exp1, xf)
accuracy(gardner1, xf)
accuracy(taylor1, xf)
accuracy(forecast(sbest, h = h), xf)


# 4.0 Level Stationarity ####
# Normal and Partial Autocorrelation Functions ACF & PACF
acf(xt)
pacf(xt)
# Augmented Dickey-Fuller Test ADF
adf.test(xt, alternative = "stationary")
# Kwiatkowski-Phillips-Schmidt-Shin Test KPSS
kpss.test(xt)

# 4.1. First Difference Stationarity
plot(
  x,
  type = "l",
  main = "Non-Difference (raw)",
  ylab = "Level",
  xlab = "Day"
)

plot(
  diff(x),
  type = "l",
  main = "First Difference Stationarity",
  ylab = "Returns",
  xlab = "Day"
)
abline(h = 0, col = "red")
# 4.2 Log-Difference Stationarity
plot(
  diff(log(x)),
  type = "l",
  main = "Log-Difference Stationarity",
  ylab = "Log Returns",
  xlab = "Day"
)
abline(h = 0, col = "red")

# Normal and Partial Autocorrelation Functions ACF & PACF
acf(diff(xt))
pacf(diff(xt))
# Augmented Dickey-Fuller Test ADF
adf.test(diff(xt), alternative = "stationary")
# Kwiatkowski-Phillips-Schmidt-Shin Test KPSS
kpss.test(diff(xt))

# 4.3. ARIMA Models Specification ####
# Normal and Partial Autocorrelation Functions ACF & PACF
acf(xt)
pacf(xt)
acf(diff(xt))
pacf(diff(xt))

# 4.4. Random Walk Model ARIMA(0,1,0) without constant
arw1 <- rwf(xt, h = h)
arw2 <- Arima(xt, order = c(0, 1, 0))
grw1 <- rwf(log(xt), h = h)
grw2 <- Arima(log(xt), order = c(0, 1, 0))
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
grwd1 <- rwf(log(xt), drift = T, h = h)
grwd2 <- Arima(log(xt), order = c(0, 1, 0), include.mean = T)
plot(forecast(arwd1, h = h),
     main = "Random Walk Model with Drift ARIMA(0,1,0) with constant",
     ylab = "Level",
     xlab = "Day")
lines(x)
# ARIMA Coefficients
summary(arwd2)

# 4.6. First Order Autoregressive ARIMA(1,0,0) with constant
ar <- Arima(xt, order = c(1, 0, 0), include.mean = T)
plot(forecast(ar, h = h),
     main = "First Order Autoregressive ARIMA(1,0,0) with constant",
     ylab = "Level",
     xlab = "Day")
lines(x)
# Coefficients
summary(ar)

# 4.7. Differentiated First Order Autoregressive ARIMA(1,1,0) with constant
dar1a <- Arima(xt, order = c(1, 1, 0), include.mean = T)
dar1b <- Arima(diff(xt), order = c(1, 0, 0), include.mean = T)
plot(forecast(dar1a, h = h),
     main = "Differentiated First Order Autoregressive ARIMA(1,0,0) with constant",
     ylab = "Level",
     xlab = "Day")
lines(x)
# ARIMA Coefficients
summary(dar1b)

# 4.8. Brown's Simple Exponential Smoothing ARIMA(0,1,1) without constant
abrown <- Arima(xt, order = c(0, 1, 1))
plot(forecast(abrown, h = h),
     main = "Brown's Simple Exponential Smoothing ARIMA(0,1,1) without constant",
     ylab = "Level",
     xlab = "Day")
lines(x)
# Coefficients
summary(abrown)

# 4.9. Simple Exponential Smoothing  with Growth ARIMA(0,1,1) with constant
abrownga <- Arima(xt, order = c(0, 1, 1), include.mean = T)
abrowngb <- Arima(diff(xt), order = c(0, 0, 1), include.mean = T)
plot(
  forecast(abrownga, h = h),
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
plot(forecast(aholt1, h = h),
     main = "Holt's Linear Trend ARIMA(0,2,1) with constant",
     ylab = "Level",
     xlab = "Day")
plot(forecast(aholt3, h = h),
     main = "Holt's Linear Trend ARIMA(0,2,2) without constant",
     ylab = "Level",
     xlab = "Day")
lines(x)
# ARIMA Coefficients
summary(aholt2)
summary(aholt3)

# 4.11. Gardner's Additive Damped Trend ARIMA(1,1,2) without constant
agardner <- Arima(xt, order = c(1, 1, 2))
plot(
  forecast(agardner, h = h),
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
plot(forecast(srwd1, h = h),
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
plot(forecast(srt, h = h),
     main = "Seasonal Random Trend ARIMA(0,1,0)x(0,1,0)m without constant",
     ylab = "Level",
     xlab = "Day")
lines(x)
# Coefficients
summary(srt)

# 4.14. General Seasonal Model ARIMA(0,1,1)x(0,1,1)m without constant
gseas <- Arima(xt, order = c(0, 1, 1),
               seasonal = c(0, 1, 1))
plot(forecast(gseas, h = h),
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
plot(forecast(gsar1a, h = h),
     main = "General First Order Autoregressive Seasonal Model ARIMA(1,0,1)x(0,1,1)m with constant",
     ylab = "Level",
     xlab = "Day")
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
plot(forecast(sdar1a, h = h),
     main = "Seasonally Differentiated First Order Autoregressive ARIMA(1,0,0)x(0,1,0)m with constant",
     ylab = "Level",
     xlab = "Day")
lines(x)
# ARIMA Coefficients
summary(sdar1b)

# 4.17. Holt-Winters Additive Seasonality ARIMA (0,1,6)X(0,1,0)5 without constant
ahwa <- Arima(xt, order = c(0, 1, 6), seasonal = c(0, 1, 0))
plot(forecast(ahwa, h = h),
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
plot(forecast(abest, h=h))
# 4.19. Model Forecasting Accuracy
accuracy(forecast(arw1, h = h), xf)
accuracy(forecast(ar, h = h), xf)
accuracy(forecast(dar1a, h = h), xf)
accuracy(forecast(abrown, h = h), xf)
accuracy(forecast(abrownga, h = h), xf)
accuracy(forecast(aholt1, h = h), xf)
accuracy(forecast(agardner, h = h), xf)
accuracy(forecast(srwd1, h = h), xf)
accuracy(forecast(srt, h = h), xf)
accuracy(forecast(gseas, h = h), xf)
accuracy(forecast(gsar1a, h = h), xf)
accuracy(forecast(sdar1a, h = h), xf)
accuracy(forecast(ahwa, h = h), xf)
accuracy(forecast(abest, h = h), xf)

# 4.20. Residuals White Noise
# Best fitting and forecasting model
abest
# Normal and Partial Autocorrelation Functions ACF & PACF
acf(residuals(abest))
pacf(residuals(abest))
# Ljung.Box Autocorrelation Test
Box.test(residuals(abest), lag = 10, type = "Ljung")
