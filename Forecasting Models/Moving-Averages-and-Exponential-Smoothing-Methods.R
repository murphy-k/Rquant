# 3. Moving Averages and Exponential Smoothing Methods

# 1.1. Load R packages
library("forecast")
library("tseries")

# 1.2. Set working directory
# getwd()
# setwd("C:/.../Forecasting Models with R")

# 1.3. Read .CSV file
x <- read.csv("GLD_Daily.csv", header = T)
plot(
  x,
  type = "l",
  main = "Daily Apple Stock Prices 10/2014-10/2015",
  ylab = "Level",
  xlab = "Day"
)

# 1.4. Delimit training range
xt <- window(x[, 2], end = 449)
plot(
  xt,
  type = "l",
  main = "Daily Apple Stock Prices 10/2014-09/2015",
  ylab = "Level",
  xlab = "Day",
  xlim = c(1, 274)
)

# 1.5. Delimit forecasting test range
xf <- window(x[, 2], start = 450)
plot(
  xf,
  type = "l",
  main = "Daily Apple Stock Prices 10/2015",
  ylab = "Level",
  xlab = "Day"
)

# 1.6. Automatic script run
# For .txt file
## Ctrl + A and paste in console
# For .R file
## source("1. Forecasting Models Data.R",echo=T)

#########


# 2.1. Arithmetic Mean
mean <- meanf(xt, h = 22)
# plot(mean,main="Arithmetic Mean Method",ylab="Level",xlab="Day")
# lines(x)

# 2.2. NaÔve or Random Walk Method
rw1 <- naive(xt, h = 22)
rw2 <- rwf(xt, h = 22)
# plot(rw1,main="NaÔve or Random Walk Method 1",ylab="Level",xlab="Day")
# plot(rw2,main="NaÔve or Random Walk Method 2",ylab="Level",xlab="Day")
# lines(x)

# 2.3. Seasonal Random Walk Method
srw <- snaive(xt, h = 22)
# plot(srw,main="Seasonal NaÔve Method",ylab="Level",xlab="Day")
# lines(x)

# 2.4. Random Walk with Drift Method
rwd <- rwf(xt, drift = T, h = 22)
# plot(rwd,main="Random Walk with Drift Method",ylab="Level",xlab="Day")
# lines(x)

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
  forecast(sma5, h = 22),
  main = "Simple Moving Average SMA",
  ylab = "Level",
  xlab = "Day",
  col = 4
)
lines(x)

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
## exp2 <- ets(xt,model="AMN",damped=F)
# Chart
plot(exp1,
     main = "Exponential Trend Method ETS(A,M,N)",
     ylab = "Level",
     xlab = "Day")
lines(x)
# Smoothing Parameters
## exp2
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
## taylor2 <- ets(xt,model="AMN",damped=T)
# Chart
plot(taylor1,
     main = "Taylor's Multiplicative Damped Trend Method ETS(A,Md,N)",
     ylab = "Level",
     xlab = "Day")
lines(x)
# Smoothing Parameters
## taylor2
## ets model combination not supported

# 3.7. Holt-Winters Additive Method ETS(A,A,A)
## hwa1 <- hw(xt,h=22,seasonal="additive")
## hwa2 <- ets(xt,model="AAA",damped=F)
# Chart
## plot(hwa1,main="Holt-Winters Additive Method ETS(A,A,A)",ylab="Level",xlab="Day")
## lines(x)
# Smoothing Parameters
## hwa2
## Ets error = Non-seasonal data

# 3.8. Holt-Winters Multiplicative Method ETS(A,A,M)
## hwm1 <- hw(xt,h=22,seasonal="multiplicative")
## hwm2 <- ets(xt,model="AAM",damped=F)
# Chart
## plot(hwm1,main="Holt-Winters Multiplicative Method ETS(A,A,M)",ylab="Level",xlab="Day")
## lines(x)
# Smoothing Parameters
## hwm2
## Ets error = Non-seasonal data

# 3.9. Holt-Winters Damped Method ETS(A,Ad,M)
## hwd1 <- hw(xt,h=22,seasonal="multiplicative",damped=T)
## hwd2 <- ets(xt,model="AAM",damped=T)
# Chart
## plot(hwd1,main="Holt-Winters Damped Method ETS(A,Ad,M)",ylab="Level",xlab="Day")
## lines(x)
# Smoothing Parameters
## hwd2
## Ets error= Non-seasonal data

# 3.10. Method Selection
# Best fitting training set
# Training set = Data from 1 to 252 (1/oct/2014 - 29/sep/2015)
sbest <- ets(xt)
sbest
# Decomposition of ETS method
plot(ets(xt))

# 3.11. Method Forecasting Accuracy
# Best forecast test set
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
