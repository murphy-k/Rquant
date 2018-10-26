# 2. Simple Forecasting Methods


# 1.1. Load R packages
library("forecast")
library("tseries")


# 1.3. Read .CSV file
x <- read.csv("BTCUSD_Daily.csv", header = T)
plot(
  x,
  type = "l",
  main = "Daily GLD Stock Prices 07/18/2010-12/29/2017",
  ylab = "Level",
  xlab = "Day"
)

# 1.4. Delimit training range
xt <- window(x[, 2], end = 2701)
plot(
  xt,
  type = "l",
  main = "Daily GLD Stock Prices 01/2016-11/2017",
  ylab = "Level",
  xlab = "Day"
)

# 1.5. Delimit forecasting test range 
# This is the training range +1 
xf <- window(x[, 2], start = 2702)
plot(
  xf,
  type = "l",
  main = "Daily GLD Stock Prices 10/2015",
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

# 2.2. NaÔve or Random Walk Method
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
