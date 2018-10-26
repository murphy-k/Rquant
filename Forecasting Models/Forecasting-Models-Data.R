# 1. Forecasting Models Data

# 1.1. Load R packages
library("forecast")
library("tseries")

# 1.2. Set working directory
# getwd()
# setwd("C:/.../Forecasting Models with R")

# 1.3. Read .CSV file
x <- read.csv("Apple_Daily.csv", header = T)
plot(
  x,
  type = "l",
  main = "Daily Apple Stock Prices 10/2014-10/2015",
  ylab = "Level",
  xlab = "Day"
)

# 1.4. Delimit training range
xt <- window(x[, 2], end = 252)
plot(
  xt,
  type = "l",
  main = "Daily Apple Stock Prices 10/2014-09/2015",
  ylab = "Level",
  xlab = "Day",
  xlim = c(1, 274)
)

# 1.5. Delimit forecasting test range
xf <- window(x[, 2], start = 253)
plot(
  xf,
  type = "l",
  main = "Daily Apple Stock Prices 10/2015",
  ylab = "Level",
  xlab = "Day"
)