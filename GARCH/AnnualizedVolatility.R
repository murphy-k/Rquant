# Annualized Volatility

library(quantmod)
library(xts)
library(PerformanceAnalytics)

# Data ####
# Get data
sp500prices <- getSymbols("^GSPC", auto.assign = FALSE)
sp500prices <- sp500prices$GSPC.Adjusted

# Plot daily S&P 500 prices
plot(sp500prices)
# Compute daily returns
sp500ret <- CalculateReturns(sp500prices)
# Check the class of sp500ret
class(sp500ret)
# Plot daily returns
plot(sp500ret)

# Statistics ####
sp500ret <- na.trim(sp500ret)
# Compute the daily standard deviation for the complete sample
sd(sp500ret)
# Compute the annualized volatility for the complete sample
sqrt(252) * sd(sp500ret)
# Compute the daily standard deviation for the year 2009
sqrt(252) * sd(sp500ret["2009"])
# Compute the annualized standard deviation for the year 2017
sqrt(252) * sd(sp500ret["2017"])

# Rolling volatility ####

# Showing two plots on the same figure
par(mfrow = c(2, 1))
# Compute the rolling 1 month estimate of annualized volatility
chart.RollingPerformance(
  R = sp500ret["2000::2017"],
  width = 22,
  FUN = "sd.annualized",
  scale = 252,
  main = "One month rolling volatility"
)
# Compute the rolling 3 months estimate of annualized volatility
chart.RollingPerformance(
  R = sp500ret["2000::2017"],
  width = 66,
  FUN = "sd.annualized",
  scale = 252,
  main = "Three months rolling volatility"
)
