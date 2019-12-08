# GJR-GARCH estimation
library(rugarch)
library(xts)
library(PerformanceAnalytics)
library(quantmod)

# Data ####
sp500prices <- getSymbols("^GSPC", auto.assign = FALSE)
sp500prices <- sp500prices$GSPC.Adjusted
sp500ret <- CalculateReturns(sp500prices)
sp500ret <- na.trim(sp500ret)

# Estimation ####
# Plot the return series
plot(sp500ret)

# Specify the garch model to be used
garchspec <- ugarchspec(
  mean.model = list(armaOrder = c(0, 0)),
  variance.model = list(model = "sGARCH"),
  distribution.model = "sstd"
)

# Estimate the model
garchfit <- ugarchfit(data = sp500ret, spec = garchspec)

# Inspect the coefficients
coef(garchfit)

# Compute stdret using residuals()
stdret <- residuals(garchfit, standardize = TRUE)

# Compute stdret using fitted() and sigma()
stdret <- (sp500ret - fitted(garchfit)) / sigma(garchfit)

# Load the package PerformanceAnalytics and make the histogram
chart.Histogram(
  stdret,
  methods = c("add.normal", "add.density"),
  colorset = c("gray", "red", "blue")
)

# GJR versus S ####
MSFT <-
  getSymbols('MSFT',
             auto.assign = FALSE,
             from = "1999-01-01",
             to = "2017-12-31")
msftret <- CalculateReturns(MSFT$MSFT.Adjusted)
msftret <- na.trim(msftret)

# Specify a standard GARCH model with constant mean
sgarchspec <- ugarchspec(
  mean.model = list(armaOrder = c(0, 0)),
  variance.model = list(model = "sGARCH"),
  distribution.model = "norm"
)

# Estimate the model
sgarchfit <- ugarchfit(data = msftret, spec = sgarchspec)

# Use the method sigma to retrieve the estimated volatilities
sgarchvol <- sigma(sgarchfit)

# Specify the GJR GARCH model
garchspec <- ugarchspec(
  mean.model = list(armaOrder = c(0, 0)),
  variance.model = list(model = "gjrGARCH"),
  distribution.model = "sstd"
)

# Estimate the model and compute volatility
gjrgarchfit <- ugarchfit(data = msftret, spec = garchspec)
gjrgarchvol <- sigma(gjrgarchfit)

# Compare volatility
plotvol <- plot(abs(msftret), col = "grey")
plotvol <- addSeries(gjrgarchvol, col = "red", on = 1)
plotvol <- addSeries(sgarchvol, col = "blue", on = 1)
plotvol