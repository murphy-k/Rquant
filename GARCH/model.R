# GARCH (1,1) Model
library(rugarch)

# Data ####
sp500prices <- getSymbols("^GSPC", auto.assign = FALSE)
sp500prices <- sp500prices$GSPC.Adjusted
sp500ret <- na.trim(sp500ret)

# Visualizing prediction errors ####
# Compute the mean daily return
m <- mean(sp500ret)
# Define the series of prediction errors
e <- sp500ret - m
# Plot the absolute value of the prediction errors
par(mfrow = c(2, 1), mar = c(3, 2, 2, 2))
plot(abs(e))
# Plot the acf of the absolute prediction errors
acf(abs(e))

# Recursive GARCH variance ####
omega = sd(sp500ret) ^ 2
alpha = 0.1
beta = 0.8
nobs = length(sp500ret)
e2 = e ^ 2
predvar <- vector(length = length(sp500ret))

# Compute the predicted variances
predvar[1] <- var(sp500ret)
for (t in 2:nobs) {
  predvar[t] <- omega + alpha * e2[t - 1] + beta * predvar[t - 1]
}

# Create annualized predicted volatility
ann_predvol <-
  xts(sqrt(252) * sqrt(predvar), order.by = time(sp500ret))

# Plot the annual predicted volatility in 2008 and 2009
par(mfrow = c(1, 1))
plot(ann_predvol["2008::2009"], main = "Ann. S&P 500 vol in 2008-2009")

# rugarch ####
# Using the rugarch package estimation is done by maximum likelihood:
# Find the parameter values for which the GARCH model is most likely to have
# generated the observed return series.

# Specify a standard GARCH model with constant mean
garchspec <- ugarchspec(
  mean.model = list(armaOrder = c(0, 0)),
  variance.model = list(model = "sGARCH"),
  distribution.model = "norm"
)

# Estimate the model
garchfit <- ugarchfit(data = sp500ret, spec = garchspec)

# Use the method sigma to retrieve the estimated volatilities
garchvol <- sigma(garchfit)

# Plot the volatility for 2017
plot(garchvol["2019"])

# Predicted Volatilities ####
# Compute unconditional volatility
sqrt(uncvariance(garchfit))

# Print last 10 ones in garchvol
tail(garchvol, 10)

# Forecast volatility 5 days ahead and add
garchforecast <- ugarchforecast(fitORspec = garchfit, n.ahead = 10)

# Extract the predicted volatilities and print them
print(sigma(garchforecast))
