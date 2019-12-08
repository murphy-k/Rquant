library(rugarch)

# Data ####
sp500prices <- getSymbols("^GSPC", auto.assign = FALSE)
sp500prices <- sp500prices$GSPC.Adjusted
sp500ret <- na.trim(sp500ret)

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

# Plot the volatility for 2019
plot(garchvol["2019"])


# Asset Allocations ####
# According to the two-fund separation theorem of James Tobin, you should invest
# a proportion w of your wealth in a risky portfolio and the remainder in a risk
# free asset, like a US Treasury bill. When you target a portfolio with 5%
# annualized volatility, and the annualized volatility of the risky asset is σt,
# then you should invest 0.05/σt in the risky asset.

# Compute the annualized volatility
annualvol <- sigma(garchfit) * sqrt(252)
# Compute the annualized volatility
annualvol <- sqrt(252) * sigma(garchfit)
# Compute the 5% vol target weights
vt_weights <- 0.05 / annualvol
# Compare the annualized volatility to the portfolio weights in a plot
plot(merge(annualvol, vt_weights), multi.panel = TRUE)
