# GARCH-in-mean

MSFT <-
  getSymbols('MSFT',
             auto.assign = FALSE,
             from = "1999-01-01",
             to = "2017-12-31")
msftret <- CalculateReturns(MSFT$MSFT.Adjusted)
msftret <- na.trim(msftret)

# Specify AR(1)-GJR GARCH model
garchspec <- ugarchspec(
  mean.model = list(armaOrder = c(1, 0)),
  variance.model = list(model = "gjrGARCH"),
  distribution.model = "sstd"
)
# Estimate the model
garchfit <- ugarchfit(data = msftret, spec = garchspec)

# Print the coefficients
coef(garchfit)[c(1:2)]

# GARCH-in-Mean specification and estimation
gim_garchspec <- ugarchspec(
  mean.model = list(
    armaOrder = c(0, 0),
    archm = TRUE,
    archpow = 2
  ),
  variance.model = list(model = "gjrGARCH"),
  distribution.model = "sstd"
)
gim_garchfit <- ugarchfit(data = msftret, spec = gim_garchspec)

# Predicted mean returns and volatility of GARCH-in-mean
gim_mean <- fitted(gim_garchfit)
gim_vol <- sigma(gim_garchfit)
