# Packages ####
library(quantmod)
library(PortfolioAnalytics)

# Clear plots and environment
rm(list = ls())
dev.off(dev.list()["RStudioGD"])

# Variable Handling ####
instruments <- c("SPY", "TLT", "GLD")
start_date <- "2017-01-01"
suppressMessages((
  getSymbols(
    instruments,
    src = "yahoo",
    auto.assign = TRUE,
    from = start_date
  )
))

portfolio <-
  cbind(SPY$SPY.Close, TLT$TLT.Close, GLD$GLD.Close)
d_portfolio <- diff(portfolio)
# Basic Syntax ####
chartSeries(
  SPY,
  name = "SPDR SP500",
  xlab = "Date",
  ylab = "Price",
  bar.type = "ohlc"
)
# Plot two charts on same graphical window
par(mfrow = c(3, 1))
plot(TLT$TLT.Close, main = "TLT")
plot(SPY$SPY.Close, main = "S&P-500")
plot(GLD$GLD.Close, main = "SPDR Gold ETF")
# Replot with reduced margin and character sizes
par(mfrow = c(3, 1),
    mex = 0.75,
    cex = 0.2)
plot(SPY$SPY.Close, main = "SPDR S&P-500")
plot(GLD$GLD.Close, main = "Gold")
plot(TLT$TLT.Close, main = "Bonds")

par(mfrow = c(1, 1)) # reset parameters for plot()

# Histograms ####
roc_TLT <- ROC(TLT$TLT.Close)
hist(
  roc_TLT,
  main = "TLT Returns Histogram",
  xlab = "Percent Return",
  ylab = "Percent Occurence",
  probability = TRUE,
  breaks = 36
)
plot(density(roc_TLT, na.rm = TRUE))
# Correlation ####
cor(as.data.frame(portfolio), method = "pearson")
