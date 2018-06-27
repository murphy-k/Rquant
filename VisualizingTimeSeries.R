# Packages ####
library(quantmod)
library(PerformanceAnalytics)

# Clear plots and environment
rm(list = ls())
dev.off(dev.list()["RStudioGD"])

# Variable Handling ####
instruments <- c("TSLA", "SPY", "TLT", "GLD")
start_date <- "2017-01-01"
suppressMessages((
  getSymbols(
    instruments,
    src = "yahoo",
    auto.assign = TRUE,
    from = start_date
  )
))

portfolio <- cbind(TSLA$TSLA.Close, SPY$SPY.Close, TLT$TLT.Close,GLD$GLD.Close)
d_portfolio <- diff(portfolio)
# Basic Syntax ####
head(SPY)
plot.xts(SPY$SPY.Close,
         main = "SPDR SP500",
         xlab = "Date",
         ylab = "Price")
# Plot two charts on same graphical window
par(mfrow = c(2, 1))
plot(TSLA$TSLA.Close, main = "Tesla")
plot(SPY$SPY.Close, main = "SPDR S&P-500")

# Replot with reduced margin and character sizes
par(mfrow = c(3, 1),
    mex = 0.75,
    cex = 0.2)
plot(SPY$SPY.Close, main = "SPDR S&P-500")
plot(GLD$GLD.Close, main = "Gold")
plot(TLT$TLT.Close, main = "Bonds")

par(mfrow = c(1, 1)) # reset parameters for plot()

# Histograms ####

hist(
  diff(SPY$SPY.Close),
  main = "SPY Returns Histogram",
  xlab = "Percent Return",
  ylab = "Percent Occurence",
  probability = TRUE,
  breaks = 50
)
plot(density(d_portfolio, na.rm = TRUE))
# Correlation ####
cor(as.data.frame(portfolio), method = "pearson")