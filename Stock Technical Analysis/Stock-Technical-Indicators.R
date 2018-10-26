# 2. Stock Technical Indicators

# 1. Stock Technical Analysis

# 1.1. Load R packages
library("TTR")
library("quantmod")
library("PerformanceAnalytics")

# 1.2. Set working directory
# getwd()
# setwd("C:/.../Stock Technical Analysis with R")

# 1.3. Get data
getSymbols("AAPL", src = "google")

# 1.5. Delimit data range
aapl <- window(AAPL['2016-11-03::2017-11-03'])

# 1.4. Technical Analysis Charts
#lineChart(aapl)
barChart(aapl)
#candleChart(aapl)
##########

# 2. Stock Technical Indicators

# 2.1. Lagging Stock Technical Indicators

# 2.1.1. Moving Averages MA, SMA(5 & 21), EMA(5 & 21)

# Simple Moving Average
sma5 <- SMA(Cl(aapl), n = 5)
sma21 <- SMA(Cl(aapl), n = 21)
# Technical Analysis Chart
barChart(aapl)
addSMA(n = 5, col = 4)
addSMA(n = 21, col = 6)
# Manual Chart
plot(Cl(aapl), main = "Simple Moving Averages SMA(5 & 21)")
lines(sma5, col = 4)
lines(sma21, col = 6)

# Exponential Moving Average
ema5 <- EMA(Cl(aapl), n = 5)
ema21 <- EMA(Cl(aapl), n = 21)
# Technical Analysis Chart
barChart(aapl)
addEMA(n = 5, col = 4)
addEMA(n = 21, col = 6)
# Manual Chart
plot(Cl(aapl), main = "Exponential Moving Averages EMA(5 & 21)")
lines(ema5, col = 4)
lines(ema21, col = 6)

# 2.1.2. Bollinger Bands BB(20,2)
bb <- BBands(cbind(Hi(aapl), Lo(aapl), Cl(aapl)), n = 20, sd = 2)
# Technical Analysis Chart
barChart(aapl)
addBBands(n = 20, sd = 2)
# Manual Chart
plot(Cl(aapl), main = "Bollinger Bands BB(20,2)")
# Lower and Upper Bands
lines(bb[, 1], col = 4)
lines(bb[, 3], col = 4)
# Middle Band
lines(bb[, 2], col = 5)

# 2.1.3. Parabolic Stop and Reverse SAR(0.02,0.2)
sar <- SAR(cbind(Hi(aapl), Lo(aapl)), accel = c(0.02, 0.2))
# Technical Analysis Chart
barChart(aapl)
addSAR(accel = c(0.02, 0.2))
# Manual Chart
plot(Cl(aapl), main = "Parabolic Stop and Reverse SAR(0.02,0.2)")
points(sar, col = 4)

#########

# 2.2. Leading Stock Technical Indicators

# 2.2.1. Average Directional Movement Index ADX(14)
adx <- ADX(cbind(Hi(aapl), Lo(aapl), Cl(aapl)), n = 14)
# Technical Analysis Chart
barChart(aapl)
addADX(n = 14)

# 2.2.2. Commodity Channel Index CCI(20,0.015)
cci <- CCI(cbind(Hi(aapl), Lo(aapl), Cl(aapl)), n = 20, c = 0.015)
# Technical Analysis Chart
barChart(aapl)
addCCI(n = 20, c = 0.015)

# 2.2.3. Moving Averages Covergence/Divergence MACD(12,26,9)
macd <- MACD(Cl(aapl),
             nFast = 12,
             nSlow = 26,
             nSig = 9)
# Technical Analysis Chart
barChart(aapl)
addMACD()

# 2.2.4. Rate Of Change ROC(21)
roc <- ROC(aapl, n = 21)
# Technical Analysis Chart
barChart(aapl)
addROC(n = 21)

# 2.2.5. Relative Strength Index RSI(14)
rsi <- RSI(Cl(aapl), n = 14)
# Technical Analysis Chart
barChart(aapl,
         TA = NULL)
addRSI(n = 14)

# 2.2.6. Stochastic Momentum Index SMI(13,2,25,9)
smi <-
  SMI(
    cbind(Hi(aapl), Lo(aapl), Cl(aapl)),
    n = 13,
    nFast = 2,
    nSlow = 25,
    nSig = 9
  )
# Technical Analysis Chart
barChart(aapl,
         TA = NULL)
addSMI(n = 13)

# 2.2.7. Williams %R(14)
wpr <- WPR(cbind(Hi(aapl), Lo(aapl), Cl(aapl)), n = 14)
# Technical Analysis Chart
barChart(aapl)
addWPR(n = 14)