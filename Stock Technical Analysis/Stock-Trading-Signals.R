# 3. Stock Trading Signals

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
# lineChart(aapl)
# barChart(aapl)
# candleChart(aapl)


##########

# 2. Stock Technical Indicators

# 2.1. Lagging Stock Technical Indicators

# 2.1.1. Moving Averages MA, SMA(5 & 21), EMA(5 & 21)

# Simple Moving Averages SMA(5 & 21)
sma5 <- SMA(Cl(aapl), n = 5)
sma21 <- SMA(Cl(aapl), n = 21)
# Technical Analysis Chart
# barChart(aapl)
# addSMA(n=5,col=4)
# addSMA(n=21,col=6)
# Manual Chart
# plot(Cl(aapl),main="Simple Moving Averages SMA(5 & 21)")
# lines(sma5,col=4)
# lines(sma21,col=6)

# Exponential Moving Averages EMA(5 & 21)
ema5 <- EMA(Cl(aapl), n = 5)
ema21 <- EMA(Cl(aapl), n = 21)
# Technical Analysis Chart
# barChart(aapl)
# addEMA(n=5,col=4)
# addEMA(n=21,col=6)
# Manual Chart
# plot(Cl(aapl),main="Exponential Moving Averages EMA(5 & 21)")
# lines(ema5,col=4)
# lines(ema21,col=6)

# 2.1.2. Bollinger Bands BB(20,2)
bb <- BBands(cbind(Hi(aapl), Lo(aapl), Cl(aapl)), n = 20, sd = 2)
# Technical Analysis Chart
# barChart(aapl)
# addBBands(n=20,sd=2)
# Manual Chart
# plot(Cl(aapl),main="Bollinger Bands BB(20,2)")
# Lower and Upper Bands
# lines(bb[,1],col=4)
# lines(bb[,3],col=4)
# Middle Band
# lines(bb[,2],col=5)

# 2.1.3. Parabolic Stop and Reverse SAR(0.02,0.2)
sar <- SAR(cbind(Hi(aapl), Lo(aapl)), accel = c(0.02, 0.2))
# Technical Analysis Chart
# barChart(aapl)
# addSAR(accel=c(0.02, 0.2))
# Manual Chart
# plot(Cl(aapl),main="Parabolic Stop and Reverse SAR(0.02,0.2)")
# points(sar,col=4)

#########

# 2.2. Leading Stock Technical Indicators

# 2.2.1. Average Directional Movement Index ADX(14)
adx <- ADX(cbind(Hi(aapl), Lo(aapl), Cl(aapl)), n = 14)
# Technical Analysis Chart
# barChart(aapl)
# addADX(n=14)

# 2.2.2. Commodity Channel Index CCI(20,0.015)
cci <- CCI(cbind(Hi(aapl), Lo(aapl), Cl(aapl)), n = 20, c = 0.015)
# Technical Analysis Chart
# barChart(aapl)
# addCCI(n=20,c=0.015)

# 2.2.3. Moving Averages Covergence/Divergence MACD(12,26,9)
macd <- MACD(Cl(aapl),
             nFast = 12,
             nSlow = 26,
             nSig = 9)
# Technical Analysis Chart
# barChart(aapl)
# addMACD()

# 2.2.4. Rate Of Change ROC(21)
roc <- ROC(aapl, n = 21)
# Technical Analysis Chart
# barChart(aapl)
# addROC(n=21)

# 2.2.5. Relative Strength Index RSI(14)
rsi <- RSI(Cl(aapl), n = 14)
# Technical Analysis Chart
# barChart(aapl)
# addRSI(n=14)

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
# barChart(aapl)
# addSMI(n=13)

# 2.2.7. Williams %R(14)
wpr <- WPR(cbind(Hi(aapl), Lo(aapl), Cl(aapl)), n = 14)
# Technical Analysis Chart
# barChart(aapl)
# addWPR(n=14)

#########

# 3. Stock Trading Signals

# 3.1. Single Indicator Trading Signals

# 3.1.1. Moving Averages MA, SMA(5 & 21), EMA(5 & 21) Trading Signals

# Simple Moving Averages SMA(5 & 21) Price Crossover Trading Signals
lineChart(aapl)
addSMA(n = 5, col = 4)
addSMA(n = 21, col = 6)
# Price Crossover Trading Signals
sma5tr <-
  Lag(ifelse(
    Lag(Cl(aapl)) < Lag(sma5) &
      Cl(aapl) > sma5,
    1,
    ifelse(Lag(Cl(aapl)) > Lag(sma5) & Cl(aapl) < sma5, -1, 0)
  ))
sma5tr[is.na(sma5tr)] <- 0
sma21tr <-
  Lag(ifelse(
    Lag(Cl(aapl)) < Lag(sma21) &
      Cl(aapl) > sma21,
    1,
    ifelse(Lag(Cl(aapl)) > Lag(sma21) & Cl(aapl) < sma21, -1, 0)
  ))
sma21tr[is.na(sma21tr)] <- 0

# Exponential Moving Averages EMA (5 & 21) Price Crossover Trading Signals
lineChart(aapl)
addEMA(n = 5, col = 4)
addEMA(n = 21, col = 6)
# Price Crossover Trading Signals
ema5tr <-
  Lag(ifelse(
    Lag(Cl(aapl)) < Lag(ema5) &
      Cl(aapl) > ema5,
    1,
    ifelse(Lag(Cl(aapl)) > Lag(ema5) & Cl(aapl) < ema5, -1, 0)
  ))
ema5tr[is.na(ema5tr)] <- 0
ema21tr <-
  Lag(ifelse(
    Lag(Cl(aapl)) < Lag(ema21) &
      Cl(aapl) > ema21,
    1,
    ifelse(Lag(Cl(aapl)) > Lag(ema21) & Cl(aapl) < ema21, -1, 0)
  ))
ema21tr[is.na(ema21tr)] <- 0

# SMA(5 & 21) & EMA(5 & 21) Double Crossover Trading Signals
# Double Crossover Trading Signals
smatr <-
  Lag(ifelse(
    Lag(sma5) < Lag(sma21) &
      sma5 > sma21,
    1,
    ifelse(Lag(sma5) > Lag(sma21) & sma5 < sma21, -1, 0)
  ))
smatr[is.na(smatr)] <- 0
ematr <-
  Lag(ifelse(
    Lag(ema5) < Lag(ema21) &
      ema5 > ema21,
    1,
    ifelse(Lag(ema5) > Lag(ema21) & ema5 < ema21, -1, 0)
  ))
ematr[is.na(ematr)] <- 0

# 3.1.2. Bollinger Bands BB(20,2) Trading Signals
lineChart(aapl)
addBBands(n = 20, sd = 2)
# Bands Crossover Trading Signals
bbtr <-
  Lag(ifelse(
    Lag(Cl(aapl)) < Lag(bb[, 1]) &
      Cl(aapl) > bb[, 1],
    1,
    ifelse(Lag(Cl(aapl)) < Lag(bb[, 3]) & Cl(aapl) > bb[, 3], -1, 0)
  ))
bbtr[is.na(bbtr)] <- 0

# 3.1.3. Parabolic Stop And Reverse SAR(0.02,0.2) Trading Signals
lineChart(aapl)
addSAR(accel = c(0.02, 0.2))
# Stop And Reverse Trading Signals
sartr <-
  Lag(ifelse(
    Lag(Cl(aapl)) < Lag(sar) &
      Cl(aapl) > sar,
    1,
    ifelse(Lag(Cl(aapl)) > Lag(sar) & Cl(aapl) < sar, -1, 0)
  ))
sartr[is.na(sartr)] <- 0

# 3.1.4. Average Directional Movement Index ADX(14) Trading Signals
lineChart(aapl)
addADX(n = 14)
# Band and Double Crossover Trading Signals
adxtr <-
  Lag(ifelse(
    Lag(adx[, 1]) < Lag(adx[, 2]) &
      adx[, 1] > adx[, 2] &
      adx[, 4] > 20,
    1,
    ifelse(Lag(adx[, 1]) > Lag(adx[, 2]) &
             adx[, 1] < adx[, 2] & adx[, 4] > 20, -1, 0)
  ))
adxtr[is.na(adxtr)] <- 0

# 3.1.5. Commodity Channel Index CCI(20,0.015) Trading Signals
lineChart(aapl)
addCCI(n = 20, c = 0.015)
# Bands Crossover Trading Signals
ccitr <-
  Lag(ifelse(Lag(cci) < (-100) &
               cci > (-100), 1, ifelse(Lag(cci) < 100 &
                                         cci > 100, -1, 0)))
ccitr[is.na(ccitr)] <- 0

# 3.1.6. Moving Averages Covergence/Divergence MACD(12,26,9) Trading Signals
lineChart(aapl)
addMACD()
# Signal and Centerline Crossover Trading Signals
smacdtr <-
  Lag(ifelse(
    Lag(macd[, 1]) < Lag(macd[, 2]) &
      macd[, 1] > macd[, 2],
    1,
    ifelse(Lag(macd[, 1]) > Lag(macd[, 2]) &
             macd[, 1] < macd[, 2], -1, 0)
  ))
smacdtr[is.na(smacdtr)] <- 0
cmacdtr <-
  Lag(ifelse(Lag(macd[, 1]) < 0 &
               macd[, 1] > 0, 1, ifelse(Lag(macd[, 1]) > 0 &
                                          macd[, 1] < 0, -1, 0)))
cmacdtr[is.na(cmacdtr)] <- 0

# 3.1.7. Rate Of Change ROC(21) Trading Signals
lineChart(aapl)
addROC(n = 21)
# Bands Crossover Trading Signals
roctr <-
  Lag(ifelse(
    Lag(roc[, 4]) < (-0.10) &
      roc[, 4] > (-0.10),
    1,
    ifelse(Lag(roc[, 4]) < 0.10 & roc[, 4] > 0.10, -1, 0)
  ))
roctr[is.na(roctr)] <- 0

# 3.1.8. Relative Strength Index RSI(14) Trading Signals
lineChart(aapl)
addRSI(n = 14)
# Bands Crossover Trading Signals
rsitr <-
  Lag(ifelse(Lag(rsi) < 30 & rsi > 30, 1, ifelse(Lag(rsi) < 70 &
                                                   rsi > 70, -1, 0)))
rsitr[is.na(rsitr)] <- 0

# 3.1.9. Stochastic Momentum Index SMI(13,2,25,9) Trading Signals
lineChart(aapl)
addSMI(n = 13)
# Signal Crossover Trading Signals
smitr <-
  Lag(ifelse(
    Lag(smi[, 1]) < Lag(smi[, 2]) &
      smi[, 1] > smi[, 2],
    1,
    ifelse(Lag(smi[, 1]) > Lag(smi[, 2]) &
             smi[, 1] < smi[, 2], -1, 0)
  ))
smitr[is.na(smitr)] <- 0

# 3.1.10. Williams %R(14) Trading Signals
lineChart(aapl)
addWPR(n = 14)
# Bands Crossover Trading Signals
wprtr <-
  Lag(ifelse(Lag(wpr) > 0.80 &
               wpr < 0.80, 1, ifelse(Lag(wpr) > 0.20 &
                                       wpr < 0.20, -1, 0)))
wprtr[is.na(wprtr)] <- 0

# 3.2. Multiple Indicators Trading Signals

# 3.2.1. Commodity Channel Index CCI(20,0.015) and SMA(5) Trading Signals
lineChart(aapl)
addCCI(n = 20, c = 0.015)
addSMA(n = 5, col = 4)
# Price Crossover and Bands Crossover Confirmation Trading Signals
ccismatr <-
  Lag(ifelse(
    Lag(Cl(aapl)) < Lag(sma5) &
      Cl(aapl) > sma5 &
      cci < (-100),
    1,
    ifelse(Lag(Cl(aapl)) > Lag(sma5) &
             Cl(aapl) < sma5 & cci > 100, -1, 0)
  ))
ccismatr[is.na(ccismatr)] <- 0

# 3.2.2. Rate Of Change ROC(21) and SMA(5) Trading Signals
lineChart(aapl)
addROC(n = 21)
addSMA(n = 5, col = 4)
# Price Crossover and Bands Crossover Confirmation Trading Signals
rocsmatr <-
  Lag(ifelse(
    Lag(Cl(aapl)) < Lag(sma5) &
      Cl(aapl) > sma5 &
      roc[, 4] < (-0.10),
    1,
    ifelse(Lag(Cl(aapl)) > Lag(sma5) & Cl(aapl) < sma5 &
             roc[, 4] > 0.10, -1, 0)
  ))
rocsmatr[is.na(rocsmatr)] <- 0

# 3.2.3. Relative Strength Index RSI(14), SMA(5) Trading Signals
lineChart(aapl)
addRSI(n = 14)
addSMA(n = 5, col = 4)
# Price Crossover and Bands Crossover Confirmation Trading Signals
rsismatr <-
  Lag(ifelse(
    Lag(Cl(aapl)) < Lag(sma5) &
      Cl(aapl) > sma5 &
      rsi < 30,
    1,
    ifelse(Lag(Cl(aapl)) > Lag(sma5) &
             Cl(aapl) < sma5 & rsi > 70, -1, 0)
  ))
rsismatr[is.na(rsismatr)] <- 0

# 3.2.4. Stochastic Momentum Index SMI(13,2,25,9), SMA(5) Trading Signals
lineChart(aapl)
addSMI(n = 13)
addSMA(n = 5, col = 4)
# Price Crossover and Bands Crossover Confirmation Trading Signals
smismatr <-
  Lag(ifelse(
    Lag(Cl(aapl)) < Lag(sma5) &
      Cl(aapl) > sma5 &
      smi[, 1] < (-40),
    1,
    ifelse(Lag(Cl(aapl)) > Lag(sma5) & Cl(aapl) < sma5 &
             smi[, 1] > 40, -1, 0)
  ))
smismatr[is.na(smismatr)] <- 0

# 3.2.5. Williams %R(14) and Simple Moving Average SMA(5) Trading Signals
lineChart(aapl)
addWPR(n = 14)
addSMA(n = 5, col = 4)
# Price Crossover and Bands Crossover Confirmation Trading Signals
wprsmatr <-
  Lag(ifelse(
    Lag(Cl(aapl)) < Lag(sma5) &
      Cl(aapl) > sma5 &
      wpr > 0.80,
    1,
    ifelse(Lag(Cl(aapl)) > Lag(sma5) &
             Cl(aapl) < sma5 & wpr < 0.20, -1, 0)
  ))
wprsmatr[is.na(wprsmatr)] <- 0

##########
