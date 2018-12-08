# 5. Strategies Performance Comparison

# 1. Stock Technical Analysis

# 1.1. Load R packages
library("TTR")
library("quantmod")
library("PerformanceAnalytics")

# 1.3. Get data
getSymbols("SPY", src = "yahoo")

# 1.5. Delimit data range
spy <- window(SPY['2017-11-03::2018-12-01'])

# 1.4. Technical Analysis Charts
lineChart(spy)
barChart(spy)


# 1.5. Automatic script run
# For .txt file
## Ctrl + A and paste in console
# For .R file
## source("Stock Technical Analysis with R Code.R",echo=T)

##########

# 2. Stock Technical Indicators

# 2.1. Lagging Stock Technical Indicators

# 2.1.1. Moving Averages MA, Simple Moving Averages SMA(5 & 21), Exponential Moving Averages EMA(5 & 21)

# Simple Moving Averages SMA(5 & 21)
sma5 <- SMA(Cl(spy), n = 5)
sma21 <- SMA(Cl(spy), n = 21)
# Technical Analysis Chart
# barChart(spy)
# addSMA(n=5,col=4)
# addSMA(n=21,col=6)
# Manual Chart
# plot(Cl(spy),main="Simple Moving Averages SMA(5 & 21)")
# lines(sma5,col=4)
# lines(sma21,col=6)

# Exponential Moving Averages EMA(5 & 21)
ema5 <- EMA(Cl(spy), n = 5)
ema21 <- EMA(Cl(spy), n = 21)
# Technical Analysis Chart
# barChart(spy)
# addEMA(n=5,col=4)
# addEMA(n=21,col=6)
# Manual Chart
# plot(Cl(spy),main="Exponential Moving Averages EMA(5 & 21)")
# lines(ema5,col=4)
# lines(ema21,col=6)

# 2.1.2. Bollinger Bands BB(20,2)
bb <- BBands(cbind(Hi(spy), Lo(spy), Cl(spy)), n = 20, sd = 2)
# Technical Analysis Chart
# barChart(spy)
# addBBands(n=20,sd=2)
# Manual Chart
# plot(Cl(spy),main="Bollinger Bands BB(20,2)")
# Lower and Upper Bands
# lines(bb[,1],col=4)
# lines(bb[,3],col=4)
# Middle Band
# lines(bb[,2],col=5)

# 2.1.3. Parabolic Stop and Reverse SAR(0.02,0.2)
sar <- SAR(cbind(Hi(spy), Lo(spy)), accel = c(0.02, 0.2))
# Technical Analysis Chart
# barChart(spy)
# addSAR(accel=c(0.02, 0.2))
# Manual Chart
# plot(Cl(spy),main="Parabolic Stop and Reverse SAR(0.02,0.2)")
# points(sar,col=4)

#########

# 2.2. Leading Stock Technical Indicators

# 2.2.1. Average Directional Movement Index ADX(14)
adx <- ADX(cbind(Hi(spy), Lo(spy), Cl(spy)), n = 14)
# Technical Analysis Chart
# barChart(spy)
# addADX(n=14)

# 2.2.2. Commodity Channel Index CCI(20,0.015)
cci <- CCI(cbind(Hi(spy), Lo(spy), Cl(spy)), n = 20, c = 0.015)
# Technical Analysis Chart
# barChart(spy)
# addCCI(n=20,c=0.015)

# 2.2.3. Moving Averages Covergence/Divergence MACD(12,26,9)
macd <- MACD(Cl(spy),
             nFast = 12,
             nSlow = 26,
             nSig = 9)
# Technical Analysis Chart
# barChart(spy)
# addMACD()

# 2.2.4. Rate Of Change ROC(21)
roc <- ROC(spy, n = 21)
# Technical Analysis Chart
# barChart(spy)
# addROC(n=21)

# 2.2.5. Relative Strength Index RSI(14)
rsi <- RSI(Cl(spy), n = 14)
# Technical Analysis Chart
# barChart(spy)
# addRSI(n=14)

# 2.2.6. Stochastic Momentum Index SMI(13,2,25,9)
smi <-
  SMI(
    cbind(Hi(spy), Lo(spy), Cl(spy)),
    n = 13,
    nFast = 2,
    nSlow = 25,
    nSig = 9
  )
# Technical Analysis Chart
# barChart(spy)
# addSMI(n=13)

# 2.2.7. Williams %R(14)
wpr <- WPR(cbind(Hi(spy), Lo(spy), Cl(spy)), n = 14)
# Technical Analysis Chart
# barChart(spy)
# addWPR(n=14)

#########

# 3. Stock Trading Signals

# 3.1. Single Indicator Trading Signals

# 3.1.1. Moving Averages MA, Simple Moving Averages SMA(5 & 21), Exponential Moving Averages EMA(5 & 21) Trading Signals

# Simple Moving Averages SMA(5 & 21) Price Crossover Trading Signals
# lineChart(spy)
# addSMA(n=5,col=4)
# addSMA(n=21,col=6)
# Price Crossover Trading Signals
sma5tr <-
  Lag(ifelse(
    Lag(Cl(spy)) < Lag(sma5) &
      Cl(spy) > sma5,
    1,
    ifelse(Lag(Cl(spy)) > Lag(sma5) & Cl(spy) < sma5, -1, 0)
  ))
sma5tr[is.na(sma5tr)] <- 0
sma21tr <-
  Lag(ifelse(
    Lag(Cl(spy)) < Lag(sma21) &
      Cl(spy) > sma21,
    1,
    ifelse(Lag(Cl(spy)) > Lag(sma21) & Cl(spy) < sma21, -1, 0)
  ))
sma21tr[is.na(sma21tr)] <- 0

# Exponential Moving Averages EMA (5 & 21) Price Crossover Trading Signals
# lineChart(spy)
# addEMA(n=5,col=4)
# addEMA(n=21,col=6)
# Price Crossover Trading Signals
ema5tr <-
  Lag(ifelse(
    Lag(Cl(spy)) < Lag(ema5) &
      Cl(spy) > ema5,
    1,
    ifelse(Lag(Cl(spy)) > Lag(ema5) & Cl(spy) < ema5, -1, 0)
  ))
ema5tr[is.na(ema5tr)] <- 0
ema21tr <-
  Lag(ifelse(
    Lag(Cl(spy)) < Lag(ema21) &
      Cl(spy) > ema21,
    1,
    ifelse(Lag(Cl(spy)) > Lag(ema21) & Cl(spy) < ema21, -1, 0)
  ))
ema21tr[is.na(ema21tr)] <- 0

# Simple & Exponential Moving Averages SMA(5 & 21) & EMA(5 & 21) Double Crossover Trading Signals
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
# lineChart(spy)
# addBBands(n=20,sd=2)
# Bands Crossover Trading Signals
bbtr <-
  Lag(ifelse(
    Lag(Cl(spy)) < Lag(bb[, 1]) &
      Cl(spy) > bb[, 1],
    1,
    ifelse(Lag(Cl(spy)) < Lag(bb[, 3]) & Cl(spy) > bb[, 3], -1, 0)
  ))
bbtr[is.na(bbtr)] <- 0

# 3.1.3. Parabolic Stop And Reverse SAR(0.02,0.2) Trading Signals
# lineChart(spy)
# addSAR(accel=c(0.02, 0.2))
# Stop And Reverse Trading Signals
sartr <-
  Lag(ifelse(
    Lag(Cl(spy)) < Lag(sar) &
      Cl(spy) > sar,
    1,
    ifelse(Lag(Cl(spy)) > Lag(sar) & Cl(spy) < sar, -1, 0)
  ))
sartr[is.na(sartr)] <- 0

# 3.1.4. Average Directional Movement Index ADX(14) Trading Signals
# lineChart(spy)
# addADX(n=14)
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
# lineChart(spy)
# addCCI(n=20,c=0.015)
# Bands Crossover Trading Signals
ccitr <-
  Lag(ifelse(Lag(cci) < (-100) &
               cci > (-100), 1, ifelse(Lag(cci) < 100 & cci > 100, -1, 0)))
ccitr[is.na(ccitr)] <- 0

# 3.1.6. Moving Averages Covergence/Divergence MACD(12,26,9) Trading Signals
# lineChart(spy)
# addMACD()
# Signal and Centerline Crossover Trading Signals
smacdtr <-
  Lag(ifelse(
    Lag(macd[, 1]) < Lag(macd[, 2]) &
      macd[, 1] > macd[, 2],
    1,
    ifelse(Lag(macd[, 1]) > Lag(macd[, 2]) & macd[, 1] < macd[, 2], -1, 0)
  ))
smacdtr[is.na(smacdtr)] <- 0
cmacdtr <-
  Lag(ifelse(Lag(macd[, 1]) < 0 &
               macd[, 1] > 0, 1, ifelse(Lag(macd[, 1]) > 0 & macd[, 1] < 0, -1, 0)))
cmacdtr[is.na(cmacdtr)] <- 0

# 3.1.7. Rate Of Change ROC(21) Trading Signals
# lineChart(spy)
# addROC(n=21)
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
# lineChart(spy)
# addRSI(n=14)
# Bands Crossover Trading Signals
rsitr <-
  Lag(ifelse(Lag(rsi) < 30 & rsi > 30, 1, ifelse(Lag(rsi) < 70 &
                                                   rsi > 70, -1, 0)))
rsitr[is.na(rsitr)] <- 0

# 3.1.9. Stochastic Momentum Index SMI(13,2,25,9) Trading Signals
# lineChart(spy)
# addSMI(n=13)
# Signal Crossover Trading Signals
smitr <-
  Lag(ifelse(
    Lag(smi[, 1]) < Lag(smi[, 2]) &
      smi[, 1] > smi[, 2],
    1,
    ifelse(Lag(smi[, 1]) > Lag(smi[, 2]) & smi[, 1] < smi[, 2], -1, 0)
  ))
smitr[is.na(smitr)] <- 0

# 3.1.10. Williams %R(14) Trading Signals
# lineChart(spy)
# addWPR(n=14)
# Bands Crossover Trading Signals
wprtr <-
  Lag(ifelse(Lag(wpr) > 0.80 &
               wpr < 0.80, 1, ifelse(Lag(wpr) > 0.20 & wpr < 0.20, -1, 0)))
wprtr[is.na(wprtr)] <- 0

# 3.2. Multiple Indicators Trading Signals

# 3.2.1. Commodity Channel Index CCI(20,0.015) and Simple Moving Average SMA(5) Trading Signals
# lineChart(spy)
# addCCI(n=20,c=0.015)
# addSMA(n=5,col=4)
# Price Crossover and Bands Crossover Confirmation Trading Signals
ccismatr <-
  Lag(ifelse(
    Lag(Cl(spy)) < Lag(sma5) &
      Cl(spy) > sma5 &
      cci < (-100),
    1,
    ifelse(Lag(Cl(spy)) > Lag(sma5) & Cl(spy) < sma5 & cci > 100, -1, 0)
  ))
ccismatr[is.na(ccismatr)] <- 0

# 3.2.2. Rate Of Change ROC(21) and Simple Moving Average SMA(5) Trading Signals
# lineChart(spy)
# addROC(n=21)
# addSMA(n=5,col=4)
# Price Crossover and Bands Crossover Confirmation Trading Signals
rocsmatr <-
  Lag(ifelse(
    Lag(Cl(spy)) < Lag(sma5) &
      Cl(spy) > sma5 &
      roc[, 4] < (-0.10),
    1,
    ifelse(Lag(Cl(spy)) > Lag(sma5) & Cl(spy) < sma5 &
             roc[, 4] > 0.10, -1, 0)
  ))
rocsmatr[is.na(rocsmatr)] <- 0

# 3.2.3. Relative Strength Index RSI(14) and Simple Moving Average SMA(5) Trading Signals
# lineChart(spy)
# addRSI(n=14)
# addSMA(n=5,col=4)
# Price Crossover and Bands Crossover Confirmation Trading Signals
rsismatr <-
  Lag(ifelse(
    Lag(Cl(spy)) < Lag(sma5) &
      Cl(spy) > sma5 &
      rsi < 30,
    1,
    ifelse(Lag(Cl(spy)) > Lag(sma5) & Cl(spy) < sma5 & rsi > 70, -1, 0)
  ))
rsismatr[is.na(rsismatr)] <- 0

# 3.2.4. Stochastic Momentum Index SMI(13,2,25,9) and Simple Moving Average SMA(5) Trading Signals
# lineChart(spy)
# addSMI(n=13)
# addSMA(n=5,col=4)
# Price Crossover and Bands Crossover Confirmation Trading Signals
smismatr <-
  Lag(ifelse(
    Lag(Cl(spy)) < Lag(sma5) &
      Cl(spy) > sma5 &
      smi[, 1] < (-40),
    1,
    ifelse(Lag(Cl(spy)) > Lag(sma5) & Cl(spy) < sma5 &
             smi[, 1] > 40, -1, 0)
  ))
smismatr[is.na(smismatr)] <- 0

# 3.2.5. Williams %R(14) and Simple Moving Average SMA(5) Trading Signals
# lineChart(spy)
# addWPR(n=14)
# addSMA(n=5,col=4)
# Price Crossover and Bands Crossover Confirmation Trading Signals
wprsmatr <-
  Lag(ifelse(
    Lag(Cl(spy)) < Lag(sma5) &
      Cl(spy) > sma5 &
      wpr > 0.80,
    1,
    ifelse(Lag(Cl(spy)) > Lag(sma5) & Cl(spy) < sma5 & wpr < 0.20, -1, 0)
  ))
wprsmatr[is.na(wprsmatr)] <- 0

##########

# 4. Stock Trading Strategies

# 4.1. Moving Averages MA, Simple Moving Averages SMA(5 & 21), Exponential Moving Averages EMA(5 & 21) Trading Strategies

# Simple Moving Averages SMA(5 & 21) Price Crossover Trading Strategies

# Price Crossover Trading Strategies
sma5sig <- ifelse(sma5tr > 1, 0, 1)
for (i in 1:length(Cl(spy))) {
  sma5sig[i] <-
    ifelse(sma5tr[i] == 1, 1, ifelse(sma5tr[i] == -1, 0, sma5sig[i - 1]))
}
sma5sig[is.na(sma5sig)] <- 1
sma21sig <- ifelse(sma21tr > 1, 0, 1)
for (i in 1:length(Cl(spy))) {
  sma21sig[i] <-
    ifelse(sma21tr[i] == 1, 1, ifelse(sma21tr[i] == -1, 0, sma21sig[i - 1]))
}
sma21sig[is.na(sma21sig)] <- 1

# Exponential Moving Averages EMA (5 & 21) Price Crossover Trading Strategies

# Price Crossover Trading Strategies
ema5sig <- ifelse(ema5tr > 1, 0, 1)
for (i in 1:length(Cl(spy))) {
  ema5sig[i] <-
    ifelse(ema5tr[i] == 1, 1, ifelse(ema5tr[i] == -1, 0, ema5sig[i - 1]))
}
ema5sig[is.na(ema5sig)] <- 1
ema21sig <- ifelse(ema21tr > 1, 0, 1)
for (i in 1:length(Cl(spy))) {
  ema21sig[i] <-
    ifelse(ema21tr[i] == 1, 1, ifelse(ema21tr[i] == -1, 0, ema21sig[i - 1]))
}
ema21sig[is.na(ema21sig)] <- 1

# Simple & Exponential Moving Averages SMA(5 & 21) & EMA(5 & 21) Double Crossover Trading Strategies

# Double Crossover Trading Strategies
smasig <- ifelse(smatr > 1, 0, 1)
for (i in 1:length(Cl(spy))) {
  smasig[i] <-
    ifelse(smatr[i] == 1, 1, ifelse(smatr[i] == -1, 0, smasig[i - 1]))
}
smasig[is.na(smasig)] <- 1
emasig <- ifelse(ematr > 1, 0, 1)
for (i in 1:length(Cl(spy))) {
  emasig[i] <-
    ifelse(ematr[i] == 1, 1, ifelse(ematr[i] == -1, 0, emasig[i - 1]))
}
emasig[is.na(emasig)] <- 1

# 4.2. Bollinger Bands BB(20,2) Trading Strategy
# Bands Crossover Trading Strategy
bbsig <- ifelse(bbtr > 1, 0, 1)
for (i in 1:length(Cl(spy))) {
  bbsig[i] <- ifelse(bbtr[i] == 1, 1, ifelse(bbtr[i] == -1, 0, bbsig[i - 1]))
}
bbsig[is.na(bbsig)] <- 1

# 4.3. Parabolic Stop And Reverse SAR(0.02,0.2) Trading Strategy
# Stop And Reverse Trading Strategy
sarsig <- ifelse(sartr > 1, 0, 1)
for (i in 1:length(Cl(spy))) {
  sarsig[i] <-
    ifelse(sartr[i] == 1, 1, ifelse(sartr[i] == -1, 0, sarsig[i - 1]))
}
sarsig[is.na(sarsig)] <- 1

# 4.4. Average Directional Movement Index ADX(14) Trading Strategy
# Band and Double Crossover Trading Strategy
adxsig <- ifelse(adxtr > 1, 0, 1)
for (i in 1:length(Cl(spy))) {
  adxsig[i] <-
    ifelse(adxtr[i] == 1, 1, ifelse(adxtr[i] == -1, 0, adxsig[i - 1]))
}
adxsig[is.na(adxsig)] <- 1

# 4.5. Commodity Channel Index CCI(20,0.015) Trading Strategy
# Bands Crossover Trading Strategy
ccisig <- ifelse(ccitr > 1, 0, 1)
for (i in 1:length(Cl(spy))) {
  ccisig[i] <-
    ifelse(ccitr[i] == 1, 1, ifelse(ccitr[i] == -1, 0, ccisig[i - 1]))
}
ccisig[is.na(ccisig)] <- 1

# 4.6. Moving Averages Covergence/Divergence MACD(12,26,9) Trading Strategies
# Signal and Centerline Crossover Trading Strategies
smacdsig <- ifelse(smacdtr > 1, 0, 1)
for (i in 1:length(Cl(spy))) {
  smacdsig[i] <-
    ifelse(smacdtr[i] == 1, 1, ifelse(smacdtr[i] == -1, 0, smacdsig[i - 1]))
}
smacdsig[is.na(smacdsig)] <- 1
cmacdsig <- ifelse(cmacdtr > 1, 0, 1)
for (i in 1:length(Cl(spy))) {
  cmacdsig[i] <-
    ifelse(cmacdtr[i] == 1, 1, ifelse(cmacdtr[i] == -1, 0, cmacdsig[i - 1]))
}
cmacdsig[is.na(cmacdsig)] <- 1

# 4.7. Rate Of Change ROC(21) Trading Strategy
# Bands Crossover Trading Strategy
rocsig <- ifelse(roctr > 1, 0, 1)
for (i in 1:length(Cl(spy))) {
  rocsig[i] <-
    ifelse(roctr[i] == 1, 1, ifelse(roctr[i] == -1, 0, rocsig[i - 1]))
}
rocsig[is.na(rocsig)] <- 1

# 4.8. Relative Strength Index RSI(14) Trading Strategy
# Bands Crossover Trading Strategy
rsisig <- ifelse(rsitr > 1, 0, 1)
for (i in 1:length(Cl(spy))) {
  rsisig[i] <-
    ifelse(rsitr[i] == 1, 1, ifelse(rsitr[i] == -1, 0, rsisig[i - 1]))
}
rsisig[is.na(rsisig)] <- 1

# 4.9. Stochastic Momentum Index SMI(13,2,25,9) Trading Strategy
# Signal Crossover Trading Strategy
smisig <- ifelse(smitr > 1, 0, 1)
for (i in 1:length(Cl(spy))) {
  smisig[i] <-
    ifelse(smitr[i] == 1, 1, ifelse(smitr[i] == -1, 0, smisig[i - 1]))
}
smisig[is.na(smisig)] <- 1

# 4.10. Williams %R(14) Trading Strategy
# Bands Crossover Trading Strategy
wprsig <- ifelse(wprtr > 1, 0, 1)
for (i in 1:length(Cl(spy))) {
  wprsig[i] <-
    ifelse(wprtr[i] == 1, 1, ifelse(wprtr[i] == -1, 0, wprsig[i - 1]))
}
wprsig[is.na(wprsig)] <- 1

# 4.11. Commodity Channel Index CCI(20,0.015) and Simple Moving Average SMA(5) Trading Strategy
# Price Crossover and Bands Crossover Confirmation Trading Strategy
ccismasig <- ifelse(ccismatr > 1, 0, 1)
for (i in 1:length(Cl(spy))) {
  ccismasig[i] <-
    ifelse(ccismatr[i] == 1, 1, ifelse(ccismatr[i] == -1, 0, ccismasig[i -
                                                                         1]))
}
ccismasig[is.na(ccismasig)] <- 1

# 4.12. Rate Of Change ROC(21) and Simple Moving Average SMA(5) Trading Strategy
# Price Crossover and Bands Crossover Confirmation Trading Strategy
rocsmasig <- ifelse(rocsmatr > 1, 0, 1)
for (i in 1:length(Cl(spy))) {
  rocsmasig[i] <-
    ifelse(rocsmatr[i] == 1, 1, ifelse(rocsmatr[i] == -1, 0, rocsmasig[i -
                                                                         1]))
}
rocsmasig[is.na(rocsmasig)] <- 1

# 4.13. Relative Strength Index RSI(14) and Simple Moving Average SMA(5) Trading Strategy
# Price Crossover and Bands Crossover Confirmation Trading Strategy
rsismasig <- ifelse(rsismatr > 1, 0, 1)
for (i in 1:length(Cl(spy))) {
  rsismasig[i] <-
    ifelse(rsismatr[i] == 1, 1, ifelse(rsismatr[i] == -1, 0, rsismasig[i -
                                                                         1]))
}
rsismasig[is.na(rsismasig)] <- 1

# 4.14. Stochastic Momentum Index SMI(13,2,25,9) and Simple Moving Average SMA(5) Trading Strategy
# Price Crossover and Bands Crossover Confirmation Trading Strategy
smismasig <- ifelse(smismatr > 1, 0, 1)
for (i in 1:length(Cl(spy))) {
  smismasig[i] <-
    ifelse(smismatr[i] == 1, 1, ifelse(smismatr[i] == -1, 0, smismasig[i -
                                                                         1]))
}
smismasig[is.na(smismasig)] <- 1

# 4.15. Williams %R(14) and Simple Moving Average SMA(5) Trading Strategy
# Price Crossover and Bands Crossover Confirmation Trading Strategy
wprsmasig <- ifelse(wprsmatr > 1, 0, 1)
for (i in 1:length(Cl(spy))) {
  wprsmasig[i] <-
    ifelse(wprsmatr[i] == 1, 1, ifelse(wprsmatr[i] == -1, 0, wprsmasig[i -
                                                                         1]))
}
wprsmasig[is.na(wprsmasig)] <- 1

##########

# 5. Strategies Performance Comparison

# 5.1. Moving Averages MA, Simple Moving Averages SMA(5 & 21), Exponential Moving Averages EMA(5 & 21) Strategies Performance Comparison

# Simple Moving Averages SMA(5 & 21) Price Crossover Strategies Performance Comparison
# Price Crossover Strategy Returns/Equity Curve
ret <- ROC(Cl(spy))
ret[1] <- 0
bhstrat <- ret
sma5strat <- ret * sma5sig
sma5stratc <-
  ifelse((sma5tr == 1 |
            sma5tr == -1) & sma5sig != Lag(sma5sig),
         (ret - 0.01) * sma5sig,
         ret * sma5sig)
sma21strat <- ret * sma21sig
sma21stratc <-
  ifelse((sma21tr == 1 |
            sma21tr == -1) &
           sma21sig != Lag(sma21sig),
         (ret - 0.01) * sma21sig,
         ret * sma21sig
  )
# Price Crossover Strategy Performance Comparison
smacomp <-
  cbind(sma5strat, sma5stratc, sma21strat, sma21stratc, bhstrat)
colnames(smacomp) <-
  c("SMA(5)", "SMA(5) TC", "SMA(21)", "SMA(21) TC", "Buy & Hold")
table.AnnualizedReturns(smacomp)
charts.PerformanceSummary(smacomp)

# EMA (5 & 21) Price Crossover Strategies Performance Comparison
# Price Crossover Strategy Returns/Equity Curve
ema5strat <- ret * ema5sig
ema5stratc <-
  ifelse((ema5tr == 1 |
            ema5tr == -1) & ema5sig != Lag(ema5sig),
         (ret - 0.001) * ema5sig,
         ret * ema5sig)
ema21strat <- ret * ema21sig
ema21stratc <-
  ifelse((ema21tr == 1 |
            ema21tr == -1) &
           ema21sig != Lag(ema21sig),
         (ret - 0.001) * ema21sig,
         ret * ema21sig
  )
# Price Crossover Strategy Performance Comparison
emacomp <-
  cbind(ema5strat, ema5stratc, ema21strat, ema21stratc, bhstrat)
colnames(emacomp) <-
  c("EMA(5)", "EMA(5) TC", "EMA(21)", "EMA(21) TC", "Buy & Hold")
table.AnnualizedReturns(emacomp)
charts.PerformanceSummary(emacomp)

# Simple & Exponential Moving Averages SMA(5 & 21) & EMA(5 & 21) Double Crossover Strategies Performance Comparison
# Double Crossover Strategy Returns/Equity Curve
smastrat <- ret * smasig
smastratc <-
  ifelse((smatr == 1 |
            smatr == -1) & smasig != Lag(smasig),
         (ret - 0.01) * smasig,
         ret * smasig)
emastrat <- ret * emasig
emastratc <-
  ifelse((ematr == 1 |
            ematr == -1) & emasig != Lag(emasig),
         (ret - 0.01) * emasig,
         ret * emasig)
# Double Crossover Strategy Performance Comparison
macomp <- cbind(smastrat, smastratc, emastrat, emastratc, bhstrat)
colnames(macomp) <-
  c("SMA(5 & 21)",
    "SMA(5 & 21) TC",
    "EMA(5 & 21)",
    "EMA(5 & 21) TC",
    "Buy & Hold")
table.AnnualizedReturns(macomp)
charts.PerformanceSummary(macomp)

# 5.2. Bollinger Bands BB(20,2) Strategy Performance Comparison
# Bands Strategy Returns/Equity Curve
bbstrat <- ret * bbsig
bbstratc <-
  ifelse((bbtr == 1 |
            bbtr == -1) & bbsig != Lag(bbsig),
         (ret - 0.01) * bbsig,
         ret * bbsig)
# Bands Strategy Performance Comparison
bbcomp <- cbind(bbstrat, bbstratc, bhstrat)
colnames(bbcomp) <- c("BB(20,2)", "BB(20,2) TC", "Buy & Hold")
table.AnnualizedReturns(bbcomp)
charts.PerformanceSummary(bbcomp)

# 5.3. Parabolic Stop And Reverse SAR(0.02,0.2) Strategy Performance Comparison
# Stop And Reverse Strategy Returns/Equity Curve
sarstrat <- ret * sarsig
sarstratc <-
  ifelse((sartr == 1 |
            sartr == -1) & sarsig != Lag(sarsig),
         (ret - 0.01) * sarsig,
         ret * sarsig)
# Stop And Reverse Strategy Performance Comparison
sarcomp <- cbind(sarstrat, sarstratc, bhstrat)
colnames(sarcomp) <-
  c("SAR(0.02,0.2)", "SAR(0.02,0.2) TC", "Buy & Hold")
table.AnnualizedReturns(sarcomp)
charts.PerformanceSummary(sarcomp)

# 5.4. Average Directional Movement Index ADX(14) Strategy Performance Comparison
# Band and Double Crossover Strategy Returns/Equity Curve
adxstrat <- ret * adxsig
adxstratc <-
  ifelse((adxtr == 1 |
            adxtr == -1) & adxsig != Lag(adxsig),
         (ret - 0.01) * adxsig,
         ret * adxsig)
# Band and Double Crossover Strategy Performance Comparison
adxcomp <- cbind(adxstrat, adxstratc, bhstrat)
colnames(adxcomp) <- c("ADX(14)", "ADX(14) TC", "Buy & Hold")
table.AnnualizedReturns(adxcomp)
charts.PerformanceSummary(adxcomp)

# 5.5. Commodity Channel Index CCI(20,0.015) Strategy Performance Comparison
# Bands Crossover Strategy Returns/Equity Curve
ccistrat <- ret * ccisig
ccistratc <-
  ifelse((ccitr == 1 |
            ccitr == -1) & ccisig != Lag(ccisig),
         (ret - 0.01) * ccisig,
         ret * ccisig)
# Bands Crossover Strategy Performance Comparison
ccicomp <- cbind(ccistrat, ccistratc, bhstrat)
colnames(ccicomp) <-
  c("CCI(20,0.015)", "CCI(20,0.015) TC", "Buy & Hold")
table.AnnualizedReturns(ccicomp)
charts.PerformanceSummary(ccicomp)

# 5.6. Moving Averages Covergence/Divergence MACD(12,26,9) Strategies Performance Comparison
# Signal and Centerline Strategy Returns/Equity Curve
smacdstrat <- ret * smacdsig
smacdstratc <-
  ifelse((smacdtr == 1 |
            smacdtr == -1) &
           smacdsig != Lag(smacdsig),
         (ret - 0.005) * smacdsig,
         ret * smacdsig
  )
cmacdstrat <- ret * cmacdsig
cmacdstratc <-
  ifelse((cmacdtr == 1 |
            cmacdtr == -1) &
           cmacdsig != Lag(cmacdsig),
         (ret - 0.005) * cmacdsig,
         ret * cmacdsig
  )
# Signal and Centerline Strategy Performance Comparison
macdcomp <-
  cbind(smacdstrat, smacdstratc, cmacdstrat, cmacdstratc, bhstrat)
colnames(macdcomp) <-
  c(
    "MACD(12,26) vs Signal",
    "MACD(12,26) vs Signal TC",
    "MACD(12,26) vs Centerline",
    "MACD(12,26) vs Centerline TC",
    "Buy & Hold"
  )
table.AnnualizedReturns(macdcomp)
charts.PerformanceSummary(macdcomp)

# 5.7. Rate Of Change ROC(21) Strategy Performance Comparison
# Bands Crossover Strategy Returns/Equity Curve
rocstrat <- ret * rocsig
rocstratc <-
  ifelse((roctr == 1 |
            roctr == -1) & rocsig != Lag(rocsig),
         (ret - 0.01) * rocsig,
         ret * rocsig)
# Bands Crossover Strategy Performance Comparison
roccomp <- cbind(rocstrat, rocstratc, bhstrat)
colnames(roccomp) <- c("ROC(21)", "ROC(21) TC", "Buy & Hold")
table.AnnualizedReturns(roccomp)
charts.PerformanceSummary(roccomp)

# 5.8. Relative Strength Index RSI(14) Strategy Performance Comparison
# Bands Crossover Strategy Returns/Equity Curve
rsistrat <- ret * rsisig
rsistratc <-
  ifelse((rsitr == 1 |
            rsitr == -1) & rsisig != Lag(rsisig),
         (ret - 0.01) * rsisig,
         ret * rsisig)
# Bands Crossover Strategy Performance Comparison
rsicomp <- cbind(rsistrat, rsistratc, bhstrat)
colnames(rsicomp) <- c("RSI(14)", "RSI(14) TC", "Buy & Hold")
table.AnnualizedReturns(rsicomp)
charts.PerformanceSummary(rsicomp)

# 5.9. Stochastic Momentum Index SMI(13,2,25,9) Strategy Performance Comparison
# Signal Strategy Returns/Equity Curve
smistrat <- ret * smisig
smistratc <-
  ifelse((smitr == 1 |
            smitr == -1) & smisig != Lag(smisig),
         (ret - 0.01) * smisig,
         ret * smisig)
# Signal Strategy Performance Comparison
smicomp <- cbind(smistrat, smistratc, bhstrat)
colnames(smicomp) <-
  c("SMI(13,2,25) vs Signal", "SMI(13,2,25) vs Signal TC", "Buy & Hold")
table.AnnualizedReturns(smicomp)
charts.PerformanceSummary(smicomp)

# 5.10. Williams %R(14) Strategy Performance Comparison
# Bands Crossover Strategy Returns/Equity Curve
wprstrat <- ret * wprsig
wprstratc <-
  ifelse((wprtr == 1 |
            wprtr == -1) & wprsig != Lag(wprsig),
         (ret - 0.01) * wprsig,
         ret * wprsig)
# Bands Crossover Strategy Performance Comparison
wprcomp <- cbind(wprstrat, wprstratc, bhstrat)
colnames(wprcomp) <- c("%R(14)", "%R(14) TC", "Buy & Hold")
table.AnnualizedReturns(wprcomp)
charts.PerformanceSummary(wprcomp)

# 5.11. Commodity Channel Index CCI(20,0.015) and Simple Moving Average SMA(5) Strategy Performance Comparison
# Price Crossover and Bands Crossover Confirmation Strategy Returns/Equity Curve
ccismastrat <- ret * ccismasig
ccismastratc <-
  ifelse((ccismatr == 1 |
            ccismatr == -1) &
           ccismasig != Lag(ccismasig),
         (ret - 0.01) * ccismasig,
         ret * ccismasig
  )
# Price Crossover and Bands Crossover Confirmation Strategy Performance Comparison
ccismacomp <- cbind(ccismastrat, ccismastratc, bhstrat)
colnames(ccismacomp) <-
  c("SMA(5) and CCI(20,0.015)",
    "SMA(5) and CCI(20,0.015) TC",
    "Buy & Hold")
table.AnnualizedReturns(ccismacomp)
charts.PerformanceSummary(ccismacomp)

# 5.12. Rate Of Change ROC(21) and Simple Moving Average SMA(5) Strategy Performance Comparison
# Price Crossover and Bands Crossover Confirmation Strategy Returns/Equity Curve
rocsmastrat <- ret * rocsmasig
rocsmastratc <-
  ifelse((rocsmatr == 1 |
            rocsmatr == -1) &
           rocsmasig != Lag(rocsmasig),
         (ret - 0.01) * rocsmasig,
         ret * rocsmasig
  )
# Price Crossover and Bands Crossover Confirmation Strategy Performance Comparison
rocsmacomp <- cbind(rocsmastrat, rocsmastratc, bhstrat)
colnames(rocsmacomp) <-
  c("SMA(5) and ROC(21)", "SMA(5) and ROC(21) TC", "Buy & Hold")
table.AnnualizedReturns(rocsmacomp)
charts.PerformanceSummary(rocsmacomp)

# 5.13. Relative Strength Index RSI(14) and Simple Moving Average SMA(5) Strategy Performance Comparison
# Price Crossover and Bands Crossover Confirmation Strategy Returns/Equity Curve
rsismastrat <- ret * rsismasig
rsismastratc <-
  ifelse((rsismatr == 1 |
            rsismatr == -1) &
           rsismasig != Lag(rsismasig),
         (ret - 0.01) * rsismasig,
         ret * rsismasig
  )
# Price Crossover and Bands Crossover Confirmation Strategy Performance Comparison
rsismacomp <- cbind(rsismastrat, rsismastratc, bhstrat)
colnames(rsismacomp) <-
  c("SMA(5) and RSI(14)", "SMA(5) and RSI(14) TC", "Buy & Hold")
table.AnnualizedReturns(rsismacomp)
charts.PerformanceSummary(rsismacomp)

# 5.14. Stochastic Momentum Index SMI(13,2,25,9) and Simple Moving Average SMA(5) Strategy Performance Comparison
# Price Crossover and Bands Crossover Confirmation Strategy Returns/Equity Curve
smismastrat <- ret * smismasig
smismastratc <-
  ifelse((smismatr == 1 |
            smismatr == -1) &
           smismasig != Lag(smismasig),
         (ret - 0.01) * smismasig,
         ret * smismasig
  )
# Price Crossover and Bands Crossover Confirmation Strategy Performance Comparison
smismacomp <- cbind(smismastrat, smismastratc, bhstrat)
colnames(smismacomp) <-
  c("SMA(5) and SMI(13,2,25)",
    "SMA(5) and SMI(13,2,25) TC",
    "Buy & Hold")
table.AnnualizedReturns(smismacomp)
charts.PerformanceSummary(smismacomp)

# 5.15. Williams %R(14) and Simple Moving Average SMA(5) Strategy Performance Comparison
# Price Crossover and Bands Crossover Confirmation Strategy Returns/Equity Curve
wprsmastrat <- ret * wprsmasig
wprsmastratc <-
  ifelse((wprsmatr == 1 |
            wprsmatr == -1) &
           wprsmasig != Lag(wprsmasig),
         (ret - 0.01) * wprsmasig,
         ret * wprsmasig
  )
# Price Crossover and Bands Crossover Confirmation Strategy Performance Comparison
wprsmacomp <- cbind(wprsmastrat, wprsmastratc, bhstrat)
colnames(wprsmacomp) <-
  c("SMA(5) and %R(14)", "SMA(5) and %R(14) TC", "Buy & Hold")
table.AnnualizedReturns(wprsmacomp)
charts.PerformanceSummary(wprsmacomp)

##########
