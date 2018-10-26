# Volatility Trading Analysis with R
# Volatility Analysis

# 1. Load R Packages
library("Quandl")
Quandl.api_key("shVi-_QjPbmUAfvVBMzw")
library("TTR")
library("forecast")
library("quantmod")
library("fGarch") # Load only after estimating historical volatilities
# unloadNamespace("fGarch") # Use first if TTR volatility function is masked
# unloadNamespace("fBasics") # Use second if TTR volatility function is masked

# 2. Data Downloading
htickers <- "CBOE/VIX/4"
htickers2 <- "^GSPC"
hdata <-
  Quandl(htickers,
         type = "xts",
         start_date = "2007-01-01",
         end_date = "2017-01-01")
getSymbols(htickers2,
           src = 'yahoo',
           from = "2007-01-01",
           to = "2017-01-01")
hdata <- cbind(GSPC[, 1:4], hdata)
hdata <- hdata[complete.cases(hdata), ]

# 3. Historical Volatility Estimation
hspxohlc <- hdata[, 1:4]

# 3.1. Close to Close Estimation
hvolcc <- volatility(hspxohlc,
                     calc = "close",
                     n = 21,
                     N = 252)
# 3.1.1. Close to Close Estimation Chart
plot(hvolcc, main = "Close to Close Volatility Estimation")
legend("topright",
       col = "black",
       lty = 1,
       legend = "cc")

# 3.2. Parkinson Estimation
hvolp <- volatility(hspxohlc,
                    calc = "parkinson",
                    n = 21,
                    N = 252)
# 3.2.1. Parkinson Estimation Efficiency
hvolper <- hvolp ^ 2 / hvolcc ^ 2
max(hvolper[21:2518])
mean(hvolper[21:2518])
# 3.2.2. Parkinson Estimation Comparison Chart
plot(hvolcc, main = "Parkinson Estimation Comparison Chart")
lines(hvolp, col = "blue")
legend(
  "topright",
  col = c("black", "blue"),
  lty = 1,
  legend = c("cc", "p")
)

# 3.3. Garman-Klass Estimation
hvolgk <- volatility(hspxohlc,
                     calc = "garman.klass",
                     n = 21,
                     N = 252)
# 3.3.1. Garman-Klass Estimation Efficiency
hvolgker <- hvolgk ^ 2 / hvolcc ^ 2
max(hvolgker[21:2518])
mean(hvolgker[21:2518])
# 3.3.2. Garman-Klass Estimation Comparison Chart
plot(hvolcc, main = "Garman-Klass Estimation Comparison Chart")
lines(hvolgk, col = "red")
legend(
  "topright",
  col = c("black", "red"),
  lty = 1,
  legend = c("cc", "gk")
)

# 3.4. Rogers-Satchell Estimation
hvolrs <- volatility(hspxohlc,
                     calc = "rogers.satchell",
                     n = 21,
                     N = 252)
# 3.4.1. Rogers-Satchell Estimation Efficiency
hvolrser <- hvolrs ^ 2 / hvolcc ^ 2
max(hvolrser[21:2518])
mean(hvolrser[21:2518])
# 3.4.2. Rogers-Satchell Estimation Comparison Chart
plot(hvolcc, main = "Rogers-Satchell Estimation Comparison Chart")
lines(hvolrs, col = "orange")
legend(
  "topright",
  col = c("black", "orange"),
  lty = 1,
  legend = c("cc", "rs")
)

# 3.5. Garman-Klass-Yang-Zhang Estimation
hvolgkyz <- volatility(hspxohlc,
                       calc = "gk.yz",
                       n = 21,
                       N = 252)
# 3.5.1. Garman-Klass-Yang-Zhang Estimation Efficiency
hvolgkyzer <- hvolgkyz ^ 2 / hvolcc ^ 2
max(hvolgkyzer[22:2518])
mean(hvolgkyzer[22:2518])
# 3.5.2. Garman-Klass-Yang-Zhang Estimation Comparison Chart
plot(hvolcc, main = "Garman-Klass-Yang-Zhang Estimation Comparison Chart")
lines(hvolgkyz, col = "green")
legend(
  "topright",
  col = c("black", "green"),
  lty = 1,
  legend = c("cc", "gkyz")
)

# 3.6. Yang-Zhang Estimation
hvolyz <- volatility(hspxohlc,
                     calc = "yang.zhang",
                     n = 21,
                     N = 252)
# 3.6.1. Yang-Zhang Estimation Efficiency
hvolyzer <- hvolyz ^ 2 / hvolcc ^ 2
max(hvolyzer[22:2518])
mean(hvolyzer[22:2518])
# 3.6.2. Yang-Zhang Estimation Comparison Chart
plot(hvolcc, main = "Yang-Zhang Estimation Comparison Chart")
lines(hvolyz, col = "cyan")
legend(
  "topright",
  col = c("black", "cyan"),
  lty = 1,
  legend = c("cc", "yz")
)

# 3.7. Historical Volatility Estimation Efficiency Comparison
hvoler <- cbind(hvolper, hvolgker, hvolrser, hvolgkyzer, hvolyzer)
apply(hvoler[22:2518], 2, max)
apply(hvoler[22:2518], 2, mean)

# 4. Historical Volatility Forecasting
hvolcc21 <- Lag(hvolcc, k = 21)

# 4.1. Random Walk Forecast
hrwfcst <- hvolcc21
# 4.1.1. Random Walk Forecast Chart
plot(hvolcc, main = "Historical CC Volatility vs Random Walk Forecast")
lines(hrwfcst, col = "blue")
legend(
  "topright",
  col = c("black", "blue"),
  lty = 1,
  legend = c("cc", "rwfcst")
)

# 4.2. Historical Mean Forecast
hmeanfcst <- mean(hvolcc21[42:2518])
# 4.2.2. Historical Mean Forecast Chart
plot(hvolcc, main = "Historical CC Volatility vs Mean Forecast")
abline(h = hmeanfcst, col = "red")
legend(
  "topright",
  col = c("black", "red"),
  lty = 1,
  legend = c("cc", "meanfcst")
)

# 4.3. Simple Moving Average Forecast
hsmafcst <- SMA(hvolcc21, n = 21)
# 4.3.1. SMA Forecast Chart
plot(hvolcc, main = "Historical CC Volatility vs SMA Forecast")
lines(hsmafcst, col = "orange")
legend(
  "topright",
  col = c("black", "orange"),
  lty = 1,
  legend = c("cc", "smafcst")
)

# 4.4. Exponentially Weighted Moving Average Forecast
hemafcst <- EMA(hvolcc21, n = 21)
# 4.4.1. EMA Forecast Chart
plot(hvolcc, main = "Historical CC Volatility vs EMA Forecast")
lines(hemafcst, col = "green")
legend(
  "topright",
  col = c("black", "green"),
  lty = 1,
  legend = c("cc", "emafcst")
)

# 4.5. Autoregressive Integrated Moving Average Forecast

# 4.5.1. Estimate Best ARIMA Model
bestarimamodel <- auto.arima(hvolcc21)
bestarimamodel

# 4.5.2. Calcuate ARIMA Forecast with Best Model Characteristics
harimamodel <- arima(hvolcc21, order = c(0, 1, 0), include.mean = T)
harimafcst <- fitted.values(harimamodel)

# 4.5.3. Create Undated Historical CC Volatility Time Series
harimabind <- cbind(hvolcc, harimafcst)
colnames(harimabind) <- c("volcc", "harimafcst")
uhvolcc <- harimabind[, 1]

# 4.5.4. ARIMA Forecast Chart
plot(uhvolcc, type = "l", main = "Historical CC Volatility vs ARIMA Forecast")
lines(harimafcst, col = "cyan")
legend(
  "topright",
  col = c("black", "cyan"),
  lty = 1,
  legend = c("cc", "arimafcst")
)

# 4.6. General Autoregressive Conditional Heteroscedasticity Forecast

# 4.6.1. Calculate GARCH Forecast with Best ARIMA Model Forecast
hgarchmodel <-
  garchFit(formula = ~ garch(1, 1), data = harimafcst[42:2518])
# GARCH Forecast Calculated with Fixed Variance and Conditional Standard Deviation
hgarchvar <- hgarchmodel@fitted
hgarchsigma <- hgarchmodel@sigma.t
hgarchfcst <- hgarchvar + hgarchsigma

# 4.6.2. GARCH Forecast Chart
plot(uhvolcc, type = "l", main = "Historical CC Volatility vs GARCH Forecast")
lines(Lag(hgarchfcst, k = 42), col = "pink")
legend(
  "topright",
  col = c("black", "pink"),
  lty = 1,
  legend = c("cc", "garchfcst")
)

# 4.7. Historical Volatility Forecasting Accuracy

# 4.7.1. Create Undated Volatility Forecast Time Series
hfcst <-
  cbind(uhvolcc,
        hrwfcst,
        hmeanfcst,
        hsmafcst,
        hemafcst,
        harimafcst,
        hgarchfcst)
colnames(hfcst) <-
  cbind("hvolcc",
        "hrwfcst",
        "hmeanfcst",
        "hsmafcst",
        "hemafcst",
        "harimafcst",
        "hgarchfcst")
uhvolcc2 <- uhvolcc[62:2518]
uhrwfcst <- hfcst[62:2518, 2]
uhmeanfcst <- hfcst[62:2518, 3]
uhsmafcst <- hfcst[62:2518, 4]
uhemafcst <- hfcst[62:2518, 5]
uharimafcst <- hfcst[62:2518, 6]
uhgarchfcst <- hfcst[62:2518, 7]

# 4.7.2. Calculate Historical Volatility Forecasting Accuracy
accuracy(uhrwfcst, uhvolcc2)
accuracy(uhmeanfcst, uhvolcc2)
accuracy(uhsmafcst, uhvolcc2)
accuracy(uhemafcst, uhvolcc2)
accuracy(uharimafcst, uhvolcc2)
accuracy(uhgarchfcst, uhvolcc2)

# 5. Implied Volatility
ivol <- Lag(hdata[, 5] / 100, k = 21)

# 5.1. VIX Chart
plot(hvolcc, type = "l", main = "Historical CC Volatility vs VIX Implied Volatility")
lines(ivol, col = "blue")
legend(
  "topright",
  col = c("black", "blue"),
  lty = 1,
  legend = c("cc", "pvix")
)

# 5.2. Implied Volatility Forecasting Accuracy

# 5.2.1. Created Undated VIX Time Series
ifcst <- cbind(ivol, harimafcst)
colnames(ifcst) <- c("ivol", "harimafcst")
uivol <- ifcst[62:2518, 1]

# 5.2.2. Calculate Implied Volatility Forecasting Accuracy
accuracy(uivol, uhvolcc2)
