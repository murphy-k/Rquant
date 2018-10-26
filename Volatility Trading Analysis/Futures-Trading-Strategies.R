# Volatility Trading Analysis with R
# Futures Trading Strategies

# 1. Load R Packages
library("Quandl")
Quandl.api_key("shVi-_QjPbmUAfvVBMzw")
library("TTR")
library("quantmod")
library("PerformanceAnalytics")

# 2. Data Downloading
ftickers <- c("CBOE/VIX/4", "CHRIS/CBOE_VX1/4", "CBOE/SKEW")
ftickers2 <- c("^GSPC", "^SP500TR", "SPY", "SPXH", "TRSK")
fdata <- Quandl(ftickers, type = "xts", end_date = "2017-01-01")
getSymbols(ftickers2, src = 'yahoo', to = "2017-01-01")
fdata <- cbind(GSPC[, 1:4], fdata[, 1:2], SP500TR[, 6], fdata[, 3])
fdata2 <- cbind(SPY[, 6], SPXH[, 6], TRSK[, 6])

# 2.1. Volatility and Stocks Data
vsdata <- cbind(fdata[, 5], fdata[, 7])
vsdata <- vsdata[complete.cases(vsdata), ]

# 2.2. Volatility Risk Premium Data
vrpdata <- fdata[, 1:5]
vrpdata <- vrpdata[complete.cases(vrpdata), ]

# 2.3. Volatility Term Structure Data
vtsdata <- window(fdata[, 5:6], start = "2007-04-01")
vtsdata <- vtsdata[complete.cases(vtsdata), ]

# 2.4. Volatility Skew Data
vskew <- fdata[, 8]
vskew <- vskew[complete.cases(vskew), ]

# 2.5. Volatility Risk Assessment Data
vradata <- fdata[, 1:4]
vradata <- vradata[complete.cases(vradata), ]

# 2.6. Volatility Hedge Futures Data
spxhdata <- fdata2[, 1:2]
spxhdata <- spxhdata[complete.cases(spxhdata), ]

# 2.7. Volatility Tail Hedge Futures Data
trskdata <- cbind(fdata2[, 1], fdata2[, 3])
trskdata <- trskdata[complete.cases(trskdata), ]

# 3. Volatility and Stocks

# 3.1. Volatility and Stocks Chart
corvix <- vsdata[, 1] / 100
corspxtr <- vsdata[, 2]
par(mfcol = c(1, 2))
plot(corspxtr, main = "S&P500TR")
plot(corvix, main = "VIX")
par(mfcol = c(1, 1))
par(mfrow = c(1, 1))

# 3.2. Volatility and Stocks Returns Correlation
corvixret <- dailyReturn(corvix, type = "log")
corspxtrret <- dailyReturn(corspxtr, type = "log")
cor(corspxtrret, corvixret)

# 4. Volatility Risk Premium

# 4.1. Volatility Risk Premium Calculation
fspxohlc <- vrpdata[, 1:4]
vix <- Lag(vrpdata[, 5] / 100, k = 21)
hvol <- volatility(fspxohlc,
                   calc = "close",
                   n = 21,
                   N = 252)
vrp <- vix - hvol
vrpsma21 <- SMA(vrp, n = 21)

# 4.2. VIX vs Historical CC Volatility Chart
plot(hvol, main = "VIX vs Historical CC Volatility")
lines(vix, col = "blue")
legend(
  "topright",
  col = c("black", "blue"),
  lty = 1,
  legend = c("hvol", "vix")
)

# 4.3. Volatility Risk Premium Chart
plot(vrpsma21, main = "Volatility Risk Premium")
abline(h = 0, col = "blue")
legend("bottomright",
       col = "black",
       lty = 1,
       legend = "vrpsma21")

# 5. Volatility Term Structure

# 5.1. Volatility Term Structure Calculation
svix <- vtsdata[, 1] / 100
fvix1 <- vtsdata[, 2] / 100
vts <- svix - fvix1
vtssma21 <- SMA(vts, n = 21)

# 5.2. VIX vs 1 Month VIX Futures Chart
plot(svix, main = "VIX vs 1 Month VIX Futures Chart")
lines(fvix1, col = "orange")
legend(
  "topright",
  col = c("black", "orange"),
  lty = 1,
  legend = c("svix", "fvix1")
)

# 5.3. Volatility Term Structure Chart
plot(vtssma21, main = "Volatility Term Structure")
abline(h = 0, col = "orange")
legend("topright",
       col = "black",
       lty = 1,
       legend = "vtssma21")

# 6. Volatility Skew
vskewsma21 <- SMA(vskew, n = 21)
plot(vskew, main = "Volatility Skew")
lines(vskewsma21, col = "red")
legend(
  "topleft",
  col = c("black", "red"),
  lty = 1,
  legend = c("vskew", "vskewsma21")
)

# 7. Volatility Risk Assessment

# 7.1. Historical Stocks Volatility Index
spxvol <- volatility(vradata,
                     calc = "close",
                     n = 21,
                     N = 252) * 100
plot(spxvol, main = "Historical Stocks Volatility Index")
legend("topleft",
       col = "black",
       lty = 1,
       legend = "spxvol")

# 7.2. Historical Stocks Volatility Index Monthly Differences
spxvolmdiff <- spxvol - Lag(spxvol, k = 21)
spxvolmdiff <- spxvolmdiff[complete.cases(spxvolmdiff), ]
plot(spxvolmdiff, main = "Historical Stocks Volatility Index Monthly Differences")
abline(h = mean(spxvolmdiff), col = "orange")
abline(h = mean(spxvolmdiff) + 3 * sd(spxvolmdiff), col = "green")
abline(h = mean(spxvolmdiff) - 3 * sd(spxvolmdiff), col = "green")
legend(
  "bottomright",
  col = c("black", "orange", "green"),
  lty = 1,
  legend = c("spxvolmdiff", "mean", "+/- 3 stdev"),
  cex = 0.75
)

# 7.3. Historical Stocks Volatility Index Monthly Differences Normality
qqnorm(spxvolmdiff)
qqline(spxvolmdiff, col = "red")

# 8. Volatility Hedge Futures Strategy
fspyret1 <- dailyReturn(spxhdata[, 1], type = "arithmetic")
spxhret <- dailyReturn(spxhdata[, 2], type = "arithmetic")
colnames(fspyret1) <- "fspyret1"
colnames(spxhret) <- "spxhret"
spxhcomp <- cbind(spxhret, fspyret1)
charts.PerformanceSummary(spxhcomp)
table.AnnualizedReturns(spxhcomp)
chart.RiskReturnScatter(spxhcomp)
sapply(spxhcomp, max)
sapply(spxhcomp, min)
skewness(spxhcomp)
kurtosis(spxhcomp)

# 9. Volatility Tail Hedge Futures Strategy
fspyret2 <- dailyReturn(trskdata[, 1], type = "arithmetic")
trskret <- dailyReturn(trskdata[, 2], type = "arithmetic")
colnames(fspyret2) <- "fspyret2"
colnames(trskret) <- "trskret"
trskcomp <- cbind(trskret, fspyret2)
charts.PerformanceSummary(trskcomp)
table.AnnualizedReturns(trskcomp)
chart.RiskReturnScatter(trskcomp)
sapply(trskcomp, max)
sapply(trskcomp, min)
skewness(trskcomp)
kurtosis(trskcomp)
