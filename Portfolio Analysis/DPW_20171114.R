# S&P 500 Index
getSymbols("SPY",
           src = "yahoo",
           from = "2007-11-13",
           to = "2017-11-13")
mSPY = to.monthly(SPY)
mSPY = mSPY[, 6]
mSPYret = mSPY / Lag(mSPY) - 1
mSPYret[is.na(mSPYret)] <- 0
colnames(mSPYret) <- c("mSPYret")
aSPYret = mSPY / Lag(mSPY, 12) - 1
aSPYret[is.na(aSPYret)] <- 0
colnames(aSPYret) <- c("aSPYret")
plot(mSPY,
     main = "U.S. SPY Cap Stock Market Prices",
     xlab = "Dates",
     ylab = "Monthly Prices")
plot(mSPYret,
     main = "U.S. SPY Cap Stock Market Returns",
     xlab = "Dates",
     ylab = "Monthly Returns")
table.AnnualizedReturns(mSPYret)
charts.PerformanceSummary(mSPYret)

# Vanguard Growth Index
getSymbols("VIGRX",
           src = "yahoo",
           from = "2007-11-13",
           to = "2017-11-13")
mVIGRX = to.monthly(VIGRX)
mVIGRX = mVIGRX[, 6]
mVIGRXret = mVIGRX / Lag(mVIGRX) - 1
mVIGRXret[is.na(mVIGRXret)] <- 0
colnames(mVIGRXret) <- c("mVIGRXret")
aVIGRXret = mVIGRX / Lag(mVIGRX, 12) - 1
aVIGRXret[is.na(aVIGRXret)] <- 0
colnames(aVIGRXret) <- c("alargeret")
plot(mVIGRX,
     main = "Vanguard Growth Index Investor Fund Prices",
     xlab = "Dates",
     ylab = "Monthly Prices")
plot(mVIGRXret,
     main = "Vanguard Growth Index Investor Fund Returns",
     xlab = "Dates",
     ylab = "Monthly Returns")
table.AnnualizedReturns(mVIGRXret)
charts.PerformanceSummary(mVIGRXret)


getSymbols("VIVAX",
           src = "yahoo",
           from = "2007-11-13",
           to = "2017-11-13")
mVIVAX = to.monthly(VIVAX)
mVIVAX = mVIVAX[, 6]
mVIVAXret = mVIVAX / Lag(mVIVAX) - 1
mVIVAXret[is.na(mVIVAXret)] <- 0
colnames(mVIVAXret) <- c("mVIVAXret")
aVIVAXret = mVIVAX / Lag(mVIVAX, 12) - 1
aVIVAXret[is.na(aVIVAXret)] <- 0
colnames(aVIVAXret) <- c("alargeret")
plot(mVIVAX,
     main = "Vanguard Value Index Investor Fund Prices",
     xlab = "Dates",
     ylab = "Monthly Prices")
plot(mVIVAXret,
     main = "Vanguard Value Index Investor Fund Returns",
     xlab = "Dates",
     ylab = "Monthly Returns")
table.AnnualizedReturns(mVIVAXret)
charts.PerformanceSummary(mVIVAXret)

# 2.3.10. U.S. Market Cap Stock Markets Returns Comparison
comp = cbind(mSPYret, mVIGRXret, mVIVAXret)
table.AnnualizedReturns(comp)
charts.PerformanceSummary(comp)

