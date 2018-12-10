# Volatility Trading Analysis with R
rm(list = ls())

# 1.0 Load R Packages ####
library("Quandl")
library("quantmod")
library("PerformanceAnalytics")

# 2.0 Data Downloading ####
tickers <- c("^SP500TR", "^BXM", "SPY", "PBP", "PUTW")
getSymbols(tickers,
           src = 'yahoo',
           from = "2007-12-31",
           to = Sys.Date())
odata <-
  cbind(SP500TR[, 6], BXM[, 6], PBP[, 6], SPY[, 6], PUTW[, 6])

# 2.1 Returns Risk Assessment Data
ospxdata <- odata$SP500TR.Adjusted
ospxdata <- ospxdata[complete.cases(ospxdata), ]
bxmddata <- odata$BXM.Adjusted
bxmddata <- bxmddata[complete.cases(bxmddata)]

# 3.0 Returns Risk Assessment ####

# 3.1. Historical Stocks Index Monthly Returns
spxmret <- ospxdata / Lag(ospxdata, k = 21) - 1
spxmret <- spxmret[complete.cases(spxmret), ]
plot.zoo(spxmret, main = "Historical Stocks Index Monthly Returns", ylim = c(-0.35, 0.35))
abline(h = mean(spxmret), col = "blue")
abline(h = mean(spxmret) + 3 * sd(spxmret), col = "red")
abline(h = mean(spxmret) - 3 * sd(spxmret), col = "red")
abline(h = mean(spxmret) + 2 * sd(spxmret), col = "orange")
abline(h = mean(spxmret) - 2 * sd(spxmret), col = "orange")
abline(h = mean(spxmret) + 1 * sd(spxmret), col = "yellow")
abline(h = mean(spxmret) - 1 * sd(spxmret), col = "yellow")


# 3.2. Historical Stocks Index Monthly Returns Normality
qqnorm(spxmret)
qqline(spxmret, col = "red")
