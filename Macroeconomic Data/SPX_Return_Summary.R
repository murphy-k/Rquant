# Load R Packages
library("tseries")
library("quantmod")
library("Quandl")
library("PortfolioAnalytics")
library("PerformanceAnalytics")

# 1.0 Initial Settings
Start <- "1800-01-01"
End <- "2017-12-31"

mspx <-
  Quandl(
    "MULTPL/SP500_REAL_PRICE_MONTH",
    api_key = "5TJZgFo3SkynavER6dMR",
    type = "xts",
    collapse = "monthly",
    start_date = Start,
    end_date = End
  )

mspxret = mspx / Lag(mspx) - 1
mspxret[is.na(mspxret)] <- 0
colnames(mspxret) <- c("mspxret")
aspxret = mspx / Lag(mspx, 12) - 1
aspxret[is.na(aspxret)] <- 0
colnames(aspxret) <- c("aspxret")
plot(mspx,
     main = " S&P 500 Index",
     xlab = "Dates",
     ylab = "Monthly Prices")
plot(
  mspxret,
  main = "S&P 500 Index Returns",
  xlab = "Dates",
  ylab = "Monthly Returns",
  type = 'h',
  up.col = "black"
  
)
table.AnnualizedReturns(mspxret)
charts.PerformanceSummary(mspxret)

