# Pricing Options
library(fOptions)


dte <- function(n) {
  expTime <<- as.numeric(n / 365)
  print(expTime)
}

n <- as.Date("2019-05-17") - Sys.Date()
print(n)

dte(23)

type <- "c"
stockPrice <- 30
strikePrice <- 40
rateInterest <- 0.00
borrowRate <- 0.00
impliedVol <- 0.71

# Vanilla Pricing ####
GBSOption(
  TypeFlag = type,
  S = stockPrice,
  X = strikePrice,
  Time = expTime,
  r = rateInterest,
  b = borrowRate,
  sigma = impliedVol
)

# Greeks ####
GBSGreeks(
  Selection = "delta",
  TypeFlag = type,
  S = stockPrice,
  X = strikePrice,
  Time = expTime,
  r = rateInterest,
  b = borrowRate,
  sigma = impliedVol
)  
