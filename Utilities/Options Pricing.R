# Pricing Options
library(fOptions)


dte <- function(n) {
  expTime <<- as.numeric(n / 365)
  print(expTime)
}

n <- as.Date("2019-05-17") - Sys.Date()
print(n)

dte(n)

type <- "c"
stockPrice <- 32.39
strikePrice <- 35
rateInterest <- 0.00
borrowRate <- 0.00
impliedVol <- 0.2986

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
