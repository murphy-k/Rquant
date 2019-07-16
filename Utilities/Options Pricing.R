# Pricing Options
library(fOptions)


dte <- function(n) {
  expTime <<- as.numeric(n / 365)
  print(expTime)
}

n <- as.Date("2019-07-19") - Sys.Date()
print(n)

dte(70)

type <- "c"
stockPrice <- 25.5
strikePrice <- 26
rateInterest <- 0.00
borrowRate <- 0.00
impliedVol <- 0.2867

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
