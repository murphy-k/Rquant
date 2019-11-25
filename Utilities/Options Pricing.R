# Options Pricing
library(fOptions)

dte <- function(n) {
  expTime <<- as.numeric(n / 365)
  print(expTime)
}

n <- as.Date("2019-08-23") - Sys.Date()
print(n)
dte(1)

# Construct the options contract
type <- "c"
stockPrice <- 157
strikePrice <- 147
rateInterest <- 0.00
borrowRate <- 0.00
impliedVol <- 0.0

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
