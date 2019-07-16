# Pricing Options
library(fOptions)


dte <- function(n) {
  expTime <<- as.numeric(n / 365)
  print(expTime)
}

n <- as.Date("2019-05-10") - Sys.Date()
print(n)

dte(2)

type <- "c"
stockPrice <- 138
strikePrice <- 136
rateInterest <- 0.00
borrowRate <- 0.00
impliedVol <- 0.53

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
