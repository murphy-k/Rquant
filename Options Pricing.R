# Pricing Options
library(fOptions)
rm(list=ls())

dte <- function(n) {
  expTime <<- n / 365
  print(expTime)
}
dte(8)
type <- "c"
stockPrice <- 27
strikePrice <- 26
rateInterest <- 0.01
borrowRate <- 0.00
impliedVol <- 0.46

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
