# Pricing Options
library(fOptions)


dte <- function(n) {
  expTime <<- as.numeric(n / 365)
  print(expTime)
}

<<<<<<< HEAD:Utilities/Options Pricing.R
n <- as.Date("2019-07-19") - Sys.Date()
print(n)

dte(70)

type <- "c"
stockPrice <- 25.5
strikePrice <- 26
rateInterest <- 0.00
borrowRate <- 0.00
impliedVol <- 0.2867
=======
n <- as.Date("2019-05-10") - Sys.Date()
print(n)

dte(2)

type <- "c"
stockPrice <- 138
strikePrice <- 136
rateInterest <- 0.00
borrowRate <- 0.00
impliedVol <- 0.53
>>>>>>> 2979aa0ba5f4541d2b82b07527df7e42ac043b1f:Quantitative Analysis/Options Pricing.R

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
