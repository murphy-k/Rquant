# 1. Load R packages
library(quantstrat)

# VWAP Calculation for multiple orders
orderSizes <- c(15, 85, 100)
orderPrices <- c(821.625, 820.60, 825)

price_VWAP <-
  sum((orderSizes * orderPrices)/sum(orderSizes))


price_VWAP
