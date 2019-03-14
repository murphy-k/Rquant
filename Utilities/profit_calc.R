# 1. Load R packages
library(quantstrat)
rm(list = ls())
# VWAP Calculation for multiple orders
buy_sizes <- c(100, 100, 4350, 100, 20350)
buy_prices <- c(1.77, 1.78, 1.79, 1.78, 1.80)
total_bought <- sum(buy_sizes)

buy_cost_basis <-
  sum((buy_sizes * buy_prices) / sum(buy_sizes))

sell_sizes <- c(1000, 614, 555, 1345, 4755, 1400, 3975, 1200, 10156)
sell_prices <- c(1.99, 1.95, 1.93, 1.92, 1.94, 1.91, 1.90, 1.88, 1.87)
total_sold <- sum(sell_sizes)

sell_cost_basis <- sum((sell_sizes*sell_prices) / sum(sell_sizes))

profit <- round((sell_cost_basis - buy_cost_basis) * total_sold,digits = 2)
print(paste("$", profit))
