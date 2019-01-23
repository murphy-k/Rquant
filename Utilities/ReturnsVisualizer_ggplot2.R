# ggplot2 Returns Visualizing workspace
rm(list = ls())
dev.off(dev.list()["RStudioGD"])
library(dplyr)
library(quantmod)
library(ggplot2)
library(magrittr)

# Data Download
ticker <- "DAN"
lookback <- 1000
getSymbols(ticker,
           src = "yahoo",
           from = "2007-01-01",
           warnings = FALSE)

# Subset data
x <-
  window(get(ticker), start = (Sys.Date()-lookback), end = Sys.Date())
chartSeries(x)

# Convert returns to DailyReturn percentage
x_ret <- dailyReturn(x)
x_ret <- as.xts(x_ret)
acf(x_ret, lag.max = 100)
# View Returns as a line plot
ggplot(data = x_ret, aes(x = Index , y = x_ret$daily.returns)) +
  geom_line() +
  geom_hline(yintercept = mean(x_ret),
             color = "blue",
             linetype = "dashed") +
  geom_hline(
    yintercept = c(
      mean(x_ret) + sd(x_ret),
      mean(x_ret) + (sd(x_ret) * 2),
      mean(x_ret) - sd(x_ret),
      mean(x_ret) - (sd(x_ret) * 2)
    ),
    color = "black",
    linetype = "solid"
  )

# View returns as a histogram
ggplot(data = x_ret, aes(x_ret$daily.returns)) +
  geom_histogram(bins = 200,
                 aes(y = ..density..),
                 colour = "black",
                 fill = "white") +
  geom_density(alpha = .2, fill = "white") +
  geom_vline(
    aes(xintercept = mean(x_ret$daily.returns)),
    color = "blue",
    linetype = "dashed",
    size = 1
  ) +
  geom_vline(aes(xintercept = mean(x_ret) + sd(x_ret))) +
  geom_vline(aes(xintercept = mean(x_ret) - sd(x_ret))) +
  geom_vline(aes(xintercept = mean(x_ret) + (sd(x_ret) * 2))) +
  geom_vline(aes(xintercept = mean(x_ret) - (sd(x_ret) * 2)))

zscore <- function(z, p) {
  round(((p - mean(z)) / sd(z)), digits = 5)
}
z_score <- zscore(x_ret, p = 0.1)
z_score
pnorm(z_score, lower.tail = FALSE)
