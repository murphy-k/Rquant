# ggplot2 Returns Visualizing workspace
rm(list = ls())
dev.off(dev.list()["RStudioGD"])
library(dplyr)
library(quantmod)
library(ggplot2)
library(magrittr)

# Data Download
ticker <- "SPY"
start_date <- "2018-01-01"
end_date <- Sys.Date()
getSymbols(
  ticker,
  src = "yahoo",
  from = start_date,
  to = end_date,
  warnings = FALSE
)

# Subset data
x <-
  window(get(ticker), start = start_date, end = end_date)
chartSeries(x)

# Convert returns to Return percentage
x_ret <- dailyReturn(x)
x_ret <- as.xts(x_ret)
acf(x_ret, lag.max = 100)
acf(x, lag.max = 100)
# View instrument as a line plot
ggplot(data = x, aes(x = Index , y = x[, 1])) +
  geom_line()

# View Returns as a line plot
ggplot(data = x_ret, aes(x = Index , y = x_ret[, 1])) +
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
ggplot(data = x_ret, aes(x_ret[, 1])) +
  geom_histogram(
    bins = (length(x_ret) * 0.5),
    aes(y = ..density..),
    colour = "black",
    fill = "white"
  ) +
  geom_density(alpha = .2, fill = "white") +
  geom_vline(
    aes(xintercept = mean(x_ret[, 1])),
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
z_score <- zscore(x_ret, p = last(x_ret))
z_score
pnorm(z_score, lower.tail = FALSE)
