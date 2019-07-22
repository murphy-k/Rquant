# ggplot2 Returns Visualizing workspace
library(dplyr)
library(quantmod)
library(ggplot2)
library(magrittr)

rm(list = ls())
dev.off(dev.list()["RStudioGD"])

# Data ####
ticker <- "SPY"
start_date <- "2000-01-01"
end_date <- Sys.Date()
getSymbols(
  ticker,
  src = "yahoo",
  from = start_date,
  to = end_date,
  warnings = FALSE
)

# Subset ####
x <-
  window(get(ticker), start = start_date, end = end_date)


# Returns ####
x_ret <- dailyReturn(x, type = "log")
x_ret <- as.xts(x_ret)
acf(x_ret, lag.max = sqrt(length(x_ret)))

mean_x_ret <- round(mean(x_ret), digits = 4) * 100
sd_x_ret <- round(sd(x_ret), digits = 4) * 100

# Stock Plot ####
p_price <- ggplot(data = x, aes(x = Index , y = x$SPY.Close)) +
  geom_line() +
  labs(title = paste(ticker, "Daily Stock Price")) +
  xlab("Date") +
  ylab("Price ($)") +
  scale_y_log10() +
  geom_hline(
    yintercept = last(x$SPY.Close),
    color = "black",
    linetype = "dashed"
  )
p_price

# Returns Line ####
p_returns_line <-
  ggplot(data = x_ret, aes(x = Index , y = (x_ret[, 1]) * 100)) +
  geom_col() +
  geom_hline(yintercept = mean(x_ret),
             color = "blue",
             linetype = "dashed") +
  geom_hline(
    yintercept = c(
      mean_x_ret + sd_x_ret,
      mean_x_ret + (sd_x_ret * 2),
      mean_x_ret - sd_x_ret,
      mean_x_ret - (sd_x_ret * 2)
    ),
    color = "black",
    linetype = "solid"
  ) +
  labs(title = "Daily Log Change") +
  xlab("Date") +
  ylab("Log Change (%)")
p_returns_line

# Returns histogram ####
p_hist <- ggplot(data = x_ret, aes((x_ret[, 1]) * 100)) +
  geom_histogram(
    bins = round(sqrt(length(x_ret))),
    aes(y = ..density..),
    colour = "black",
    fill = "white"
  ) +
  geom_density(alpha = .2, fill = "white") +
  geom_vline(aes(xintercept = mean(x_ret[, 1])),
             color = "blue",
             linetype = "dashed") +
  geom_vline(aes(xintercept = mean_x_ret + sd_x_ret)) +
  geom_vline(aes(xintercept = mean_x_ret - sd_x_ret)) +
  geom_vline(aes(xintercept = mean_x_ret + (sd_x_ret * 2))) +
  geom_vline(aes(xintercept = mean_x_ret - (sd_x_ret * 2))) +
  labs(title = "Daily Log Returns Distribution") +
  xlab("Log Returns (%)") +
  ylab("Density")
p_hist

gridExtra::grid.arrange(p_price, p_returns_line, ncol = 1)

# Z-score ####
zscore <- function(z, p) {
  round(((p - mean(z)) / sd(z)), digits = 3)
}

# Calculating likelihood of hitting a certain strike. 
logchg <- round(last(x_ret), 4)
price <- 10
strike <- 9.00

chg <- round(1 - (price / strike), digits = 4)
print(paste("To hit your strike, price must change", chg * 100, "%"))

z_score <- zscore(z = x_ret, p = chg)
print(paste("This is a z-score equivalent of: ", z_score))

prob <- round(pnorm(z_score, lower.tail = FALSE), digits = 4)
print(paste("Observations > z-score:",
            prob * 100, "%"))
print(paste("Observations < z-score:", 100 - (prob * 100), "%"))

# number of days in the dataset
as.Date(index(x_ret[1,])) - Sys.Date()
