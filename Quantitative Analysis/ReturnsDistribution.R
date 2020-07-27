# ggplot2 Returns Visualizing workspace
library(dplyr)
library(quantmod)
library(ggplot2)
library(magrittr)

rm(list = ls())
dev.off(dev.list()["RStudioGD"])

# Data ####
ticker <- "^GSPC"
start_date <- "1950-01-01"
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

p_price <- ggplot(data = x, aes(x = Index , y = x$GSPC.Close)) +
  geom_line() +
  labs(title = paste("SP500")) +
  xlab("Date") +
  ylab("Price ($)") +
  scale_y_log10() +
  geom_hline(
    yintercept = last(x$GSPC.Close),
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

gridExtra::grid.arrange(p_price, p_hist, ncol = 1)

# Z-score ####
zscore <- function(z, p) {
  round(((p - mean(z)) / sd(z)), digits = 3)
}

z <- zscore(x_ret, last(x_ret))

# The function pnorm returns the integral from −∞ to q of the pdf of the normal
# distribution where q is a Z-score.
p_ <-
  pnorm(
    q = z,
    mean = mean_x_ret,
    sd = sd_x_ret,
    lower.tail = TRUE
  ) %>% round(5)

print(paste(p_ * 100, "% of observations since", start_date, "have been lower"))


# Calculating likelihood of hitting a certain strike.
logchg <- round(last(x_ret), 4)
price <- last(GSPC$GSPC.Close)
strike <- 3000

chg <- round(1 - (price / strike), digits = 4)
print(paste("To hit your strike, price must change", chg * 100, "%"))

z_score <- zscore(z = x_ret, p = chg)
print(paste("This is a z-score equivalent of: ", z_score))

prob <- round(pnorm(z_score, lower.tail = FALSE), digits = 4)
print(paste("Observations > z-score:",
            prob * 100, "%"))
print(paste("Observations < z-score:", 100 - (prob * 100), "%"))

# number of days in the dataset
as.Date(index(x_ret[1, ])) - Sys.Date()
length(x_ret)
