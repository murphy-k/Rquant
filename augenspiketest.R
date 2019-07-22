library(quantmod)
library(gridExtra)
library(timetk)
library(tidyverse)

rm(list = ls())

slideapply <- function(x, n, FUN = sd) {
  v <- c(rep(NA, length(x)))
  for (i in n:length(x)) {
    v[i] <- FUN(x[(i - n + 1):i])
  }
  return(v)
}

augenSpike <- function(x, n = 20) {
  prchg <- c(NA, diff(x))
  lgchg <- c(NA, diff(log(x)))
  stdevlgchg <- slideapply(lgchg, n, sd)
  stdpr <- x * stdevlgchg
  #shuffle things up one
  stdpr <- c(NA, stdpr[-length(stdpr)])
  spike <- prchg / stdpr
  return(spike)
}

start_date = "2000-01-01"
end_date = Sys.Date()
ticker <- "AAPL"
x <-
  getSymbols(ticker,
             from = start_date,
             to = end_date,
             auto.assign = FALSE)
x <- tk_tbl(x)
x <- `colnames<-`(x,
                     c("date",
                       "open",
                       "high",
                       "low",
                       "close",
                       "volume",
                       "adj"))
x$logchg <- ROC(x$close)
x$volatility <- slideapply(x$logchg, 20, sd)
asd <- augenSpike(x = x$close)
x$spike <- asd

# Subset x
x <- x %>%
  filter(date > "2005-11-02" & date < "2007-01-13")

p_chart <- ggplot(x, aes(date, close)) +
  geom_line()
p_volatility <- ggplot(x, aes(date, volatility)) + geom_line()

p_augenspike <- ggplot(x, aes(date, spike)) + geom_col() +
  scale_y_continuous(
    name = "StDev Spike",
    limits = c(-3, 6),
    breaks = c(-6, -4, -2, 0, 2, 4, 6)
  )

grid.arrange(p_chart, p_volatility, ncol = 1)
grid.arrange(p_chart, p_augenspike, ncol = 1)

