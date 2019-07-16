library(tidyquant)

# Augen Standard Deviation Function ####
slideapply <- function(x, n, FUN = sd) {
  v <- c(rep(NA, length(x)))
  for (i in n:length(x)) {
    v[i] <- FUN(x[(i - n + 1):i])
  }
  return(v)
}
augenSD <- function(x, n = 20) {
  prchg <- c(NA, diff(x))
  lgchg <- c(NA, diff(log(x)))
  stdevlgchg <- slideapply(lgchg, n, sd)
  stdpr <- x * stdevlgchg
  #shuffle things up one
  stdpr <- c(NA, stdpr[-length(stdpr)])
  augen_sd <- prchg / stdpr
  return(augen_sd)
}

start_date <- "2009-01-01"
end_date <- Sys.Date()
ticker <- "SPY"

# Basic Visual ####
x <-
  getSymbols(ticker,
             auto.assign = FALSE,
             from = start_date,
             to = end_date)
asd <- augenSD(x = as.vector(Cl(x)), n = 90)
x$augenstdev <- asd
ggplot(data = x, aes(x = Index, y = x$augenstdev)) + geom_line()


# tidyquant Visual ####
spy <- tq_get("SPY", from = start_date, to = end_date)
spy$spike <- asp
spy %>%
  filter(date >= as.Date("2007-01-01") &
           date <= as.Date("2008-01-01")) %>%
  ggplot(aes(x = date, y = close)) +
  geom_candlestick(aes(
    open = open,
    high = high,
    low = low,
    close = close
  )) +
  labs(title = "SPY Candlestick Chart", y = "Closing Price", x = "")


# Viewing SD moves 