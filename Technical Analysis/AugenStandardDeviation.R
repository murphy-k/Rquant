library(tidyquant)

# Augen Standard Deviation Function ####
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
# Basic Visual ####
SPY <- getSymbols("SPY", auto.assign = FALSE)
asp <- augenSpike(as.vector(Cl(SPY)))
SPY$spike <- asp
start_date <- "2000-01-01"
end_date <- Sys.Date()
plot(
  window(
    SPY$spike,
    start = start_date,
    end = end_date,
    type = "h",
    main = "Augen Standard Deviation (SPY)"
  )
)
plot(window(SPY$SPY.Close,
            start = "2007-01-01",
            end = "2008-01-01"))

# tidyquant Visual ####
spy <- tq_get("SPY", from = start_date, to = end_date)

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
