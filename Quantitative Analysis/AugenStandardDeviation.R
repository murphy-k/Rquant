library(tidyquant)
library(timetk)
library(gridExtra)
rm(list = ls())

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

start_date <- "2000-01-01"
end_date <- Sys.Date()
ticker <- "SPY"

# Basic Visual ####
x <-
  getSymbols(ticker,
             auto.assign = FALSE,
             from = start_date,
             to = end_date)
asd <- augenSD(x = as.vector(Cl(x)), n = 20)
x$augenstdev <- asd

p_stockchart <-
  ggplot(data = x, aes(x = Index, y = x[,4])) + geom_line()

p_augensd <- ggplot(data = x, aes(x = Index, y = x$augenstdev)) +
  geom_col() +
  labs(title = "Daily Std. Dev Moves") +
  xlab("Date") +
  ylab("Standard Deviations")

grid.arrange(p_stockchart, p_augensd, ncol = 1)

ggplot(data = x_tbl, aes(x = date, y = logdiff)) + geom_line() +
  geom_hline(aes(yintercept = mean_logdiff)) +
  geom_hline(aes(yintercept = mean_logdiff + (sd_logdiff * 2))) +
  geom_hline(aes(yintercept = mean_logdiff - (sd_logdiff * 2)))

# Adding log difference of actual price change
x$logdiff <- diff(
  x = x[, 4],
  lag = 1,
  log = TRUE,
  na.pad = TRUE
)

mean_logdiff <- mean(na.trim(x$logdiff))
sd_logdiff <- sd(na.trim(x$logdiff))

x_tbl <- tk_tbl(x)
x_tbl <-
  `colnames<-`(
    x_tbl,
    c(
      "date",
      "open",
      "high",
      "low",
      "close",
      "volume",
      "adj",
      "augenstdev",
      "logdiff"
    )
  )

x_tbl$outliers <-
  ifelse(
    x_tbl$augenstdev >= 2 &
      x_tbl$logdiff < (mean_logdiff + (sd_logdiff * 2)),
    1,
    ifelse(x_tbl$augenstdev <= -2 &
             x_tbl$logdiff > (mean_logdiff - (sd_logdiff * 2)),
           -1,
           0)
  )

x_tbl %>% filter(date > "2010-01-01") %>%
  ggplot(aes(x = date, y = close, color = outliers)) +
  geom_point() +
  scale_colour_gradient(high = "red", low = "green")

ggplot(data = x_tbl, aes(
  x = x_tbl$logdiff,
  y = x_tbl$augenstdev,
  col = outliers
)) +
  geom_point() +
  labs(title = "Daily Std.Dev Moves vs. Log Difference of Price Change") +
  xlab("% Price Change") +
  ylab("Standard Deviations") +
  geom_vline(aes(xintercept = mean_logdiff + (sd_logdiff * 2))) +
  geom_vline(aes(xintercept = mean_logdiff - (sd_logdiff * 2))) +
  geom_hline(aes(yintercept = 2), linetype = 2) +
  geom_hline(aes(yintercept = -2), linetype = 2)

