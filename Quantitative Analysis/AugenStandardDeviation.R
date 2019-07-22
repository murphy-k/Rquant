library(tidyquant)
library(timetk)
library(gridExtra)

rm(list = ls())

# Functions ####
# Sliding volatility calculation
slideapply <- function(x, n, FUN = sd) {
  v <- c(rep(NA, length(x)))
  for (i in n:length(x)) {
    v[i] <- FUN(x[(i - n + 1):i])
  }
  return(v)
}
# augen Spike calculation
augenSpike <- function(x, n = 20) {
  prchg <- c(NA, diff(x))
  lgchg <- c(NA, diff(log(x)))
  stdevlgchg <- slideapply(lgchg, n, sd)
  stdpr <- x * stdevlgchg
  stdpr <- c(NA, stdpr[-length(stdpr)])
  augen_sd <- prchg / stdpr
  return(augen_sd)
}

# Analysis ####
years_back <- 3
end_date = Sys.Date()
start_date = end_date - 365 * years_back
ticker <- "IBM"
x <-
  getSymbols(ticker,
             from = start_date,
             to = end_date,
             auto.assign = FALSE)
# coerce to tibble and rename columns
x <- tk_tbl(x)
x <- `colnames<-`(x,
                  c("date",
                    "open",
                    "high",
                    "low",
                    "close",
                    "volume",
                    "adj"))
# calculate log change and sliding 1 day standard deviation
x$logchg <- ROC(x$close)
x$volatility <- slideapply(x$logchg, 20, sd)
# recast price into standard deviations
asd <- augenSpike(x = x$close)
x$spike <- asd

# build charts
p_chart <- ggplot(x, aes(date, close)) +
  geom_line()
p_volatility <- ggplot(x, aes(date, volatility)) + geom_line()

p_augenspike <- ggplot(x, aes(date, spike)) + geom_col() +
  scale_y_continuous(
    name = "StDev Spike",
    limits = c(-3, 6),
    breaks = c(-6, -4, -2, 0, 2, 4, 6)
  )
p_logchg <- ggplot(data = x, aes(x = date, y = logchg)) + geom_col()
# View price and volatility
grid.arrange(p_chart, p_volatility, ncol = 1)
# view price and augen spikes
grid.arrange(p_chart, p_augenspike, ncol = 1)
# compare log changes with price spikes
grid.arrange(p_logchg, p_augenspike, ncol = 1)
# calculate mean and sd for log changes
mean_logchg <- mean(na.trim(x$logchg))
sd_logchg <- sd(na.trim(x$logchg))
# create a column with -1 if augen spike > 2 and log change is within 2 sd.
# a +1 if the augen spike > 2 and the log change is within 2 sd.
# A zero otherwise. Shows us if price recast as sd move (with sliding vol) is
# not fitting well with a normal distribution. "How well behaved is this stock?"

x$outliers <-
  ifelse(x$spike >= 2 &
           x$logchg < (mean_logchg + (sd_logchg * 2)),
         1,
         ifelse(x$spike <= -2 &
                  x$logchg > (mean_logchg - (sd_logchg * 2)),
                -1,
                0))

p_outliersChart <-
  ggplot(x, aes(x = date, y = close, color = outliers)) +
  geom_point() +
  scale_colour_gradient(high = "red", low = "green")
grid.arrange(p_outliersChart, p_augenspike, ncol = 1)


x$abs_sd <- abs(x$spike)
ggplot(data = x, aes(
  x = x$logchg,
  y = x$abs_sd,
  col = outliers
)) +
  geom_point() +
  labs(title = print(paste0(ticker))) +
  xlab("Log Change") +
  ylab("Std Dev") +
  geom_vline(aes(xintercept = mean_logchg + (sd_logchg * 2))) +
  geom_vline(aes(xintercept = mean_logchg - (sd_logchg * 2))) +
  geom_hline(aes(yintercept = 2), linetype = 2) +
  geom_segment(aes(
    x = 0,
    y = 0,
    xend = mean_logchg + (sd_logchg * 2),
    yend = 2
  ), color = "black") +
  geom_segment(aes(
    x = 0,
    y = 0,
    xend = mean_logchg - (sd_logchg * 2),
    yend = 2
  ), color = "black")
