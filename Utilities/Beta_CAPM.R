# A quick script to demonstrate the magnitude of moves in SMH, NVDA and SPY
rm(list = ls())

# 1.0 Packages ####
library(quantmod)

symbols <- c("SPY", "SMH", "NVDA")

getSymbols(symbols,
           src = "yahoo",
           from = "2017-01-01",
           to = Sys.Date())

SPY_ret <- dailyReturn(SPY)
SMH_ret <- dailyReturn(SMH)
NVDA_ret <- dailyReturn(NVDA)

group <- cbind(SPY_ret, SMH_ret, NVDA_ret)
colnames(group) <- c("SPY Returns", "SMH Returns", "NVDA Returns")
head(group)
plot.zoo(group)

avg_SPY_ret <- mean(SPY_ret)
avg_SMH_ret <- mean(SMH_ret)
avg_NVDA_ret <- mean(NVDA_ret)

sd_SPY_ret <- sd(SPY_ret)
sd_SMH_ret <- sd(SMH_ret)
sd_NVDA_ret <- sd(NVDA_ret)

print(c(sd_SPY_ret, sd_SMH_ret, sd_NVDA_ret))
sd_NVDA_ret / sd_SPY_ret



start_date <- "2008-01-01"
end_date <- "2017-01-01"
NVDA <-
  getSymbols("NVDA",
             from = start_date,
             to = end_date,
             auto.assign = F)
SPY <- getSymbols("SPY",
                  from = start_date,
                  to = end_date,
                  auto.assign = F)

r <- function(x) {
  m <- to.monthly(x[, 6])[, 4]
  diff(m) / lag(m)
}
coef(lm(r(NVDA) ~ r(SPY)))
