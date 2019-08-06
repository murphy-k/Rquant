library(forecast)
getSymbols("GDPC1", src = "FRED")
GDP <- GDPC1
rm(GDPC1)
GDP$pct_chg <- ROC(x = GDP, n = 1)
GDP$pct_chg <- GDP$pct_chg
GDP <- na.trim(GDP)

avg <- mean(x = GDP$pct_chg)
sd <- sd(x = GDP$pct_chg)

ggplot(data = GDP, aes(Index, pct_chg)) +
  geom_line() +
  labs(title = "GDP Growth (%)") +
  xlab("date") +
  ylab("Percent Change") +
  geom_hline(aes(yintercept = avg), color = "blue") +
  geom_hline(aes(yintercept = sd * 2)) +
  geom_hline(aes(yintercept = sd * -2))


fit <- auto.arima(GDP$pct_chg)
autoplot(forecast(fit, h = 1))
plot(hist(GDP$pct_chg))
