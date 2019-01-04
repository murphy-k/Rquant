# ggplot2 Returns Visualizing workspace
rm(list = ls())
dev.off(dev.list()["RStudioGD"])

library(dplyr)
library(quantmod)
library(ggplot2)
library(magrittr)
# Data Download

getSymbols("AAPL", src = "yahoo", from = "1983-01-01")


# Structure and head()
str(AAPL)
head(AAPL)
chartSeries(AAPL)
# Convert returns to DailyReturn percentage
AAPL_ret <- dailyReturn(AAPL)
AAPL_meanReturn <- round(mean(AAPL_ret), digits = 5)
AAPL_sdReturn <- round(sd(AAPL_ret), digits = 5)
plus1sd <- AAPL_meanReturn + AAPL_sdReturn
minus1sd <- AAPL_meanReturn - AAPL_sdReturn
plus2sd <- AAPL_meanReturn + (AAPL_sdReturn * 2)
minus2sd <- AAPL_meanReturn - (AAPL_sdReturn * 2)
AAPL_ret <- as.xts(AAPL_ret)



# View returns as a histogram
ggplot(data = AAPL_ret, aes(AAPL_ret$daily.returns)) +
  geom_histogram(bins = 200,
                 aes(y = ..density..),
                 colour = "black",
                 fill = "white") +
  #geom_density(alpha=.2, fill="white") +
  geom_vline(
    aes(xintercept = mean(AAPL_ret$daily.returns)),
    color = "blue",
    linetype = "dashed",
    size = 1
  ) +
  geom_vline(aes(xintercept = plus1sd)) +
  geom_vline(aes(xintercept = minus1sd)) +
  geom_vline(aes(xintercept = plus2sd)) +
  geom_vline(aes(xintercept = minus2sd))

ggplot(data = AAPL_ret, aes(x = Index , y = AAPL_ret$daily.returns)) +
  geom_line() +
  geom_hline(
    yintercept = c(plus1sd, plus2sd, minus1sd, minus2sd),
    color = "blue",
    linetype = "dashed"
  )

summary(AAPL_ret)

print(AAPL_sdReturn)
last(AAPL_ret)
z_score <-
  ((last(AAPL_ret) - AAPL_meanReturn) / AAPL_sdReturn) %>%
  round(digits = 2)
print(z_score)
pnorm(z_score, lower.tail = FALSE)
