# ggplot2 Returns Visualizing workspace
rm(list = ls())
dev.off(dev.list()["RStudioGD"])

library(dplyr)
library(quantmod)
library(ggplot2)
library(magrittr)
# Data Download

getSymbols("SPY", src = "yahoo", from = Sys.Date()-10*365)


# Structure and head()
str(SPY)
head(SPY)
chartSeries(SPY)
# Convert returns to DailyReturn percentage
SPY_ret <- dailyReturn(SPY)
SPY_meanReturn <- round(mean(SPY_ret), digits = 5)
SPY_sdReturn <- round(sd(SPY_ret), digits = 5)
plus1sd <- SPY_meanReturn + SPY_sdReturn
minus1sd <- SPY_meanReturn - SPY_sdReturn
plus2sd <- SPY_meanReturn + (SPY_sdReturn * 2)
minus2sd <- SPY_meanReturn - (SPY_sdReturn * 2)
SPY_ret <- as.xts(SPY_ret)



# View returns as a histogram
ggplot(data = SPY_ret, aes(SPY_ret$daily.returns)) +
  geom_histogram(bins = 200,
                 aes(y = ..density..),
                 colour = "black",
                 fill = "white") +
  #geom_density(alpha=.2, fill="white") +
  geom_vline(
    aes(xintercept = mean(SPY_ret$daily.returns)),
    color = "blue",
    linetype = "dashed",
    size = 1
  ) +
  geom_vline(aes(xintercept = plus1sd)) +
  geom_vline(aes(xintercept = minus1sd)) +
  geom_vline(aes(xintercept = plus2sd)) +
  geom_vline(aes(xintercept = minus2sd))

ggplot(data = SPY_ret, aes(x = Index , y = SPY_ret$daily.returns)) +
  geom_line() +
  geom_hline(
    yintercept = c(plus1sd, plus2sd, minus1sd, minus2sd),
    color = "blue",
    linetype = "dashed"
  )

summary(SPY_ret)

zscore <- function(x) {
  round(((last(x) - mean(x)) / sd(x)),digits = 3)
}
z_score <- zscore(SPY_ret)
pnorm(z_score, lower.tail = FALSE)
