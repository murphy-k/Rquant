# ggplot2 Returns Visualizing workspace
rm(list = ls())

library(dplyr)
library(quantmod)
library(ggplot2)
# Data Download

getSymbols("QQQ", src = "yahoo", from = "2006-02-06")


# Structure and head()
str(QQQ)
head(QQQ)
chartSeries(QQQ)
# Convert returns to DailyReturn percentage
QQQ_ret <- dailyReturn(QQQ)
QQQ_meanReturn <- mean(QQQ_ret)
QQQ_sdReturn <- sd(QQQ_ret)
plus1sd <- QQQ_meanReturn + QQQ_sdReturn
minus1sd <- QQQ_meanReturn - QQQ_sdReturn
plus2sd <- QQQ_meanReturn + (QQQ_sdReturn * 2)
minus2sd <- QQQ_meanReturn - (QQQ_sdReturn * 2)
QQQ_ret <- as.xts(QQQ_ret)



# View returns as a histogram
ggplot(data = QQQ_ret, aes(QQQ_ret$daily.returns)) +
  geom_histogram(bins = 100,
                 aes(y = ..density..),
                 colour = "black",
                 fill = "white") +
  #geom_density(alpha=.2, fill="white") +
  geom_vline(
    aes(xintercept = mean(QQQ_ret$daily.returns)),
    color = "blue",
    linetype = "dashed",
    size = 1
  ) +
  geom_vline(aes(xintercept = plus1sd)) +
  geom_vline(aes(xintercept = minus1sd)) +
  geom_vline(aes(xintercept = plus2sd)) +
  geom_vline(aes(xintercept = minus2sd))
ggplot(QQQ_ret, aes(x = Index , y = QQQ_ret$daily.returns)) +
  geom_line()
summary(QQQ_ret * 100)

print(QQQ_sdReturn)
last(QQQ_ret)
z_score <- ((last(QQQ_ret)) - QQQ_meanReturn) / QQQ_sdReturn
print(z_score)
pnorm(z_score, lower.tail = TRUE)

cor(QQQ,QQQ)
