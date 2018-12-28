# ggplot2 Returns Visualizing workspace
rm(list = ls())

library(dplyr)
library(quantmod)
library(ggplot2)
# Data Download
years <- 5
getSymbols("UPRO", src = "yahoo", from = (Sys.Date() - (365 * years)))


# Structure and head()
str(UPRO)
head(UPRO)

# Convert returns to DailyReturn percentage
UPRO_ret <- dailyReturn(UPRO)
UPRO_meanReturn <- mean(UPRO_ret)
UPRO_sdReturn <- sd(UPRO_ret)
plus1sd <- UPRO_meanReturn + UPRO_sdReturn
minus1sd <- UPRO_meanReturn - UPRO_sdReturn
plus2sd <- UPRO_meanReturn + (UPRO_sdReturn * 2)
minus2sd <- UPRO_meanReturn - (UPRO_sdReturn * 2)
UPRO_ret <- as.xts(UPRO_ret)



# View returns as a histogram
ggplot(data = UPRO_ret, aes(UPRO_ret$daily.returns)) +
  geom_histogram(bins = 100,
                 aes(y = ..density..),
                 colour = "black",
                 fill = "white") +
  #geom_density(alpha=.2, fill="white") +
  geom_vline(
    aes(xintercept = mean(UPRO_ret$daily.returns)),
    color = "blue",
    linetype = "dashed",
    size = 1
  ) +
  geom_vline(aes(xintercept = plus1sd)) +
  geom_vline(aes(xintercept = minus1sd)) +
  geom_vline(aes(xintercept = plus2sd)) +
  geom_vline(aes(xintercept = minus2sd))
ggplot(UPRO_ret, aes(x = Index , y = UPRO_ret$daily.returns)) +
  geom_line()
summary(UPRO_ret * 100)

print(UPRO_sdReturn)
last(UPRO_ret)
z_score <- ((last(UPRO_ret)) - UPRO_meanReturn) / UPRO_sdReturn
print(z_score)
pnorm(z_score, lower.tail = TRUE)
