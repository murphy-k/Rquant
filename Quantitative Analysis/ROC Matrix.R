library(quantmod)
library(data.table)
library(timetk)
library(ggplot2)

ticker <- "QQQ"
df <- getSymbols(ticker,auto.assign = FALSE)
df <- df[,4]
df <- `colnames<-`(df,"close")
View(df)

df$ROC1 <- ROC(x = df[,1],n = 1)
df$ROC2 <- ROC(x = df[,1], n = 2)
df$ROC3 <- ROC(x = df[,1], n = 3)
df$ROC4 <- ROC(x = df[,1], n = 4)
df$ROC5 <- ROC(x = df[,1], n = 5)
df$ROC6 <- ROC(x = df[,1], n = 6)
df$ROC7 <- ROC(x = df[,1], n = 7)
df$ROC8 <- ROC(x = df[,1], n = 8)
df$ROC9 <- ROC(x = df[,1], n = 9)
df$ROC10 <- ROC(x = df[,1], n = 10)

df$signal <- df$ROC10 < -0.10
df <- na.trim(df)
df_tbl <- tk_tbl(df)
ggplot(df_tbl, aes(index, close)) +
  geom_point(aes(x=df_tbl$index, y=df_tbl$close, color=df_tbl$signal)) + 
  labs(title = "QQQ",subtitle = "10-Day ROC < 10.00%") +
  xlab("Date") + 
  ylab("Price")