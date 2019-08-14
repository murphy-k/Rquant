ticker  <- "QQQ"
df <- getSymbols(ticker, auto.assign = FALSE, from = date)

# df$Signal <- df[, 3] >= shift(df[, 3], n = 1)
df$Signal <- 1
df$Returns <-
  shift(df[, 1], n = 2, type = "lead") - shift(df[, 4], n = 1, type = "lead")
df$Results <- df$Signal * df$Returns
df$bh <- df[, 4] - shift(df[, 4])
df <- na.trim(df)
df$Equity <- cumsum(df$Results)
df$HoldEquity <- cumsum(df$bh)

df_tbl <- tk_tbl(df)
ggplot(df_tbl, aes(index)) +
  geom_line(aes(y = HoldEquity, col = "Buy and Hold Equity")) +
  geom_line(aes(y = Equity, col = "Strategy Equity")) + labs(
    title = paste("Strategy Equity vs. Buy and Hold:", ticker),
    caption = paste("Today:", Sys.Date())
  ) + xlab("Date") + ylab("Return")
