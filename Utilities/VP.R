library(quantmod)
getSymbols("GE", src = "yahoo", from = "2018-01-01" to = "2019-01-03")
df <-
  data.frame(
    Date = index(GE),
    Adjusted = GE$GE.Adjusted,
    Volume = GE$GE.Volume
  )
dt <- as.data.table(unique(df, by = Date))
vol <-
  aggregate(cbind(GE.Volume) ~ GE.Adjusted, sum, data = dt)
vol <- data.frame(vol$GE.Volume, vol$GE.Adjusted)
plot(vol, type = "s")
