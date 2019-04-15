library(PerformanceAnalytics)
library(quantmod)

getSymbols(
  c("XOP", "SPY", "USO", "XLE"),
  src = "yahoo",
  auto.assign = TRUE,
  warnings = FALSE
)

startDate <- "2018-06-01"
endDate <- Sys.Date()
XOP_ts <- window(x = XOP$XOP.Close,
                 start = startDate,
                 end = endDate)
SPY_ts <- window(x = SPY$SPY.Close,
                 start = startDate,
                 end = endDate)
USO_ts <- window(x = USO$USO.Close,
                 start = startDate,
                 end = endDate)
XLE_ts <- window(x = XLE$XLE.Close,
                 start = startDate,
                 end = endDate)
df <- cbind(XOP_ts, SPY_ts, USO_ts, XLE_ts)
chart.RollingCorrelation(
  Ra = df[, c(2:4)],
  Rb = df[, 1],
  width = 20,
  #colorset = tol6qualitative,
  legend.loc = "bottomright",
  main = "Rolling Correlations",
  type = "l"
)
