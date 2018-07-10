library(PortfolioAnalytics)
library(TTR)
library(Quandl)
# Clear plots and environment
rm(list = ls())
dev.off(dev.list()["RStudioGD"])

# Data Download ####
GDP <- getSymbols("GDPC1",
                  src = "FRED",
                  auto.assign = FALSE)
M2 <- getSymbols("M2V",
                 src = "FRED",
                 auto.assign = FALSE)
Prob_Recession <- getSymbols("RECPROUSM156N",
                             src = "FRED",
                             auto.assign = FALSE)

# Visualize ####
plot.xts(GDP, grid.ticks.on = "years")
plot.xts(M2, grid.ticks.on = "years")
plot.xts(Prob_Recession, grid.ticks.on = "years",
         subset = "1999::2018")

pctchg_GDP <- na.trim(ROC(GDP, n = 1) * 100)
pctchg_M2 <- na.trim(ROC(M2, n = 1) * 100)
plot.xts(
  pctchg_GDP,
  type = "h",
  up.col = "black",
  dn.col = "red",
  grid.ticks.on = "years"
)
plot.xts(
  pctchg_M2,
  type = "h",
  up.col = "black",
  dn.col = "red",
  grid.ticks.on = "years"
)
print(mean_pctchg_GDP <- mean(pctchg_GDP))
chart.Histogram(
  pctchg_GDP,
  probability = TRUE,
  main = "GDP %-Chg Distribution",
  methods = c("add.density", "add.normal")
)

quantmod::getSymbols(Symbols = "SPY",
                     src = "yahoo",
                     from = "2018-01-01")
pctchg_SPY <- ROC(SPY$SPY.Close, n=1)
plot(x = pctchg_SPY)

