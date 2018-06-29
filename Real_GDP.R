library(xts)
library(zoo)
library(PerformanceAnalytics)
library(TTR)
library(magrittr)
# Clear plots and environment
rm(list = ls())
dev.off(dev.list()["RStudioGD"])

# Data Download ####
GDP <-
  as.data.frame(readr::read_csv("~/Downloads/GDPC1/Quarterly.csv"))
GDP_xts <- as.xts(GDP[, -1], order.by = GDP[, 1])

M2_velocity <- as.data.frame(readr::read_csv("~/Downloads/M2V.csv"))
M2_velocity_xts <-
  as.xts(M2_velocity[, -1], order.by = M2_velocity[, 1])


# Visualize ####
plot.xts(GDP_xts)
pctchg_GDP <- na.trim(ROC(GDP_xts, n = 1) * 100)

plot.xts(
  pctchg_GDP,
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
                     from = "2000-01-01")
SPY$SPY.Close %>%
  ROC(n = 1) %>% summary()
chart.Histogram(
  probability = TRUE,
  show.outliers = TRUE,
  methods = c("add.normal", "add.density")
)
