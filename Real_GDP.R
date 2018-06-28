library(xts)
library(zoo)
library(TTR)
# Clear plots and environment
rm(list = ls())
dev.off(dev.list()["RStudioGD"])

# Data Download ####
GDP <- as.data.frame(readr::read_csv("~/Downloads/GDPC1/Quarterly.csv"))
GDP_xts <- as.xts(GDP[, -1], order.by = GDP[, 1])

M2_velocity <- as.data.frame(readr::read_csv("~/Downloads/M2V.csv"))
M2_velocity_xts <- as.xts(M2_velocity[, -1], order.by = M2_velocity[, 1])


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
mean_pctchg_GDP <- mean(pctchg_GDP)
abline(h = mean_pctchg_GDP)
hist(pctchg_GDP, probability = TRUE, main = "GDP %-Chg Distribution")
lines(density(pctchg_GDP, na.rm = TRUE), col = "red")
