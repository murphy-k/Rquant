library(xts)
library(zoo)

# Clear plots and environment
rm(list = ls())
dev.off(dev.list()["RStudioGD"])

# Data Download ####
GDP <-
  as.data.frame(readr::read_csv("~/Downloads/GDPC1/Quarterly.csv"))
GDP_xts <- as.xts(GDP[, -1], order.by = GDP[, 1])

plot.xts(GDP_xts)
pctchg_GDP <- (ROC(GDP_xts, n = 1))
plot.xts(pctchg_GDP,
         type = "h",
         up.col = "black",
         dn.col = "red")

hist(pctchg_GDP, probability = TRUE, main = "GDP %-Chg Distribution")
lines(density(pctchg_GDP, na.rm = TRUE), col = "red")

