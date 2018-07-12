# Packages ####
library(quantmod)
library(qrmdata)
library(qrmtools)
library(Quandl)

rm(list = ls())
dev.off(dev.list()["RStudioGD"])

# Data Series ####
getSymbols(
  c("^DJI", "GLD"),
  src = "yahoo",
  auto.assign = TRUE,
  from = "2017-01-01"
)
DJI <- DJI$DJI.Close
GLD <- GLD$GLD.Close
OIL <-
  Quandl("FRED/DCOILWTICO",
         collapse = "daily",
         type = "xts")
colnames(OIL) <- 'WTI.Close'
coms <- cbind(GLD["2017"], OIL["2017"])
x_coms <- diff(log(coms))
plot.zoo(x_coms,plot.type = "multiple", type = "h")
pairs(as.zoo(x_coms))

# Calculate average and standard deviation of DJI
d_DJI <- dailyReturn(DJI)
mu <- mean(d_DJI)
sigma <- sd(d_DJI)
plot(d_DJI, type = "h")

hist(d_DJI, nclass = 20, probability = TRUE)
plot(density(d_DJI))

# Make a Q-Q plot of d_DJI and add a red line
qqnorm(d_DJI)
qqline(d_DJI, col = "red")

# Calculate the length of d_DJI as n
n <- length(d_DJI)
# Generate n standard normal variables, make a Q-Q plot, add a red line
x1 <- rnorm(n)
qqnorm(x1)
qqline(x1, col = "red")

