# Packages ####
library(quantmod)
library(qrmdata)
library(qrmtools)
library(Quandl)
library(moments)
library(QRM)


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
plot.zoo(x_coms, plot.type = "multiple", type = "h")
pairs(as.zoo(x_coms))

# Calculate average and standard deviation of DJI
dj_returns <- dailyReturn(DJI)
mu <- mean(dj_returns)
sigma <- sd(dj_returns)
plot(dj_returns, type = "h")

hist(dj_returns, nclass = 20, probability = TRUE)
plot(density(dj_returns))

# Make a Q-Q plot of dj_returns and add a red line
qqnorm(dj_returns)
qqline(dj_returns, col = "red")

# Calculate the length of dj_returns as n
n <- length(dj_returns)
# Generate n standard normal variables, make a Q-Q plot, add a red line
x1 <- rnorm(n)
qqnorm(x1)
qqline(x1, col = "red")

# Moments ####
# Calculate skewness and kurtosis of dj_returns
skewness(dj_returns)
kurtosis(dj_returns)
# Carry out a Jarque-Bera test for dj_returns
jarque.test(as.vector(dj_returns))

# student t dist ####
tfit <- fit.st(dj_returns)

# Define tpars, nu, mu, and sigma
tpars <- tfit$par.ests
nu <- tpars[1]
mu <- tpars[2]
sigma <- tpars[3]
