# 1. Packages ####
library(quantmod)
library(qrmdata)
library(qrmtools)
library(Quandl)
library(moments)
library(QRM)
rm(list = ls())
dev.off(dev.list()["RStudioGD"])

# 2. Setup ####
# 2.1. Data Downloading
getSymbols(
  c("^GSPC", "GLD"),
  src = "yahoo",
  auto.assign = TRUE,
  from = "2017-01-01"
)
SPX <- GSPC$GSPC.Close
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

# Calculate average and standard deviation of SPX
SPX_ret <- dailyReturn(SPX)
mu <- mean(SPX_ret)
sigma <- sd(SPX_ret)
plot(SPX_ret, type = "h")

hist(SPX_ret, nclass = 20, probability = TRUE)
plot(density(SPX_ret))

# Make a Q-Q plot of SPX_ret and add a red line
qqnorm(SPX_ret)
qqline(SPX_ret, col = "red")

# Calculate the length of SPX_ret as n
n <- length(SPX_ret)
# Generate n standard normal variables, make a Q-Q plot, add a red line
x1 <- rnorm(n)
qqnorm(x1)
qqline(x1, col = "red")

# Moments ####
# Calculate skewness and kurtosis of SPX_ret
skewness(SPX_ret)
kurtosis(SPX_ret)
# Carry out a Jarque-Bera test for SPX_ret
jarque.test(as.vector(SPX_ret))

# student t dist ####
tfit <- fit.st(SPX_ret)

# Define tpars, nu, mu, and sigma
tpars <- tfit$par.ests
nu <- tpars[1]
mu <- tpars[2]
sigma <- tpars[3]

# VaR ####
# Make a sequence of 100 x-values going from -4*sigma to 4*sigma
xvals <- seq(from = -4 * sigma,
             to = 4 * sigma,
             length.out = 100)

# Compute the density of a N(mu, sigma^2) distribution at xvals
ndens <- dnorm(xvals, mean = mu, sd = sigma)

# Plot ndens against xvals
plot(xvals, ndens, type = "l")

# Compute the 99% VaR and 99% ES of a N(mu, sigma^2) distribution
VaR99 <- qnorm(0.99, mean = mu, sd = sigma)
ES99 <- ESnorm(0.99, mu = mu, sd = sigma)

# Draw vertical lines at VaR99 and ES99 in red and green
abline(v = VaR99, col = "red")
abline(v = ES99, col = "green")
