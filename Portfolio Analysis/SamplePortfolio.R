# Load packages.
library(quantmod)
library(ggplot2)
library(moments)
options(scipen = 999)

# Clear global environment.
rm(list = ls())

# Clear plots
dev.off(dev.list()["RStudioGD"])

# Set seed to generate the same random numbers each time.
set.seed(42)

# Download SP500 data from yahoo and create an xts of close prices.
# Generate an xts of daily returns for SPY
getSymbols("SPY", src = "yahoo", auto.assign = TRUE)
SPY_close <- SPY$SPY.Close

# Round, drop N/A values, and multiply by 100 to transform into 'Percents'.
SPY_dailyret <- round(na.trim(dailyReturn(SPY_close)), digits = 3)

# Plot out daily returns for SPY.
chartSeries(
  SPY_dailyret,
  theme = chartTheme('white'),
  show.grid = FALSE,
  up.col = "black"
)

# Calculate our mean and standard deviation
mean <- round(mean(SPY_dailyret), digits = 5)
sd <- round(sd(SPY_dailyret), digits = 5)

# Visualize histogram and QQ-Plot
hist(SPY_dailyret, breaks = 50, probability = TRUE)
lines(density(SPY_dailyret))
qqnorm(SPY_dailyret)
qqline(SPY_dailyret)

# Create a random sample with the same paramaters
norm <- rnorm(n = length(SPY_dailyret),
              mean = mean,
              sd = sd)
hist(norm, breaks = 50, probability = TRUE)

# Visualize histogram and QQ-Plot of the random sample
qqnorm(norm)
qqline(norm, col = "red")
qqline(SPY_dailyret, col = "blue")

# Normality Testing
jarque.test(as.vector(x = SPY_dailyret))
jarque.test(as.vector(x = norm))
shapiro.test(as.vector(SPY_dailyret))
shapiro.test(as.vector(norm))


# Version 2
InitialEquity = 1000
Equity <-
  InitialEquity + (InitialEquity * round(sample(
    norm, size = 1, replace = TRUE
  ), digits = 5))

sampleRun <-
  as.data.frame(sample(x = norm, size = 100, replace = TRUE))

sampleRun <- `colnames<-`(sampleRun, "sample")
head(sampleRun)
sampleRun$choice <-
  as.vector(sample(
    x = list("Yes", "No"),
    size = 100,
    replace = TRUE
  ))
head(sampleRun)
