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
x <- SPY$SPY.Close

# Round, drop N/A values, and multiply by 100 to transform into 'Percents'.
x_ret <- round(na.trim(dailyReturn(x[,1])), digits = 3)

# Plot out daily returns for SPY.
ggplot(data = x_ret, aes(x = Index , y = x_ret[, 1])) +
  geom_col(width = 5) +
  geom_hline(yintercept = mean(x_ret),
             color = "blue",
             linetype = "dashed") +
  geom_hline(
    yintercept = c(
      mean(x_ret) + sd(x_ret),
      mean(x_ret) + (sd(x_ret) * 2),
      mean(x_ret) - sd(x_ret),
      mean(x_ret) - (sd(x_ret) * 2)
    ),
    color = "black",
    linetype = "solid"
  ) +
  labs(title = "SPY Daily Returns",
       subtitle = "(With Mean/2SD)") +
  xlab("Date") +
  ylab("Return (%)")

# Calculate our mean and standard deviation
mean <- round(mean(x_ret), digits = 5)
sd <- round(sd(x_ret), digits = 5)

# Visualize histogram and QQ-Plot
ggplot(data = x_ret, aes(x_ret[, 1])) +
  geom_histogram(
    bins = (length(x_ret) * 0.5),
    aes(y = ..density..),
    colour = "black",
    fill = "white"
  ) +
  geom_density(alpha = .2, fill = "white") +
  geom_vline(
    aes(xintercept = mean(x_ret[, 1])),
    color = "blue",
    linetype = "dashed",
    size = 1
  ) +
  geom_vline(aes(xintercept = mean(x_ret) + sd(x_ret))) +
  geom_vline(aes(xintercept = mean(x_ret) - sd(x_ret))) +
  geom_vline(aes(xintercept = mean(x_ret) + (sd(x_ret) * 2))) +
  geom_vline(aes(xintercept = mean(x_ret) - (sd(x_ret) * 2))) +
  labs(title = "SPY Daily Returns Distribution",
       subtitle = "(With Mean/2SD)") +
  xlab("Return (%)") +
  ylab("Density")
qqnorm(x_ret)
qqline(x_ret, col = "blue")

# Create a random sample with the same paramaters
norm <- rnorm(n = length(x_ret),
              mean = mean,
              sd = sd)
hist(norm, breaks = 50, probability = TRUE)

# Visualize histogram and QQ-Plot of the random sample
qqnorm(norm)
qqline(norm, col = "red")
qqline(x_ret, col = "blue")

# Normality Testing
jarque.test(as.vector(x = x_ret))
jarque.test(as.vector(x = norm))
shapiro.test(as.vector(x_ret))
shapiro.test(as.vector(norm))

