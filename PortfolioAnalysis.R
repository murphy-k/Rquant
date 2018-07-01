# Packages ####
library(quantmod)
library(PortfolioAnalytics)

# Clear plots and environment
rm(list = ls())
dev.off(dev.list()["RStudioGD"])
Sys.setenv(TZ = 'UTC')


# Intro to Portfolio Analytics ####
suppressMessages((
  getSymbols(
    c("CTL", "COST", "DTO", "HSY", "TUES"),
    src = "yahoo",
    auto.assign = TRUE,
    from = "2000-01-01"
  )
))
qty <- c(31.787, 102.249, 1000, 101.443, 2000)
quotes <- getQuote(tickers, src = "yahoo")
dollar_values <- qty * quotes$Last
weights <- round((dollar_values / sum(dollar_values) * 100), 2)
portfolio <-
  as.data.frame(cbind(dollar_values, weights), row.tickers = tickers)

summary(portfolio)
barplot(
  portfolio$weights,
  main = "Portfolio Weights",
  ylab = "Percent of Portfolio",
  xlab = "Instrument",
  names.arg = tickers
)
cat("Total Value of Equities =  $", (sum(dollar_values)))

# Intro to Optimization of a Portfolio ####
suppressMessages((
  getSymbols(
    c("SPY", "TLT", "GLD"),
    src = "yahoo",
    auto.assign = TRUE,
    from = "2005-01-01"
  )
))

SPY <- SPY$SPY.Close
TLT <- TLT$TLT.Close
GLD <- GLD$GLD.Close

SPY_scaled <- scale(SPY)
TLT_scaled <- scale(TLT)
GLD_scaled <- scale(GLD)

chart_Series(SPY, main = "SPY vs TLT vs GLD")
add_TA(TLT, col = "blue", on = 1)
add_TA(GLD, col = "yellow", on = 1)

chart_Series(SPY_scaled, main = "SPY vs TLT vs GLD")
add_TA(TLT_scaled, col = "blue", on = 1)
add_TA(GLD_scaled, col = "yellow", on = 1)

SPY_returns <- round(dailyReturn(SPY), digits = 3)
TLT_returns <- round(dailyReturn(TLT), digits = 3)
GLD_returns <- round(dailyReturn(GLD), digits = 3)
plot.xts(SPY_returns,
         type = "h")
plot.xts(TLT_returns,
         type = "h")
plot.xts(GLD_returns,
         type = "h")
index_returns <-
  cbind(SPY_returns,
        TLT_returns,
        GLD_returns)
names(index_returns) <-
  c("SPY", "TLT ", "GLD ")
# Create the portfolio specification
port_spec <- portfolio.spec(colnames(index_returns))
# Add a full investment constraint such that the weights sum to 1
port_spec <-
  add.constraint(portfolio = port_spec, type = "full_investment")
# Add a long only constraint such that weight of an asset is between 0 and 1
port_spec <-
  add.constraint(portfolio = port_spec, type = "long_only")
# Add an objective to minimize portfolio standard deviation
port_spec <-
  add.objective(portfolio = port_spec,
                type = "risk",
                name = "StdDev")
# Solve the optimization problem
opt <-
  optimize.portfolio(index_returns,
                     portfolio = port_spec,
                     optimize_method = "ROI")

print(opt) # Print the results of the optimization
extractWeights(opt) # Extract the optimal weights
chart.Weights(opt) # Chart the optimal weights


# Workflow for Portfolio Analytics ####
# Create a portfolio specification object using colnames of index_returns
port_spec <- portfolio.spec(colnames(index_returns))
# Get the class of the portfolio specification object
class(port_spec)
# Print the portfolio specification object
print(port_spec)
# Add the weight sum constraint
port_spec <-
  add.constraint(
    portfolio = port_spec,
    type = "weight_sum",
    min_sum = 1,
    max_sum = 1
  )

# Add the box constraint
port_spec <-
  add.constraint(
    portfolio = port_spec,
    type = "box",
    min = c(.1, .1, .1),
    max = 0.40
  )

# Add the group constraint
port_spec <-
  add.constraint(
    portfolio = port_spec,
    type = "group",
    groups = list(c(1), c(2, 3)),
    group_min = 0.40,
    group_max = 0.60
  )
# Print the portfolio specification object
print(port_spec)
# Add a return objective to maximize mean return
port_spec <-
  add.objective(portfolio = port_spec,
                type = "return",
                name = "mean")

# Add a risk objective to minimize portfolio standard deviation
port_spec <-
  add.objective(portfolio = port_spec,
                type = "risk",
                name = "StdDev")

# Add a risk budget objective
port_spec <-
  add.objective(
    portfolio = port_spec,
    type = "risk_budget",
    name = "StdDev",
    min_prisk = 0.05,
    max_prisk = .10
  )

# Print the portfolio specification object
print(port_spec)

# Run a single period optimization using random portfolios as the optimization method
opt <-
  optimize.portfolio(
    R = index_returns,
    portfolio = port_spec,
    optimize_method = "ROI",
    trace = TRUE
  )

# Print the output of the single-period optimization
print(opt)
# Run the optimization backtest with quarterly rebalancing
opt_rebal <-
  optimize.portfolio.rebalancing(
    R = index_returns,
    portfolio = port_spec,
    optimize_method = "ROI",
    trace = TRUE,
    search_size = 1000,
    rebalance_on = "years",
    training_period = 60,
    rolling_window = 60
  )


# Print the output of the optimization backtest
print(opt_rebal)

# Extract the objective measures for the single period optimization
extractObjectiveMeasures(opt)
# Extract the objective measures for the optimization backtest
extractObjectiveMeasures(opt_rebal)

# Extract the optimal weights for the single period optimization
extractWeights(opt)

# Chart the weights for the single period optimization
chart.Weights(opt)

# Extract the optimal weights for the optimization backtest
extractWeights(opt_rebal)

# Chart the weights for the optimization backtest
chart.Weights(opt_rebal)

# Workflow II ####


data("edhec") # Load data
asset_returns <- edhec # Assign data to a variable

# Create a vector of equal weights
equal_weights <- rep(1 / ncol(asset_returns), ncol(asset_returns))

# Compute the benchmark returns
r_benchmark <- Return.portfolio(asset_returns,
                                weights = equal_weights,
                                rebalance_on = "quarters")
colnames(r_benchmark) <- "benchmark"

# Plot the benchmark returns
plot.xts(r_benchmark, type = "h")


# Create the portfolio specification
port_spec <- portfolio.spec(colnames(asset_returns))

# Add a full investment constraint such that the weights sum to 1
port_spec <-
  add.constraint(portfolio = port_spec, type = "full_investment")

# Add a long only constraint such that the weight of an asset is between 0 and 1
port_spec <-
  add.constraint(portfolio = port_spec, type = "long_only")

# Add an objective to minimize portfolio standard deviation
port_spec <-
  add.objective(portfolio = port_spec,
                type = "risk",
                name = "StdDev")

# Print the portfolio specification
print(port_spec)

# Run the optimization
opt_rebal_base <- optimize.portfolio.rebalancing(
  R = asset_returns,
  portfolio = port_spec,
  optimize_method = "ROI",
  rebalance_on = "quarters",
  training_period = 60,
  rolling_window = 60
)

# Print the results
print(opt_rebal_base)

# Chart the weights
chart.Weights(opt_rebal_base)

# Compute the portfolio returns
returns_base <-
  Return.portfolio(R = asset_returns, weights = extractWeights(opt_rebal_base))
colnames(returns_base) <- "base"
