# 1. Packages ####
library(quantmod)
library(PortfolioAnalytics)
library(magrittr)
rm(list = ls())
dev.off(dev.list()["RStudioGD"])

# 2. Setup ####
start.date <- "2008-01-01"
end.date <- Sys.Date()

Sys.setenv(TZ = 'UTC')

# 2.1 Data Downloading
tickers <- suppressMessages((
  getSymbols(
    c("CTL", "COST", "HSY", "LGORF"),
    src = "yahoo",
    auto.assign = TRUE,
    from = start.date,
    to = end.date
  )
))
# Current Portfolio ####
qty <- c(32.537, 102.488, 102.392, 5000)
quotes <- getQuote(tickers, src = "yahoo") %>%
  View()

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

# Optimization of Portfolio Intro ####
suppressMessages((
  getSymbols(
    c(
      "SPY",
      "TLT",
      "GLD",
      "SHY",
      "TLT",
      "AGG",
      "CTL",
      "COST",
      "HSY",
      "LGORF"
    ),
    src = "yahoo",
    auto.assign = TRUE,
    from = start.date,
    to = end.date
  )
))

SPY <- SPY$SPY.Adjusted
TLT <- TLT$TLT.Adjusted
GLD <- GLD$GLD.Adjusted
AGG <- AGG$AGG.Adjusted
CTL <- CTL$CTL.Adjusted
COST <- COST$COST.Adjusted
HSY <- HSY$HSY.Adjusted
LGORF <- LGORF$LGORF.Adjusted

SPY_scaled <- scale(SPY)
TLT_scaled <- scale(TLT)
GLD_scaled <- scale(GLD)
AGG_scaled <- scale(AGG)
CTL_scaled <- scale(CTL)
COST_scaled <- scale(COST)
HSY_scaled <- scale(HSY)
LGORF_scaled <- scale(LGORF)

chart_Series(SPY_scaled, name = "Comparison")
add_TA(TLT_scaled, col = "blue", on = 1)
add_TA(GLD_scaled, col = "gold", on = 1)
add_TA(AGG_scaled, col = "black", on = 1)

SPY_returns <- round(dailyReturn(SPY), digits = 3) %>%
  `colnames<-`("SPY.Returns")
TLT_returns <- round(dailyReturn(TLT), digits = 3) %>%
  `colnames<-`("TLT.Returns")
GLD_returns <- round(dailyReturn(GLD), digits = 3) %>%
  `colnames<-`("GLD.Returns")
AGG_returns <- round(dailyReturn(AGG), digits = 3) %>%
  `colnames<-`("AGG.Returns")
CTL_returns <- round(dailyReturn(CTL), digits = 3) %>%
  `colnames<-`("CTL.Returns")
COST_returns <- round(dailyReturn(COST), digits = 3) %>%
  `colnames<-`("COST.Returns")
HSY_returns <- round(dailyReturn(HSY), digits = 3) %>%
  `colnames<-`("HSY.Returns")
LGORF_returns <- round(dailyReturn(LGORF), digits = 3) %>%
  `colnames<-`("LGORF.Returns")

Portfolio <-
  cbind(
    SPY_returns,
    TLT_returns,
    GLD_returns,
    AGG_returns,
    CTL_returns,
    COST_returns,
    HSY_returns,
    LGORF_returns
  )
plot.xts(Portfolio,
         observation.based = TRUE,
         legend.loc = "bottom")
summary(Portfolio)
names(Portfolio)
# Opt Practice ####
# Create the portfolio specification
port_spec <- portfolio.spec(colnames(Portfolio))
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
port_spec <-
  add.objective(portfolio = port_spec,
                type = "return",
                name = "mean")
# Solve the optimization problem
opt <-
  optimize.portfolio(Portfolio,
                     portfolio = port_spec,
                     optimize_method = "ROI")
print(opt) # Print the results of the optimization
extractWeights(opt) # Extract the optimal weights
chart.Weights(opt) # Chart the optimal weights

# Workflow for Portfolio Analytics ####
rm(port_spec)
# Create a portfolio specification object using colnames of Portfolio
port_spec <- portfolio.spec(colnames(Portfolio))
# Get the class of the portfolio specification object
class(port_spec)
# Print the portfolio specification object
print(port_spec)
# Add a full investment constraint such that the weights sum to 1
port_spec <-
  add.constraint(portfolio = port_spec, type = "full_investment")
# Add a long only constraint such that weight of an asset is between 0 and 1
port_spec <-
  add.constraint(portfolio = port_spec, type = "long_only")

# Add the weight sum constraint
port_spec <-
  add.constraint(
    portfolio = port_spec,
    type = "weight_sum",
    min_sum = 1,
    max_sum = 1
  )

# Add the box constraint
# port_spec <-
#  add.constraint(
#    portfolio = port_spec,
#    type = "box",
#    min = c(.2, .2, .2, .2, .2, .2, .2, .2),
#    max = 0.40
#  )

# Add the group constraint
# port_spec <-
#  add.constraint(
#    portfolio = port_spec,
#    type = "group",
#    groups = list(c(1, 2, 3, 4), c(5, 6, 7, 8)),
#    group_min = 0.20,
#    group_max = 0.90
#  )

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
# port_spec <-
#  add.objective(
#    portfolio = port_spec,
#    type = "risk_budget",
#    name = "StdDev",
#    min_prisk = 0.05,
#    max_prisk = .10
#  )

# Print the portfolio specification object
print(port_spec)

# Run a single period optimization using random portfolios as the optimization method
opt <-
  optimize.portfolio(
    R = Portfolio,
    portfolio = port_spec,
    optimize_method = "ROI",
    trace = TRUE
  )

# Print the output of the single-period optimization
print(opt)
extractWeights(opt) # Extract the optimal weights
chart.Weights(opt) # Chart the optimal weights
# Rebalancing ####
# Run the optimization backtest with quarterly rebalancing
opt_rebal <-
  optimize.portfolio.rebalancing(
    R = Portfolio,
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

# Workflow Example ####
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
  Return.portfolio(R = asset_returns,
                   weights = extractWeights(opt_rebal_base))
colnames(returns_base) <- "base"
