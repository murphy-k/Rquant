# 1. Packages ####
library(quantmod)
library(PortfolioAnalytics)
library(magrittr)

rm(list = ls())
dev.off(dev.list()["RStudioGD"])

# 2. Setup ####
start.date <- "2018-01-01"
end.date <- Sys.Date()

Sys.setenv(TZ = 'UTC')

# 2.1 Data Downloading
tickers <- suppressMessages((
  getSymbols(
    c("CTL", "COST", "HSY"),
    src = "yahoo",
    auto.assign = TRUE,
    from = start.date,
    to = end.date
  )
))
# Current Portfolio ####
qty <- c(32.537, 102.488, 102.139)
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

# Optimization of Portfolio Intro ####
suppressMessages((
  getSymbols(
    c(
      "SPY",
      "TLT",
      "VNQ",
      "VNQI",
      "BND",
      "EEM",
      "GLD"
    ),
    src = "yahoo",
    auto.assign = TRUE,
    from = start.date,
    to = end.date
  )
))

SPY <- SPY$SPY.Adjusted
TLT <- TLT$TLT.Adjusted
VNQ <- VNQ$VNQ.Adjusted
VNQI <- VNQI$VNQI.Adjusted
BND <- BND$BND.Adjusted
EEM <- EEM$EEM.Adjusted
GLD <- GLD$GLD.Adjusted

SPY_scaled <- scale(SPY)
TLT_scaled <- scale(TLT)
VNQ_scaled <- scale(VNQ)
VNQI_scaled <- scale(VNQI)
BND_scaled <- scale(BND)
EEM_scaled <- scale(EEM)
GLD_scaled <- scale(GLD)

chart_Series(SPY_scaled, name = "Comparison")
add_TA(TLT_scaled, col = "blue", on = 1)
add_TA(VNQ_scaled, col = "gold", on = 1)
add_TA(VNQI_scaled, col = "black", on = 1)
add_TA(BND_scaled, col = "purple", on = 1)
add_TA(EEM_scaled, col = "green", on = 1)
add_TA(GLD_scaled, col = "pink", on = 1)

SPY_returns <- round(dailyReturn(SPY), digits = 3) %>%
  `colnames<-`("SPY.Returns")
TLT_returns <- round(dailyReturn(TLT), digits = 3) %>%
  `colnames<-`("TLT.Returns")
VNQ_returns <- round(dailyReturn(VNQ), digits = 3) %>%
  `colnames<-`("VNQ.Returns")
VNQI_returns <- round(dailyReturn(VNQI), digits = 3) %>%
  `colnames<-`("VNQI.Returns")
BND_returns <- round(dailyReturn(BND), digits = 3) %>%
  `colnames<-`("BND.Returns")
EEM_returns <- round(dailyReturn(EEM), digits = 3) %>%
  `colnames<-`("EEM.Returns")
GLD_returns <- round(dailyReturn(GLD), digits = 3) %>%
  `colnames<-`("GLD.Returns")


Portfolio <-
  cbind(
    SPY_returns,
    TLT_returns,
    VNQ_returns,
    VNQI_returns,
    BND_returns,
    EEM_returns,
    GLD_returns
  )
plot.xts(Portfolio,
         observation.based = TRUE, legend.loc = "topright")
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
port_spec <-
  add.constraint(
    portfolio = port_spec,
    type = "box",
    min = c(.1, .1, .1, .1, .1, .1, .1),
    max = 0.30
  )

# Add the group constraint
#port_spec <-
#  add.constraint(
#    portfolio = port_spec,
#    type = "group",
#    groups = list(c(1, 2, 3), c(4, 5, 6, 7)),
#    group_min = 0.50,
#    group_max = 0.70
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
