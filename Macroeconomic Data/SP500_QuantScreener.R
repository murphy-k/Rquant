rm(list = ls())
dev.off(dev.list()["RStudioGD"])

# Packages ####
library(quantmod)   # get stock prices, stock analysis functions
library(xts)        # extensible time series functions
library(rvest)      # web scraping
library(tidyverse)  # ggplot2, purrr, dplyr, tidyr, readr, tibble
library(stringr)    # working with strings
library(forcats)    # working with factors
library(lubridate)  # working with dates in tibble / data frames
library(plotly)     # interactive plots
library(corrplot)   # visualizing correlation plots

# Symbol D/L ####
# Downloading symbols using quantmod package and viewing the instrument
getSymbols(
  "SPY",
  src = "yahoo",
  auto.assign = TRUE,
  from = Sys.Date() - 7306,
  to = Sys.Date()
)
# Understanding the class and structure of our object
SPY %>% class()
SPY %>% str()
SPY %>% head()

# Visualizing the SPY price over time
SPY %>%
  chartSeries(TA = "addBBands(200);
              addVo();
              addMACD()",
              subset = "2016::",
              theme = "white")
#  Calculating the logarithmic returns of SPY
SPY %>%
  Ad() %>%
  dailyReturn(type = "log") %>%
  head()

log_returns <- SPY %>%
  Ad() %>%
  dailyReturn(type = "log")
names(log_returns) <- "Log.Returns"
# Visualize a histogram and density of SPY.Log.Returns
(log_returns * 100) %>%
  ggplot(aes(x = Log.Returns)) +
  geom_histogram(bins = 100) +
  geom_density() +
  geom_rug(alpha = 0.5) + ggtitle("'SPY' Log-% Returns ")
# What is the maximum log return
max(log_returns)
# Given the maximum, where is it located within the xts
which(log_returns == max(log_returns))
log_returns[2510,]
chartSeries(x = SPY, subset = "2008-08::2008-10-13")
# Examine the distribution of the log returns with quantile()
probs <- c(.005, .025, .25, .5, .75, .975, .995)
dist_log_returns <- log_returns %>%
  quantile(probs = probs, na.rm = TRUE)
dist_log_returns

mean_log_returns <- mean(log_returns, na.rm = TRUE)
sd_log_retuns <- sd(log_returns, na.rm = TRUE)

# Re-transform to get the actual returns
mean_log_returns %>%
  exp()

# This means that the mean daily return is
(exp(mean_log_returns) - 1) * 100
# percent more than the previous day's price.
# This compounds at a daily exponential rate.

# Random Walk Process ####

# We have a mean and a standard devation so we can simulate a RW process
# Each year has 252 trading days - we will simulate 1000 (3.97 years)

# Parameters
N <- 1000
mu <- mean_log_returns
sigma <- sd_log_retuns
day <- 1:N
price_init <- SPY$SPY.Adjusted[[nrow(SPY$SPY.Adjusted)]]
SPY$SPY.Adjusted[[nrow(SPY$SPY.Adjusted)]]
# Simulate prices
set.seed(1)
price <- c(price_init, rep(NA, N - 1))
for (i in 2:N) {
  price[i] <- price [i - 1] * exp(rnorm(1, mu, sigma))
}
price_sim <- cbind(day, price) %>%
  as_tibble()

# Visualize the price simulation
price_sim %>%
  ggplot(aes(day, price)) +
  geom_line() +
  ggtitle(str_c("SPY: Simulated Prices for ", N, " Trading Days"))

# Monte Carlo Simulation ####
# We repeatedly perform the random walk process simulation many times

# Monte Carlo Parameters
N <- 252 # number of days to simulate
M <- 252 # number of monte carlo simulations

# Simulate prices
set.seed(1)
monte_carlo_matrix <- matrix(nrow = N, ncol = M)
for (j in 1:M) {
  monte_carlo_matrix[[1, j]] <- price_init
  for (i in 2:N) {
    monte_carlo_matrix[[i, j]] <-
      monte_carlo_matrix[[i - 1, j]] * exp(rnorm(1, mu, sigma))
  }
}
# Format and organize data frame
price_sim <- cbind(day, monte_carlo_matrix) %>%
  as_tibble()
nm <- str_c("Sim.", seq(1, M))
nm <- c("Day", nm)
names(price_sim) <- nm
price_sim <- price_sim %>%
  gather(key = "Simulation", value = "Stock.Price", -(Day))
# Visualize Simulation
price_sim %>%
  ggplot(aes(x = Day, y = Stock.Price, Group = Simulation)) +
  geom_line(alpha = 0.05) +
  ggtitle(str_c(
    "SPY: ",
    M,
    " Monte Carlo Simulations for Prices Over ",
    N,
    " Trading Days"
  ))
end_stock_prices <- price_sim %>%
  filter(Day == max(Day))
dist_end_stock_prices <-
  quantile(end_stock_prices$Stock.Price, probs = probs)
dist_end_stock_prices %>% round(2)

# Are these numbers realistic? how does it compare to CAGR?

# CAGR Comparison ####
# Inputs
N_hist <- nrow(SPY) / 252
p_start_hist <- SPY$SPY.Adjusted[[1]]
p_end_hist <- SPY$SPY.Adjusted[[nrow(SPY)]]
N_sim <- N / 252
p_start_sim <- p_end_hist
p_end_sim <- dist_end_stock_prices[[4]]
# CAGR calculations
CAGR_hist <- (p_end_hist / p_start_hist) ^ (1 / N_hist) - 1
CAGR_sim <- (p_end_sim / p_start_sim) ^ (1 / N_sim) - 1
print(c(CAGR_hist, CAGR_sim) * 100)

# SP500 Scrape ####

# Web-scrape SP500 stock list
sp_500 <-
  read_html("https://en.wikipedia.org/wiki/List_of_S%26P_500_companies") %>%
  html_node("table.wikitable") %>%
  html_table() %>%
  select('Ticker symbol', Security, 'GICS Sector', 'GICS Sub Industry') %>%
  as_tibble()
# Name formatting
names(sp_500) <- sp_500 %>%
  names() %>%
  str_to_lower() %>%
  make.names()
# Show results
sp_500

# Categorical data inspection
sp_500 %>%
  lapply(function(x)
    x %>% unique() %>% length()) %>%
  unlist() # show in condensed format

# There are 505 symbols and 505 securities.
sp_500 %>%
  group_by(security) %>%
  summarize(count = n()) %>%
  filter(count > 1)

# Using forcats::fct_reorder() to organize the gics.sector by greatest freq
# (count) forcats package makes organizing factor categories in R easier.

sp_500 %>%
  # Summarize by frequency
  group_by(gics.sector) %>%
  summarise(count = n()) %>%
  # visualize
  ggplot(aes(x = gics.sector %>% fct_reorder(count),
             y = count)) + geom_bar(stat = "identity") + geom_text(
               aes(label = count),
               size = 3,
               nudge_y = 4,
               nudge_x = .1
             ) + scale_y_continuous(limits = c(0, 100)) +
  ggtitle(label = "Sector Frequency Among SP500 Stocks") +
  xlab(label = "GICS Sector") +
  theme(plot.title = element_text(size = 16)) + coord_flip()

# Get Prices f(x) ####
get_stock_prices <-
  function(ticker, return_format = "tibble", ...) {
    # get stock prices
    stock_prices_xts <-
      getSymbols(Symbols = ticker,
                 auto.assign = FALSE,
                 src = "yahoo",
                 ...)
    # rename
    names(stock_prices_xts) <-
      c("Open", "High", "Low", "Close", "Volume", "Adjusted")
    # return in xts format if tibble is not specified
    if (return_format == "tibble") {
      stock_prices <- stock_prices_xts %>%
        as_tibble() %>%
        rownames_to_column(var = "Date") %>%
        mutate(Date = ymd(Date))
    } else {
      stock_prices <- stock_prices_xts
    }
    stock_prices
  }
"SPY" %>%
  get_stock_prices(return_format = "xts") %>%
  head()
"SPY" %>%
  get_stock_prices(return_format = "tibble") %>%
  head()
"SPY" %>%
  get_stock_prices() %>%
  head()
# Notice that instead of creating an object in the global environment, an
# object was created as a local variable and immediately output to the screen.
# This is important when using map() which needs to return a local object.
# The object returned has a consistent column name structure, this is needed
# if we plan to unnest() the object. Tibbles can be nested and unnested.

# Log Rets f(x) ####

# A wrapper for quantmod::periodReturns() that takes a tibble or xts of prices
# and converts it to a log return in xts or tibble format.

get_log_returns <-
  function(x,
           return_format = "tibble",
           period = 'daily',
           ...) {
    # convert tibble to xts
    if (!is.xts(x)) {
      x <- xts(x[, -1], order.by = x$Date)
    }
    # get log returns
    log_returns_xts <-
      periodReturn(x = x$Adjusted,
                   type = "log",
                   period = period,
                   ...)
    # rename
    names(log_returns_xts) <- "Log.Returns"
    # return in xts format if tibble is not specified
    if (return_format == "tibble") {
      log_returns <- log_returns_xts %>%
        as_tibble() %>%
        rownames_to_column(var = "Date") %>%
        mutate(Date = ymd(Date))
    } else {
      log_returns <- log_returns_xts
    }
    log_returns
  }
"SPY" %>% get_stock_prices(return_format = "tibble") %>%
  get_log_returns(return_format = "tibble")

sp_500 <- sp_500 %>%
  mutate(
    stock.prices = map(ticker.symbol,
                       function(.x)
                         get_stock_prices(
                           .x,
                           return_format = "tibble",
                           from = "2007-01-01",
                           to = Sys.Date()
                         )),
    log.returns = map(stock.prices,
                      function(.x)
                        get_log_returns(.x, return_format = "tibble")),
    mean.log.returns = map_dbl(log.returns, ~ mean(.$Log.Returns)),
    sd.log.returns = map_dbl(log.returns, ~ sd(.$Log.Returns)),
    n.trade.days = map_dbl(stock.prices, nrow)
  )
## Dropping names with a '.' in them to run through example.
sp_500 <- sp_500[-72,]
sp_500 <- sp_500[-81,]
sp_500 <- sp_500[-84,]
sp_500 %>% head()
