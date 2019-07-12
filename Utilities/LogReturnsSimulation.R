#' ---	
#' title: "Log-Returns Report & Monte Carlo Sim"	
#' author: "Kyle Evan Murphy"	
#' date: "`r format(Sys.time(), '%d %B, %Y')`"	
#' output:	
#'   pdf_document: default	
#'   html_document: default	
#' editor_options:	
#'   chunk_output_type: console	
#' code_folding: hide	
#' ---	
#' 	
#' 	
knitr::opts_chunk$set(echo = TRUE)	
#' 	
#' 	
#' 	
library(quantmod)   # get stock prices, stock analysis functions	
library(xts)        # extensible time series functions	
library(rvest)      # web scraping	
library(tidyverse)  # ggplot2, purrr, dplyr, tidyr, readr, tibble	
library(stringr)    # working with strings	
library(forcats)    # working with factors	
library(lubridate)  # working with dates in tibble / data frames	
library(plotly)     # interactive plots	
library(corrplot)   # visualizing correlation plots	
#' 	
#' 	
#' ### Overview	
#' 	
#' 	
getSymbols(	
  "SPY",	
  src = "yahoo",	
  auto.assign = TRUE,	
  from = "2017-01-01",	
  to = Sys.Date()	
)	
SPY %>%	
  chartSeries(	
    TA = "addSMA(n = 100, col = 'black');	
  addSMA(n=200, col = 'purple');	
  addRSI();",	
    subset = "2018-08-01::",	
    theme = "white",	
    major.ticks = "months"	
  )	
#' 	
#' 	
#' 	
#' Calculate the log returns of "SPY". Latest observations:	
#' 	
SPY %>%	
  Ad() %>%	
  dailyReturn(type = "log") %>%	
  round(digits = 3) %>%	
  tail()	
#' 	
#' First Observations:	
#' 	
SPY %>%	
  Ad() %>%	
  dailyReturn(type = "log") %>%	
  round(digits = 3) %>%	
  head()	

#' 	
#' 	
log_returns <- SPY %>%	
  Ad() %>%	
  dailyReturn(type = "log")	
names(log_returns) <- "Log.Returns"	
#' 	
#' 	
(	
  log_returns * 100	
) %>%	
  ggplot(aes(x = Log.Returns)) +	
  geom_histogram(bins = 100) +	
  geom_density() +	
  geom_rug(alpha = 0.5) + ggtitle("SPY Log-Returns %")	
#' 	
#' 	
#' Quantiles:	
#' 	
probs <- c(.005, .025, .25, .5, .75, .975, .995)	
dist_log_returns <- log_returns %>%	
  quantile(probs = probs, na.rm = TRUE)	
dist_log_returns %>%	
  round(digits = 3)	
#' 	
#' 	
#' 	
#' Mean and Std.Dev:	
#' 	
mean_log_returns <-	
  mean(log_returns, na.rm = TRUE) %>% round(digits = 5)	
sd_log_retuns <-	
  sd(log_returns, na.rm = TRUE) %>% round(digits = 5)	
print(c(	
  "Mean log return:",	
  mean_log_returns,	
  "Std.Dev of log returns:",	
  sd_log_retuns	
))	
#' 	
#' 	
#' 	
#' 	
mean_log_returns %>%	
  exp()	
#' 	
#' 	
#' Average daily percent return:	
#' 	
round((exp(mean_log_returns) - 1) * 100, digits = 3)	
#' 	
#' 	
#' ### Simulations	
#' 	
#' Random-Walk simulation (252 days):	
#' 	
set.seed(1)	
N <- 20	
mu <- mean_log_returns	
sigma <- sd_log_retuns	
day <- 1:N	
price_init <- SPY$SPY.Adjusted[[nrow(SPY$SPY.Adjusted)]]	
SPY$SPY.Adjusted[[nrow(SPY$SPY.Adjusted)]]	
# Simulate prices	
price <- c(price_init, rep(NA, N - 1))	
for (i in 2:N) {	
  price[i] <- price [i - 1] * exp(rnorm(1, mu, sigma))	
}	
price_sim <- cbind(day, price) %>%	
  as_tibble()	
#' 	
#' 	
#' 	
# Visualize the price simulation	
price_sim %>%	
  ggplot(aes(day, price)) +	
  geom_line() +	
  ggtitle(str_c("SPY: Simulated Prices for ", N, " Trading Days"))	
#' 	
#' 	
#' 	
#' Monte-Carlo analysis:	
#' 	
N <- 20 # number of days to simulate	
M <- 1000 # number of monte carlo simulations	
#' 	
#' 	
#' 	
#' 	
#' 	
set.seed(1)	
monte_carlo_matrix <- matrix(nrow = N, ncol = M)	
for (j in 1:M) {	
  monte_carlo_matrix[[1, j]] <- price_init	
  for (i in 2:N) {	
    monte_carlo_matrix[[i, j]] <-	
      monte_carlo_matrix[[i - 1, j]] * exp(rnorm(1, mu, sigma))	
  }	
}	
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
  geom_line(alpha = 0.10, col = "black") +	
  ggtitle(str_c(	
    "SPY: ",	
    M,	
    " Monte Carlo Simulations for Prices Over ",	
    N,	
    " Trading Days"	
  ))	
#' 	
#' 	
#' 	
#' Boundaries:	
#' 	
print(c("Starting Price: ", round(SPY$SPY.Adjusted[[nrow(SPY)]], digits = 3)))	
end_stock_prices <- price_sim %>%	
  filter(Day == max(Day))	
dist_end_stock_prices <-	
  quantile(end_stock_prices$Stock.Price, probs = probs)	
dist_end_stock_prices %>% round(2)	
#' 	
#' 	
#' Median Monte-Carlo simulation price:	
#' 	
print(round(dist_end_stock_prices[[4]],digits = 2))	
#' 	
#' 	
#' CAGR Comparison:	
#' 	
N_hist <- nrow(SPY) / N	
p_start_hist <- SPY$SPY.Adjusted[[1]]	
p_end_hist <- SPY$SPY.Adjusted[[nrow(SPY)]]	
N_sim <- N / N	
p_start_sim <- p_end_hist	
p_end_sim <- dist_end_stock_prices[[4]]	
#' 	
#' 	
#' 	
#' 	
CAGR_hist <- (p_end_hist / p_start_hist) ^ (1 / N_hist) - 1	
CAGR_sim <- (p_end_sim / p_start_sim) ^ (1 / N_sim) - 1	
print(c("SPY CAGR %: ", round(CAGR_hist, digits = 4) * 100))	
print(c("Simulation CAGR %: ", round(CAGR_sim, digits = 4) * 100))	
#' 	
