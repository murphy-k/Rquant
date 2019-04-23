# If I want exposure to the broad market from the short side - how can I do that
# with options without paying the large amount of premium on SPY puts?
# I suspect I could find a stock with a high correlation to SPY that is lower in
# its price. Solving my issue of exposure to the SPY with lower price premium in
# the puts.
rm(list = ls())
library(quantmod)
# for scraping
library(rvest)
# blanket import for core tidyverse packages
library(tidyverse)
# tidy financial analysis
library(tidyquant)
# tidy data cleaning functions
library(janitor)

# We have the SPY data but I would like to see which stocks in the entire 505
# in the SP500 correlate closely with it, I will use a lookback period of three
# years.


today <- Sys.Date()
# subtract 3 years from the current date
# date = today %m+% years(-3)
date = "2019-01-01"
# pass SP500 ticker ^GSPC to tq_get function
SP500 = tq_get("^GSPC", from = date)
SP500 %>%
  head()

# We need to create a vector of all companies in the SP500, iterate over this
# vector and call tq_get() on each element in the vector, returning a dataframe
# for each ticker, then combine all data frames into one dataframe. The current
# SP500 stocks are located here:
# https://en.wikipedia.org/wiki/List_of_S%26P_500_companies

# get the URL for the wikipedia page with all SP500 symbols
url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"

# use that URL to scrape the SP500 table using rvest
tickers <- url %>%
  # read the HTML from the webpage
  read_html() %>%
  # one way to get table
  #html_nodes(xpath='//*[@id="mw-content-text"]/div/table[1]') %>%
  # easier way to get table
  html_nodes(xpath = '//*[@id="constituents"]') %>%
  html_table()
#create a vector of tickers
sp500tickers <- tickers[[1]]
sp500tickers = sp500tickers %>% mutate(Symbol = case_when(
  Symbol == "BRK.B" ~ "BRK-B",
  Symbol == "BF.B" ~ "BF-B",
  TRUE ~ as.character(Symbol)
))
get_symbols = function(ticker = "AAPL") {
  df = tq_get(ticker, from = date) %>%
    mutate(symbol = rep(ticker, length(date)))
}

tickers_df = map(sp500tickers$Symbol, get_symbols) %>%
  bind_rows()

tickers_df = tickers_df %>%
  left_join(sp500tickers, by = c('symbol' = 'Symbol')) %>%
  clean_names() %>%
  select(date:security, gics_sector, gics_sub_industry)

tickers_df %>%
  select(symbol) %>%
  distinct() %>%
  count() %>%
  select("Total Number of Tickers" = n)

# Viewing an individual ticker's price history
ticker = "MMM"
tickers_df %>%
  filter(symbol == !!ticker) %>%
  ggplot(aes(date, adjusted)) +
  geom_line()

# Converting the price history to returns
daily_sector = tickers_df %>%
  group_by(security, gics_sector, symbol) %>%
  tq_transmute(select     = adjusted,
               mutate_fun = periodReturn,
               period     = "daily") %>%
  ungroup()


# View average returns top performers
avg_return = daily_sector %>%
  group_by(security, gics_sector) %>%
  summarise(avg_return = round(mean(daily.returns), 4),
            Volatility =   sd(daily.returns)) %>%
  arrange(desc(avg_return), desc(Volatility))
avg_return %>% head()


# View as a bar plot
avg_return %>% head(20) %>%
  ggplot(aes(reorder(security, -avg_return), avg_return, fill = avg_return)) +
  geom_col() +
  coord_flip() +
  labs(title = "Highest Average Return Past 3 Years",
       x = "Security",
       y = "Average Return") +
  theme_classic() +
  theme(legend.position = "none")


# Average returns given volatility
avg_return %>%
  ggplot(aes(avg_return, Volatility)) +
  geom_text(aes(label = security), size = 3) +
  labs(title = "Average Return vs Volatility Over Last 3 Years In SP500",
       x = "Average Return",
       subtitle = "Data Source: Yahoo Finance") +
  theme_minimal()
