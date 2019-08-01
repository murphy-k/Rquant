library(quantmod)
library(rvest)
library(tidyverse)
library(tidyquant)
library(janitor)
library(plotly)
library(dplyr)

Sys.setenv("plotly_username"="murphy-k")
Sys.setenv("plotly_api_key"="OUdMTfG3WkNoGLtWC6AK")

rm(list = ls())

today <- Sys.Date()

# subtract 3 months from the current date
#date = today %m+% months(-3)
date = "2019-01-01"
# pass SP500 ticker ^GSPC to tq_get function
SP500 = tq_get("^GSPC", from = date)

# We need to create a vector of all companies in the SP500, iterate over this
# vector and call tq_get() on each element in the vector, returning a dataframe
# for each ticker, then combine all data frames into one dataframe. The current
# SP500 stocks are located here:
# https://en.wikipedia.org/wiki/List_of_S%26P_500_companies

# get the URL for the wikipedia page with all SP500 symbols
url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"

# use that URL to scrape the SP500 table using rvest
sp500_scrape <- url %>%
  # read the HTML from the webpage
  read_html() %>%
  # one way to get table
  #html_nodes(xpath='//*[@id="mw-content-text"]/div/table[1]') %>%
  # easier way to get table
  html_nodes(xpath = '//*[@id="constituents"]') %>%
  html_table()

#create a vector of sp500_scrape
sp500_table <- sp500_scrape[[1]]

sp500_table = sp500_table %>%
  mutate(Symbol = case_when(
    Symbol == "BRK.B" ~ "BRK-B",
    Symbol == "BF.B" ~ "BF-B",
    TRUE ~ as.character(Symbol)
  ))

get_symbols = function(ticker = "AAPL") {
  df = tq_get(ticker, from = date) %>%
    mutate(symbol = rep(ticker, length(date)))
}

# Data Download ####
# Get all the symbols and place them in a tidy dataframe
sp500_df = map(sp500_table$Symbol, get_symbols) %>%
  bind_rows()

sp500_df = sp500_df %>%
  left_join(sp500_table, by = c('symbol' = 'Symbol')) %>%
  clean_names() %>%
  select(date:security, gics_sector, gics_sub_industry)

sp500_df %>%
  select(symbol) %>%
  distinct() %>%
  count() %>%
  select("Total Number of Tickers" = n)

# Viewing an individual ticker's price history
ticker <- "JWN"
sp500_df %>%
  filter(symbol == !!ticker) %>%
  ggplot(aes(date, adjusted)) +
  geom_line() + labs(title = paste("Daily Chart:", ticker)) +
  xlab("Date") +
  ylab("Price")

# Converting the price history to returns
daily_sector = sp500_df %>%
  group_by(security, gics_sector, symbol) %>%
  tq_transmute(select     = adjusted,
               mutate_fun = periodReturn,
               period     = "daily") %>%
  ungroup()


# View average returns top performers
avg_return = daily_sector %>%
  group_by(security, gics_sector) %>%
  summarise(avg_return = round(mean(daily.returns*100), 4),
            Volatility = sd(daily.returns*100)) %>%
  arrange(desc(avg_return), desc(Volatility))
avg_return %>% head()


# View the top 4.95% of SP500 in average daily returns
avg_return %>%
  head(25) %>%
  ggplot(aes(reorder(security, -avg_return), avg_return, fill = avg_return)) +
  geom_col() +
  coord_flip() +
  labs(title = "Highest Average Return",
       x = "Security",
       y = "Average Return") +
  theme_classic() +
  theme(legend.position = "none")

# View the bottom of 4.95% in average daily returns
avg_return %>%
  tail(25) %>%
  ggplot(aes(reorder(security, -avg_return), avg_return, fill = avg_return)) +
  geom_col() +
  coord_flip() +
  labs(title = "Lowest Average Return",
       x = "Security",
       y = "Average Return") +
  theme_classic() +
  theme(legend.position = "none")

# Average returns given volatility
returns_plot <- avg_return %>%
  ggplot(aes(avg_return, Volatility, color = gics_sector)) +
  geom_point(alpha = 0, show.legend = TRUE) +
  geom_text(aes(label = security, color = gics_sector),
            size = 3,
            show.legend = FALSE) +
  labs(title = "Average Return vs. Volatility (SP500)",
       x = "Average Return",
       subtitle = paste("Data Start:", date)) +
  guides(color = guide_legend("Sector",
                              override.aes = list(size = 2, alpha = 1))) +
  theme_minimal()
returns_plot


p <- plot_ly(
  avg_return,
  x = ~ avg_return,
  y = ~ Volatility,
  type = 'scatter',
  mode = 'markers+text',
  text = ~ security,
  textposition = 'middle right',
  #color = ~ gics_sector,
  colors = 'Paired',
  textfont = list(size = 8)
) %>%
  layout(
    title = paste('Average Return vs Volatility (SP500)' ,date, "to", Sys.Date()),
    xaxis = list(title = 'Average Return', zeroline = TRUE),
    yaxis = list(title = 'Volatility', zeroline = TRUE)
  )
api_create(p, filename = "SP500 Avg.Returns vs. St Dev")
