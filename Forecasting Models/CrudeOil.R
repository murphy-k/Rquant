# 1.0 - load packages required. 'Quandl' for the data pull, 'astsa' for ARIMA
#       modeling and 'forecast' for the auto.arima() function.
library(Quandl)
library(astsa)
library(forecast)
rm(list = ls(all = TRUE))
# 1.1 -  initial settings including today's date and the starting date
today <- Sys.Date()
starting_date <- "2016-01-01"

# 1.2 - pull variables including weekly oil imports/exports/supply in crude
#       barrels from the EIA database in Quandl.com
# Variables ####

oil_imports = Quandl(
  "EIA/PET_WCRIMUS2_W",
  api_key = "shVi-_QjPbmUAfvVBMzw",
  type = "xts",
  start_date = starting_date,
  end_date = today
)
oil_exports = Quandl(
  "EIA/PET_WCREXUS2_W",
  type = "xts",
  api_key = "shVi-_QjPbmUAfvVBMzw",
  start_date = starting_date,
  end_date = today
)
oil_supply = Quandl(
  "EIA/PET_WCRSTUS1_W",
  api_key = "shVi-_QjPbmUAfvVBMzw",
  type = "xts",
  collapse = "weekly",
  start_date = starting_date,
  end_date = today
)
oil_crudeSeries = Quandl(
  "CHRIS/CME_CL1",
  api_key = "shVi-_QjPbmUAfvVBMzw",
  type = "xts",
  collapse = "weekly",
  start_date = starting_date,
  end_date = today
)
oil_crudePrice <- oil_crudeSeries$Last
.indexwday(oil_crudePrice)

# Data Visualization ####
head(oil_crudePrice, n = 5)
quantmod::chart_Series(oil_crudePrice)
acf2(oil_crudePrice, max.lag = 52)

head(oil_supply, n = 5)
quantmod::chart_Series(oil_supply)
acf2(oil_supply, max.lag = 52)

length(oil_crudePrice)
length(oil_supply)

correlation_coefficient <-
  cor.test(x = as.vector(oil_crudePrice), y = as.vector(oil_supply))
print(correlation_coefficient)


# ARIMA Modeling ####
d_oil_supply = diff(oil_supply)
quantmod::chart_Series(d_oil_supply)
acf2(d_oil_supply, max.lag = 52)

oil_supply_fit <- auto.arima(oil_supply)
d_oil_supply_fit <- auto.arima(d_oil_supply)

plot(forecast(oil_supply_fit))
plot(forecast(d_oil_supply_fit))

sarima.for(
  oil_supply,
  p = 0,
  d = 2,
  q = 1,
  n.ahead = 16
)
sarima.for(
  d_oil_supply,
  p = 0,
  d = 1,
  q = 1,
  n.ahead = 16
)

plot(oil_crudePrice)
acf2(oil_crudePrice, max.lag = 52)
d_oil_crudePrice = diff(oil_crudePrice)
plot(d_oil_crudePrice)
acf2(d_oil_crudePrice, max.lag = 52)

oil_crudePrice_fit <- auto.arima(oil_crudePrice)
d_oil_crudePrice_fit <- auto.arima(d_oil_crudePrice)

plot(forecast(oil_crudePrice_fit))
plot(forecast(d_oil_crudePrice_fit))
