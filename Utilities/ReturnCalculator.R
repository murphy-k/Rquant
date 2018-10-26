# 1. Load R packages
library(scales)

# 2. Initial Settings

# 2.1 Set date parameters
dateBought <- as.Date("2017-10-19", tz = "UTC")
dateSold <- strptime("2017-10-31", format = "%Y-%m-%d", tz = "UTC")

# 2.2 Calculate the number of days the position was open
timePeriod <-
  difftime(as.POSIXct(dateSold),
           as.POSIXct(dateBought, tz = "UTC"),
           units = "days")
print(timePeriod)

# 2.3 Input BUY/SELL prices and position size - and number of days open
GBTC_priceBought <- 678.95
GBTC_priceSold <- 820.7537
positionSize <- 100
daysOpen <- 12

# 2.4 Input number of Transactions (2x #-Trades) and commission costs
transactionNumber <- 3
commissionCost <- 8.95

# 3. Profit Calculations

# 3.1 Profit per Share
grossProfitperShare <- (GBTC_priceSold - GBTC_priceBought)


# 3.2 Gross Profit
grossEquityProfit <- (grossProfitperShare * positionSize)


# 3.3 Net Profit
netEquityProfit <-
  (grossEquityProfit) - (commissionCost * transactionNumber)
Profits <- dollar(netEquityProfit)


# 3.4 Profit per day
profitPerDay <- (netEquityProfit / daysOpen)
DailyProfits <- dollar(profitPerDay)

# 4. Display Results
print(grossProfitperShare)
dollar(grossEquityProfit)
print(Profits)
print(DailyProfits)
