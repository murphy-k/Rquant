# Efficiency Ratio

# Perry Kaufman's "Trading Systems and Methods" describes an "Efficiency Ratio"
# which is calculated by dividing the price change over a period by the absolute
# sum of the price movements that occured during that period. It can help to 
# differentiate between trending and sideways markets. 


# Efficiency Ratio = Direction / Volatility
# where:
#       Direction = abs(close - close[n])
#       Volatility = n * sum(abs(close-close[1]))
#         where:
#                   n = period length