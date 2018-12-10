library(quantmod)

# 1.0 Extract SPY close values into it's own data frame. (6th column)

getSymbols("SPY", src="yahoo",auto.assign = TRUE)

# 2.0 Define a moving average function
ma <- function(data.frame, n=200){
  res = data.frame
  for(i in n:length(data.frame)){
    res[i] = mean(data.frame[(i-n):i])
  }
  res
}
SPY_filt <- ma(SPY$SPY.Adjusted)
plot(SPY_filt)
