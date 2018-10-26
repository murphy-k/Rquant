# 1.0 Extract SPY close values into it's own data frame. (6th column)
spy_raw <- data.frame(SPY[,6])

# 2.0 Define a moving average function
ma <- function(data.frame, n=2){
  res = data.frame
  for(i in n:length(data.frame)){
    res[i] = mean(data.frame[(i-n):i])
  }
  res
}

spy_filtered <- ma(spy_raw[1:4336,])

spy_filtered <- as.data.frame(spy_filtered)

cbind(spy_raw, spy_filtered)
