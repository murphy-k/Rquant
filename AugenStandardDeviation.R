library(quantmod)

slideapply <- function(x, n, FUN = sd) {
  v <- c(rep(NA, length(x)))
  
  
  for (i in n:length(x)) {
    v[i] <- FUN(x[(i - n + 1):i])
  }
  return(v)
}


augenSpike <- function(x, n = 20) {
  prchg <- c(NA, diff(x))
  lgchg <- c(NA, diff(log(x)))
  stdevlgchg <- slideapply(lgchg, n, sd)
  stdpr <- x * stdevlgchg
  #shuffle things up one
  stdpr <- c(NA, stdpr[-length(stdpr)])
  spike <- prchg / stdpr
  return(spike)
}

getSymbols("SPY")

spy <- SPY
asp <- augenSpike(as.vector(Cl(spy)))
spy$spike <- asp
plot(
  window(spy$spike, start = "2019-06-01", to = Sys.Date()),
  type = "h",
  main = "Augen Standard Deviation (SPY)"
)
