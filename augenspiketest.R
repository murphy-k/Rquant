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

start_date = "2005-01-01"
end_date = "2008-01-01"
getSymbols("SPY", from = start_date, to = end_date)

sp <- SPY["2005-11-02::2007-01-12"]
asp <- augenSpike(as.vector(Cl(sp)))
sp$spike <- asp
barplot(
  sp$spike,
  main = "Augen Price Spike SPY 2011",
  xlab = "Time Daily",
  ylab = "Price Spike in Std Dev"
)

# book example
aub <- data.frame(c(47.58, 47.78, 48.09, 47.52, 48.47, 48.38, 49.30, 49.61, 50.03, 51.65, 51.65, 51.57, 50.60, 50.45, 50.83, 51.08, 51.26, 50.89, 50.51, 51.42, 52.09, 55.83, 55.79, 56.20))
colnames(aub) <- c("Close")
aub$PriceChg <- c(NA, diff(aub$Close))
aub$LnChg <- ROC(aub$Close)
aub$StDevLgChg<-slideapply(aub$LnChg, 20, sd)
aub$StdDevPr <- aub$Close * aub$StDevLgChg


pr <- aub$StdDevPr
pr <- c(NA, pr[-length(pr)])


aub$Spike <- aub$PriceChg / pr
aub
