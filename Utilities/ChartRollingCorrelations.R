library(PerformanceAnalytics)
library(quantmod)
rm(list=ls())

getSymbols("DAN", src="yahoo", auto.assign = TRUE, warnings=FALSE)
chart.RollingCorrelation(Ra = SPY$SPY.Close, Rb = DAN$DAN.Close,width = 10)

# First we get the data
data(managers)
chart.RollingCorrelation(managers[, 1:6, drop=FALSE], 
                         managers[, 8, drop=FALSE], 
                         colorset=rich8equal, legend.loc="bottomright", 
                         width=24, main = "Rolling 12-Month Correlation")
