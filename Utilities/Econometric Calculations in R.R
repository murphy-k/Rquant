# Financial Equations with R FinCal package
# Portfolio Gainz Edition


# 1. Load R packages
library(FinCal)
library(quantstrat)
# 2. Initial Settings
initEquity <- 109000
currentEquity <- 189763.41
returnPercent <- 0.05
yearsCompounding <- 15

# 3. Future Value Calculations
futurePortfolio <- fv.simple(r = returnPercent,
                             n = yearsCompounding,
                             pv = -currentEquity)

# 3.1 Future Value Percent Return
percentGain <- ((futurePortfolio / currentEquity) - 1)

# 4. Display Results
plot(x = 15,
     y = futurePortfolio)