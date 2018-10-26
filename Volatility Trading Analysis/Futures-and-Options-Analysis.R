# Volatility Trading Analysis with R
# Futures and Options Analysis

# 1. Load R Packages
library("Quandl")
Quandl.api_key("shVi-_QjPbmUAfvVBMzw")
library("TTR")
library("quantmod")
library("fOptions") # Load only after variable creation
# unloadNamespace("fOptions") # Use first if TTR volatility function is masked
# unloadNamespace("fBasics") # Use second if TTR volatility function is masked

# 2. Data Downloading and Variable Creation

# 2.1. Data Downloading
fotickers <- c("USTREASURY/YIELD/1", "CHRIS/CME_ES1/4")
fotickers2 <- c("^GSPC", "^SP500TR")
getSymbols(fotickers2,
           src = 'yahoo',
           from = "2016-01-01",
           to = "2017-01-01")
fodata <-
  Quandl(fotickers,
         type = "xts",
         start_date = "2016-01-01",
         end_date = "2017-01-01")
fodata <- cbind(GSPC[, 1:4], fodata, SP500TR[, 4])
fodata <- fodata[complete.cases(fodata), ]

# 2.2. Spot Price
s <- last(fodata[, 4])
colnames(s) <- "Spot"

# 2.3. Strike Price
k <- round(s, digits = 0)
colnames(k) <- "Strike"

# 2.4. Annualized Risk Free Rate
r <- last((fodata[, 5] / 100))
colnames(r) <- "Rf"

# 2.5. Annualized Monthly Volatility
yvol <- volatility(fodata[, 1:4],
                   calc = "close",
                   n = 21,
                   N = 252)
vol <- last(yvol)
colnames(vol) <- "Vol"

# 2.6. Annual Dividend Yield
yspret <- yearlyReturn(fodata[, 4])
ysptrret <- yearlyReturn(fodata[, 7])
ydyield <- ysptrret - yspret
q <- last(ydyield)

# 2.7. Time to Expiration (in Years)
t <- 1 / 12

# 2.8. Future Historical Price
hf <- last(fodata[, 6])
colnames(hf) <- "Future.Hist"

# 3. Monthly Future Calculation

# 3.1. Monthly Future Price Calculation
ef <- s * exp((r - q) * t)
colnames(ef) <- "Future.Est"
fcomp <- cbind(s, hf, ef)
fcomp

# 4. Monthly Option Calculation

# 4.1. Black an Scholes Option Price Calculation

# 4.1.1. European ATM Call Option Price Calculation
s2 <- as.numeric(s)
k2 <- as.numeric(k)
r2 <- as.numeric(r)
q2 <- as.numeric(q)
vol2 <- as.numeric(vol)
cbs <- GBSOption(
  TypeFlag = "c",
  S = s2,
  X = k2,
  Time = t,
  r = r2,
  b = q2,
  sigma = vol2
)
cbs

# 4.1.2. European ATM Put Option Price Calculation
pbs <- GBSOption(
  TypeFlag = "p",
  S = s2,
  X = k2,
  Time = t,
  r = r2,
  b = q2,
  sigma = vol2
)
pbs

# 4.1.3. European ATM Call Option Greeks Calculation
cbsdelta <-
  GBSGreeks(
    Selection = "delta",
    TypeFlag = "c",
    S = s2,
    X = k2,
    Time = t,
    r = r2,
    b = q2,
    sigma = vol2
  )
cbsgamma <-
  GBSGreeks(
    Selection = "gamma",
    TypeFlag = "c",
    S = s2,
    X = k2,
    Time = t,
    r = r2,
    b = q2,
    sigma = vol2
  )
cbsvega <-
  GBSGreeks(
    Selection = "vega",
    TypeFlag = "c",
    S = s2,
    X = k2,
    Time = t,
    r = r2,
    b = q2,
    sigma = vol2
  )
cbstheta <-
  GBSGreeks(
    Selection = "theta",
    TypeFlag = "c",
    S = s2,
    X = k2,
    Time = t,
    r = r2,
    b = q2,
    sigma = vol2
  )
cbsgreeks <- cbind(cbsdelta, cbsgamma, cbsvega, cbstheta)
cbsgreeks

# 4.2. Binomial Tree Option Price Calculation

# 4.2.1. European ATM Call Option Three Steps Branching Calculation
cbt <-
  CRRBinomialTreeOption(
    TypeFlag = "ce",
    S = s2,
    X = k2,
    Time = t,
    r = r2,
    b = q2,
    sigma = vol2,
    n = 3
  )
cbt

# 4.2.2. European ATM Call Option Three Steps Branching Calculation
pbt <-
  CRRBinomialTreeOption(
    TypeFlag = "pe",
    S = s2,
    X = k2,
    Time = t,
    r = r2,
    b = q2,
    sigma = vol2,
    n = 3
  )
pbt

# 4.3. Option Price Comparison

# 4.3.1. Call Option Price Comparison
ccomp <- cbind(cbs@price, cbt@price)
colnames(ccomp) <- c("cbs", "cbt")
ccomp

# 4.3.2. Put Option Price Comparison
pcomp <- cbind(pbs@price, pbt@price)
colnames(pcomp) <- c("pbs", "pbt")
pcomp
