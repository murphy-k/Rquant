# 1. Investment Portfolio Analysis Data ####

# 1. Load R packages
library("tseries")
library("quantmod")
library("Quandl")
Quandl.api_key("5TJZgFo3SkynavER6dMR")
library("PortfolioAnalytics")
library("PerformanceAnalytics")
library("DEoptim")

rm(list = ls())
dev.off(dev.list()["RStudioGD"])

# Initial Settings
startDate <- Sys.Date() - (365*10)
endDate <- Sys.Date()

# 2. Asset Classes ####
# 2.1. Cash and Cash Equivalents ####
# 2.1.1. U.S. Total Money Market (Treasury Bills less than 1 year maturity) annual yield
treasury = Quandl(
  "USTREASURY/YIELD",
  type = "xts",
  collapse = "monthly",
  start_date = startDate,
  end_date = endDate
)
# Cash and Cash Equivalents as 1 month Treasury Bills annual yield
TBill_1Month = treasury[, 1]
TBill_1Month_ret = (TBill_1Month / 100) / 12
colnames(TBill_1Month_ret) = "TBill_1Month_ret"
TBill_1Month_annret = TBill_1Month / 100
colnames(TBill_1Month_annret) = "TBill_1Month_annret"
plot(TBill_1Month_ret,
     main = "1 Month U.S. Treasury Bills Returns",
     xlab = "Dates",
     ylab = "Monthly Returns")
table.AnnualizedReturns(TBill_1Month_ret)
charts.PerformanceSummary(TBill_1Month_ret)

# 2.2. Fixed Income or Bonds ####

# 2.2.1. 	iShares Core U.S. Aggregate Bond ETF 'AGG'
getSymbols("AGG",
           src = "yahoo",
           from = startDate,
           to = endDate)
AggBonds = to.monthly(AGG)
AggBonds = AggBonds[, 6]
AggBonds_ret = AggBonds / Lag(AggBonds) - 1
AggBonds_ret[is.na(AggBonds_ret)] <- 0
colnames(AggBonds_ret) <- c("AggBonds_ret")
AggBonds_annret = AggBonds / Lag(AggBonds, 12) - 1
AggBonds_annret[is.na(AggBonds_annret)] <- 0
colnames(AggBonds_annret) <- c("AggBonds_annret")
plot(AggBonds,
         main = "U.S. Total Bond Market Prices",
         xlab = "Dates",
         ylab = "Monthly Prices")
plot(
  AggBonds_ret,
  main = "U.S. Total Bond Market Returns",
  xlab = "Dates",
  ylab = "Monthly Returns",
  type = "h"
)
table.AnnualizedReturns(AggBonds_ret)
charts.PerformanceSummary(AggBonds_ret)

# 2.2.2. U.S. Short Term Bond Market iShares 1-3 Year Treasury Bond ETF
getSymbols("SHY",
           src = "yahoo",
           from = startDate,
           to = endDate)
mshort = to.monthly(SHY)
mshort = mshort[, 6]
mshortret = mshort / Lag(mshort) - 1
mshortret[is.na(mshortret)] <- 0
colnames(mshortret) <- c("mshortret")
ashortret = mshort / Lag(mshort, 12) - 1
ashortret[is.na(ashortret)] <- 0
colnames(ashortret) <- c("ashortret")
plot(mshort,
         main = "U.S. Short Term Bond Market Prices",
         xlab = "Dates",
         ylab = "Monthly Prices")
plot(
  mshortret,
  main = "U.S. Short Term Market Returns",
  xlab = "Dates",
  ylab = "Monthly Returns",
  type = "h"
)
table.AnnualizedReturns(mshortret)
charts.PerformanceSummary(mshortret)

# 2.2.3. U.S. Long Term Bond Market (Barclays U.S. Long Term Treasuries Index,
# Vanguard VUSTX Mutual Fund, U.S. Treasury Bonds with more than ten years maturity)
getSymbols("VUSTX",
           src = "yahoo",
           from = startDate,
           to = endDate)
mlong = to.monthly(VUSTX)
mlong = mlong[, 6]
mlongret = mlong / Lag(mlong) - 1
mlongret[is.na(mlongret)] <- 0
colnames(mlongret) <- c("mlongret")
alongret = mlong / Lag(mlong, 12) - 1
alongret[is.na(alongret)] <- 0
colnames(alongret) <- c("alongret")
plot(mlong,
         main = "U.S. Long Term Bond Market Prices",
         xlab = "Dates",
         ylab = "Monthly Prices")
plot(mlongret,
         main = "U.S. Long Term Market Returns",
         xlab = "Dates",
         ylab = "Monthly Returns")
table.AnnualizedReturns(mlongret)
charts.PerformanceSummary(mlongret)

# 2.2.4. International Total Bond Market (Barclays Global Aggregate Bond Market, Invesco AUBAX Mutual Fund, International Investment Grade Bond Market)
getSymbols("AUBAX",
           src = "yahoo",
           from = startDate,
           to = endDate)
mibonds = to.monthly(AUBAX)
mibonds = mibonds[, 6]
mibondsret = mibonds / Lag(mibonds) - 1
mibondsret[is.na(mibondsret)] <- 0
colnames(mibondsret) <- c("mibondsret")
aibondsret = mibonds / Lag(mibonds, 12) - 1
aibondsret[is.na(aibondsret)] <- 0
colnames(aibondsret) <- c("aibondsret")
plot(mibonds,
         main = "International Total Bond Market Prices",
         xlab = "Dates",
         ylab = "Monthly Prices")
plot(mibondsret,
         main = "International Total Bond Market Returns",
         xlab = "Dates",
         ylab = "Monthly Returns")
table.AnnualizedReturns(mibondsret)
charts.PerformanceSummary(mibondsret)

# 2.2.5. U.S. Cash, Cash Equivalents and Fixed Income or Bond Markets Returns Comparison
usbondscomp = cbind(TBill_1Month_ret, mshortret, AggBonds_ret, mlongret)
table.AnnualizedReturns(usbondscomp)
charts.PerformanceSummary(usbondscomp,
                          main = "U.S Fixed Income Returns")

# 2.2.6. U.S. and International Total Bond Markets Returns Comparison
bondscomp = cbind(AggBonds_ret, mibondsret)
table.AnnualizedReturns(bondscomp)
charts.PerformanceSummary(bondscomp, main = "U.S & International Fixed Income Return Comparison")

# 2.3. Equities or Stocks ####

# 2.3.1. U.S. Total Stock Market (Russell 3000 Index, Vanguard VTSMX Mutual Fund)
getSymbols("VTSMX",
           src = "yahoo",
           from = startDate,
           to = endDate)
mstocks = to.monthly(VTSMX)
mstocks = mstocks[, 6]
mstocksret = mstocks / Lag(mstocks) - 1
mstocksret[is.na(mstocksret)] <- 0
colnames(mstocksret) <- c("mstocksret")
astocksret = mstocks / Lag(mstocks, 12) - 1
astocksret[is.na(astocksret)] <- 0
colnames(astocksret) <- c("astocksret")
plot(mstocks,
         main = "U.S. Total Stock Market Prices",
         xlab = "Dates",
         ylab = "Monthly Prices")
plot(mstocksret,
         main = "U.S. Total Stock Market Returns",
         xlab = "Dates",
         ylab = "Monthly Returns")
table.AnnualizedReturns(mstocksret)
charts.PerformanceSummary(mstocksret)

# 2.3.2. U.S. Large Cap Stock Market (S&P 500 Index, Vanguard VFINX Mutual Fund)
getSymbols("VFINX",
           src = "yahoo",
           from = startDate,
           to = endDate)
mlarge = to.monthly(VFINX)
mlarge = mlarge[, 6]
mlargeret = mlarge / Lag(mlarge) - 1
mlargeret[is.na(mlargeret)] <- 0
colnames(mlargeret) <- c("mlargeret")
alargeret = mlarge / Lag(mlarge, 12) - 1
alargeret[is.na(alargeret)] <- 0
colnames(alargeret) <- c("alargeret")
plot(mlarge,
         main = "U.S. Large Cap Stock Market Prices",
         xlab = "Dates",
         ylab = "Monthly Prices")
plot(mlargeret,
         main = "U.S. Large Cap Stock Market Returns",
         xlab = "Dates",
         ylab = "Monthly Returns")
table.AnnualizedReturns(mlargeret)
charts.PerformanceSummary(mlargeret)

# 2.3.3. U.S. Small Cap Stock Market (Russell 2000 Index, Vanguard NAESX Mutual Fund)
getSymbols("NAESX",
           src = "yahoo",
           from = startDate,
           to = endDate)
msmall = to.monthly(NAESX)
msmall = msmall[, 6]
msmallret = msmall / Lag(msmall) - 1
msmallret[is.na(msmallret)] <- 0
colnames(msmallret) <- c("msmallret")
asmallret = msmall / Lag(msmall, 12) - 1
asmallret[is.na(asmallret)] <- 0
colnames(asmallret) <- c("asmallret")
plot(msmall,
         main = "U.S. Small Cap Stock Market Prices",
         xlab = "Dates",
         ylab = "Monthly Prices")
plot(msmallret,
         main = "U.S. Small Cap Stock Market Returns",
         xlab = "Dates",
         ylab = "Monthly Returns")
table.AnnualizedReturns(msmallret)
charts.PerformanceSummary(msmallret)

# 2.3.4. U.S. Small Cap Growth Stock Market (Russell 2000 Growth Index, Vanguard VISGX Mutual Fund)
getSymbols("VISGX",
           src = "yahoo",
           from = startDate,
           to = endDate)
mgrowth = to.monthly(VISGX)
mgrowth = mgrowth[, 6]
mgrowthret = mgrowth / Lag(mgrowth) - 1
mgrowthret[is.na(mgrowthret)] <- 0
colnames(mgrowthret) <- c("mgrowthret")
agrowthret = mgrowth / Lag(mgrowth, 12) - 1
agrowthret[is.na(agrowthret)] <- 0
colnames(agrowthret) <- c("agrowthret")
plot(mgrowth,
         main = "U.S. Small Cap Growth Stock Market Prices",
         xlab = "Dates",
         ylab = "Monthly Prices")
plot(mgrowthret,
         main = "U.S. Small Cap Growth Stock Market Returns",
         xlab = "Dates",
         ylab = "Monthly Returns")
table.AnnualizedReturns(mgrowthret)
charts.PerformanceSummary(mgrowthret)

# 2.3.5. U.S. Small Cap Value Stock Market (Russell 2000 Growth Index, Vanguard VISVX Mutual Fund)
getSymbols("VISVX",
           src = "yahoo",
           from = startDate,
           to = endDate)
mvalue = to.monthly(VISVX)
mvalue = mvalue[, 6]
mvalueret = mvalue / Lag(mvalue) - 1
mvalueret[is.na(mvalueret)] <- 0
colnames(mvalueret) <- c("mvalueret")
avalueret = mvalue / Lag(mvalue, 12) - 1
avalueret[is.na(avalueret)] <- 0
colnames(avalueret) <- c("avalueret")
plot(mvalue,
         main = "U.S. Small Cap Value Stock Market Prices",
         xlab = "Dates",
         ylab = "Monthly Prices")
plot(mvalueret,
         main = "U.S. Small Cap Value Stock Market Returns",
         xlab = "Dates",
         ylab = "Monthly Returns")
table.AnnualizedReturns(mvalueret)
charts.PerformanceSummary(mvalueret)

# 2.3.6. International Total Stock Market (MSCI International Index, Vanguard VGTSX Mutual Fund)
getSymbols("VGTSX",
           src = "yahoo",
           from = startDate,
           to = endDate)
mistocks = to.monthly(VGTSX)
mistocks = mistocks[, 6]
mistocksret = mistocks / Lag(mistocks) - 1
mistocksret[is.na(mistocksret)] <- 0
colnames(mistocksret) <- c("mistocksret")
aistocksret = mistocks / Lag(mistocks, 12) - 1
aistocksret[is.na(aistocksret)] <- 0
colnames(aistocksret) <- c("aistocksret")
plot(mistocks,
         main = "International Total Stock Market Prices",
         xlab = "Dates",
         ylab = "Monthly Prices")
plot(mistocksret,
         main = "International Total Stock Market Returns",
         xlab = "Dates",
         ylab = "Monthly Returns")
table.AnnualizedReturns(mistocksret)
charts.PerformanceSummary(mistocksret)

# 2.3.7. International Developed Stock Market (MSCI International Developed Index, Vanguard VTMGX Mutual Fund)
getSymbols("VTMGX",
           src = "yahoo",
           from = startDate,
           to = endDate)
mdeveloped = to.monthly(VTMGX)
mdeveloped = mdeveloped[, 6]
mdevelopedret = mdeveloped / Lag(mdeveloped) - 1
mdevelopedret[is.na(mdevelopedret)] <- 0
colnames(mdevelopedret) <- c("mdevelopedret")
adevelopedret = mdeveloped / Lag(mdeveloped, 12) - 1
adevelopedret[is.na(adevelopedret)] <- 0
colnames(adevelopedret) <- c("adevelopedret")
plot(mdeveloped,
         main = "International Developed Stock Market Prices",
         xlab = "Dates",
         ylab = "Monthly Prices")
plot(mdevelopedret,
         main = "International Developed Stock Market Returns",
         xlab = "Dates",
         ylab = "Monthly Returns")
table.AnnualizedReturns(mdevelopedret)
charts.PerformanceSummary(mdevelopedret)

# 2.3.8. International Emerging Stock Market (MSCI International Emerging Index, Vanguard VEIEX Mutual Fund)
getSymbols("VEIEX",
           src = "yahoo",
           from = startDate,
           to = endDate)
memerging = to.monthly(VEIEX)
memerging = memerging[, 6]
memergingret = memerging / Lag(memerging) - 1
memergingret[is.na(memergingret)] <- 0
colnames(memergingret) <- c("memergingret")
aemergingret = memerging / Lag(memerging, 12) - 1
aemergingret[is.na(aemergingret)] <- 0
colnames(aemergingret) <- c("aemergingret")
plot(memerging,
         main = "International Emerging Stock Market Prices",
         xlab = "Dates",
         ylab = "Monthly Prices")
plot(memergingret,
         main = "International Emerging Stock Market Returns",
         xlab = "Dates",
         ylab = "Monthly Returns")
table.AnnualizedReturns(memergingret)
charts.PerformanceSummary(memergingret)

# 2.3.9. U.S. and International Total Stock Markets Returns Comparison
stockscomp = cbind(mstocksret, mistocksret)
table.AnnualizedReturns(stockscomp)
charts.PerformanceSummary(stockscomp)

# 2.3.10. U.S. Market Cap Stock Markets Returns Comparison
sizecomp = cbind(mlargeret, mstocksret, msmallret)
table.AnnualizedReturns(sizecomp)
charts.PerformanceSummary(sizecomp)

# 2.3.11. U.S. Investment Style Stock Markets Returns Comparison
stylecomp = cbind(msmallret, mvalueret, mgrowthret)
table.AnnualizedReturns(stylecomp)
charts.PerformanceSummary(stylecomp)

# 2.3.12. International Stock Markets Returns Comparison
istockscomp = cbind(mdevelopedret, mistocksret, memergingret)
table.AnnualizedReturns(istockscomp)
charts.PerformanceSummary(istockscomp)

# 2.4. Commodities ####

# 2.4.1. Oil WTI Spot Price FOB (U.S. Energy Information Administration Data)
moil = Quandl(
  "EIA/PET_RWTC_D",
  type = "xts",
  collapse = "monthly",
  start_date = startDate,
  end_date = endDate
)
moilret = moil / Lag(moil) - 1
moilret[is.na(moilret)] <- 0
colnames(moilret) <- c("moilret")
aoilret = moil / Lag(moil, 12) - 1
aoilret[is.na(aoilret)] <- 0
colnames(aoilret) <- c("aoilret")
plot(moil,
         main = "Oil WTI Spot Prices",
         xlab = "Dates",
         ylab = "Monthly Prices")
plot(moilret,
         main = "Oil WTI Spot Price Returns",
         xlab = "Dates",
         ylab = "Monthly Returns")
table.AnnualizedReturns(moilret)
charts.PerformanceSummary(moilret)

# 2.4.2. Gold Price (Deutsche Bundesbank Data)
mgold = Quandl(
  "LBMA/GOLD",
  type = "xts",
  collapse = "monthly",
  start_date = startDate,
  end_date = endDate
)
mgoldret = mgold[, 2] / Lag(mgold[, 2]) - 1
mgoldret[is.na(mgoldret)] <- 0
colnames(mgoldret) <- c("mgoldret")
agoldret = mgold[, 2] / Lag(mgold[, 2], 12) - 1
agoldret[is.na(agoldret)] <- 0
colnames(agoldret) <- c("agoldret")
plot(mgold[, 2],
         main = "Gold Prices",
         xlab = "Dates",
         ylab = "Monthly Prices")
plot(mgoldret,
         main = "Gold Price Returns",
         xlab = "Dates",
         ylab = "Monthly Returns")
table.AnnualizedReturns(mgoldret)
charts.PerformanceSummary(mgoldret)

# 2.5. Real Estate ####

# U.S. Real Estate Investment Trust (U.S. Real Estate Investment Trust (MSCI U.S. REIT Index), Vanguard: VGSIX Mutual Fund)
getSymbols("VGSIX",
           src = "yahoo",
           from = startDate,
           to = endDate)
mreit = to.monthly(VGSIX)
mreit = mreit[, 6]
mreitret = mreit / Lag(mreit) - 1
mreitret[is.na(mreitret)] <- 0
colnames(mreitret) <- c("mreitret")
areitret = mreit / Lag(mreit, 12) - 1
areitret[is.na(areitret)] <- 0
colnames(areitret) <- c("areitret")
plot(mreit,
         main = "U.S. Real Estate Investment Trust Prices",
         xlab = "Dates",
         ylab = "Monthly Prices")
plot(mreitret,
         main = "U.S. Real Estate Investment Trust Returns",
         xlab = "Dates",
         ylab = "Monthly Returns")
table.AnnualizedReturns(mreitret)
charts.PerformanceSummary(mreitret)

# 2.6. Currency or Foreign Exchange ####

# 2.6.1. USD Broad Trade Weighted Index (Bloomberg Dollar Total Return, Federal Reserve Economic Data)
mbroad = Quandl(
  "FRED/TWEXB",
  type = "xts",
  collapse = "monthly",
  start_date = startDate,
  end_date = endDate
)
mbroadret = mbroad / Lag(mbroad) - 1
mbroadret[is.na(mbroadret)] <- 0
colnames(mbroadret) <- c("mbroadret")
abroadret = mbroad / Lag(mbroad, 12) - 1
abroadret[is.na(abroadret)] <- 0
colnames(abroadret) <- c("abroadret")
plot(mbroad,
         main = "USD Broad Trade Weighted Index Prices",
         xlab = "Dates",
         ylab = "USD Monthly Prices")
plot(mbroadret,
         main = "USD Broad Trade Weighted Index Returns",
         xlab = "Dates",
         ylab = "USD Monthly Returns")
table.AnnualizedReturns(mbroadret)
charts.PerformanceSummary(mbroadret)

# 2.6.2. USD Major Currencies Trade Weighted Index (Deutsche Bank Long US Dollar Index, Federal Reserve Economic Data)
mmajor = Quandl(
  "FRED/DTWEXM",
  type = "xts",
  collapse = "monthly",
  start_date = startDate,
  end_date = endDate
)
mmajorret = mmajor / Lag(mmajor) - 1
mmajorret[is.na(mmajorret)] <- 0
colnames(mmajorret) <- c("mmajorret")
amajorret = mmajor / Lag(mmajor, 12) - 1
amajorret[is.na(amajorret)] <- 0
colnames(amajorret) <- c("amajorret")
plot(mmajor,
         main = "USD Major Currencies Trade Weighted Index Prices",
         xlab = "Dates",
         ylab = "USD Monthly Prices")
plot(mmajorret,
         main = "USD Major Currencies Trade Weighted Index Returns",
         xlab = "Dates",
         ylab = "USD Monthly Returns")
table.AnnualizedReturns(mmajorret)
charts.PerformanceSummary(mmajorret)

# 2.7. Main Asset Classes Comparison ####
assetscomp <-
  cbind(AggBonds_ret,
        mstocksret,
        moilret,
        mgoldret,
        mreitret,
        mbroadret)
table.AnnualizedReturns(assetscomp)
charts.PerformanceSummary(assetscomp)

# 3. Returns and Risks
# U.S. Large Cap Stock Market (S&P 500 Index, Vanguard VFINX Mutual Fund)
# (Market: U.S. Total Stock Market, Risk Free Rate: U.S. Total Money Market Mean)

# 3.1. Expected Returns

# 3.1.1. Mean
alargemean <- mean(alargeret)

# 3.1.2. Median
alargemedian <- median(alargeret)

# 3.1.3. Compounded Annual Growth Rate CAGR
mlargecagr <- Return.annualized(mlargeret, scale = 12)
colnames(mlargecagr) <- c("mlargecagr")

# 3.1.4. Expected Returns First Comparison
areturnscomp1 <- cbind(alargemean, alargemedian, mlargecagr)
areturnscomp1

# 3.2. Risk

# 3.2.1. Standard Deviation
alargestdev <- StdDev(alargeret)
colnames(alargestdev) <- c("alargestdev")

# 3.2.2. Mean Absolute Deviation
alargemad <- MeanAbsoluteDeviation(alargeret)

# 3.2.3. Annualized Standard Deviation
mlargestdeva <- StdDev.annualized(mlargeret)
colnames(mlargestdeva) <- c("mlargestdeva")

# 3.2.4. Risk Metric Comparison
ariskcomp <- cbind(alargestdev, alargemad, mlargestdeva)
ariskcomp

# 3.2.5. Implied Volatility (CBOE VIX Volatility Index)
# Annualized Volatility with 68% of confidence
mvixq = Quandl(
  "CBOE/VIX",
  type = "xts",
  collapse = "monthly",
  start_date = startDate,
  end_date = endDate
)
mvixa = mvixq[, 4] / 100
plot(mvixa,
         main = "CBOE VIX Volatility Index",
         xlab = "Dates",
         ylab = "Annualized Volatility")
mvix = mvixa / sqrt(12)
mvixamean <- mean(mvixa)
mvixmean <- mean(mvix)
mlargestdev <- StdDev(mlargeret)
colnames(mlargestdev) <- "mlargestdev"
# Volatility Comparison
mvolcomp <- cbind(mlargestdev, mvixmean)
mvolcomp
avolcomp <- cbind(mlargestdeva, alargestdev, mvixamean)
avolcomp

# 3.2.6. Normalized Rate of Return
plot(alargeret,
         main = "U.S. Large Cap Stocks Annual Returns",
         xlab = "Dates",
         ylab = "Annual Returns")
nlargeret <- as.numeric(alargeret)
nlargeret <-
  (nlargeret[13:140] - alargemean) / as.numeric(alargestdev)
nlargeret <- ts(nlargeret, frequency = 12, start = c(2007, 4))
plot(nlargeret,
         main = "U.S. Large Cap Stocks Normalized Annual Returns",
         xlab = "Dates",
         ylab = "Normalized Annual Returns")

# 3.2.7. Geometric Expected Rate of Return
galargemean <-
  exp(log(1 + alargemean) - (alargestdev ^ 2 / (2 * (1 + alargemean) ^ 2))) - 1
colnames(galargemean) <- c("galargemean")

# 3.2.8. Expected Returns Second Comparison
areturnscomp2 <-
  cbind(alargemean, alargemedian, mlargecagr, galargemean)
areturnscomp2

# 3.3. Risks Normality

# 3.3.1. Skewness
mlargeskew <- skewness(mlargeret)
alargeskew <- skewness(alargeret)
largeskew <- cbind(mlargeskew, alargeskew)
largeskew

# 3.3.2. Kurtosis
mlargekurt <- kurtosis(mlargeret)
alargekurt <- kurtosis(alargeret)
largekurt <- cbind(mlargekurt, alargekurt)
largekurt

# 3.3.3. Jarque-Bera Test
# p-value < 0.05 then normality assumption is rejected with 95% confidence
mlargejarque <- jarque.bera.test(mlargeret)
mlargejarque
alargejarque <- jarque.bera.test(alargeret)
alargejarque

# 3.3.4. Value at Risk
mlargevar <- VaR(mlargeret, p = 0.99, method = "modified")
colnames(mlargevar) <- c("mlargevar")
alargevar <- VaR(alargeret, p = 0.99, method = "modified")
colnames(alargevar) <- c("alargevar")
mlargevar
alargevar

# 3.4. Returns and Risks Relationships

# 3.4.1. Correlation
aassetsdata <-
  data.frame(TBill_1Month_annret, AggBonds_annret, astocksret)
aassetscor <- cor(aassetsdata)
aassetscor
alargedata <- data.frame(alargeret, astocksret)
alargecor <- cor(alargedata)
alargecor

# 3.4.2. Covariance
aassetscov <- cov(aassetsdata)
aassetscov
alargecov <- cov(alargedata)
alargecov

# 3.4.3. Coefficient of Determination
aassetsr2 <- aassetscor ^ 2
aassetsr2
alarger2 <- alargecor ^ 2
alarger2

# 3.5. Assets and Market Relationship (Systematic Risk) ####

# 3.5.1. Capital Asset Pricing Model CAPM

# CAPM Beta Coefficient
acashmean <- mean(TBill_1Month_annret)
astocksmean <- mean(astocksret)
abetacapm <-
  CAPM.beta(Ra = alargeret, Rb = astocksret, Rf = acashmean)
abetacapm

# CAPM Jensen's Alpha Intercept
aalphacapm <-
  CAPM.alpha(Ra = alargeret, Rb = astocksret, Rf = acashmean)
aalphacapm

# CAPM Expected Returns
aretcapm <- acashmean + abetacapm * (astocksmean - acashmean)
aretcapm

# CAPM Residual Variance (Unsystematic Risk)
alargecapmbetaest <- lm(alargeret ~ astocksret)
alargeresvar <- summary(alargecapmbetaest)$sigma
alargeresvar


# 3.6. Systematic Risk Hedge

# 3.6.1. Put Call Parity (buy S&P 500, sell 1-month ATM S&P 500 calls)
# Buy Write/Covered Call (CBOE BXM Index, buy S&P 500, sell 1-month ATM S&P 500 calls)
getSymbols("^BXM",
           src = "yahoo",
           from = startDate,
           to = endDate)
dbxm = BXM[, 6]
colnames(dbxm) <- "dbxm"
mbxmret = monthlyReturn(dbxm)
colnames(mbxmret) <- "mbxmret"
dlargey = VFINX[, 6]
colnames(dlargey) <- "dlargey"
mlargerety = monthlyReturn(dlargey)
colnames(mlargerety) <- "mlargerety"
# Put Call Parity Comparison
paritycomp = cbind(mlargerety, mbxmret)
table.AnnualizedReturns(paritycomp)
charts.PerformanceSummary(paritycomp)

# 3.6.2. Tail Risk Hedge (buy S&P 500 TR, buy OTM S&P 500 calls)
# CBOE VXTH Index (98.5% buy S&P 500 TR, 1% buy 1-month 15%-30% delta VIX calls,
# 0.5% 1-month 30%-50% delta VIX calls)
mvxth = Quandl(
  "CBOE/VXTH",
  type = "xts",
  collapse = "monthly",
  start_date = startDate,
  end_date = endDate
)
mvxthret = mvxth / Lag(mvxth) - 1
mvxthret[is.na(mvxthret)] <- 0
colnames(mvxthret) <- c("mvxthret")
avxthret = mvxth / Lag(mvxth, 12) - 1
avxthret[is.na(avxthret)] <- 0
colnames(avxthret) <- c("avxthret")
# Tail Risk Comparison
tailcomp = cbind(mlargeret, mvxthret)
table.AnnualizedReturns(tailcomp)
charts.PerformanceSummary(tailcomp)

# 3.7. Hedge Funds
# Eureka Equal Weighted Hedge Fund Index
mhedgeretq = Quandl(
  "EUREKA/473",
  type = "xts",
  collapse = "monthly",
  start_date = startDate,
  end_date = endDate
)
mhedgeret = mhedgeretq / 100
colnames(mhedgeret) = "mhedgeret"
# Hedge Funds Comparison
hedgecomp = cbind(mlargeret, mhedgeret)
table.AnnualizedReturns(hedgecomp)
charts.PerformanceSummary(hedgecomp)

# 3.8. Portfolio Leverage

# 3.8.1. Maximum Leverage Calculation

# Maximum Monthly Leverage Ratio Annualized
mmaxlargelev = 1 / abs(Return.annualized(min(mlargeret), scale = 12) - max(TBill_1Month_annret))
mmaxlargelev

# Maximum Annual Leverage Ratio
amaxlargelev = 1 / abs(min(alargeret) - max(TBill_1Month_annret))
amaxlargelev

# 3.8.2. U.S. Large Cap Stock Market 2x Daily Leverage (ProShares SSO ETF)
getSymbols("SSO",
           src = "yahoo",
           from = startDate,
           to = endDate)
m2xlarge = to.monthly(SSO)
m2xlarge = m2xlarge[, 6]
m2xlargeret = m2xlarge / Lag(m2xlarge) - 1
m2xlargeret[is.na(m2xlargeret)] <- 0
colnames(m2xlargeret) <- c("m2xlargeret")
a2xlargeret = m2xlarge / Lag(m2xlarge, 12) - 1
a2xlargeret[is.na(a2xlargeret)] <- 0
colnames(a2xlargeret) <- c("a2xlargeret")

# 3.8.3. Leveraged Portfolios Returns Comparison
levcomp = cbind(mlargeret, m2xlargeret)
table.AnnualizedReturns(levcomp)
charts.PerformanceSummary(levcomp)



# 4. Portfolio Theory ####

# 4.1. Portfolio Performance

# 4.1.1. Sharpe Ratio
mlargemean <- mean(mlargeret)
mcashmean <- mean(TBill_1Month_ret)
alargesharpe <- SharpeRatio.annualized(mlargeret, Rf = mcashmean)
alargesharpe

# 4.1.2. Treynor Ratio
alargetreynor <-
  TreynorRatio(Ra = alargeret, Rb = astocksret, Rf = acashmean)
alargetreynor

# 4.1.3. Sortino Ratio
alargesortino <- SortinoRatio(R = alargeret, MAR = acashmean)
alargesortino

# 4.1.4. Kelly Ratio
alargekelly <-
  KellyRatio(R = alargeret, Rf = acashmean, method = "half")
alargekelly

# 4.2. Portfolio Benchmarks (Monthly Rebalancing)

# 4.2.1. Naive Global Portfolio
mnaiveret <-
  0.25 * AggBonds_ret + 0.25 * mstocksret + 0.25 * mibondsret + 0.25 * mistocksret
colnames(mnaiveret) <- "mnaiveret"

# 4.2.2. Roche Global Portfolio
mrocheret <-
  0.24 * AggBonds_ret + 0.18 * mstocksret + 0.33 * mibondsret + 0.25 * mistocksret
colnames(mrocheret) <- "mrocheret"

# 4.2.3. Bogle U.S. 60% Stocks, 40% Bonds Portfolio
mbogleret <- 0.40 * AggBonds_ret + 0.60 * mstocksret
colnames(mbogleret) <- "mbogleret"

# 4.2.4. Ferri Core Four Portfolio
mferriret <-
  0.20 * AggBonds_ret + 0.48 * mstocksret + 0.24 * mistocksret + 0.08 * mreitret
colnames(mferriret) <- "mferriret"

# 4.2.5. Bernestein No-Brainer Portfolio
mbernret <-
  0.25 * mshortret + 0.25 * mlargeret + 0.25 * msmallret + 0.25 * mdevelopedret
colnames(mbernret) <- "mbernret"

# 4.2.6. Benchmark Portfolios Returns Comparison
benchcomp = cbind(mnaiveret, mrocheret, mbogleret, mferriret, mbernret)
table.AnnualizedReturns(benchcomp)
charts.PerformanceSummary(benchcomp)

# 4.3. Portfolio Optimization ####

# 4.3.1. Mean Maximization Portfolio

# Portfolio Assets Returns Matrix
mportret <- cbind(AggBonds_ret, mstocksret, mibondsret, mistocksret)

# Portfolio Specifications
mmeanp <- portfolio.spec(assets = colnames(mportret))

# Portfolio Constraints
mmeanp <-
  add.constraint(mmeanp,
                 type = "weight_sum",
                 min_sum = 0.99,
                 max_sum = 1.01)
mmeanp <- add.constraint(mmeanp, type = "long_only")

# Portfolio Objectives
mmeanp <- add.objective(mmeanp, type = "return", name = "mean")

# Portfolio Optimization (Monthly Rebalancing)
mmeanpopt <-
  optimize.portfolio(
    R = mportret,
    portfolio = mmeanp,
    optimize_method = "DEoptim",
    search_size = 5000,
    trace = "TRUE"
  )
mmeanpopt$weights

# Portfolio Backtesting
mbondsw1 <- mmeanpopt$weights[1]
mstocksw1 <- mmeanpopt$weights[2]
mibondsw1 <- mmeanpopt$weights[3]
mistocksw1 <- mmeanpopt$weights[4]
mmeanpret <-
  mbondsw1 * AggBonds_ret + mstocksw1 * mstocksret + mibondsw1 * mibondsret + mistocksw1 * mistocksret
colnames(mmeanpret) <- "mmeanpret"

# 4.3.2. Standard Deviation Minimization Portfolio

# Portfolio Specifications
mstdevp <- portfolio.spec(assets = colnames(mportret))

# Portfolio Constraints
mstdevp <-
  add.constraint(mstdevp,
                 type = "weight_sum",
                 min_sum = 0.99,
                 max_sum = 1.01)
mstdevp <- add.constraint(mstdevp, type = "long_only")

# Portfolio Objectives
mstdevp <- add.objective(mstdevp, type = "risk", name = "StdDev")

# Portfolio Optimization
mstdevpopt <-
  optimize.portfolio(
    R = mportret,
    portfolio = mstdevp,
    optimize_method = "DEoptim",
    search_size = 5000,
    trace = "TRUE"
  )
mstdevpopt$weights

# Portfolio Backtesting (Monthly Rebalancing)
mbondsw2 <- mstdevpopt$weights[1]
mstocksw2 <- mstdevpopt$weights[2]
mibondsw2 <- mstdevpopt$weights[3]
mistocksw2 <- mstdevpopt$weights[4]
mstdevpret <-
  mbondsw2 * AggBonds_ret + mstocksw2 * mstocksret + mibondsw2 * mibondsret + mistocksw2 * mistocksret
colnames(mstdevpret) <- "mstdevpret"

# 4.3.3. Quadratic Utility/Mean Maximization and
# Standard Deviation Minimization Portfolio

# Portfolio Specifications
msharpep <- portfolio.spec(assets = colnames(mportret))

# Portfolio Constraints
msharpep <-
  add.constraint(msharpep,
                 type = "weight_sum",
                 min_sum = 0.99,
                 max_sum = 1.01)
msharpep <- add.constraint(msharpep, type = "long_only")

# Portfolio Objectives
msharpep <- add.objective(msharpep, type = "return", name = "mean")
msharpep <- add.objective(msharpep, type = "risk", name = "StdDev")

# Portfolio Optimization
msharpepopt <-
  optimize.portfolio(
    R = mportret,
    portfolio = msharpep,
    optimize_method = "DEoptim",
    search_size = 5000,
    trace = "TRUE"
  )
msharpepopt$weights

# Portfolio Backtesting (Monthly Rebalancing)
mbondsw3 <- msharpepopt$weights[1]
mstocksw3 <- msharpepopt$weights[2]
mibondsw3 <- msharpepopt$weights[3]
mistocksw3 <- msharpepopt$weights[4]
msharpepret <-
  mbondsw3 * AggBonds_ret + mstocksw3 * mstocksret + mibondsw3 * mibondsret + mistocksw3 * mistocksret
colnames(msharpepret) <- "msharpepret"

# 4.3.4. Maximize Mean and Minimize Value at Risk (VaR) Portfolio

# Portfolio Specifications
mtailp <- portfolio.spec(assets = colnames(mportret))

# Portfolio Constraints
mtailp <-
  add.constraint(mtailp,
                 type = "weight_sum",
                 min_sum = 0.99,
                 max_sum = 1.01)
mtailp <- add.constraint(mtailp, type = "long_only")

# Portfolio Objectives
mtailp <- add.objective(mtailp, type = "return", name = "mean")
mtailp <-
  add.objective(mtailp,
                type = "risk",
                name = "VaR",
                arguments = list(p = 0.99))

# Portfolio Optimization
mtailpopt <-
  optimize.portfolio(
    R = mportret,
    portfolio = mtailp,
    optimize_method = "DEoptim",
    search_size = 5000,
    trace = "TRUE"
  )
mtailpopt$weights

# Portfolio Backtesting (Monthly Rebalancing)
mbondsw4 <- mtailpopt$weights[1]
mstocksw4 <- mtailpopt$weights[2]
mibondsw4 <- mtailpopt$weights[3]
mistocksw4 <- mtailpopt$weights[4]
mtailpret <-
  mbondsw4 * AggBonds_ret + mstocksw4 * mstocksret + mibondsw4 * mibondsret + mistocksw4 * mistocksret
colnames(mtailpret) <- "mtailpret"

# 4.3.5. Optimized Portfolios Returns Comparison
optcomp = cbind(mmeanpret, mstdevpret, msharpepret, mtailpret)
table.AnnualizedReturns(optcomp)
charts.PerformanceSummary(optcomp)

# 5.3. Investment Costs

# 5.3.1. Compounded Annual Growth Rate Comparison
# Vanguard VFIAX Mutual Fund 0.05% Annual Expense Ratio
msp500cagr <- mlargecagr + 0.0005
colnames(msp500cagr) <- "msp500cagr"
# Morningstar Category Average 1.10% Annual Expense Ratio
mlargemeancagr <- msp500cagr - 0.0110
colnames(mlargemeancagr) <- "mlargemeancagr"
costscomp1 <- cbind(msp500cagr, mlargecagr, mlargemeancagr)
costscomp1

# 5.3.2. Cummulative Returns Comparison
msp500cum <- ((1 + msp500cagr) ^ 10) - 1
colnames(msp500cum) <- "msp500cum"
mlargecum <- ((1 + mlargecagr) ^ 10) - 1
colnames(mlargecum) <- "mlargecum"
mlargemeancum <- ((1 + mlargemeancagr) ^ 10) - 1
colnames(mlargemeancum) <- "mlargemeancum"
costscomp2 <- cbind(msp500cum, mlargecum, mlargemeancum)
costscomp2

# 5.3.3. Hedge Funds Comparison
table.AnnualizedReturns(hedgecomp)
charts.PerformanceSummary(hedgecomp)
