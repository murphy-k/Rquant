# Volatility Trading Analysis with R
# Options Trading Strategies


# 1. Load R Packages
library("Quandl")
library("quantmod")
library("PerformanceAnalytics")
Quandl.api_key("shVi-_QjPbmUAfvVBMzw")

# 2. Data Downloading
otickers <- c("CBOE/BXMD", "CBOE/PUT", "CBOE/VXTH")
otickers2 <- c("^GSPC", "^SP500TR")
otickers3 <- c("SPY", "PBP", "PUTW", "VIXH")
odata <-
  Quandl(otickers,
         type = "xts",
         start_date = "1997-01-01",
         end_date = "2017-01-01")
getSymbols(otickers2,
           src = 'yahoo',
           from = "1997-01-01",
           to = "2017-01-01")
odata <- cbind(GSPC[, 6], SP500TR[, 6], odata[, 1:3])
getSymbols(otickers3,
           src = 'yahoo',
           from = "1997-01-01",
           to = "2017-01-01")
odata2 <- cbind(SPY[, 6], PBP[, 6], PUTW[, 6], VIXH[, 6])

# 2.1. Returns Risk Assessment Data
ospxdata <- odata[, 1]
ospxdata <- ospxdata[complete.cases(ospxdata),]

# 2.2. Buy-Write Options Strategy Data
bxmddata <- odata[, 2:3]
bxmddata <- bxmddata[complete.cases(bxmddata),]
pbpdata <- odata2[, 1:2]
pbpdata <- pbpdata[complete.cases(pbpdata),]

# 2.3. Put-Write Options Strategy Data
putdata <- cbind(odata[, 2], odata[, 4])
putdata <- putdata[complete.cases(putdata),]
putwdata1 <- cbind(odata2[, 1], odata2[, 3])
putwdata1 <- putwdata1[complete.cases(putwdata1),]
putwdata2 <-  cbind(odata[, 4], odata2[, 3])
putwdata2 <- putwdata2[complete.cases(putwdata2),]

# 2.4. Volatility Tail Hedge Options Strategy Data
vxthdata <- cbind(odata[, 2], odata[, 5])
vxthdata <- vxthdata[complete.cases(vxthdata),]
vixhdata1 <- cbind(odata2[, 1], odata2[, 4])
vixhdata1 <- vixhdata1[complete.cases(vixhdata1),]
vixhdata2 <-  cbind(odata[, 5], odata2[, 4])
vixhdata2 <- vixhdata2[complete.cases(vixhdata2),]

# 3. Returns Risk Assessment

# 3.1. Historical Stocks Index Monthly Returns
spxmret <- ospxdata / Lag(ospxdata, k = 21) - 1
spxmret <- spxmret[complete.cases(spxmret),]
plot(spxmret, main = "Historical Stocks Index Monthly Returns", ylim = c(-0.35, 0.35))
abline(h = mean(spxmret), col = "orange")
abline(h = mean(spxmret) + 3 * sd(spxmret), col = "green")
abline(h = mean(spxmret) - 3 * sd(spxmret), col = "green")
abline(h = mean(spxmret) + 2 * sd(spxmret), col = "red")
abline(h = mean(spxmret) - 2 * sd(spxmret), col = "red")
abline(h = mean(spxmret) + 1 * sd(spxmret), col = "yellow")
abline(h = mean(spxmret) - 1 * sd(spxmret), col = "yellow")


# 3.2. Historical Stocks Index Monthly Returns Normality
qqnorm(spxmret)
qqline(spxmret, col = "red")

# 4. Buy-Write Options Strategy

# 4.1. BXMD Performance Comparison
osptrret1 <- dailyReturn(bxmddata[, 1], type = "arithmetic")
bxmdret <- dailyReturn(bxmddata[, 2], type = "arithmetic")
colnames(osptrret1) <- "osptrret1"
colnames(bxmdret) <- "bxmdret"
bxmdcomp <- cbind(bxmdret, osptrret1)
charts.PerformanceSummary(bxmdcomp)
table.AnnualizedReturns(bxmdcomp)
chart.RiskReturnScatter(bxmdcomp)
sapply(bxmdcomp, max)
sapply(bxmdcomp, min)
skewness(bxmdcomp)
kurtosis(bxmdcomp)

# 4.2. PBP Performance Comparison
ospyret1 <- dailyReturn(pbpdata[, 1], type = "arithmetic")
pbpret1 <- dailyReturn(pbpdata[, 2], type = "arithmetic")
colnames(ospyret1) <- "ospyret1"
colnames(pbpret1) <- "pbpret1"
pbpcomp <- cbind(pbpret1, ospyret1)
charts.PerformanceSummary(pbpcomp)
table.AnnualizedReturns(pbpcomp)
chart.RiskReturnScatter(pbpcomp)
sapply(pbpcomp, max)
sapply(pbpcomp, min)
skewness(pbpcomp)
kurtosis(pbpcomp)

# 5. Put-Write Options Strategy

# 5.1. PUT Performance Comparison
osptrret2 <- dailyReturn(putdata[, 1], type = "arithmetic")
putret1 <- dailyReturn(putdata[, 2], type = "arithmetic")
colnames(osptrret2) <- "osptrret2"
colnames(putret1) <- "putret1"
putcomp <- cbind(putret1, osptrret2)
charts.PerformanceSummary(putcomp)
table.AnnualizedReturns(putcomp)
chart.RiskReturnScatter(putcomp)
sapply(putcomp, max)
sapply(putcomp, min)
skewness(putcomp)
kurtosis(putcomp)

# 5.2. PUTW Performance Comparison
ospyret2 <- dailyReturn(putwdata1[, 1], type = "arithmetic")
putwret1 <- dailyReturn(putwdata1[, 2], type = "arithmetic")
colnames(ospyret2) <- "ospyret2"
colnames(putwret1) <- "putwret1"
putwcomp1 <- cbind(putwret1, ospyret2)
charts.PerformanceSummary(putwcomp1)
table.AnnualizedReturns(putwcomp1)
chart.RiskReturnScatter(putwcomp1)
sapply(putwcomp1, max)
sapply(putwcomp1, min)
skewness(putwcomp1)
kurtosis(putwcomp1)

# 5.3. PUTW Tracking Comparison
putret2 <- dailyReturn(putwdata2[, 1], type = "arithmetic")
putwret2 <- dailyReturn(putwdata2[, 2], type = "arithmetic")
colnames(putret2) <- "putret2"
colnames(putwret2) <- "putwret2"
putwcomp2 <- cbind(putwret2, putret2)
charts.PerformanceSummary(putwcomp2)
table.AnnualizedReturns(putwcomp2)
chart.RiskReturnScatter(putwcomp2)
sapply(putwcomp2, max)
sapply(putwcomp2, min)
skewness(putwcomp2)
kurtosis(putwcomp2)

# 6. Volatility Tail Hedge Options Strategy

# 6.1. VXTH Performance Comparison
osptrret3 <- dailyReturn(vxthdata[, 1], type = "arithmetic")
vxthret1 <- dailyReturn(vxthdata[, 2], type = "arithmetic")
colnames(osptrret3) <- "osptrret3"
colnames(vxthret1) <- "vxthret1"
vxthcomp <- cbind(vxthret1, osptrret3)
charts.PerformanceSummary(vxthcomp)
table.AnnualizedReturns(vxthcomp)
chart.RiskReturnScatter(vxthcomp)
sapply(vxthcomp, max)
sapply(vxthcomp, min)
skewness(vxthcomp)
kurtosis(vxthcomp)

# 6.2. VIXH Performance Comparison
ospyret3 <- dailyReturn(vixhdata1[, 1], type = "arithmetic")
vixhret1 <- dailyReturn(vixhdata1[, 2], type = "arithmetic")
colnames(ospyret3) <- "ospyret3"
colnames(vixhret1) <- "vixhret1"
vixhcomp1 <- cbind(vixhret1, ospyret3)
charts.PerformanceSummary(vixhcomp1)
table.AnnualizedReturns(vixhcomp1)
chart.RiskReturnScatter(vixhcomp1)
sapply(vixhcomp1, max)
sapply(vixhcomp1, min)
skewness(vixhcomp1)
kurtosis(vixhcomp1)

# 6.3. VIXH Tracking Comparison
vxthret2 <- dailyReturn(vixhdata2[, 1], type = "arithmetic")
vixhret2 <- dailyReturn(vixhdata2[, 2], type = "arithmetic")
colnames(vxthret2) <- "vxthret2"
colnames(vixhret2) <- "vixhret2"
vixhcomp2 <- cbind(vixhret2, vxthret2)
charts.PerformanceSummary(vixhcomp2)
table.AnnualizedReturns(vixhcomp2)
chart.RiskReturnScatter(vixhcomp2)
sapply(vixhcomp2, max)
sapply(vixhcomp2, min)
skewness(vixhcomp2)
kurtosis(vixhcomp2)
