# Multiple Regression Analysis with R

#####
# 1. Load R Packages
library("xts")
library("e1071")
library("MASS")
library("tseries")
library("lmtest")
library("car")
library("forecast") 
#####
# 2. Data Reading
data <-
  read.csv(file = "Multiple-Regression-Analysis-Data.txt", header = T)
dataxts <-
  xts(data[, 2:10], order.by = as.Date(data[, 1], origin = "1899-12-30"))

# 3. Variables Definition

# 3.1. Dependent Variable
stocks <- dataxts[, 1]
plot(stocks, main = "stocks dependent variable")
summary(stocks)

# 3.2. Independent Variables

# 3.2.1. Rates Independent Variables
t1y <- dataxts[, 2]
plot(t1y, main = "t1y independent variable")
summary(t1y)
t10y <- dataxts[, 3]
plot(t10y, main = "t10y independent variable")
summary(t10y)
hyield <- dataxts[, 4]
plot(hyield, main = "hyield independent variable")
summary(hyield)

# 3.2.2. Prices Independent Variables
cpi <- dataxts[, 5]
plot(cpi, main = "cpi independent variable")
summary(cpi)
ppi <- dataxts[, 6]
plot(ppi, main = "ppi independent variable")
summary(ppi)
oil <- dataxts[, 7]
plot(oil, main = "oil independent variable")
summary(oil)

# 3.2.3. Macroeconomic Independent Variables
indpro <- dataxts[, 8]
plot(indpro, main = "indpro independent variable")
summary(indpro)
pce <- dataxts[, 9]
plot(pce, main = "pce independent variable")
summary(pce)

# 3.3. Variables Descriptive Statistics
vmean <- sapply(dataxts, mean)
vmean
vsd <- sapply(dataxts, sd)
vsd
vskew <- sapply(dataxts, skewness)
vskew
vkurt <- sapply(dataxts, kurtosis)
vkurt

# 3.4. Independent Variables Standardization
st1y <- (t1y - mean(t1y)) / sd(t1y)
st10y <- (t10y - mean(t10y)) / sd(t10y)
shyield <- (hyield - mean(hyield)) / sd(hyield)
scpi <- (cpi - mean(cpi)) / sd(cpi)
sppi <- (ppi - mean(ppi)) / sd(ppi)
soil <- (oil - mean(oil)) / sd(oil)
sindpro <- (indpro - mean(indpro)) / sd(indpro)
spce <- (pce - mean(pce)) / sd(pce)
#####
# 4. Multiple Regression
lmreg <- lm(stocks ~ st1y + st10y + shyield + scpi + sppi + soil + sindpro +
              spce)
summary(lmreg)
sivar <- cbind(st1y, st10y, shyield, scpi, sppi, soil, sindpro, spce)
anova(lm(stocks ~ sivar))

# 4.1. Multiple Regression Chart
plot(stocks, main = "Multiple Regression Chart")
lines(lmreg$fitted.values, col = "darkblue")
legend(
  "bottomright",
  col = c("black", "darkblue"),
  lty = 1,
  legend = c("stocks", "lmreg$fitted.values"),
  cex = 0.5
)

# 5. Multiple Regression Assumptions

# 5.1. Correct Specification
summary(lmreg)

# 5.1.1. Variable Selection (Step 1)
csreg1 <- lm(stocks ~ st10y + shyield + scpi + sppi + soil + sindpro + spce)
summary(csreg1)

# 5.1.2. Variable Selection (Step 2)
csreg2 <- lm(stocks ~ st10y + shyield + scpi + soil + sindpro + spce)
summary(csreg2)

# 5.1.3. Variable Selection (Step 3)
csreg3 <- lm(stocks ~ st10y + shyield + scpi + soil + sindpro)
summary(csreg3)

# 5.2. No Linear Dependency
# Multicollinearity Test
# No Multicollinerity = Principal Diagonal < 10
csvar <- cbind(st10y, shyield, scpi, soil, sindpro)
cmat <- cor(csvar)
cmat
icmat <- ginv(cmat)
icmat

# 5.3. Correct Functional Form
# Ramsey-RESET Test
# Linearity (Ramsey-RESET test) = fitted.values^2 p-value > 0.05
fitted2 <- csreg3$fitted.values ^ 2
cffreg <- lm(stocks ~ st10y + shyield + scpi + soil + sindpro + fitted2)
summary(cffreg)

# 5.3.1. Nonlinearity (Step 1)
st10y2 <- st10y ^ 2
shyield2 <- shyield ^ 2
scpi2 <- scpi ^ 2
soil2 <- soil ^ 2
sindpro2 <- sindpro ^ 2
nlreg1 <-
  lm(stocks ~ st10y + st10y2 + shyield + shyield2 + scpi + scpi2 + soil +
       soil2 + sindpro + sindpro2)
summary(nlreg1)

# 5.3.2. Nonlinearity (Step 2)
nlreg2 <- lm(stocks ~ st10y + shyield + scpi + soil + sindpro + sindpro2)
summary(nlreg2)

# 5.4. Residuals Normality
# Jarque-Bera Test
# Normality (Jarque-Bera test) = residuals Jarque-Bera statistic < 5.99
nlreg2res <- nlreg2$residuals
jarque.bera.test(nlreg2res)

# 5.5. Residuals No Autocorrelation
# Breusch-Godfrey Test
# No Autocorrelation (Breusch-Godfrey test) = residuals(-1) p-value > 0.05

# 5.5.1. Residuals No Autocorrelation Breusch-Godfrey Test 1
lnlreg2res <- lag(nlreg2res, k = 1)
lnlreg2res[is.na(lnlreg2res)] <- 0
nareg <-
  lm(nlreg2res ~ st10y + shyield + scpi + soil + sindpro + sindpro2 + lnlreg2res)
summary(nareg)

# 5.5.2. Residuals No Autocorrelation Breusch-Godfrey Test 2
bgtest(stocks ~ st10y + shyield + scpi + soil + sindpro + sindpro2)

# 5.3. Residuals Homoscedasticity
# White and Breusch-Pagan Tests
# Homoscedasticity (White test) = significance F > 0.05
# Homoscedasticity (Breusch-Pagan test) = significance F > 0.05

# 5.3.1. Residuals Homoscedasticity White Test 1
nlreg2res2 <- nlreg2res ^ 2
sindpro22 <- sindpro2 ^ 2
rhreg1 <-
  lm(
    nlreg2res2 ~ st10y + st10y2 + shyield + shyield2 + scpi + scpi2 + soil +
      soil2 + sindpro + sindpro2 + sindpro22
  )
summary(rhreg1)

# 5.3.2. Residuals Homoscedasticity Breusch-Pagan Test 1
bptest(stocks ~ st10y + shyield + scpi + soil + sindpro + sindpro2)

# 5.3.3. Box-Cox Transformation
qqnorm(stocks)
qqline(stocks, col = "red")
c <- 0.00001 - min(stocks)
pstocks <- stocks + c
lambda <- powerTransform(pstocks)
bcstocks <- bcPower(pstocks, lambda = lambda$lambda)
qqnorm(bcstocks)
qqline(bcstocks, col = "red")

# 5.3.4. Residuals Homoscedasticity White Test 2
bcreg <- lm(bcstocks ~ st10y + shyield + scpi + soil + sindpro + sindpro2)
bcregres2 <- bcreg$residuals ^ 2
rhreg2 <-
  lm(
    bcregres2 ~ st10y + st10y2 + shyield + shyield2 + scpi + scpi2 + soil +
      soil2 + sindpro + sindpro2 + sindpro22
  )
summary(rhreg2)

# 5.3.5. Residuals Homoscedasticity Breusch-Pagan Test 2
bptest(bcstocks ~ st10y + shyield + scpi + soil + sindpro + sindpro2)

# 6. Forecasting Accuracy

# 6.1. Training and Testing Ranges
tdata <- window(dataxts, end = "2012-12-31")
fdata <- window(dataxts, start = "2013-01-31")

# 6.2. Variable Definition

# 6.2.1. Dependent Variable
tstocks <- tdata[, 1]

# 6.2.2. Independent Variables
tt1y <- tdata[, 2]
tt10y <- tdata[, 3]
thyield <- tdata[, 4]
tcpi <- tdata[, 5]
tppi <- tdata[, 6]
toil <- tdata[, 7]
tindpro <- tdata[, 8]
tpce <- tdata[, 9]

# 6.2.3. Independent Variables Standardization
stt1y <- (tt1y - mean(tt1y)) / sd(tt1y)
stt10y <- (tt10y - mean(tt10y)) / sd(tt10y)
sthyield <- (thyield - mean(thyield)) / sd(thyield)
stcpi <- (tcpi - mean(tcpi)) / sd(tcpi)
stppi <- (tppi - mean(tppi)) / sd(tppi)
stoil <- (toil - mean(toil)) / sd(toil)
stindpro <- (tindpro - mean(tindpro)) / sd(tindpro)
stpce <- (tpce - mean(tpce)) / sd(tpce)

# 6.3. Multiple Regression
tlmreg <-
  lm(tstocks ~ stt1y + stt10y + sthyield + stcpi + stppi + stoil + stindpro +
       stpce)
summary(tlmreg)

# 6.4. Correct Specification
summary(tlmreg)

# 6.4.1. Variable Selection (Step 1)
tcsreg1 <-
  lm(tstocks ~ stt10y + sthyield + stcpi + stppi + stoil + stindpro + stpce)
summary(tcsreg1)

# 6.4.2. Variable Selection (Step 2)
tcsreg2 <- lm(tstocks ~ stt10y + sthyield + stcpi + stppi + stindpro + stpce)
summary(tcsreg2)

# 6.4.3. Variable Selection (Step 3)
tcsreg3 <- lm(tstocks ~ sthyield + stcpi + stppi + stindpro + stpce)
summary(tcsreg3)

# 6.4.4. Variable Selection (Step 4)
tcsreg4 <- lm(tstocks ~ sthyield + stcpi + stindpro + stpce)
summary(tcsreg4)

# 6.4.5. Variable Selection (Step 5)
tcsreg5 <- lm(tstocks ~ sthyield + stindpro + stpce)
summary(tcsreg5)

# 6.4.6. Variable Selection (Step 6)
tcsreg6 <- lm(tstocks ~ sthyield + stpce)
summary(tcsreg6)

# 6.4.7. Variable Selection (Step 7)
tcsreg7 <- lm(tstocks ~ sthyield)
summary(tcsreg7)

# 6.5. Correct Functional Form
# Ramsey-RESET Test
# Linearity (Ramsey-RESET test) = fitted.values^2 p-value > 0.05
tfitted2 <- tcsreg7$fitted.values ^ 2
tcffreg <- lm(tstocks ~ sthyield + tfitted2)
summary(tcffreg)

# 6.6. Residuals Normality
# Jarque-Bera Test
# Normality (Jarque-Bera test) = residuals Jarque-Bera statistic < 5.99
tcsreg7res <- tcsreg7$residuals
jarque.bera.test(tcsreg7res)

# 6.7. Residuals No Autocorrelation
# Breusch-Godfrey Test
# No Autocorrelation (Breusch-Godfrey test) = residuals(-1) p-value > 0.05

# 6.7.1. Residuals No Autocorrelation Breusch-Godfrey Test 1
ltcsreg7res <- lag(tcsreg7res, k = 1)
ltcsreg7res[is.na(ltcsreg7res)] <- 0
tnareg <- lm(tcsreg7res ~ sthyield + ltcsreg7res)
summary(tnareg)

# 6.7.2. Residuals No Autocorrelation Breusch-Godfrey Test 2
bgtest(tstocks ~ sthyield)

# 6.8. Residuals Homoscedasticity
# White and Breusch-Pagan Tests
# Homoscedasticity (White test) = significance F > 0.05
# Homoscedasticity (Breusch-Pagan test) = significance F > 0.05

# 6.8.1. Residuals Homoscedasticity White Test 1
tcsreg7res2 <- tcsreg7res ^ 2
sthyield2 <- sthyield ^ 2
trhreg1 <- lm(tcsreg7res2 ~ sthyield + sthyield2)
summary(trhreg1)

# 6.8.2. Residuals Homoscedasticity Breusch-Pagan Test 1
bptest(tstocks ~ sthyield)

# 6.8.3. Box-Cox Transformation
qqnorm(tstocks)
qqline(tstocks, col = "red")
tc <- 0.00001 - min(tstocks)
tpstocks <- tstocks + tc
tlambda <- powerTransform(tpstocks)
tbcstocks <- bcPower(tpstocks, lambda = tlambda$lambda)
qqnorm(tbcstocks)
qqline(tbcstocks, col = "red")

# 6.8.4. Residuals Homoscedasticity White Test 2
tbcreg <- lm(tbcstocks ~ sthyield)
tbcregres2 <- tbcreg$residuals ^ 2
trhreg2 <- lm(tbcregres2 ~ sthyield + sthyield2)
summary(trhreg2)

# 6.8.5. Residuals Homoscedasticity Breusch-Pagan Test 2
bptest(tbcstocks ~ sthyield)

# 6.9. Forecating Accuracy Comparison
fstocks <- fdata[, 1]
lfstocks <- lag(fstocks, k = 1)
lfstocks[is.na(lfstocks)] <- 0
fhyield <- fdata[, 4]
sfhyield <- (fhyield - mean(fhyield)) / sd(fhyield)
mstocks <-
  tcsreg7$coefficients[1] + tcsreg7$coefficients[2] * sfhyield

# 6.9.1. Forecating Accuracy Tests
mstocks2 <- as.numeric(mstocks)
fstocks2 <- as.numeric(fstocks)
lfstocks2 <- as.numeric(lfstocks)
accuracy(mstocks2, fstocks2)
accuracy(lfstocks2, fstocks2)
mase <-
  accuracy(mstocks2, fstocks2)[5] / accuracy(lfstocks2, fstocks2)[5]
mase

# 6.9. Forecating Accuracy Chart
plot(fstocks, main = "Forecasting Accuracy Chart")
lines(lfstocks, col = "darkblue")
lines(mstocks, col = "darkred")
legend(
  "bottomleft",
  col = c("black", "darkblue", "darkred"),
  lty = 1,
  legend = c("fstocks", "lfstocks", "mstocks"),
  cex = 0.5
)
