# 1. Regression Machine Learning Data ####

# 1.1. Load R packages 
library("caret")
library("corrplot")
library("e1071")
library("elasticnet")
library("forecast")
library("kernlab")
library("lars")
library("nnet")
library("party")
library("penalized")
library("plyr")
library("quantmod")
library("randomForest")
library("rpart")
library("xgboost")

# 1.2. Data Reading
data <- read.csv("/Users/murphyky18/rQuant/Machine Learning/Regression-Machine-Learning-Data.txt",header=T)
spy <- xts(data[,2],order.by=as.Date(data[,1]))

# 2. Feature Creation ####

# 2.1. Target Feature
rspy <- dailyReturn(spy,type="arithmetic")

# 2.2. Predictor Features
rspy1 <- lag(rspy,k=1)
rspy2 <- lag(rspy,k=2)
rspy3 <- lag(rspy,k=3)
rspy4 <- lag(rspy,k=4)
rspy5 <- lag(rspy,k=5)
rspy6 <- lag(rspy,k=6)
rspy7 <- lag(rspy,k=7)
rspy8 <- lag(rspy,k=8)
rspy9 <- lag(rspy,k=9)

# 2.3. All Features
rspyall <- cbind(rspy,rspy1,rspy2,rspy3,rspy4,rspy5,rspy6,rspy7,rspy8,rspy9)
colnames(rspyall) <- c("rspy","rspy1","rspy2","rspy3","rspy4","rspy5","rspy6","rspy7","rspy8","rspy9")
rspyall <- na.exclude(rspyall)

# 3. Range Delimiting ####

# 3.1. Training Range
rspyt <- window(rspyall,end="2014-01-01")

# 3.2. Testing Range
rspyf <- window(rspyall,start="2014-01-01")

# 4. Predictor Features Selection ####

# 4.2. Predictor Features Linear Regression
lmta <- lm(rspy~rspy1+rspy2+rspy3+rspy4+rspy5+rspy6+rspy7+rspy8+rspy9,data=rspyt)
summary(lmta)
lmtb <- lm(rspy~rspy1+rspy2+rspy5,data=rspyt)
summary(lmtb)
# Clear Plots area before running code
par(mfrow=c(1,2))
plot(coredata(rspyt$rspy1),coredata(rspyt$rspy),xlab="rspy1t",ylab="rspyt")
abline(lm(rspyt$rspy~rspyt$rspy1),col="red")
plot(coredata(rspyt$rspy2),coredata(rspyt$rspy),xlab="rspy2t",ylab="rspyt")
abline(lm(rspyt$rspy~rspyt$rspy2),col="red")
plot(coredata(rspyt$rspy5),coredata(rspyt$rspy),xlab="rspy5t",ylab="rspyt")
abline(lm(rspyt$rspy~rspyt$rspy5),col="red")
par(mfrow=c(1,1))

# 4.2. Predictor Features Correlation
crspyt <- round(cor(rspyt[,2:10]),2)
crspyt
corrplot(crspyt,type="lower")

# 4.3. Predictor Features Selection Filter Methods

# 4.3.1. Univariate Filters
# Selection by filter control
sbfctrlt <- sbfControl(functions=lmSBF) 
sbft <- sbf(rspy~rspy1+rspy2+rspy3+rspy4+rspy5+rspy6+rspy7+rspy8+rspy9,data=rspyt,sbfControl=sbfctrlt)
sbft

# 4.4. Predictor Features Selection Wrapper Methods

# 4.4.1. Recursive Feature Elimination
# Deterministic wrapper method - Recursive feature elimination
rfectrlt <- rfeControl(functions=lmFuncs)
rfet <- rfe(rspy~rspy1+rspy2+rspy3+rspy4+rspy5+rspy6+rspy7+rspy8+rspy9,data=rspyt,rfeControl=rfectrlt)
rfet

# 4.5. Predictor Features Selection Embedded Methods
# Least absolute shrinkage and selection operator
lassot <- train(rspy~rspy1+rspy2+rspy3+rspy4+rspy5+rspy6+rspy7+rspy8+rspy9,data=rspyt,method="lasso")
predictors(lassot)

# 4.6. Predictor Features Extraction

# 4.6.1. Principal Component Analysis
pcat <- princomp(rspyt[,2:10])
summary(pcat)
plot(pcat)

# 5. Algorithm Training and Testing ####

# 5.1. Algorithm Training Optimal Parameters Selection Control

# 5.1.1. Time Series Cross-Validation
tsctrlt <-# time series control - t (within training rage)
  trainControl(
    method = "timeslice", # by time
    initialWindow = 1008, # within this training range - 1008/252=4yrs
    horizon = 252,        # test within training range (1yr)
    fixedWindow = TRUE,   # rolling time series cross validation
    skip = 252            # step forward is same size as testing range 
  )

# 6. Generalized Linear Models ####

# 6.1. Linear Regression

# 6.1.1. Linear Regression Training
lmsa <- Sys.time()
lmta <- train(rspy~rspy1+rspy2+rspy5,data=rspyt,method="lm")
lmea <- Sys.time()
lmea-lmsa
lmsb <- Sys.time()
lmtb <- train(rspy~rspy1+rspy2+rspy3+rspy4+rspy5+rspy6+rspy7+rspy8+rspy9,data=rspyt,method="lm",preProcess="pca")
lmeb <- Sys.time()
lmeb-lmsb

# Linear Regression Training Results
lmta$results
lmtb$results

# 6.1.2. Linear Regression Testing
lmfa <- predict.train(lmta,newdata=rspyf)
lmfa <- xts(lmfa,order.by=as.Date(names(lmfa)))
lmfb <- predict.train(lmtb,newdata=rspyf)
lmfb <- xts(lmfb,order.by=as.Date(names(lmfb)))

# 6.1.3. Linear Regression Testing Charts
plot(rspyf[,1],type="h",main="Linear Regression A Testing Chart")
lines(lmfa,col="blue")
plot(rspyf[,1],type="h",main="Linear Regression B Testing Chart")
lines(lmfb,col="green")

# 6.1.4. Linear Regression Forecasting Accuracy
# Convert xts to ts for accuracy function
rspyfts <- ts(coredata(rspyf[,1]),frequency=252,start=c(2014,1))
lmftsa <- ts(coredata(lmfa),frequency=252,start=c(2014,1))
lmftsb <- ts(coredata(lmfb),frequency=252,start=c(2014,1))
accuracy(lmftsa,rspyfts)
accuracy(lmftsb,rspyfts)

# 6.2. Elastic Net Regression

# 6.2.1. Elastic Net Regression Training
penlmsa <- Sys.time()
penlmta <-
  train(
    rspy ~ rspy1 + rspy2 + rspy5,
    data = rspyt,
    method = "penalized",
    trControl = tsctrlt
  )
penlmea <- Sys.time()
penlmea - penlmsa

penlmsb <- Sys.time()
penlmtb <-
  train(
    rspy ~ rspy1 + rspy2 + rspy3 + rspy4 + rspy5 + rspy6 + rspy7 + rspy8 + rspy9,
    data = rspyt,
    method = "penalized",
    preProcess = "pca",
    trControl = tsctrlt
  )
penlmeb <- Sys.time()
penlmeb - penlmsb

# Elastic Net Regression Optimal Training Parameters
penlmta$bestTune
plot(penlmta)
penlmtb$bestTune
plot(penlmtb)

# Elastic Net Regression Training Results
penlmta$results
penlmtb$results

# 6.2.2. Elastic Net Regression Testing
penlmfa <- predict.train(penlmta,newdata=rspyf)
penlmfa <- xts(penlmfa,order.by=as.Date(names(penlmfa)))
penlmfb <- predict.train(penlmtb,newdata=rspyf)
penlmfb <- xts(penlmfb,order.by=as.Date(names(penlmfb)))

# 6.2.3. Elastic Net Regression Testing Charts
plot(rspyf[,1],type="l",main="Elastic Net Regression A Testing Chart")
lines(penlmfa,col="blue")
plot(rspyf[,1],type="l",main="Elastic Net Regression B Testing Chart")
lines(penlmfb,col="green")

# 6.2.4. Elastic Net Regression Forecasting Accuracy
# Convert xts to ts for accuracy function
penlmftsa <- ts(coredata(penlmfa),frequency=252,start=c(2014,1))
penlmftsb <- ts(coredata(penlmfb),frequency=252,start=c(2014,1))
accuracy(penlmftsa,rspyfts)
accuracy(penlmftsb,rspyfts)

# 7. Similarity Methods ####

# 7.1. K Nearest Neighbors KNN Regression 

# 7.1.1. KNN Training
knnsa <- Sys.time()
knnta <- train(rspy~rspy1+rspy2+rspy5,data=rspyt,method="knn",trControl=tsctrlt)
knnea <- Sys.time()
knnea-knnsa
knnsb <- Sys.time()
knntb <- train(rspy~rspy1+rspy2+rspy3+rspy4+rspy5+rspy6+rspy7+rspy8+rspy9,data=rspyt,method="knn",preProcess="pca",trControl=tsctrlt)
knneb <- Sys.time()
knneb-knnsb

# KNN Regression Optimal Training Parameters
knnta$bestTune
plot(knnta)
knntb$bestTune
plot(knntb)

# KNN Regression Training Results
knnta$results
knntb$results

# 7.1.2. KNN Regression Testing
knnfa <- predict.train(knnta,newdata=rspyf)
knnfa <- cbind(index(rspyf),as.data.frame(knnfa))
knnfa <- xts(knnfa[,2],order.by=as.Date(knnfa[,1]))
knnfb <- predict.train(knntb,newdata=rspyf)
knnfb <- cbind(index(rspyf),as.data.frame(knnfb))
knnfb <- xts(knnfb[,2],order.by=as.Date(knnfb[,1]))

# 7.1.3. KNN Regression Testing Charts
plot(rspyf[,1],type="h",main="KNN Regression A Testing Chart")
lines(knnfa,col="blue")
plot(rspyf[,1],type="h",main="KNN Regression B Testing Chart")
lines(knnfb,col="green")

# 7.1.4. KNN Regression Forecasting Accuracy
# Convert xts to ts for accuracy function
knnftsa <- ts(coredata(knnfa),frequency=252,start=c(2014,1))
knnftsb <- ts(coredata(knnfb),frequency=252,start=c(2014,1))
accuracy(knnftsa,rspyfts)
accuracy(knnftsb,rspyfts)

# 8. Frequency Methods ####

# 8.1. Decision Tree DT Regression 

# 8.1.1. DT Training
dtsa <- Sys.time()
dtta <-
  train(
    rspy ~ rspy1 + rspy2 + rspy5,
    data = rspyt,
    method = "rpart2",
    trControl = tsctrlt
  )
dtea <- Sys.time()
dtea - dtsa
dtsb <- Sys.time()
dttb <-
  train(
    rspy ~ rspy1 + rspy2 + rspy3 + rspy4 + rspy5 + rspy6 + rspy7 + rspy8 + rspy9,
    data = rspyt,
    method = "rpart2",
    preProcess = "pca",
    trControl = tsctrlt
  )
dteb <- Sys.time()
dteb - dtsb

# DT Regression Optimal Training Parameters
dtta$bestTune
plot(dtta)
dttb$bestTune
plot(dttb)

# DT Regression Training Results
dtta$results
dttb$results

# 8.1.2. DT Regression Testing
dtfa <- predict.train(dtta,newdata=rspyf)
dtfa <- xts(dtfa,order.by=as.Date(names(dtfa)))
dtfb <- predict.train(dttb,newdata=rspyf)
dtfb <- xts(dtfb,order.by=as.Date(names(dtfb)))

# 8.1.3. DT Regression Testing Charts
plot(rspyf[,1],type="l",main="DT Regression A Testing Chart")
lines(dtfa,col="blue")
plot(rspyf[,1],type="l",main="DT Regression B Testing Chart")
lines(dtfb,col="green")

# 8.1.4. DT Regression Forecasting Accuracy
# Convert xts to ts for accuracy function
dtftsa <- ts(coredata(dtfa),frequency=252,start=c(2014,1))
dtftsb <- ts(coredata(dtfb),frequency=252,start=c(2014,1))
accuracy(dtftsa,rspyfts)
accuracy(dtftsb,rspyfts)

# 9. Ensemble Methods ####

# 9.1. Random Forest RF Regression

# 9.1.1. RF Training
rfsa <- Sys.time()
rfta <- train(rspy~rspy1+rspy2+rspy5,data=rspyt,method="rf",tuneLength=2)
rfea <- Sys.time()
rfea-rfsa
rfsb <- Sys.time()
rftb <- train(rspy~rspy1+rspy2+rspy3+rspy4+rspy5+rspy6+rspy7+rspy8+rspy9,data=rspyt,method="rf",preProcess="pca",tuneLength=2)
rfeb <- Sys.time()
rfeb-rfsb

# RF Regression Optimal Training Parameters
rfta$bestTune
plot(rfta)
rftb$bestTune
plot(rftb)

# RF Regression Training Results
rfta$results
rftb$results

# 9.1.2. RF Regression Testing
rffa <- predict.train(rfta,newdata=rspyf)
rffa <- xts(rffa,order.by=as.Date(names(rffa)))
rffb <- predict.train(rftb,newdata=rspyf)
rffb <- xts(rffb,order.by=as.Date(names(rffb)))

# 9.1.3. RF Regression Testing Charts
plot(rspyf[,1],type="l",main="RF Regression A Testing Chart")
lines(rffa,col="blue")
plot(rspyf[,1],type="l",main="RF Regression B Testing Chart")
lines(rffb,col="green")

# 9.1.4. RF Regression Forecasting Accuracy
# Convert xts to ts for accuracy function
rfftsa <- ts(coredata(rffa),frequency=252,start=c(2014,1))
rfftsb <- ts(coredata(rffb),frequency=252,start=c(2014,1))
accuracy(rfftsa,rspyfts)
accuracy(rfftsb,rspyfts)

# 9.2. Extreme Gradient Boosting Machine XGBM Regression

# 9.2.1. XGBM Training
xgbmsa <- Sys.time()
xgbmta <- train(rspy~rspy1+rspy2+rspy5,data=rspyt,method="xgbTree",tuneLength=2)
xgbmea <- Sys.time()
xgbmea-xgbmsa
xgbmsb <- Sys.time()
xgbmtb <- train(rspy~rspy1+rspy2+rspy3+rspy4+rspy5+rspy6+rspy7+rspy8+rspy9,data=rspyt,method="xgbTree",preProcess="pca",tuneLength=2)
xgbmeb <- Sys.time()
xgbmeb-xgbmsb

# XGBM Regression Optimal Training Parameters
xgbmta$bestTune
plot(xgbmta)
xgbmtb$bestTune
plot(xgbmtb)

# XGBM Regression Training Results
xgbmta$results
xgbmtb$results

# 9.2.2. XGBM Regression Testing
xgbmfa <- predict.train(xgbmta,newdata=rspyf)
xgbmfa <- cbind(index(rspyf),as.data.frame(xgbmfa))
xgbmfa <- xts(xgbmfa[,2],order.by=as.Date(xgbmfa[,1]))
xgbmfb <- predict.train(xgbmtb,newdata=rspyf)
xgbmfb <- cbind(index(rspyf),as.data.frame(xgbmfb))
xgbmfb <- xts(xgbmfb[,2],order.by=as.Date(xgbmfb[,1]))

# 9.2.3. XGBM Regression Testing Charts
plot(rspyf[,1],type="l",main="XGBM Regression A Testing Chart")
lines(xgbmfa,col="blue")
plot(rspyf[,1],type="l",main="XGBM Regression B Testing Chart")
lines(xgbmfb,col="green")

# 9.2.4. XGBM Regression Forecasting Accuracy
# Convert xts to ts for accuracy function
xgbmftsa <- ts(coredata(xgbmfa),frequency=252,start=c(2014,1))
xgbmftsb <- ts(coredata(xgbmfb),frequency=252,start=c(2014,1))
accuracy(xgbmftsa,rspyfts)
accuracy(xgbmftsb,rspyfts)

# 10. Maximum Margin Methods ####

# 10.1. Linear Support Vector Machine SVM Regression

# 10.1.1. Linear SVM Regression Training
lsvmsa <- Sys.time()
lsvmta <- train(rspy~rspy1+rspy2+rspy5,data=rspyt,method="svmLinear2",trControl=tsctrlt)
lsvmea <- Sys.time()
lsvmea-lsvmsa
lsvmsb <- Sys.time()
lsvmtb <- train(rspy~rspy1+rspy2+rspy3+rspy4+rspy5+rspy6+rspy7+rspy8+rspy9,data=rspyt,method="svmLinear2",preProcess="pca",trControl=tsctrlt)
lsvmeb <- Sys.time()
lsvmeb-lsvmsb

# Linear SVM Optimal Training Parameters
lsvmta$bestTune
plot(lsvmta)
lsvmtb$bestTune
plot(lsvmtb)

# Linear SVM Regression Training Results
lsvmta$results
lsvmtb$results

# 10.1.2. Linear SVM Regression Testing
lsvmfa <- predict.train(lsvmta,newdata=rspyf)
lsvmfa <- xts(lsvmfa,order.by=as.Date(names(lsvmfa)))
lsvmfb <- predict.train(lsvmtb,newdata=rspyf)
lsvmfb <- xts(lsvmfb,order.by=as.Date(names(lsvmfb)))

# 10.1.3. Linear SVM Regression Testing Charts
plot(rspyf[,1],type="l",main="Linear SVM Regression A Testing Chart")
lines(lsvmfa,col="blue")
plot(rspyf[,1],type="l",main="Linear SVM Regression B Testing Chart")
lines(lsvmfb,col="green")

# 10.1.4. Linear SVM Regression Forecasting Accuracy
# Convert xts to ts for accuracy function
lsvmftsa <- ts(coredata(lsvmfa),frequency=252,start=c(2014,1))
lsvmftsb <- ts(coredata(lsvmfb),frequency=252,start=c(2014,1))
accuracy(lsvmftsa,rspyfts)
accuracy(lsvmftsb,rspyfts)

# 10.2. Radial Basis Function RBF Support Vector Machine SVM Regression

# 10.2.1. RBF SVM Regression Training
rsvmsa <- Sys.time()
rsvmta <- train(rspy~rspy1+rspy2+rspy5,data=rspyt,method="svmRadial",trControl=tsctrlt)
rsvmea <- Sys.time()
rsvmea-rsvmsa
rsvmsb <- Sys.time()
rsvmtb <- train(rspy~rspy1+rspy2+rspy3+rspy4+rspy5+rspy6+rspy7+rspy8+rspy9,data=rspyt,method="svmRadial",preProcess="pca",trControl=tsctrlt)
rsvmeb <- Sys.time()
rsvmeb-rsvmsb

# RBF SVM Optimal Training Parameters
rsvmta$bestTune
plot(rsvmta)
rsvmtb$bestTune
plot(rsvmtb)

# RBF SVM Regression Training Results
rsvmta$results
rsvmtb$results

# 10.2.2. RBF SVM Regression Testing
rsvmfa <- predict.train(rsvmta,newdata=rspyf)
rsvmfa <- cbind(index(rspyf),as.data.frame(rsvmfa))
rsvmfa <- xts(rsvmfa[,2],order.by=as.Date(rsvmfa[,1]))
rsvmfb <- predict.train(rsvmtb,newdata=rspyf)
rsvmfb <- cbind(index(rspyf),as.data.frame(rsvmfb))
rsvmfb <- xts(rsvmfb[,2],order.by=as.Date(rsvmfb[,1]))

# 10.2.3. RBF SVM Regression Testing Charts
plot(rspyf[,1],type="l",main="RBF SVM Regression A Testing Chart")
lines(rsvmfa,col="blue")
plot(rspyf[,1],type="l",main="RBF SVM Regression B Testing Chart")
lines(rsvmfb,col="green")

# 10.2.4. RBF SVM Regression Forecasting Accuracy
# Convert xts to ts for accuracy function
rsvmftsa <- ts(coredata(rsvmfa),frequency=252,start=c(2014,1))
rsvmftsb <- ts(coredata(rsvmfb),frequency=252,start=c(2014,1))
accuracy(rsvmftsa,rspyfts)
accuracy(rsvmftsb,rspyfts)

# 11. Multi-Layer Perceptron Methods ####

# 11.1. Artificial Neural Network ANN Regression 

# 11.1.1. ANN Regression training
annsa <- Sys.time()
annta <- train(rspy~rspy1+rspy2+rspy5,data=rspyt,method="nnet",trControl=tsctrlt)
annea <- Sys.time()
annea-annsa
annsb <- Sys.time()
anntb <- train(rspy~rspy1+rspy2+rspy3+rspy4+rspy5+rspy6+rspy7+rspy8+rspy9,data=rspyt,method="nnet",preProcess="pca",trControl=tsctrlt)
anneb <- Sys.time()
anneb-annsb

# ANN Regression Optimal Training Parameters
annta$bestTune
plot(annta)
anntb$bestTune
plot(anntb)

# ANN Regression Training Results
annta$results
anntb$results

# 11.1.2. ANN Regression Testing
annfa <- predict.train(annta,newdata=rspyf)
annfa <- cbind(index(rspyf),as.data.frame(annfa))
annfa <- xts(annfa[,2],order.by=as.Date(annfa[,1]))
annfb <- predict.train(anntb,newdata=rspyf)
annfb <- cbind(index(rspyf),as.data.frame(annfb))
annfb <- xts(annfb[,2],order.by=as.Date(annfb[,1]))

# 11.1.3. ANN Regression Testing Charts
plot(rspyf[,1],type="l",main="ANN Regression A Testing Chart")
lines(annfa,col="blue")
plot(rspyf[,1],type="l",main="ANN Regression B Testing Chart")
lines(annfb,col="green")

# 11.1.4. ANN Regression Forecasting Accuracy
# Convert xts to ts for accuracy function
annftsa <- ts(coredata(annfa),frequency=252,start=c(2014,1))
annftsb <- ts(coredata(annfb),frequency=252,start=c(2014,1))
accuracy(annftsa,rspyfts)
accuracy(annftsb,rspyfts)

# 12. Regression Machine Learning Forecasting Accuracy Comparison ####
accuracy(lmftsb,rspyfts)
accuracy(penlmftsa,rspyfts)
accuracy(knnftsb,rspyfts)
accuracy(dtftsb,rspyfts)
accuracy(rfftsb,rspyfts)
accuracy(xgbmftsb,rspyfts)
accuracy(lsvmftsb,rspyfts)
accuracy(rsvmftsb,rspyfts)
accuracy(annftsa,rspyfts)
