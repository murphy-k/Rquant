library(Quandl)
Quandl.api_key('shVi-_QjPbmUAfvVBMzw')
EURUSD <- FXCM_H1[which(FXCM_H1$symbol == 'EUR/USD'), ]
EURUSD <- EURUSD[,-1]
EURUSD <- as.xts(EURUSD)
