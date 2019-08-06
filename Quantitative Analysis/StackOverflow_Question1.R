library(quantmod)
getSymbols("QQQ")

QQQ <- QQQ$QQQ.Close
QQQ <- `colnames<-`(QQQ,"close")


n = 100

QQQ <- (cbind(QQQ, setNames(do.call(cbind, lapply(1:n, function(i)
  ROC(x = QQQ[,1], n = i))),
  paste0("ROC", 1:n))))


View(QQQ)



