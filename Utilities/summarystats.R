  library(quantmod)
  x <- getSymbols("FNGU", src="yahoo", auto.assign = FALSE)
  x_adj <- x$FNGU.Adjusted
  x_ROC <- na.trim(ROC(x_adj,n=1,type="continuous"))
  
  summary(x_ROC)
  quantile(na.trim(x_ROC))
  
  plot(x_ROC)
  hist(x_ROC,breaks = 100,probability = TRUE)
  
  x_mean <- mean(x_ROC)
  x_sd <- sd(x_ROC)
  z_score <- ((0.055) - x_mean) / x_sd
  z_score
pnorm(z_score, lower.tail = FALSE)
