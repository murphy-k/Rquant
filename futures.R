library(Quandl)
Quandl.api_key("shVi-_QjPbmUAfvVBMzw")
FCOJ <- Quandl("CHRIS/ICE_OJ1", type = "xts")
FCOJ <- FCOJ$Settle

chartSeries(FCOJ)       
