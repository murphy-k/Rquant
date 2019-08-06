library(tidyverse)
library(quantmod)
library(plotly)
library(timetk)
library(reshape2)

treasurys <- c("DGS1MO", "DGS3MO", "DGS6MO", "DGS1", "DGS2", "DGS3", "DGS5",
               "DGS7", "DGS10", "DGS20", "DGS30")

getSymbols(Symbols = treasurys, src = "FRED",auto.assign = TRUE)

treasury_df <- cbind(DGS1MO, DGS3MO, DGS6MO, DGS1, DGS2, DGS3, DGS5, DGS7,
                     DGS10, DGS20, DGS30) %>%
  tk_tbl() %>%
  `colnames<-`(c("date","1-month","3-month", "6-month","1-year","2-year",
                 "3-year","5-year","7-year","10-year","20-year","30-year"))

df <- melt(treasury_df ,  id.vars = 'date', variable.name = 'series')
ggplot(df, aes(date,value)) + geom_line(aes(colour = series))
plot_ly(df)

p <-
  plot_ly(
    treasury_df,
    x = ~ treasury_df$date,
    y = ~ treasury_df$`2-year`,
    z = ~ treasury_df$`10-year`
  ) %>%
  add_markers() %>%
  layout(scene = list(
    xaxis = list(title = 'Date'),
    yaxis = list(title = '2-Year Yield'),
    zaxis = list(title = '10-Year Yield')
  ))
p
