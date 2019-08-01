library(quantmod)
library(reshape2)
library(ggplot2)

rm(list=ls())

tickers <-
  c(
    "SPY",
    "AAPL",
    "USO",
    "TNX"
  )
getSymbols(tickers, src = "yahoo", from = (Sys.Date()-365*10))
SPY$Return <- dailyReturn(SPY$SPY.Close)
AAPL$Return <- dailyReturn(AAPL$AAPL.Close)
USO$Return <- dailyReturn(USO$USO.Close)
TNX$Return <- dailyReturn(TNX$TNX.Close)

portfolio <-
  cbind(SPY$SPY.Adjusted,
        AAPL$AAPL.Adjusted,
        USO$USO.Adjusted,
        TNX$TNX.Adjusted)
portfolio <- `names<-`(portfolio,tickers)
portfolio <- as.data.frame(portfolio)

ret_portfolio <-
  cbind(SPY$Return,
        AAPL$Return,
        USO$Return,
        TNX$Return)
ret_portfolio <- `names<-`(portfolio,tickers)
ret_portfolio <- as.data.frame(portfolio)

# View regular correlation matrix
cormat <- round(cor(ret_portfolio),2)
View(cormat)
melted_cormat <- melt(cormat)
head(melted_cormat)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()

# create a ggplot2 heatmap object
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
print(ggheatmap)

# Add Correlation coefficients
ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(1.6, 1.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

