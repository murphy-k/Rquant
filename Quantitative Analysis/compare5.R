require(ggplot2)
require(reshape2)

tickers <- c("COTY", "APC", "XRX", "CMG", "CDNS")
getSymbols(tickers)

top5 <- cbind(COTY[, 4], APC[, 4], XRX[, 4], CMG[, 4], CDNS[, 4])
top5 <- tk_tbl(top5)
top5 <- top5 %>% filter(index > "2019-01-01")


df <- melt(top5 ,  id.vars = 'index', variable.name = 'series')
# plot on same grid, each series colored differently --
# good if the series have same scale
ggplot(df, aes(index, value)) + geom_line(aes(colour = series))

# or plot on different plots
ggplot(df, aes(index, value)) + geom_line() + facet_wrap(series ~ ., scales = "free")
