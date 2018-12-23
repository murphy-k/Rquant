library(gridExtra)

# Export as .png ####
png(
  "test.png",
  height = 3 * nrow(portfolio),
  width = 150 *ncol(portfolio)
)
grid.table(cor(as.data.frame(portfolio)))
dev.off()


# Export as .pdf ####

# pdf("test.pdf", height = 20, width = 30)
# grid.table(all.mean2.stats)
# dev.off()