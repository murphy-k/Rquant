library(gridExtra)

# Export as .png ####
png(
  "test.png",
  height = 50 * nrow(all.mean2.stats),
  width = 100 * ncol(all.mean2.stats)
)
grid.table(all.mean2.stats)
dev.off()


# Export as .pdf ####

# pdf("test.pdf", height = 20, width = 30)
# grid.table(all.mean2.stats)
# dev.off()