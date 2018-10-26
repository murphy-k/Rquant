rm(list = ls())
dev.off(dev.list()["RStudioGD"])

sample_space <- c(1:80)
set.seed(1)

Draw_1 <- sample(sample_space, size = 5, replace = FALSE)
Draw_2 <- sample(sample_space, size = 5, replace = FALSE)

hist(rnorm(10000,1,0.05),breaks = 50)

