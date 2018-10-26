library(ggplot2)
library(dplyr)
rm(list = ls())
dev.off(dev.list()["RStudioGD"])
options(scipen = 999)
mtcars <- mtcars

mtcars %>%
  ggplot(aes(x = sqrt(disp), y = sqrt(mpg))) +
  geom_point(colour = "red")

cor(sqrt(mtcars$disp), sqrt(mtcars$mpg))

mtcars %>%
  ggplot(aes(x = sqrt(disp), y = sqrt(mpg))) +
  geom_point(colour = "red") +
  geom_smooth(method = "lm", fill = NA)
OLS_model <- lm(sqrt(mpg) ~ sqrt(disp), data = mtcars)
summary(OLS_model)

