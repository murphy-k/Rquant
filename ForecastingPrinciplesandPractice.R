# 5.10 Exercises ####
rm(list = ls())
dev.off(dev.list()["RStudioGD"])

daily20 <- head(elecdaily, 20)

autoplot(daily20, facets = TRUE)
daily20 %>%
  as.data.frame() %>%
  ggplot(aes(x=Temperature, y=Demand)) +
  geom_point() + 
  geom_smooth(method="lm", se=FALSE)
fit <- tslm(Demand ~ Temperature, data=daily20)
checkresiduals(fit)
forecast(fit, newdata = data.frame(Temperature=c(15,35)))

autoplot(elecdaily, facets = TRUE)
elecdaily %>%
  as.data.frame() %>%
  ggplot(aes(x=Temperature, y=Demand)) +
  geom_point() + 
  geom_smooth(method="lm", se=FALSE)
fit_all <- tslm(Demand ~ Temperature, data=elecdaily)
checkresiduals(fit)
forecast(fit, newdata = data.frame(Temperature=c(15,35)))


# Function to return ETS forecasts ####
fc_ets <- function(y, h) {
  forecast(ets(y), h = h)
}

