library(fpp2)

calls <- calls

calls %>%
  mstl() %>%
  autoplot()

calls %>%
  stlf() %>%
  autoplot() +
  xlab("Week")

sunspotarea <- sunspotarea

fit <- nnetar(sunspotarea, lambda = 0)
autoplot(forecast(fit, h = 30))

sim <- ts(matrix(0, nrow=30L, ncol=100L),
          start=end(sunspotarea)[1L]+1L)
for(i in seq(100))
  sim[,i] <- simulate(fit, nsim=30L)
autoplot(sunspotarea) + autolayer(sim)
