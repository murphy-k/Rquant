#  ####
rm(list = ls())
dev.off(dev.list()["RStudioGD"])

# Present Value ####
pv <- 100 # create 'pv' (Present Value)
r <- 0.10 # create 'r' (interest rate)
fv1 <- pv * (1 + r) # Future value calculation
fv2 <- pv * (1 + r)^2 # another future value calculation 
