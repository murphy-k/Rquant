library(Quandl)

rm(list = ls())
dev.off(dev.list()["RStudioGD"])

# Present Value ####
pv <- 100 # create 'pv' (Present Value)
r <- 0.10 # create 'r' (interest rate)
fv1 <- pv * (1 + r) # Future value calculation
fv2 <- pv * (1 + r) ^ 2 # another future value calculation

# Bond Price Calculation given cash flows
cf <- c(5, 5, 5, 5, 105) # Create vector of cash flows
cf <- data.frame(cf) # Convert to data frame
cf$t <- as.numeric(rownames(cf)) # Add column t
cf$pv_factor <- 1 / (1 + 0.06)^cf$t # Calculate pv_factor
cf$pv <- cf$cf * cf$pv_factor # Calculate pv
sum(cf$pv) # Calculate the bond price

# Bond Price Function ####
bondprc <- function(p, r, ttm, y) {
  cf <- c(rep(p * r, ttm - 1), p * (1 + r))
  cf <- data.frame(cf)
  cf$t <- as.numeric(rownames(cf))
  cf$pv_factor <- 1 / (1 + y)^cf$t
  cf$pv <- cf$cf * cf$pv_factor
  sum(cf$pv)
}
# Obtain Moody's Baa index data
baa <- Quandl("FED/RIMLPBAAR_N_M")

# Identify 9/30/16 yield
baa_yield <- subset(baa, baa$Date == "2016-09-30")

# Convert yield to decimals and view
baa_yield <- baa_yield$Value / 100
baa_yield
# Valuing a bond with similar yield

bondprc(p = 100, r = 0.05, ttm = 5, y = baa_yield)
# Generate prc_yld
prc_yld <- seq(0.02, 0.40, 0.01)

# Convert prc_yld to data frame
prc_yld <- data.frame(prc_yld)

# Calculate bond price given different yields
for (i in 1:nrow(prc_yld)) {
  prc_yld$price[i] <- bondprc(100, 0.10, 20, prc_yld$prc_yld[i])  
}

# Plot P/YTM relationship
plot(prc_yld,
     type = "l",
     col = "blue",
     main = "Price/YTM Relationship")
