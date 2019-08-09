# Risk of Ruin = ((1 - (W - L)) / (1 + (W - L)))^U
#     Where:
#       W = the probability of a desirable outcome, or a win
#       L = the probability of an undesirable outcome, or a loss
#       U = the maximum number of risks that can be taken before the individual reaches their threshold for ruin

riskRuin <- function(probWin = 0.50, capital, tradeRisk) {
  U <- capital / tradeRisk
  probLoss <- 1 - probWin
  riskofRuin <-
    round(((1 - (probWin - probLoss)) / (1 + (probWin - probLoss))) ^ U, 4) * 100
  print(paste0("Your risk of ruin is: ", riskofRuin, "%"))
}

# Trader A is successful 52% of time. 48% of the time they lose. They start with $100,000 and each trade is $5,000 max loss.
# The trader's max loss is 70,000. This is the maximum number that a trader can lose before they "ruin" is 100,000/5,000 = 20.
# Their risk of ruin is calculated as
riskRuin(0.51, 1000, 10)
x <- c(1000, 900, 800, 700, 600, 500, 400, 300, 200, 100, 50, 10)
y <-  c(96.08,95.65,95.12,94.45,93.55,92.31,90.48,87.52,81.87,67.03,44.93,1.83)
plot(
  x,
  y,
  main = "$1,000 Account",
  xlab = "($) Risk/Trade",
  ylab = "(%) Risk of Ruin",
  type = "b"
)
