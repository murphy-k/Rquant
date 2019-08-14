# Kelly Formula

# The Kelly Betting Criterion details the optimal amount of ones bankroll one
# one should bet when the winning percentage.

# Kelly % = W - [(1 - W / R)]
#   where: W = the win probability.
#          R = ratio between profit and loss.
# Example: Win = 60% likely with a 20% gain, loss = 40% likely with a 20% loss.

kelly <- function(probWin, winAmount, lossAmount) {
  kelly <- probWin - ((1 - probWin / (winAmount / lossAmount)))
  return(kelly) 
}
kelly(probWin = 0.6,
      winAmount = 0.2,
      lossAmount = 0.2)

# This works for only binary bets where the downside is a total loss of capital.
# Applicable to blackjack, or horse-racing but not for capital markets.

# A modified Kelly betting formula is expressed as follows.

# Kelly % = W/A - (1 - W) / B
#   where: W = the win probability.
#          B = profit in the event of a win.
#          A = potential loss.

mod_kelly <- function(probWin, winAmount, lossAmount) {
  modKelly <- (probWin / lossAmount) - (1-probWin) / winAmount
  return(modKelly)
}
mod_kelly(probWin = 0.6, winAmount = 0.2, lossAmount = 0.2)
