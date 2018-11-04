# Pairs Trading Analysis with R

# 1. Pairs Trading Analysis Data

# 1.1. Load R packages
library("tseries")
library("quantmod")
library("PerformanceAnalytics")
library("roll")
library("urca")

# 1.2. Data Reading
# data <- read.csv("Pairs-Trading-Analysis-Data.txt", header = T)
# data <- xts(data[, 2:7], order.by = as.Date(data[, 1]))
#####
# 2. Pairs Identification

# 2.1. Single Pair Identification

# 2.1.1. AUS-CAN Pair Identification

# getSymbols("EWA",src="yahoo",to="2017-01-01")
# getSymbols("EWC",src="yahoo",to="2017-01-01")
# aus <- EWA[,6]
# can <- EWC[,6]

getSymbols("EWA",src="google",to="2017-01-01")
getSymbols("EWC",src="google",to="2017-01-01")
aus <- EWA[,4]
can <- EWC[,4]

# aus <- data[, 1]
# can <- data[, 2]

# AUS-CAN Returns Correlation
ausret <- dailyReturn(aus)
canret <- dailyReturn(can)
cor(ausret, canret)

# AUS-CAN Chart
plot(as.zoo(aus),
     main = "AUS-CAN Pair",
     xlab = "Date",
     ylab = "")
par(new = T)
plot(as.zoo(can),
  col = "blue",
  xaxt = "n",
  yaxt = "n",
  xlab = "",
  ylab = ""
)
axis(side = 4,
     xlab = "CAN-Price")
legend(
  "topleft",
  legend = c("aus", "can"),
  col = c("black", "blue"),
  lty = 1,
  cex = 0.50
)
par(new = F)

# 2.1.2. CAN-RSA Pair Identification

# getSymbols("EZA",src="yahoo",to="2017-01-01")
# rsa <- EZA[,6]

getSymbols("EZA",src="google",to="2017-01-01")
rsa <- EZA[,4]

# rsa <- data[, 3]

# CAN-RSA Returns Correlation
rsaret <- dailyReturn(rsa)
cor(canret, rsaret)

# CAN-RSA Chart
plot(as.zoo(can),
     main = "CAN-RSA Pair",
     xlab = "Date",
     ylab = "")
par(new = T)
plot(
  as.zoo(rsa),
  col = "blue",
  xaxt = "n",
  yaxt = "n",
  xlab = "",
  ylab = ""
)
axis(side = 4)
legend(
  "topleft",
  legend = c("can", "rsa"),
  col = c("black", "blue"),
  lty = 1,
  cex = 0.5
)
par(new = F)

# 2.1.3. AUS-RSA Pair Identification

# AUS-RSA Returns Correlation
cor(ausret, rsaret)

# AUS-RSA Chart
plot(as.zoo(aus),
     main = "AUS-RSA Pair",
     xlab = "Date",
     ylab = "")
par(new = T)
plot(
  as.zoo(rsa),
  col = "blue",
  xaxt = "n",
  yaxt = "n",
  xlab = "",
  ylab = ""
)
axis(side = 4)
legend(
  "topleft",
  legend = c("aus", "rsa"),
  col = c("black", "blue"),
  lty = 1,
  cex = 0.5
)
par(new = F)

# 2.1.4. GER-FRA Pair Identification

# getSymbols("EWG",src="yahoo",to="2017-01-01")
# getSymbols("EWQ",src="yahoo",to="2017-01-01")
# ger <- EWG[,6]
# fra <- EWQ[,6]

getSymbols("EWG",src="google",to="2017-01-01")
getSymbols("EWQ",src="google",to="2017-01-01")
ger <- EWG[,4]
fra <- EWQ[,4]

# ger <- data[, 4]
# fra <- data[, 5]

# GER-FRA Returns Correlation
gerret <- dailyReturn(ger)
fraret <- dailyReturn(fra)
cor(gerret, fraret)

# GER-FRA Chart
plot(as.zoo(ger),
     main = "GER-FRA Pair",
     xlab = "",
     ylab = "")
par(new = T)
plot(
  as.zoo(fra),
  col = "green",
  xaxt = "n",
  yaxt = "n",
  xlab = "",
  ylab = ""
)
axis(side = 4)
legend(
  "topleft",
  legend = c("ger", "fra"),
  col = c("black", "green"),
  lty = 1,
  cex = 0.75
)
par(new = F)

# 2.1.5. NED-FRA Pair Identification

# getSymbols("EWN",src="yahoo",to="2017-01-01")
# ned <- EWN[,6]

getSymbols("EWN",src="google",to="2017-01-01")
ned <- EWA[,4]

# ned <- data[, 6]

# NED-FRA Returns Correlation
nedret <- dailyReturn(ned)
cor(nedret, fraret)

# NED-FRA Chart
plot(as.zoo(ned),
     main = "NED-FRA Pair",
     xlab = "",
     ylab = "")
par(new = T)
plot(
  as.zoo(fra),
  col = "green",
  xaxt = "n",
  yaxt = "n",
  xlab = "",
  ylab = ""
)
axis(side = 4)
legend(
  "topleft",
  legend = c("ned", "fra"),
  col = c("black", "green"),
  lty = 1,
  cex = 0.75
)
par(new = F)

# 2.1.6. GER-NED Pair Identification

# GER-NED Returns Correlation
cor(gerret, nedret)

# GER-NED Chart
plot(as.zoo(ger),
     main = "GER-NED Pair",
     xlab = "",
     ylab = "")
par(new = T)
plot(
  as.zoo(ned),
  col = "green",
  xaxt = "n",
  yaxt = "n",
  xlab = "",
  ylab = ""
)
axis(side = 4)
legend(
  "topleft",
  legend = c("ger", "ned"),
  col = c("black", "green"),
  lty = 1,
  cex = 0.75
)
par(new = F)

# 2.2. Multiple Pairs Identification

# 2.2.1. AUS-CAN-RSA Pairs Identification
int1 <- cbind(aus, can, rsa)
intret1 <- cbind(ausret, canret, rsaret)
colnames(intret1) <- c("ausret", "canret", "rsaret")

# AUS-CAN-RSA Returns Correlation
cor(intret1)

# 2.2.2. GER-NED-FRA Pairs Identification
int2 <- cbind(ger, ned, fra)
intret2 <- cbind(gerret, nedret, fraret)
colnames(intret2) <- c("gerret", "nedret", "fraret")

# GER-NED-FRA Returns Correlation
cor(intret2)
#####
# 3. Pairs Spread Co-Integration

# 3.1. Single Pair Spread Co-Integration

# 3.1.1. AUS-CAN Pair Spread Co-Integration

# AUS-CAN Spread Calculation
lmint1 <- lm(aus ~ 0 + can)
intb1 <- lmint1$coefficients[1]
intsp1 <- aus - intb1 * can
plot(intsp1, main = "AUS-CAN Spread")
abline(h = mean(intsp1), col = "blue")

# AUS-CAN Non-Stationary Prices
adf.test(aus)
adf.test(can)
ausdiff <- diff(aus)
candiff <- diff(can)
adf.test(ausdiff[complete.cases(ausdiff), ])
adf.test(candiff[complete.cases(candiff), ])

# AUS-CAN Stationary Spread
adf.test(intsp1)
pp.test(intsp1)
HurstIndex(intsp1)

# 3.1.2. CAN-RSA Pair Spread Co-Integration

# CAN-RSA Spread Calculation
lmint2 <- lm(can ~ 0 + rsa)
intb2 <- lmint2$coefficients[1]
intsp2 <- can - intb2 * rsa
plot(intsp2, main = "CAN-RSA Spread")
abline(h = mean(intsp2), col = "blue")

# CAN-RSA Non-Stationary Prices
adf.test(can)
adf.test(rsa)
rsadiff <- diff(rsa)
adf.test(candiff[complete.cases(candiff), ])
adf.test(rsadiff[complete.cases(rsadiff), ])

# CAN-RSA Stationary Spread
adf.test(intsp2)
pp.test(intsp2)
HurstIndex(intsp2)

# 3.1.3. AUS-RSA Pair Spread Co-Integration

# AUS-RSA Spread Calculation
lmint3 <- lm(aus ~ 0 + rsa)
intb3 <- lmint3$coefficients[1]
intsp3 <- aus - intb3 * rsa
plot(intsp3, main = "AUS-RSA Spread")
abline(h = mean(intsp3), col = "blue")

# AUS-RSA Non-Stationary Prices
adf.test(aus)
adf.test(rsa)
adf.test(ausdiff[complete.cases(ausdiff), ])
adf.test(rsadiff[complete.cases(rsadiff), ])

# AUS-RSA Stationary Spread
adf.test(intsp3)
pp.test(intsp3)
HurstIndex(intsp3)

# 3.1.4. GER-FRA Pair Spread Co-Integration

# GER-FRA Spread Calculation
lmint4 <- lm(ger ~ 0 + fra)
intb4 <- lmint4$coefficients[1]
intsp4 <- ger - intb4 * fra
plot(intsp4, main = "GER-FRA Spread")
abline(h = mean(intsp4), col = "green")

# GER-FRA Non-Stationary Prices
adf.test(ger)
adf.test(fra)
gerdiff <- diff(ger)
fradiff <- diff(fra)
adf.test(gerdiff[complete.cases(gerdiff), ])
adf.test(fradiff[complete.cases(fradiff), ])

# GER-FRA Stationary Spread
adf.test(intsp4)
pp.test(intsp4)
HurstIndex(intsp4)

# 3.1.5. NED-FRA Pair Co-Integration

# NED-FRA Spread Calculation
lmint5 <- lm(ned ~ 0 + fra)
intb5 <- lmint5$coefficients[1]
intsp5 <- ned - intb5 * fra
plot(intsp5, main = "NED-FRA Spread")
abline(h = mean(intsp5), col = "green")

# NED-FRA Non-Stationary Prices
adf.test(ned)
adf.test(fra)
neddiff <- diff(ned)
adf.test(fradiff[complete.cases(fradiff), ])
adf.test(neddiff[complete.cases(neddiff), ])

# NED-FRA Stationary Spread
adf.test(intsp5)
pp.test(intsp5)
HurstIndex(intsp5)

# 3.1.6. GER-NED Pair Co-Integration

# GER-NED Spread Calculation
lmint6 <- lm(ger ~ 0 + ned)
intb6 <- lmint6$coefficients[1]
intsp6 <- ger - intb6 * ned
plot(intsp6, main = "GER-NED Spread")
abline(h = mean(intsp6), col = "green")

# GER-NED Non-Stationary Prices
adf.test(ger)
adf.test(ned)
adf.test(gerdiff[complete.cases(gerdiff), ])
adf.test(neddiff[complete.cases(neddiff), ])

# GER-NED Stationary Spread
adf.test(intsp6)
pp.test(intsp6)
HurstIndex(intsp6)

# 3.2. Multiple Pairs Spreads Co-Integration

# 3.2.1. AUS-CAN-RSA Pairs Spreads Co-Integration

# AUS-CAN-RSA Stationary Spreads
intjt1 <- ca.jo(
  int1,
  type = "trace",
  ecdet = "none",
  K = 2,
  spec = "longrun"
)
summary(intjt1)

# AUS-CAN-RSA Spread Calculation
ausw1 <- intjt1@V[1]
canw1 <- intjt1@V[2]
rsaw1 <- intjt1@V[3]
intsp7 <- ausw1 * aus + canw1 * can + rsaw1 * rsa
plot(intsp7, main = "AUS-CAN-RSA Spread")
abline(h = mean(intsp7), col = "red")
adf.test(intsp7)
pp.test(intsp7)
HurstIndex(intsp7)

# 3.2.2. GER-NED-FRA Pairs Cointegration

# GER-NED-FRA Stationary Spreads
intjt2 <- ca.jo(
  int2,
  type = "trace",
  ecdet = "none",
  K = 2,
  spec = "longrun"
)
summary(intjt2)

# GER-NED-FRA Spread Calculation
gerw2 <- intjt2@V[1]
nedw2 <- intjt2@V[2]
fraw2 <- intjt2@V[3]
intsp8 <- gerw2 * ger + nedw2 * ned + fraw2 * fra
plot(intsp8, main = "GER-NED-FRA Spread")
abline(h = mean(intsp8), col = "red")
adf.test(intsp8)
pp.test(intsp8)
HurstIndex(intsp8)

# 4. Pairs Trading Startegies

# 4.1. Single Pair Trading Strategies

# 4.1.1. AUS-CAN Pair Trading Strategy

# AUS-CAN Rolling Spread Calculation
rlmint1 <- roll_lm(
  y = aus,
  x = can,
  intercept = F,
  width = 63
)
rintb1 <- rlmint1$coefficients[, 2]
plot(rintb1, main = "AUS-CAN Rolling Beta")
abline(h = intb1, col = "blue")
rintsp1 <- aus - rintb1 * can

# AUS-CAN Rolling Spread Z-Score
rintz1 <- roll_scale(rintsp1, width = 63)
plot(rintz1, main = "AUS-CAN Rolling Spread Z-Score")
abline(h = -2, col = "green")
abline(h = -1, col = "green", lty = 2)
abline(h = 2, col = "red")
abline(h = 1, col = "red", lty = 2)

# AUS-CAN Trading Strategy Signals
intsig1 <- Lag(ifelse(
  Lag(rintz1) > (-2) & rintz1 < (-2),
  -2,
  ifelse(
    Lag(rintz1) < (-1) & rintz1 > (-1),
    -1,
    ifelse(Lag(rintz1) < 2 & rintz1 > 2, 2,
           ifelse(Lag(rintz1) > 1 &
                    rintz1 < 1, 1, 0))
  )
))
intsig1[is.na(intsig1)] <- 0

# AUS-CAN Trading Strategy Positions
intpos1 <- ifelse(intsig1 > 2, 1, 0)
for (i in 1:length(rintz1)) {
  intpos1[i] <- ifelse(intsig1[i] == -2, 1,
                       ifelse(intsig1[i] == -1, 0,
                              ifelse(
                                intsig1[i] == 2, -1,
                                ifelse(intsig1[i] ==
                                         1, 0, intpos1[i - 1])
                              )))
}
intpos1[is.na(intpos1)] <- 0
inttr1 <- cbind(rintz1, intsig1, intpos1)
colnames(inttr1) <- c("rintz1", "intsig1", "intpos1")
View(inttr1)

# 4.1.2. CAN-RSA Pair Trading Strategy

# CAN-RSA Rolling Spread Calculation
rlmint2 <- roll_lm(
  y = can,
  x = rsa,
  intercept = F,
  width = 63
)
rintb2 <- rlmint2$coefficients[, 2]
plot(rintb2, main = "CAN-RSA Rolling Beta")
abline(h = intb2, col = "blue")
rintsp2 <- can - rintb2 * rsa

# CAN-RSA Rolling Spread Z-Score
rintz2 <- roll_scale(rintsp2, width = 63)
plot(rintz2, main = "CAN-RSA Rolling Spread Z-Score")
abline(h = -2, col = "green")
abline(h = -1, col = "green", lty = 2)
abline(h = 2, col = "red")
abline(h = 1, col = "red", lty = 2)

# CAN-RSA Trading Strategy Signals
intsig2 <- Lag(ifelse(
  Lag(rintz2) > (-2) & rintz2 < (-2),
  -2,
  ifelse(
    Lag(rintz2) < (-1) & rintz2 > (-1),
    -1,
    ifelse(Lag(rintz2) < 2 & rintz2 > 2, 2,
           ifelse(Lag(rintz2) > 1 &
                    rintz2 < 1, 1, 0))
  )
))
intsig2[is.na(intsig2)] <- 0

# CAN-RSA Trading Strategy Positions
intpos2 <- ifelse(intsig2 > 2, 1, 0)
for (i in 1:length(rintz2)) {
  intpos2[i] <- ifelse(intsig2[i] == -2, 1,
                       ifelse(intsig2[i] == -1, 0,
                              ifelse(
                                intsig2[i] == 2, -1,
                                ifelse(intsig2[i] ==
                                         1, 0, intpos2[i - 1])
                              )))
}
intpos2[is.na(intpos2)] <- 0
inttr2 <- cbind(rintz2, intsig2, intpos2)
colnames(inttr2) <- c("rintz2", "intsig2", "intpos2")
View(inttr2)

# 4.1.3. GER-FRA Pair Trading Strategy

# GER-FRA Rolling Spread Calculation
rlmint4 <- roll_lm(
  y = ger,
  x = fra,
  intercept = F,
  width = 63
)
rintb4 <- rlmint4$coefficients[, 2]
plot(rintb4, main = "GER-FRA Rolling Beta")
abline(h = intb4, col = "green")
rintsp4 <- ger - rintb4 * fra

# GER-FRA Rolling Spread Z-Score
rintz4 <- roll_scale(rintsp4, width = 63)
plot(rintz4, main = "GER-FRA Rolling Spread Z-Score")
abline(h = -2, col = "green")
abline(h = -1, col = "green", lty = 2)
abline(h = 2, col = "red")
abline(h = 1, col = "red", lty = 2)

# GER-FRA Trading Strategy Signals
intsig4 <- Lag(ifelse(
  Lag(rintz4) > (-2) & rintz4 < (-2),
  -2,
  ifelse(
    Lag(rintz4) < (-1) & rintz4 > (-1),
    -1,
    ifelse(Lag(rintz4) < 2 & rintz4 > 2, 2,
           ifelse(Lag(rintz4) > 1 &
                    rintz4 < 1, 1, 0))
  )
))
intsig4[is.na(intsig4)] <- 0

# GER-FRA Trading Strategy Positions
intpos4 <- ifelse(intsig4 > 2, 1, 0)
for (i in 1:length(rintz4)) {
  intpos4[i] <- ifelse(intsig4[i] == -2, 1,
                       ifelse(intsig4[i] == -1, 0,
                              ifelse(
                                intsig4[i] == 2, -1,
                                ifelse(intsig4[i] ==
                                         1, 0, intpos4[i - 1])
                              )))
}
intpos4[is.na(intpos4)] <- 0
inttr4 <- cbind(rintz4, intsig4, intpos4)
colnames(inttr4) <- c("rintz4", "intsig4", "intpos4")
View(inttr4)

# 4.1.5. NED-FRA Pair Trading Strategy

# NED-FRA Rolling Spread Calculation
rlmint5 <- roll_lm(
  y = ned,
  x = fra,
  intercept = F,
  width = 63
)
rintb5 <- rlmint5$coefficients[, 2]
plot(rintb5, main = "NED-FRA Rolling Beta")
abline(h = intb5, col = "green")
rintsp5 <- ned - rintb5 * fra

# NED-FRA Rolling Spread Z-Score
rintz5 <- roll_scale(rintsp5, width = 63)
plot(rintz5, main = "NED-FRA Rolling Spread Z-Score")
abline(h = -2, col = "green")
abline(h = -1, col = "green", lty = 2)
abline(h = 2, col = "red")
abline(h = 1, col = "red", lty = 2)

# NED-FRA Trading Strategy Signals
intsig5 <- Lag(ifelse(
  Lag(rintz5) > (-2) & rintz5 < (-2),
  -2,
  ifelse(
    Lag(rintz5) < (-1) & rintz5 > (-1),
    -1,
    ifelse(Lag(rintz5) < 2 & rintz5 > 2, 2,
           ifelse(Lag(rintz5) > 1 &
                    rintz5 < 1, 1, 0))
  )
))
intsig5[is.na(intsig5)] <- 0

# NED-FRA Trading Strategy Positions
intpos5 <- ifelse(intsig5 > 2, 1, 0)
for (i in 1:length(rintz5)) {
  intpos5[i] <- ifelse(intsig5[i] == -2, 1,
                       ifelse(intsig5[i] == -1, 0,
                              ifelse(
                                intsig5[i] == 2, -1,
                                ifelse(intsig5[i] ==
                                         1, 0, intpos5[i - 1])
                              )))
}
intpos5[is.na(intpos5)] <- 0
inttr5 <- cbind(rintz5, intsig5, intpos5)
colnames(inttr5) <- c("rintz5", "intsig5", "intpos5")
View(inttr5)

# 5. Pairs Startegies Performance Comparison

# 5.1. Single Pair Strategies Performance Comparison

# 5.1.1. AUS-CAN Pair Strategy Performance Comparison
intspret1 <- ausret - rintb1 * canret
intret1 <- intpos1 * intspret1
intret1c <-
  ifelse((intsig1 == -2 |
            intsig1 == -1 |
            intsig1 == 2 |
            intsig1 == 1) &
           intpos1 != Lag(intpos1),
         (intspret1 * intpos1) - 0.001,
         intspret1 * intpos1
  )
intcomp1 <- cbind(intret1, intret1c, ausret, canret)
colnames(intcomp1) <- c("intret1", "intret1c", "ausret", "canret")
table.AnnualizedReturns(intcomp1)
charts.PerformanceSummary(intcomp1)

# 5.1.2. CAN-RSA Pair Strategy Performance Comparison
intspret2 <- canret - rintb2 * rsaret
intret2 <- intpos2 * intspret2
intret2c <-
  ifelse((intsig2 == -2 |
            intsig2 == -1 |
            intsig2 == 2 |
            intsig2 == 1) &
           intpos2 != Lag(intpos2),
         (intspret2 * intpos2) - 0.001,
         intspret2 * intpos2
  )
intcomp2 <- cbind(intret2, intret2c, canret, rsaret)
colnames(intcomp2) <- c("intret2", "intret2c", "canret", "rsaret")
table.AnnualizedReturns(intcomp2)
charts.PerformanceSummary(intcomp2)

# 5.1.3. GER-FRA Pair Strategy Performance Comparison
intspret4 <- gerret - rintb4 * fraret
intret4 <- intpos4 * intspret4
intret4c <-
  ifelse((intsig4 == -2 |
            intsig4 == -1 |
            intsig4 == 2 |
            intsig4 == 1) &
           intpos4 != Lag(intpos4),
         (intspret4 * intpos4) - 0.001,
         intspret4 * intpos4
  )
intcomp4 <- cbind(intret4, intret4c, gerret, fraret)
colnames(intcomp4) <- c("intret4", "intret4c", "gerret", "fraret")
table.AnnualizedReturns(intcomp4)
charts.PerformanceSummary(intcomp4)

# 5.1.4. NED-FRA Pair Strategy Performance Comparison
intspret5 <- nedret - rintb5 * fraret
intret5 <- intpos5 * intspret5
intret5c <-
  ifelse((intsig5 == -2 |
            intsig5 == -1 |
            intsig5 == 2 |
            intsig5 == 1) &
           intpos5 != Lag(intpos5),
         (intspret5 * intpos5) - 0.001,
         intspret5 * intpos5
  )
intcomp5 <- cbind(intret5, intret5c, nedret, fraret)
colnames(intcomp5) <- c("intret5", "intret5c", "nedret", "fraret")
table.AnnualizedReturns(intcomp5)
charts.PerformanceSummary(intcomp5)