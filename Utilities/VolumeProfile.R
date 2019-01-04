library(quantmod)

getSymbols("GE", from = "2018-01-01", to = "2019-01-01")
# 1. Volume Profile 1 ####
# Original Function
vp <- function(symbol) {
  df = data.frame(
    Date = index(symbol),
    Adjusted = symbol$Adjusted,
    Volume = symbol$Volume
  )
  dt <- as.data.table(unique(df, by = Date))
  vol <- aggregate(cbind(Volume) ~ Adjusted, sum, data = dt)
  vol <- data.frame(vol$Volume, vol$Adjusted)
  hist(vol)
  return(vol)
}
# Function Decompositon
df <-
  data.frame(
    Date = index(GE),
    Adjusted = GE$GE.Adjusted,
    Volume = GE$GE.Volume
  )
dt <- as.data.table(unique(df, by = Date))
vol <- aggregate(cbind(GE.Volume) ~ GE.Adjusted, sum, data = dt)
vol <- data.frame(vol$GE.Volume, vol$GE.Adjusted)
plot(vol, type = "line")

# 2. VP 2 ####
# Original Function
vp2 <- function(symbol) {
  df = data.frame(
    Date = index(symbol),
    Adjusted = symbol$Adjusted,
    Volume = symbol$Volume
  )
  dt <- as.data.table(unique(df, by = Date))
  vol <- aggregate(cbind(Volume) ~ Adjusted, sum, data = dt)
  hmatrix <- as.matrix(vol$Volume)
  par(bg = NA)
  colnamesbarplot <- c("Adjusted", "Volume")
  options(scipen = 50, digits = 10)
  barplot(
    hmatrix,
    beside = TRUE,
    horiz = TRUE,
    axes = TRUE,
    legend.text = TRUE,
    xlab = "Volume",
    ylab = "Price"
  )
  return(vol)
}
# Function Decomposition
df <-
  data.frame(
    Date = index(GE),
    Adjusted = GE$GE.Adjusted,
    Volume = GE$GE.Volume
  )
dt <- as.data.table(unique(df, by = Date))
vol <- aggregate(cbind(GE.Volume) ~ GE.Adjusted, sum, data = dt)
hmatrix <- as.matrix(vol$GE.Volume)
par(bg = NA)
colnamesbarplot <- c("Adjusted", "Volume")
options(scipen = 50, digits = 10)
barplot(
  hmatrix,
  beside = TRUE,
  horiz = TRUE,
  axes = TRUE,
  legend.text = TRUE,
  xlab = "Volume",
  ylab = "Price"
)
