# Packages ####
library(quantmod)
library(qrmdata)
library(qrmtools)

rm(list = ls())
dev.off(dev.list()["RStudioGD"])


# Data Series ####
getSymbols("^DJI",
           src = "yahoo",
           auto.assign = TRUE,
           from = "1900-01-01")

DJI_dailyret <- dailyReturn(DJI$DJI.Close)
DJI_weeklyret <- weeklyReturn(DJI$DJI.Close)
DJI_monthlyret <- monthlyReturn(DJI$DJI.Close)
