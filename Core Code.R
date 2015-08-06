setwd("G:/Dropbox/Quantathon")
library(tseries)
library(itsmr)
source("Simulation Class.R")
source("Strategy Class.R")
oil <- read.csv("in_sample_data.csv", header=T, stringsAsFactors=F, strip.white=T)
oil$date <- as.Date(as.character(oil$date), format="%Y%m%d")
plot(oil[,2]/oil[1,2], type="l")
hcol <- rainbow(9)
for(i in 1:10){
  lines(oil[,2*i+1]/oil[1,2*i+1], col=hcol[i])
}


#Simulation
simdates <- oil$date
simlength <- length(simdates)
spotprices <- oil$oil_spot
futureprices <- oil[,c(3,5,7,9,11,13,15,17,19,21,23)]
futurers <- oil[,c(4,6,8,10,12,14,16,18,20,22,24)]
stgy1 <- strategy()
stgy1@strategy <- regimeSwitching



sim1 <- simulation()
sim1 <- startNewSimulation(sim1, simlength, simdates, spotprices, futureprices, futurers, stgy1)
sim1
sim1 <- oneDayForward(sim1)
sim1 <- runAll(sim1)
sim1
plot(sim1@pnl, type="l")
plot(sim1@cashaccount/sim1@cashaccount[1], type="l")
