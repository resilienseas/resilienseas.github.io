#temporal decorellation and covariance

#install packages
library(lubridate)
library(dplyr)
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)

#import data and organize/clean
ale <- read_csv(here("data/ALE_arag.csv"))
arq <- read_csv(here("data/ARQ_arag.csv"))
mko <- read_csv(here("data/MKO_arag.csv"))
sbh <- read_csv(here("data/SBH_arag.csv"))

ale$Site <- NULL
ale$Lat <- NULL
ale$Long <- NULL

arq$Site <- NULL
arq$Lat <- NULL
arq$Long <- NULL

sbh$Site <- NULL
sbh$Lat <- NULL
sbh$Long <- NULL

mko$Site <- NULL
mko$Lat <- NULL
mko$Long <- NULL

colnames(ale) <-c("Time_stamp_UTC", "Ale_Omega_Ar")
colnames(arq) <-c("Time_stamp_UTC", "Arq_Omega_Ar")
colnames(mko) <-c("Time_stamp_UTC", "Mko_Omega_Ar")
colnames(sbh) <-c("Time_stamp_UTC", "Sbh_Omega_Ar")

ale <- ale[!(ale$Ale_Omega_Ar == -9999),]
arq <- arq[!(arq$Arq_Omega_Ar == -9999),]
mko <- mko[!(mko$Mko_Omega_Ar == -9999),]
sbh <- sbh[!(sbh$Sbh_Omega_Ar == -9999),]

lterarag <- merge.data.frame(ale, arq, by = 'Time_stamp_UTC', all.x = T)
lterarag <- merge.data.frame(lterarag, mko, by = 'Time_stamp_UTC', all.x = T)
lterarag <- merge.data.frame(lterarag, sbh, by = 'Time_stamp_UTC', all.x = T)

ltercomplete <- na.omit(lterarag)

#before removing seasonal trends

timeseries <- par(mfrow=c(2,2))
plot(ale$Time_stamp_UTC,ale$Ale_Omega_Ar,type = "l")
plot(mko$Time_stamp_UTC,mko$Mko_Omega_Ar,type = "l")
plot(arq$Time_stamp_UTC,arq$Arq_Omega_Ar,type = "l")
plot(sbh$Time_stamp_UTC,sbh$Sbh_Omega_Ar,type = "l")

#split mko into two chunks

mko1 <- mko[c(1:18796),]
mko2 <- mko[c(18797: 44483),]

t.test(mko1$Mko_Omega_Ar, mko2$Mko_Omega_Ar)

timeseries <- par(mfrow=c(1,2))
plot(mko1$Time_stamp_UTC,mko1$Mko_Omega_Ar,type = "l")
points(mko1$Time_stamp_UTC, mko1$res_arag, col = "red")
plot(mko2$Time_stamp_UTC,mko2$Mko_Omega_Ar,type = "l")
points(mko2$Time_stamp_UTC, mko2$res_arag, col = "red")

#remove seasonal trends

mko1$month <- month(mko1$Time_stamp_UTC)
mko2$month <- month(mko2$Time_stamp_UTC)

mko1monthly <- aggregate(Mko_Omega_Ar ~ month, mko1, mean)
mko2monthly <- aggregate(Mko_Omega_Ar ~ month, mko2, mean)

mko1merge <- merge.data.frame(mko1, mko1monthly, by = 'month', all.x = T)
mko2merge <- merge.data.frame(mko2, mko2monthly, by = 'month', all.x = T)

mko1seasonality <- lm(Mko_Omega_Ar ~ month, mko1monthly)
mko2seasonality <- lm(Mko_Omega_Ar ~ month, mko2monthly)

mko1$mko_res_arag <- mko1merge$Mko_Omega_Ar.x-mko1merge$Mko_Omega_Ar.y
mko2$mko_res_arag <- mko2merge$Mko_Omega_Ar.x-mko2merge$Mko_Omega_Ar.y

timeseries <- par(mfrow=c(1,2))
plot(mko1$Time_stamp_UTC,mko1$mko_res_arag,type = "l")
plot(mko2$Time_stamp_UTC,mko2$mko_res_arag,type = "l")

#time chunks
for ( i in mko2$Time_stamp_UTC){
  mko2$difftime[i+1] = difftime(mko2$Time_stamp_UTC[i+1], mko2$Time_stamp_UTC[i])
}

















#after removing seasonal trends

ale$month <- month(ale$Time_stamp_UTC)
arq$month <- month(arq$Time_stamp_UTC)
mko$month <- month(mko$Time_stamp_UTC)
sbh$month <- month(sbh$Time_stamp_UTC)

alemonthly <- aggregate(Ale_Omega_Ar ~ month, ale, mean)
arqmonthly <- aggregate(Arq_Omega_Ar ~ month, arq, mean)
mkomonthly <- aggregate(Mko_Omega_Ar ~ month, mko, mean)
sbhmonthly <- aggregate(Sbh_Omega_Ar ~ month, sbh, mean)

plot(alemonthly)
plot(arqmonthly)
plot(mkomonthly)
plot(sbhmonthly)

aleseasonality <- lm(Ale_Omega_Ar ~ month, alemonthly)
arqseasonality <- lm(Arq_Omega_Ar ~ month, arqmonthly)
mkoseasonality <- lm(Mko_Omega_Ar ~ month, mkomonthly)
sbhseasonality <- lm(Sbh_Omega_Ar ~ month, sbhmonthly)

#remove seasonal trend from  data
ale$ale_res_arag <- ale$Ale_Omega_Ar-(coef(aleseasonality)[1]+coef(aleseasonality)[2]*ale$month)
arq$arq_res_arag <- arq$Arq_Omega_Ar-(coef(arqseasonality)[1]+coef(arqseasonality)[2]*arq$month)
mko$mko_res_arag <- mko$Mko_Omega_Ar-(coef(mkoseasonality)[1]+coef(mkoseasonality)[2]*mko$month)
sbh$sbh_res_arag <- sbh$Sbh_Omega_Ar-(coef(sbhseasonality)[1]+coef(sbhseasonality)[2]*sbh$month)

#plot after removing seasonal trend
timeseries <- par(mfrow=c(2,2))
plot(ale$Time_stamp_UTC,ale$res_arag,type = "l")
plot(mko$Time_stamp_UTC,mko$res_arag,type = "l")
plot(arq$Time_stamp_UTC,arq$res_arag,type = "l")
plot(sbh$Time_stamp_UTC,sbh$res_arag,type = "l")

#
dev.off()

quartz()

autocorr <- par(mfrow=c(2,2))
acf_ale <- acf(ale$res_arag, lag.max = 10000, , type = "correlation", xaxt = "n", xlab='Lag (days)', main = 'Autocorrelation at Alegria Reef', ylab = '')
axis(1, at=720*(0:30), labels = 10*(0:30))
acf_arq <- acf(arq$res_arag, lag.max = 10000, type="correlation", xaxt = "n", xlab='Lag (days)', main = 'Autocorrelation at Arroyo Quemado Reef', ylab = '')
axis(1, at=720*(0:30), labels = 10*(0:30))
acf_mko <- acf(mko$res_arag, lag.max = 10000, type="correlation", xaxt = "n", xlab='Lag (days)', main = 'Autocorrelation at Mohawk Reef', ylab = '')
axis(1, at=720*(0:30), labels = 10*(0:30))
acf_sbh <- acf(sbh$res_arag, lag.max = 10000, type="correlation", xaxt = "n", xlab='Lag (days)', main = 'Autocorrelation at Santa Barbara Harbor', ylab = '')
axis(1, at=720*(0:30), labels = 10*(0:30))


autocorr <- par(mfrow=c(2,2))
acf_ale <- acf(ale$Ale_Omega_Ar, lag.max = 10000, , type = "correlation", xaxt = "n", xlab='Lag (days)', main = 'Autocorrelation at Alegria Reef', ylab = '')
axis(1, at=720*(0:30), labels = 10*(0:30))
acf_arq <- acf(arq$Arq_Omega_Ar, lag.max = 10000, type="correlation", xaxt = "n", xlab='Lag (days)', main = 'Autocorrelation at Arroyo Quemado Reef', ylab = '')
axis(1, at=720*(0:30), labels = 10*(0:30))
acf_mko <- acf(mko$Mko_Omega_Ar, lag.max = 10000, type="correlation", xaxt = "n", xlab='Lag (days)', main = 'Autocorrelation at Mohawk Reef', ylab = '')
axis(1, at=720*(0:30), labels = 10*(0:30))
acf_sbh <- acf(sbh$Sbh_Omega_Ar, lag.max = 10000, type="correlation", xaxt = "n", xlab='Lag (days)', main = 'Autocorrelation at Santa Barbara Harbor', ylab = '')
axis(1, at=720*(0:30), labels = 10*(0:30))

autocorr <- par(mfrow=c(2,2))
acf_ale <- acf(ltercomplete$Ale_Omega_Ar, lag.max = 100000, , type = "correlation", xaxt = "n", xlab='Lag (days)', main = 'Autocorrelation at Alegria Reef', ylab = '')
axis(1, at=720*(0:30), labels = 10*(0:30))
acf_arq <- acf(ltercomplete$Arq_Omega_Ar, lag.max = 100000, type="correlation", xaxt = "n", xlab='Lag (days)', main = 'Autocorrelation at Arroyo Quemado Reef', ylab = '')
axis(1, at=720*(0:30), labels = 10*(0:30))
acf_mko <- acf(ltercomplete$Mko_Omega_Ar, lag.max = 100000, type="correlation", xaxt = "n", xlab='Lag (days)', main = 'Autocorrelation at Mohawk Reef', ylab = '')
axis(1, at=720*(0:30), labels = 10*(0:30))
acf_sbh <- acf(ltercomplete$Sbh_Omega_Ar, lag.max = 100000, type="correlation", xaxt = "n", xlab='Lag (days)', main = 'Autocorrelation at Santa Barbara Harbor', ylab = '')
axis(1, at=720*(0:30), labels = 10*(0:30))

autocorr <- par(mfrow=c(2,2))
acf_ale <- acf(ltercomplete$res, lag.max = 100000, , type = "correlation", xaxt = "n", xlab='Lag (days)', main = 'Autocorrelation at Alegria Reef', ylab = '')
axis(1, at=720*(0:30), labels = 10*(0:30))
acf_arq <- acf(ltercomplete$Arq_Omega_Ar, lag.max = 100000, type="correlation", xaxt = "n", xlab='Lag (days)', main = 'Autocorrelation at Arroyo Quemado Reef', ylab = '')
axis(1, at=720*(0:30), labels = 10*(0:30))
acf_mko <- acf(ltercomplete$Mko_Omega_Ar, lag.max = 100000, type="correlation", xaxt = "n", xlab='Lag (days)', main = 'Autocorrelation at Mohawk Reef', ylab = '')
axis(1, at=720*(0:30), labels = 10*(0:30))
acf_sbh <- acf(ltercomplete$Sbh_Omega_Ar, lag.max = 100000, type="correlation", xaxt = "n", xlab='Lag (days)', main = 'Autocorrelation at Santa Barbara Harbor', ylab = '')
axis(1, at=720*(0:30), labels = 10*(0:30))

autocorr <- par(mfrow=c(2,2))
acf_ale <- acf(ltercomplete$ale_res_arag, lag.max = 100000, , type = "correlation", xaxt = "n", xlab='Lag (days)', main = 'Autocorrelation at Alegria Reef', ylab = '')
axis(1, at=720*(0:30), labels = 10*(0:30))
acf_arq <- acf(ltercomplete$arq_res_arag, lag.max = 100000, type="correlation", xaxt = "n", xlab='Lag (days)', main = 'Autocorrelation at Arroyo Quemado Reef', ylab = '')
axis(1, at=720*(0:30), labels = 10*(0:30))
acf_mko <- acf(ltercomplete$mko_res_arag, lag.max = 100000, type="correlation", xaxt = "n", xlab='Lag (days)', main = 'Autocorrelation at Mohawk Reef', ylab = '')
axis(1, at=720*(0:30), labels = 10*(0:30))
acf_sbh <- acf(ltercomplete$sbh_res_arag, lag.max = 100000, type="correlation", xaxt = "n", xlab='Lag (days)', main = 'Autocorrelation at Santa Barbara Harbor', ylab = '')
axis(1, at=720*(0:30), labels = 10*(0:30))

setwd("/Users/raetaylor-burns/downloads")
dev.copy(png, 'autocorr.png', width = 500, height = 200);
dev.off ();

find(acf_arq<0, 1)

timeseries <- par(mfrow=c(2,2))
plot(ale$Time_stamp_UTC,ale$Omega_Ar,type = "l")
plot(mko$Time_stamp_UTC,mko$Omega_Ar,type = "l")
plot(arq$Time_stamp_UTC,arq$Omega_Ar,type = "l")
plot(sbh$Time_stamp_UTC,sbh$Omega_Ar,type = "l")


#PCA
lterpca <- prcomp(ltercomplete[,c(5, 9, 13)])

ggbiplot(lterpca, ellipse = TRUE, size = 0.001)



