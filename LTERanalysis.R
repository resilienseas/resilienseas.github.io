#temporal decorellation and covariance

##install packages	#install packages
library(lubridate)
library(dplyr)	
library(devtools)	
library(here)
library(rowr)

ale <- read_csv(here("data/ALE_arag.csv"))
arq <- read_csv(here("data/ARQ_arag.csv"))
mko <- read_csv(here("data/MKO_arag.csv"))
sbh <- read_csv(here("data/SBH_arag.csv"))

lter <- merge.data.frame(ale, arq, by = 'Time_stamp_UTC', all.x = T)	
lter <- merge.data.frame(lter, mko, by = 'Time_stamp_UTC', all.x = T)	
lter <- merge.data.frame(lter, sbh, by = 'Time_stamp_UTC', all.x = T)	

lter[lter == -9999] <- NA	

lter <- lter[c(1, 5, 9, 13, 17)]

names(lter) <- c("Time_Stamp_UTC", "Ale_Arag", "Arq_Arag", "Mko_Arag", "Sbh_Arag")

#remove seasonal trends

lter$month <- month(lter$Time_Stamp_UTC)	

ale_arag_monthly <- aggregate(Ale_Arag ~ month, lter, mean)	
arq_arag_monthly <- aggregate(Arq_Arag ~ month, lter, mean)	
mko_arag_monthly <- aggregate(Mko_Arag ~ month, lter, mean)	
sbh_arag_monthly <- aggregate(Sbh_Arag ~ month, lter, mean)	

ltermerge <- merge.data.frame(lter, ale_arag_monthly, by = 'month', all.x = T)	
ltermerge <- merge.data.frame(ltermerge, arq_arag_monthly, by = 'month', all.x = T)	
ltermerge <- merge.data.frame(ltermerge, mko_arag_monthly, by = 'month', all.x = T)	
ltermerge <- merge.data.frame(ltermerge, sbh_arag_monthly, by = 'month', all.x = T)	

ltermerge$ale_res_arag <- ltermerge$Ale_Arag.x-ltermerge$Ale_Arag.y	
ltermerge$arq_res_arag <- ltermerge$Arq_Arag.x-ltermerge$Arq_Arag.y	
ltermerge$mko_res_arag <- ltermerge$Mko_Arag.x-ltermerge$Mko_Arag.y	
ltermerge$sbh_res_arag <- ltermerge$Sbh_Arag.x-ltermerge$Sbh_Arag.y

ltermerge <- ltermerge[order(ltermerge$Time_Stamp_UTC),]	

ltermerge <- ltermerge[c(2, 11, 12, 13, 14)]

ltermerge$Time_Stamp_UTC <- as.POSIXct(ltermerge$Time_Stamp_UTC,format = "%Y-%m-%dT %H:%M:%SZ")	

###decorrelation time scale	

##separate time chunks based on missing data	

#ale	

alearag <- ltermerge[c(1, 2)]	

alearag <- na.omit(alearag)	

n = length(alearag$Time_stamp_UTC)	

alearag$difftime = 0	

for (i in 1:nrow(alearag)){	
  d <- difftime(alearag$Time_Stamp_UTC[i+1], alearag$Time_Stamp_UTC[i], units = "mins")	
  alearag$difftime[i] <- d	
}	

breaks <- which(alearag$difftime != 20)	

ale1 <- alearag[c(1:breaks[1]),]	
ale2 <- alearag[c((breaks[1]+1):breaks[2]),]	
ale3 <- alearag[c((breaks[2]+1):breaks[3]),]	
ale4 <- alearag[c((breaks[3]+1):breaks[4]),]	
ale5 <- alearag[c((breaks[4]+1):breaks[5]),]	
ale6 <- alearag[c((breaks[5]+1):breaks[6]),]	
ale7 <- alearag[c((breaks[6]+1):n),]	

#arq	

arqarag <- ltermerge[, c(1,3)]	

arqarag <- na.omit(arqarag)	

n = length(arqarag$Time_Stamp_UTC)	

arqarag$difftime = 0	

for (i in 1:nrow(arqarag)){	
  d <- difftime(arqarag$Time_Stamp_UTC[i+1], arqarag$Time_Stamp_UTC[i])	
  arqarag$difftime[i] <- d	
}	

breaks <- which(arqarag$difftime != 20)	

arq1 <- arqarag[c(1:breaks[1]),]	
arq2 <- arqarag[c((breaks[1]+1):breaks[2]),]	
arq3 <- arqarag[c((breaks[2]+1):n),]	

#mko	

mkoarag <- ltermerge[, c(1,4)]	

mkoarag <- na.omit(mkoarag)	

n = length(mkoarag$Time_Stamp_UTC)	

mkoarag$difftime = 0	

for (i in 1:nrow(mkoarag)){	
  d <- difftime(mkoarag$Time_Stamp_UTC[i+1], mkoarag$Time_Stamp_UTC[i])	
  mkoarag$difftime[i] <- d	
}	

breaks <- which(mkoarag$difftime != 20)	

mko1 <- mkoarag[c(1:breaks[1]),]	
mko2 <- mkoarag[c((breaks[1]+1):breaks[2]),]	
mko3 <- mkoarag[c((breaks[2]+1):breaks[3]),]	
mko4 <- mkoarag[c((breaks[3]+1):n),]	

#sbh

sbharag <- ltermerge[, c(1,5)]	

sbharag <- na.omit(sbharag)	

n = length(sbharag$Time_Stamp_UTC)	

sbharag$difftime = 0	

for (i in 1:nrow(sbharag)){	
  d <- difftime(sbharag$Time_Stamp_UTC[i+1], sbharag$Time_Stamp_UTC[i])	
  sbharag$difftime[i] <- d	
}	

breaks <- which(sbharag$difftime != 20)	

sbh1 <- sbharag[c(1:breaks[1]),]	
sbh2 <- sbharag[c((breaks[1]+1):breaks[2]),]	
sbh3 <- sbharag[c((breaks[2]+1):breaks[3]),]	
sbh4 <- sbharag[c((breaks[3]+1):breaks[4]),]	
sbh5 <- sbharag[c((breaks[4]+1):breaks[5]),]
sbh6 <- sbharag[c((breaks[5]+1):n),]

#autocorrelation

#ale
quartz()	
par(mfrow=c(4,2))	

acf_ale1 <- acf(ale1$ale_res_arag, lag.max = nrow(ale1)/3, type = "correlation", xaxt = "n", xlab='Lag (days)', main = 'Autocorrelation', ylab = '')	
axis(1, at=72*(0:50), labels = 1*(0:50))	

acf_ale2 <- acf(ale2$ale_res_arag, lag.max = nrow(ale2)/3, type = "correlation", xaxt = "n", xlab='Lag (days)', main = 'Autocorrelation', ylab = '')	
axis(1, at=72*(0:50), labels = 1*(0:50))	

acf_ale3 <- acf(ale3$ale_res_arag, lag.max = nrow(ale3)/3, type = "correlation", xaxt = "n", xlab='Lag (days)', main = 'Autocorrelation', ylab = '')	
axis(1, at=72*(0:50), labels = 1*(0:50))	

acf_ale4 <- acf(ale4$ale_res_arag, lag.max = nrow(ale4)/3, type = "correlation", xaxt = "n", xlab='Lag (days)', main = 'Autocorrelation', ylab = '')	
axis(1, at=72*(0:50), labels = 1*(0:50))	

acf_ale5 <- acf(ale5$ale_res_arag, lag.max = nrow(ale5)/3, type = "correlation", xaxt = "n", xlab='Lag (days)', main = 'Autocorrelation', ylab = '')	
axis(1, at=72*(0:50), labels = 1*(0:50))	

acf_ale6 <- acf(ale6$ale_res_arag, lag.max = nrow(ale6)/3, type = "correlation", xaxt = "n", xlab='Lag (days)', main = 'Autocorrelation', ylab = '')	
axis(1, at=72*(0:50), labels = 1*(0:50))	

acf_ale7 <- acf(ale7$ale_res_arag, lag.max = nrow(ale7)/3, type = "correlation", xaxt = "n", xlab='Lag (days)', main = 'Autocorrelation', ylab = '')	
axis(1, at=72*(0:50), labels = 1*(0:50))	

acf_ale1_df <- acf_ale1[["acf"]]
acf_ale2_df <- acf_ale2[["acf"]]	
acf_ale3_df <- acf_ale3[["acf"]]
acf_ale4_df <- acf_ale4[["acf"]]	
acf_ale5_df <- acf_ale5[["acf"]]	
acf_ale6_df <- acf_ale6[["acf"]]	
acf_ale7_df <- acf_ale7[["acf"]]	

acf_ale_tot <- cbind.fill(acf_ale1_df, acf_ale2_df, acf_ale3_df, acf_ale4_df, acf_ale5_df, acf_ale6_df, acf_ale7_df, fill = NA)	

acf_ale_final <- rowMeans(acf_ale_tot, na.rm = TRUE)	

#arq

quartz()	
par(mfrow=c(3,1))	

acf_arq1 <- acf(arq1$arq_res_arag, lag.max = nrow(ale1)/3, type = "correlation", xaxt = "n", xlab='Lag (days)', main = 'Autocorrelation', ylab = '')	
axis(1, at=72*(0:50), labels = 1*(0:50))	

acf_arq2 <- acf(arq2$arq_res_arag, lag.max = nrow(ale2)/3, type = "correlation", xaxt = "n", xlab='Lag (days)', main = 'Autocorrelation', ylab = '')	
axis(1, at=72*(0:50), labels = 1*(0:50))	

acf_arq3 <- acf(arq3$arq_res_arag, lag.max = nrow(ale3)/3, type = "correlation", xaxt = "n", xlab='Lag (days)', main = 'Autocorrelation', ylab = '')	
axis(1, at=72*(0:50), labels = 1*(0:50))	

acf_arq1_df <- acf_arq1[["acf"]]	
acf_arq2_df <- acf_arq2[["acf"]]	
acf_arq3_df <- acf_arq3[["acf"]]

acf_arq_tot <- cbind.fill(acf_arq1_df, acf_arq2_df, acf_arq3_df, fill = NA)	

acf_arq_final <- rowMeans(acf_arq_tot, na.rm = TRUE)

#mko

#mko	

quartz()	
par(mfrow=c(2, 2))	

acf_mko1 <- acf(mko1$mko_res_arag, lag.max = nrow(mko1)/3, type = "correlation", xaxt = "n", xlab='Lag (days)', main = 'Autocorrelation', ylab = '')	
axis(1, at=72*(0:30), labels = 1*(0:30))	

acf_mko2 <- acf(mko2$mko_res_arag, lag.max = nrow(mko2)/3, type = "correlation", xaxt = "n", xlab='Lag (days)', main = 'Autocorrelation', ylab = '')	
axis(1, at=72*(0:30), labels = 1*(0:30))	

acf_mko3 <- acf(mko3$mko_res_arag, lag.max = nrow(mko3)/3, type = "correlation", xaxt = "n", xlab='Lag (days)', main = 'Autocorrelation', ylab = '')	
axis(1, at=72*(0:30), labels = 1*(0:30))	

acf_mko4 <- acf(mko4$mko_res_arag, lag.max = nrow(mko4)/3, type = "correlation", xaxt = "n", xlab='Lag (days)', main = 'Autocorrelation', ylab = '')	
axis(1, at=72*(0:30), labels = 1*(0:30))	

acf_mko1_df <- acf_mko1[["acf"]]	
acf_mko2_df <- acf_mko2[["acf"]]	
acf_mko3_df <- acf_mko3[["acf"]]	
acf_mko4_df <- acf_mko4[["acf"]]	

acf_mko_tot <- cbind.fill(acf_mko1_df, acf_mko2_df,acf_mko3_df,acf_mko4_df, fill = NA)	

acf_mko_final <- rowMeans(acf_mko_tot, na.rm = TRUE)	

quartz()	
plot(acf_mko_final, type = "l", xaxt = "n", xlab='Lag (days)', main = 'Autocorrelation', ylab = '')	
axis(1, at=72*(0:50), labels = 1*(0:50))

#sbh	

quartz()	
par(mfrow=c(3, 2))	

acf_sbh1 <- acf(sbh1$sbh_res_arag, lag.max = nrow(sbh1)/3, type = "correlation", xaxt = "n", xlab='Lag (days)', main = 'Autocorrelation', ylab = '')	
axis(1, at=72*(0:30), labels = 1*(0:30))	

acf_sbh2 <- acf(sbh2$sbh_res_arag, lag.max = nrow(sbh2)/3, type = "correlation", xaxt = "n", xlab='Lag (days)', main = 'Autocorrelation', ylab = '')	
axis(1, at=72*(0:30), labels = 1*(0:30))	

acf_sbh3 <- acf(sbh3$sbh_res_arag, lag.max = nrow(sbh3)/3, type = "correlation", xaxt = "n", xlab='Lag (days)', main = 'Autocorrelation', ylab = '')	
axis(1, at=72*(0:30), labels = 1*(0:30))	

acf_sbh4 <- acf(sbh4$sbh_res_arag, lag.max = nrow(sbh4)/3, type = "correlation", xaxt = "n", xlab='Lag (days)', main = 'Autocorrelation', ylab = '')	
axis(1, at=72*(0:30), labels = 1*(0:30))	

acf_sbh5 <- acf(sbh5$sbh_res_arag, lag.max = nrow(sbh5)/3, type = "correlation", xaxt = "n", xlab='Lag (days)', main = 'Autocorrelation', ylab = '')	
axis(1, at=72*(0:30), labels = 1*(0:30))	

acf_sbh6 <- acf(sbh6$sbh_res_arag, lag.max = nrow(sbh5)/3, type = "correlation", xaxt = "n", xlab='Lag (days)', main = 'Autocorrelation', ylab = '')	
axis(1, at=72*(0:30), labels = 1*(0:30))	

acf_sbh1_df <- acf_sbh1[["acf"]]	
acf_sbh2_df <- acf_sbh2[["acf"]]	
acf_sbh3_df <- acf_sbh3[["acf"]]	
acf_sbh4_df <- acf_sbh4[["acf"]]	
acf_sbh5_df <- acf_sbh5[["acf"]]	
acf_sbh6_df <- acf_sbh6[["acf"]]	

acf_sbh_tot <- cbind.fill(acf_sbh1_df, acf_sbh2_df, acf_sbh3_df, acf_sbh4_df, acf_sbh5_df, acf_sbh6_df,fill = NA)	

acf_sbh_final <- rowMeans(acf_sbh_tot, na.rm = TRUE)

#plot

setwd("/Users/raetaylor-burns/Downloads")	

png("autocorr.png", width = 7, height = 5, units = 'in', res = 300)	

autocorrelation <- plot(acf_sbh_final, type = "l", xaxt = "n", xlab='Lag (days)', main = 'Autocorrelation', ylab = '', col = "orangered", xlim = c(0,720), family = "serif")	
lines(acf_ale_final, col = "plum")	
lines(acf_arq_final, col = "slateblue1")	
lines(acf_mko_final, col = "royalblue2")	
abline(h = 0, col = "black")	
op <- par(family = "serif")	
legend(350, 0.9, legend = c("Santa Barbara Harbor", "Alegria Reef", "Arroyo Quemado Reef", "Mohawk Reef"), fill = c("tomato", "plum", "slateblue1", "royalblue2"))	
axis(1, at=72*(0:10), labels = 1*(0:10), family = "serif")	
par(op)

dev.off()	

#correlation matrix

aragcor <- cor(ltermerge[,c(2, 3, 4, 5)], use = "pairwise.complete.obs")