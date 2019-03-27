###temporal decorellation and covariance

##install packages
library(lubridate)
library(dplyr)
library(devtools)
library(here)
library(tidyverse)

lter <- merge.data.frame(ale, arq, by = 'Time_stamp_UTC', all.x = T)
lter <- merge.data.frame(lter, mko, by = 'Time_stamp_UTC', all.x = T)
lter <- merge.data.frame(lter, sbh, by = 'Time_stamp_UTC', all.x = T)

lter[lter == -9999] <- NA

##remove seasonal trends

lter$month <- month(lter$Time_stamp_UTC)

ale_temp_monthly <- aggregate(Ale_Temperature ~ month, lter, mean)
arq_temp_monthly <- aggregate(Arq_Temperature ~ month, lter, mean)
mko_temp_monthly <- aggregate(Mko_Temperature ~ month, lter, mean)
sbh_temp_monthly <- aggregate(Sbh_Temperature ~ month, lter, mean)

ale_sal_monthly <- aggregate(Ale_Salinity ~ month, lter, mean)
arq_sal_monthly <- aggregate(Arq_Salinity ~ month, lter, mean)
mko_sal_monthly <- aggregate(Mko_Salinity ~ month, lter, mean)
sbh_sal_monthly <- aggregate(Sbh_Salinity ~ month, lter, mean)

ale_TA_monthly <- aggregate(Ale_TA ~ month, lter, mean)
arq_TA_monthly <- aggregate(Arq_TA ~ month, lter, mean)
mko_TA_monthly <- aggregate(Mko_TA ~ month, lter, mean)
sbh_TA_monthly <- aggregate(Sbh_TA ~ month, lter, mean)

ale_pCO2_monthly <- aggregate(Ale_pCO2 ~ month, lter, mean)
arq_pCO2_monthly <- aggregate(Arq_pCO2 ~ month, lter, mean)
mko_pCO2_monthly <- aggregate(Mko_pCO2 ~ month, lter, mean)
sbh_pCO2_monthly <- aggregate(Sbh_pCO2 ~ month, lter, mean)

ale_arag_monthly <- aggregate(Ale_Arag ~ month, lter, mean)
arq_arag_monthly <- aggregate(Arq_Arag ~ month, lter, mean)
mko_arag_monthly <- aggregate(Mko_Arag ~ month, lter, mean)
sbh_arag_monthly <- aggregate(Sbh_Arag ~ month, lter, mean)

ltermerge <- merge.data.frame(lter, ale_arag_monthly, by = 'month', all.x = T)
ltermerge <- merge.data.frame(ltermerge, arq_arag_monthly, by = 'month', all.x = T)
ltermerge <- merge.data.frame(ltermerge, mko_arag_monthly, by = 'month', all.x = T)
ltermerge <- merge.data.frame(ltermerge, sbh_arag_monthly, by = 'month', all.x = T)

ltermerge <- merge.data.frame(ltermerge, ale_temp_monthly, by = 'month', all.x = T)
ltermerge <- merge.data.frame(ltermerge, arq_temp_monthly, by = 'month', all.x = T)
ltermerge <- merge.data.frame(ltermerge, mko_temp_monthly, by = 'month', all.x = T)
ltermerge <- merge.data.frame(ltermerge, sbh_temp_monthly, by = 'month', all.x = T)

ltermerge <- merge.data.frame(ltermerge, ale_sal_monthly, by = 'month', all.x = T)
ltermerge <- merge.data.frame(ltermerge, arq_sal_monthly, by = 'month', all.x = T)
ltermerge <- merge.data.frame(ltermerge, mko_sal_monthly, by = 'month', all.x = T)
ltermerge <- merge.data.frame(ltermerge, sbh_sal_monthly, by = 'month', all.x = T)

ltermerge <- merge.data.frame(ltermerge, ale_TA_monthly, by = 'month', all.x = T)
ltermerge <- merge.data.frame(ltermerge, arq_TA_monthly, by = 'month', all.x = T)
ltermerge <- merge.data.frame(ltermerge, mko_TA_monthly, by = 'month', all.x = T)
ltermerge <- merge.data.frame(ltermerge, sbh_TA_monthly, by = 'month', all.x = T)

ltermerge <- merge.data.frame(ltermerge, ale_pCO2_monthly, by = 'month', all.x = T)
ltermerge <- merge.data.frame(ltermerge, arq_pCO2_monthly, by = 'month', all.x = T)
ltermerge <- merge.data.frame(ltermerge, mko_pCO2_monthly, by = 'month', all.x = T)
ltermerge <- merge.data.frame(ltermerge, sbh_pCO2_monthly, by = 'month', all.x = T)

ltermerge$ale_res_temp <- ltermerge$Ale_Temperature.x-ltermerge$Ale_Temperature.y
ltermerge$arq_res_temp <- ltermerge$Arq_Temperature.x-ltermerge$Arq_Temperature.y
ltermerge$mko_res_temp <- ltermerge$Mko_Temperature.x-ltermerge$Mko_Temperature.y
ltermerge$sbh_res_temp <- ltermerge$Sbh_Temperature.x-ltermerge$Sbh_Temperature.y

ltermerge$ale_res_sal <- ltermerge$Ale_Salinity.x-ltermerge$Ale_Salinity.y
ltermerge$arq_res_sal <- ltermerge$Arq_Salinity.x-ltermerge$Arq_Salinity.y
ltermerge$mko_res_sal <- ltermerge$Mko_Salinity.x-ltermerge$Mko_Salinity.y
ltermerge$sbh_res_sal <- ltermerge$Sbh_Salinity.x-ltermerge$Sbh_Salinity.y

ltermerge$ale_res_ta <- ltermerge$Ale_TA.x-ltermerge$Ale_TA.y
ltermerge$arq_res_ta <- ltermerge$Arq_TA.x-ltermerge$Arq_TA.y
ltermerge$mko_res_ta <- ltermerge$Mko_TA.x-ltermerge$Mko_TA.y
ltermerge$sbh_res_ta <- ltermerge$Sbh_TA.x-ltermerge$Sbh_TA.y

ltermerge$ale_res_pco2 <- ltermerge$Ale_pCO2.x-ltermerge$Ale_pCO2.y
ltermerge$arq_res_pco2 <- ltermerge$Arq_pCO2.x-ltermerge$Arq_pCO2.y
ltermerge$mko_res_pco2 <- ltermerge$Mko_pCO2.x-ltermerge$Mko_pCO2.y
ltermerge$sbh_res_pco2 <- ltermerge$Sbh_pCO2.x-ltermerge$Sbh_pCO2.y

ltermerge$ale_res_arag <- ltermerge$Ale_Arag.x-ltermerge$Ale_Arag.y
ltermerge$arq_res_arag <- ltermerge$Arq_Arag.x-ltermerge$Arq_Arag.y
ltermerge$mko_res_arag <- ltermerge$Mko_Arag.x-ltermerge$Mko_Arag.y
ltermerge$sbh_res_arag <- ltermerge$Sbh_Arag.x-ltermerge$Sbh_Arag.y

ltermerge <- ltermerge[order(ltermerge$Time_stamp_UTC),]
  
ltermerge$Time_stamp_UTC <- as.POSIXct(ltermerge$Time_stamp_UTC,format = "%Y-%m-%dT %H:%M:%SZ")

###decorrelation time scale

##separate time chunks based on missing data

#ale

alearag <- ltermerge[, c(2, 59)]

alearag <- na.omit(alearag)

n = length(alearag$Time_stamp_UTC)

alearag$difftime = 0

for (i in 1:nrow(alearag)){
  d <- difftime(alearag$Time_stamp_UTC[i+1], alearag$Time_stamp_UTC[i], units = "mins")
  alearag$difftime[i] <- d
}

breaks <- which(alearag$difftime != 20)

ale1 <- alearag[c(1:breaks[1]),]
ale2 <- alearag[c((breaks[1]+1):breaks[2]),]
ale3 <- alearag[c((breaks[2]+1):breaks[3]),]
ale4 <- alearag[c((breaks[3]+1):breaks[4]),]
ale5 <- alearag[c((breaks[4]+1):breaks[5]),]
ale6 <- alearag[c((breaks[5]+1):breaks[6]),]
ale7 <- alearag[c((breaks[6]+1):breaks[7]),]
ale8 <- alearag[c((breaks[7]+1):breaks[8]),]
ale9 <- alearag[c((breaks[8]+1):breaks[9]),]
ale10 <- alearag[c((breaks[9]+1):n),]


# test for differences between groups

alearag$group <- 0
alearag$group[c(1:breaks[1])] = 1
alearag$group[c((breaks[1]+1):breaks[2])] = 2
alearag$group[c((breaks[2]+1):breaks[3])] = 3
alearag$group[c((breaks[3]+1):breaks[4])] = 4
alearag$group[c((breaks[4]+1):breaks[5])] = 5
alearag$group[c((breaks[5]+1):breaks[6])] = 6
alearag$group[c((breaks[6]+1):breaks[7])] = 7
alearag$group[c((breaks[7]+1):breaks[8])] = 8
alearag$group[c((breaks[8]+1):breaks[9])] = 9
alearag$group[c((breaks[9]+1):n)] = 10

fgroup <- factor(alearag$group)

quartz()
ggboxplot(alearag, x = "group", y = "ale_res_arag")
res.aov <- aov(ale_res_arag ~ fgroup, data = alearag)
summary(res.aov)

TukeyHSD(res.aov)

#arq

arqarag <- ltermerge[, c(2, 60)]

arqarag <- na.omit(arqarag)

n = length(arqarag$Time_stamp_UTC)

arqarag$difftime = 0

for (i in 1:nrow(arqarag)){
  d <- difftime(arqarag$Time_stamp_UTC[i+1], arqarag$Time_stamp_UTC[i])
  arqarag$difftime[i] <- d
}

breaks <- which(arqarag$difftime != 20)

arq1 <- arqarag[c(1:breaks[1]),]
arq2 <- arqarag[c((breaks[1]+1):breaks[2]),]
arq3 <- arqarag[c((breaks[2]+1):breaks[3]),]
arq4 <- arqarag[c((breaks[3]+1):breaks[4]),]
arq5 <- arqarag[c((breaks[4]+1):breaks[5]),]
arq6 <- arqarag[c((breaks[5]+1):n),]

# test for differences between groups

arqarag$group <- 0
arqarag$group[c(1:breaks[1])] = 1
arqarag$group[c((breaks[1]+1):breaks[2])] = 2
arqarag$group[c((breaks[2]+1):breaks[3])] = 3
arqarag$group[c((breaks[3]+1):breaks[4])] = 4
arqarag$group[c((breaks[4]+1):breaks[5])] = 5
arqarag$group[c((breaks[5]+1):n)] = 6

fgroup <- factor(arqarag$group)

quartz()
ggboxplot(arqarag, x = "group", y = "arq_res_arag")
res.aov <- aov(arq_res_arag ~ fgroup, data = arqarag)
summary(res.aov)

TukeyHSD(res.aov)

#mko

mkoarag <- ltermerge[, c(2, 61)]

mkoarag <- na.omit(mkoarag)

n = length(mkoarag$Time_stamp_UTC)

mkoarag$difftime = 0

for (i in 1:nrow(mkoarag)){
  d <- difftime(mkoarag$Time_stamp_UTC[i+1], mkoarag$Time_stamp_UTC[i])
  mkoarag$difftime[i] <- d
}

breaks <- which(mkoarag$difftime != 20)

mko1 <- mkoarag[c(1:breaks[1]),]
mko2 <- mkoarag[c((breaks[1]+1):breaks[2]),]
mko3 <- mkoarag[c((breaks[2]+1):breaks[3]),]
mko4 <- mkoarag[c((breaks[3]):n),]

# test for differences between groups

mkoarag$group <- 0
mkoarag$group[c(1:breaks[1])] = 1
mkoarag$group[c((breaks[1]+1):breaks[2])] = 2
mkoarag$group[c((breaks[2]+1):breaks[3])] = 3
mkoarag$group[c((breaks[3]+1):n)] = 4

fgroup <- factor(mkoarag$group)

quartz()
ggboxplot(mkoarag, x = "group", y = "mko_res_arag")
res.aov <- aov(mko_res_arag ~ fgroup, data = mkoarag)

TukeyHSD(res.aov)

#sbh

sbharag <- ltermerge[, c(2, 62)]

sbharag <- na.omit(sbharag)

n = length(sbharag$Time_stamp_UTC)

sbharag$difftime = 0

for (i in 1:nrow(sbharag)){
  d <- difftime(sbharag$Time_stamp_UTC[i+1], sbharag$Time_stamp_UTC[i])
  sbharag$difftime[i] <- d
}

breaks <- which(sbharag$difftime != 20)

sbh1 <- sbharag[c(1:breaks[1]),]
sbh2 <- sbharag[c((breaks[1]+1):breaks[2]),]
sbh3 <- sbharag[c((breaks[2]+1):breaks[3]),]
sbh4 <- sbharag[c((breaks[3]+1):breaks[4]),]
sbh5 <- sbharag[c((breaks[4]+1):breaks[5]),]
sbh6 <- sbharag[c((breaks[5]+1):breaks[6]),]
sbh7 <- sbharag[c((breaks[6]+1):n),]

# test for differences between groups

sbharag$group <- 0
sbharag$group[c(1:breaks[1])] = 1
sbharag$group[c((breaks[1]+1):breaks[2])] = 2
sbharag$group[c((breaks[2]+1):breaks[3])] = 3
sbharag$group[c((breaks[3]+1):breaks[4])] = 4
sbharag$group[c((breaks[4]+1):breaks[5])] = 5
sbharag$group[c((breaks[5]+1):breaks[6])] = 6
sbharag$group[c((breaks[6]+1):n)] = 7

fgroup <- factor(sbharag$group)

quartz()
ggboxplot(sbharag, x = "group", y = "sbh_res_arag")
res.aov <- aov(sbh_res_arag ~ fgroup, data = sbharag)

TukeyHSD(res.aov)

##autocorrelation

# ale
quartz()
par(mfrow=c(5,2))

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

acf_ale8 <- acf(ale8$ale_res_arag, lag.max = nrow(ale8)/3, type = "correlation", xaxt = "n", xlab='Lag (days)', main = 'Autocorrelation', ylab = '')
axis(1, at=72*(0:50), labels = 1*(0:50))

acf_ale9 <- acf(ale9$ale_res_arag, lag.max = nrow(ale9)/3, type = "correlation", xaxt = "n", xlab='Lag (days)', main = 'Autocorrelation', ylab = '')
axis(1, at=72*(0:50), labels = 1*(0:50))

acf_ale10 <- acf(ale10$ale_res_arag, lag.max = nrow(ale10)/3, type = "correlation", xaxt = "n", xlab='Lag (days)', main = 'Autocorrelation', ylab = '')
axis(1, at=72*(0:50), labels = 1*(0:50))

library(rowr)

acf_ale1_df <- acf_ale1[["acf"]]
acf_ale2_df <- acf_ale2[["acf"]]
acf_ale3_df <- acf_ale3[["acf"]]
acf_ale4_df <- acf_ale4[["acf"]]
acf_ale5_df <- acf_ale5[["acf"]]
acf_ale6_df <- acf_ale6[["acf"]]
acf_ale7_df <- acf_ale7[["acf"]]
acf_ale8_df <- acf_ale8[["acf"]]
acf_ale9_df <- acf_ale9[["acf"]]
acf_ale10_df <- acf_ale10[["acf"]]

acf_ale_tot <- cbind.fill(acf_ale1_df, acf_ale2_df, acf_ale3_df, acf_ale4_df, acf_ale5_df, acf_ale6_df, acf_ale7_df, acf_ale8_df, acf_ale9_df, acf_ale10_df, fill = NA)

acf_ale_final <- rowMeans(acf_ale_tot, na.rm = TRUE)

quartz()
plot(acf_ale_final, type = "l", xaxt = "n", xlab='Lag (days)', main = 'Autocorrelation', ylab = '')
axis(1, at=72*(0:50), labels = 1*(0:50))

#arq

quartz()
par(mfrow=c(4, 3))

acf_arq1 <- acf(arq1$arq_res_arag, lag.max = nrow(arq1)/3, type = "correlation", xaxt = "n", xlab='Lag (days)', main = 'Autocorrelation', ylab = '')
axis(1, at=72*(0:30), labels = 1*(0:30))

acf_arq2 <- acf(arq2$arq_res_arag, lag.max = nrow(arq2)/3, type = "correlation", xaxt = "n", xlab='Lag (days)', main = 'Autocorrelation', ylab = '')
axis(1, at=72*(0:30), labels = 1*(0:30))

acf_arq3 <- acf(arq3$arq_res_arag, lag.max = nrow(arq3)/3, type = "correlation", xaxt = "n", xlab='Lag (days)', main = 'Autocorrelation', ylab = '')
axis(1, at=72*(0:30), labels = 1*(0:30))

acf_arq4 <- acf(arq4$arq_res_arag, lag.max = nrow(arq4)/3, type = "correlation", xaxt = "n", xlab='Lag (days)', main = 'Autocorrelation', ylab = '')
axis(1, at=72*(0:30), labels = 1*(0:30))

acf_arq5 <- acf(arq5$arq_res_arag, lag.max = nrow(arq5)/3, type = "correlation", xaxt = "n", xlab='Lag (days)', main = 'Autocorrelation', ylab = '')
axis(1, at=72*(0:30), labels = 1*(0:30))

acf_arq6 <- acf(arq6$arq_res_arag, lag.max = nrow(arq6)/3, type = "correlation", xaxt = "n", xlab='Lag (days)', main = 'Autocorrelation', ylab = '')
axis(1, at=72*(0:30), labels = 1*(0:30))

acf_arq1_df <- acf_arq1[["acf"]]
acf_arq2_df <- acf_arq2[["acf"]]
acf_arq3_df <- acf_arq3[["acf"]]
acf_arq4_df <- acf_arq4[["acf"]]
acf_arq5_df <- acf_arq5[["acf"]]
acf_arq6_df <- acf_arq6[["acf"]]

acf_arq_tot <- cbind.fill(acf_arq1_df, acf_arq2_df, acf_arq3_df, acf_arq4_df, acf_arq5_df, acf_arq6_df, fill = NA)

acf_arq_final <- rowMeans(acf_arq_tot, na.rm = TRUE)

quartz()
plot(acf_arq_final, type = "l", xaxt = "n", xlab='Lag (days)', main = 'Autocorrelation', ylab = '')
axis(1, at=72*(0:50), labels = 1*(0:50))

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

acf_sbh6 <- acf(sbh6$sbh_res_arag, lag.max = nrow(sbh6)/3, type = "correlation", xaxt = "n", xlab='Lag (days)', main = 'Autocorrelation', ylab = '')
axis(1, at=72*(0:30), labels = 1*(0:30))

acf_sbh7 <- acf(sbh7$sbh_res_arag, lag.max = nrow(sbh7)/3, type = "correlation", xaxt = "n", xlab='Lag (days)', main = 'Autocorrelation', ylab = '')
axis(1, at=72*(0:30), labels = 1*(0:30))


acf_sbh1_df <- acf_sbh1[["acf"]]
acf_sbh2_df <- acf_sbh2[["acf"]]
acf_sbh3_df <- acf_sbh3[["acf"]]
acf_sbh4_df <- acf_sbh4[["acf"]]
acf_sbh5_df <- acf_sbh5[["acf"]]
acf_sbh6_df <- acf_sbh6[["acf"]]
acf_sbh7_df <- acf_sbh7[["acf"]]

acf_sbh_tot <- cbind.fill(acf_sbh1_df, acf_sbh2_df, acf_sbh3_df, acf_sbh4_df, acf_sbh5_df, acf_sbh6_df, acf_sbh7_df,fill = NA)

acf_sbh_final <- rowMeans(acf_sbh_tot, na.rm = TRUE)


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

#PCA
library(ggbiplot)

ltercomplete <- na.omit(ltermerge)

lterpca <- prcomp(ltercomplete[,c(59, 60, 61, 62)])

quartz()
ggbiplot(lterpca, ellipse = TRUE, size = 0.001)

#look for outliers to remove
quartz()
boxplot(ltercomplete$sbh_res_arag)

outliers <- boxplot(ltercomplete$sbh_res_arag, plot=FALSE)$out

ltercomplete <- ltercomplete[-which(ltercomplete$sbh_res_arag %in% outliers),]

lterpca <- prcomp(ltercomplete[,c(59, 60, 61, 62)])

quartz()
ggbiplot(lterpca, ellipse = TRUE, size = 0.001)

aragcor <- cor(ltermerge[,c(59, 60, 61, 62)], use = "pairwise.complete.obs")
aragcov <- cov(ltermerge[,c(59, 60, 61, 62)], use = "pairwise.complete.obs")

print(lterpca)

#why is there no correlation? look at other variables to see?

tempcor <- cor(ltermerge[,c(43, 44, 45, 46)], use = "pairwise.complete.obs")
tempcov <- cov(ltermerge[,c(43, 44, 45, 46)], use = "pairwise.complete.obs")

tacov <- cov(lteromerge[,c(51, 52, 53, 54)], use = "pairwise.complete.obs")
tacor <- cor(ltermerge[,c(51, 52, 53, 54)], use = "pairwise.complete.obs")

pco2cov <- cov(lteromerge[,c(55, 56, 57, 58)], use = "pairwise.complete.obs")
pco2cor <- cor(ltermerge[,c(55, 56, 57, 58)], use = "pairwise.complete.obs")

salcov <- cov(ltermerge[,c(47, 48, 49, 50)], use = "pairwise.complete.obs")
salcor <- cor(ltermerge[,c(47, 48, 49, 50)], use = "pairwise.complete.obs")

alecor <- cor(ltermerge[,c(43, 47, 51, 55, 59)], use = "pairwise.complete.obs")
arqcor <- cor(ltermerge[,c(44, 48, 52, 56, 60)], use = "pairwise.complete.obs")
mkocor <- cor(ltermerge[,c(45, 49, 53, 57, 61)], use = "pairwise.complete.obs")
sbhcor <- cor(ltermerge[,c(46, 60, 54, 58, 62)], use = "pairwise.complete.obs")

lterpca <- prcomp(ltercomplete[,c(2, 3, 4)])

ggbiplot(lterpca, ellipse = TRUE, size = 0.001)

print(lterpca)

lterpca <- prcomp(ltercomplete[,c(2, 3)])

ggbiplot(lterpca, ellipse = TRUE, size = 0.001)

print(lterpca)

