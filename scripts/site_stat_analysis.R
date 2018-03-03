######### SBC LTER statistical analysis for ALE, MKO, SBH, ARQ sites

if (!require(pacman)) install.packages("pacman")
library(pacman)
p_load(
  here,
  plyr,
  tidyr,
  dplyr,
  readr,
  ggplot2)

# load SBC LTER csv from "data" folder in github repo
ale <- read_csv("data/ALE_arag.csv")
arq  <- read_csv("data/ARQ_arag.csv")
mko <- read_csv("data/MKO_arag.csv")
sbh  <- read_csv("data/SBH_arag.csv")

# set '-9999' values to NA
ale[ale==-9999] <- NA
arq[arq==-9999] <- NA
mko[mko==-9999] <- NA
sbh[sbh==-9999] <- NA

colnames(ale) <- c("date.time", "site", "lat", "long", "aragonite")
colnames(arq) <- c("date.time", "site", "lat", "long", "aragonite")
colnames(mko) <- c("date.time", "site", "lat", "long", "aragonite")
colnames(sbh) <- c("date.time", "site", "lat", "long", "aragonite")

head(ale) # starts 2011-06-21
tail(ale) # ends 2014-01-07
head(arq) # starts 2012-07-30
tail(arq) # ends 2015-03-06
head(mko) # starts 2012-01-11
tail(mko) # ends 2015-04-08
head(sbh) # starts 2012-09-15
tail(sbh) # ends 2015-04-29

ggplot(data = ale, aes(date.time, aragonite)) +
  geom_line()

ggplot(data = arq, aes(date.time, aragonite)) +
  geom_line()

ggplot(data = mko, aes(date.time, aragonite)) +
  geom_line()

ggplot(data = sbh, aes(date.time, aragonite)) +
  geom_line()