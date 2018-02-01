#Ocean Acidifcation Hotspot Interpolation and MPA Impact Analysis

#load packages----
if (!require(pacman)) install.packages("pacman")
library(pacman)
p_load(
  tidyverse, here, 
  rgdal,
  sp, gstat, 
  mapview)

#prepare dataset----
aragonite_data <- read_csv(here("data/WCOAC_2013_test.csv"))

#Set -999 values to N/A and remove them from data frame
aragonite_data[aragonite_data==-999.000]<-NA
aragonite_data<- na.omit(aragonite_data)

#Select for pressure <5 db to remove all observations made at lower than surface depth
aragonite_data<- aragonite_data[aragonite_data[, 5]<5,]

colnames(aragonite_data) <- c("Date", "Time", "Lat", "Long", "Pressure", "OmegaAr")

#Transform to spatial data set
coordinates(aragonite_data)<-  ~ Long + Lat

#Remove duplicate observations
zd<-zerodist(aragonite_data)
aragonite_data_2<-aragonite_data[-zd[,2],]

