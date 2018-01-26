#Madi Harris
#Kriging test
#11/2/2017

#install.packages("sp")
#install.packages("gstat")
#install.packages("tibble")

library(rgdal)
library(sp)
library(gstat)
library(tibble)


###########################
#Name working directory to my CP folder
OAdir<-"G:/"
setwd(OAdir)

#load aragonite cruise data
View(WCOAC_2012_test)
aragonite_data<-WCOAC_2012_test


#remove X7 column
aragonite_data$X7 <- NULL
aragonite_data$X8 <- NULL


#set -999 to NA
aragonite_data[aragonite_data==-999.000]<-NA

#remove NA observations from data frame
aragonite_data<- na.omit(aragonite_data)


#select for pressure <5 db
aragonite_data_subset<- aragonite_data[aragonite_data[, 5]<5,]

aragonite_data <- aragonite_data_subset
View(aragonite_data)

#information about csv before transformation
head(aragonite_data)
dim(aragonite_data)
names(aragonite_data)
class(aragonite_data)

#transform to spatial data set
coordinates(aragonite_data)<-  ~ Long + Lat

#information about the data set after transformation
head(aragonite_data)
dim(aragonite_data)
names(aragonite_data)
class(aragonite_data)

#remove duplicates
#zd<-zerodist(aragonite_data)
#aragonite_data_2<-aragonite_data[-zd[,2],]
#summary(aragonite_data_2)
#aragonite_data<-aragonite_data_2

bubble_aragonite<- bubble(aragonite_data, "OmegaAr", maxsize = 3, xlab = "Longitude", ylab ="Latitude", main = "West Coast Aragonite",scales = list(draw = T), col = "navy", pch = 20,na.rm=TRUE)
bubble_aragonite

aragonite_var<-variogram(OmegaAr ~1, data=aragonite_data)

aragonite_var
plot(aragonite_var)

aragonite_fit<-fit.variogram(aragonite_var,model=vgm(nugget=0.1,psill=0.4,range=2,model="Exp"))

aragonite_fit
plot(aragonite_var,aragonite_fit)

bbox(aragonite_data)

long<-seq(-125.63,-122.89,length=100)
lat<-seq(37.67,48.38,length=100)

aragonite_grid<-expand.grid(long,lat)
aragonite_grid
colnames(aragonite_grid)<- c("long", "lat")

coordinates(aragonite_grid) <- ~ long + lat
class(aragonite_grid)

gridded(aragonite_grid)=TRUE
class(aragonite_grid)


aragonitekrige<-krige(OmegaAr ~ 1, aragonite_data, newdata=aragonite_grid, model=aragonite_fit)
head(aragonitekrige@data)
#image(aragonitekrige)
spplot(aragonitekrige)

spplot(aragonitekrige['var1.pred'])
spplot(aragonitekrige['var1.var'])


p = aragonitekrige['var1.pred']
v = aragonitekrige['var1.var']

library(raster)

p_r = raster(p)
v_r = raster(v)

p_lo = p_r - p_r * 0.9 * v_r
plot(p_lo)

plot(p_lo < 1.6)




