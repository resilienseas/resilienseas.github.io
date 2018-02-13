#Madi Harris
#Interpolation via kriging on West Coast OA 2013 Cruise data
#11/2/2017

#This script creates an interpolated raster across the extent of the 2013 West Coast OA Cruise from Moss Landing, CA to Seattle,WA of aragonite saturation state. The starting dataset is a CSV containing six columns (Date, Time, Latitude, Longitude, Pressure (db), and Omega Aragonite). Observations are subsetted (based on depth) to only include surface level observations. The cruise took place during the month of August 2013


library(rgdal) #install.packages("rgdal")
library(sp) #install.packages("sp")
library(gstat) #install.packages("gstat")
library(tibble) #install.packages("tibble")
library(raster) #install.packages("raster")
library(fields) #install.packages("fields")
library(tidyverse) #install.packages("tidyverse")
library(mapview) #install.packages("mapview")
library(pacman) #install.packages("pacman")
library(here) #install.packages("here)


######################################################
#PART I: PREPARE DATASET
######################################################

#Name working directory to my G drive
OAdir<-"G:/"
setwd(OAdir)

#Load aragonite cruise csv data set
aragonite_data <- read_csv(here("data/WCOAC_2013_test.csv"))


#Set -999 values to NA and remove them from data frame
aragonite_data[aragonite_data==-999.000]<-NA
aragonite_data<- na.omit(aragonite_data)


#Select for pressure <5 db to remove all observations made at lower than surface depth
aragonite_data_subset<- aragonite_data[aragonite_data[, 5]<5,]
aragonite_data <- aragonite_data_subset
View(aragonite_data)

colnames(aragonite_data) <- c("Date", "Time", "Lat", "Long", "Pressure", "OmegaAr")


#Information about the data before spatial transformation
head(aragonite_data)
dim(aragonite_data)
names(aragonite_data)
class(aragonite_data)

#Transform to spatial data set
coordinates(aragonite_data)<-  ~ Long + Lat

#Information about the data to confirm that spatial transformation was successful
head(aragonite_data)
dim(aragonite_data)
names(aragonite_data)
class(aragonite_data)

#Remove duplicate observations
zd<-zerodist(aragonite_data)
aragonite_data_2<-aragonite_data[-zd[,2],]
summary(aragonite_data_2)
aragonite_data<-aragonite_data_2

#View aragonite saturation state observations plotted by latitude/longitude
bubble_aragonite<- bubble(aragonite_data, "OmegaAr", maxsize = 3, xlab = "Longitude", ylab ="Latitude", main = "West Coast Aragonite",scales = list(draw = T), col = "navy", pch = 20,na.rm=TRUE)
bubble_aragonite



######################################################
#PART II: INTERPOLATION VIA SIMPLE KRIGING
######################################################

#look for anisotropy
aragonite_var<-variogram(OmegaAr ~1, data=aragonite_data, alpha=c(0, 45, 90, 135))

#Create variogram of aragonite value
aragonite_var
plot(aragonite_var)

#Fit a model to the values based on estimated nugget, sill, and range, and anisotropy
aragonite_fit<-fit.variogram(aragonite_var,model=vgm(nugget=0.2,psill=1,range=2,model="Exp", anis=c(0, 0.3)))

aragonite_fit
plot(aragonite_var,aragonite_fit)

#Get extent of observations
bbox(aragonite_data)

#Set extent of interpolation
long<-seq(-127.61,-121.85,length=388)
lat<-seq(35.52,50.84,length=1000)

#Create grid for interpolation surface
aragonite_grid<-expand.grid(long,lat)
aragonite_grid
colnames(aragonite_grid)<- c("long", "lat")

coordinates(aragonite_grid) <- ~ long + lat
gridded(aragonite_grid)=TRUE
class(aragonite_grid)

#Run kriging on interpolation grid, based on fitted model
aragonitekrige<-krige(OmegaAr ~ 1, aragonite_data, newdata=aragonite_grid, model=aragonite_fit)
head(aragonitekrige@data)


######################################################
#PART III. KRIGING VISUALIZATION AND TRANSFORM TO RASTER
######################################################

#image(aragonitekrige)
spplot(aragonitekrige)

#Visualize interpolated aragonite saturation state values
spplot(aragonitekrige['var1.pred'],main="Interpolated Aragonite Predictions")

#Visualize variation in interpolated aragonite saturation state values
spplot(aragonitekrige["var1.var"])

#Calculate standard error of aragonite predictions
spplot(aragonitekrige['var1.var'],formula=sqrt(var1.var)~long+lat,col.regions=heat.colors(30),main="Interpolation Standard Error")

spplot(aragonitekrige['var1.pred'],col.regions=heat.colors(30),main="Aragonite Observations")

#Define coordinate system and then reproject aragonite raster
aragonite_raster<-raster(aragonitekrige, layer=1, values=TRUE)
projection(aragonite_raster) <- CRS("+proj=longlat +datum=WGS84")
aragonite_raster_prj <- projectRaster(aragonite_raster, crs=CRS('+init=EPSG:6414'),method="ngb")

mapview(aragonite_raster_prj)

class(aragonite_raster)
#writeRaster(aragonite_raster, "WCOA13_aragonite_raster_1000_anis", format="GTiff",overwrite=TRUE)


##############################################################
#PART IV: HOTSPOT MASK
######################################################
#Create Hotspot Layer: want to create a mask that displays "hotspots" of values below 1, 1.7 or 2
#Starting raster is aragonite_raster_prj

#Reclassify raster values: 
m <- c(0,1,1, 1,1.7,1.7, 1.7,2,2, 2,10,NA)
reclassifymatrix <- matrix(m, ncol=3, byrow=TRUE)
hotspotmask <- reclassify(aragonite_raster_prj, reclassifymatrix)
View(reclassifymatrix)
plot(hotspotmask)
mapview(hotspotmask)
#next steps: write as output and view in Arc
#writeRaster(hotspotmask, "WCOA13_hotspotmask_anis", format="GTiff",overwrite=TRUE)


##############################################################
#PART V. PREPARE MPA SHAPEFILE
##############################################################

#dir_spatial <- 'G:/MPA_analysis/all_mpas_update'
#layer_MPA <- 'all_mpas_update'
#poly_MPA <- readOGR(dsn=dir_spatial, layer=layer_MPA)

poly_MPA <- readOGR(dsn=path.expand("/Users/Madi/Documents/UCSB Bren/ResilienSeas/all_mpas_update"), layer="all_mpas_update")

#poly_MPA <- readOGR(dsn=path.expand("/Users/rttaylorburnscom/github/resilienseas/all_mpas_update"), layer="all_mpas_update")

#Assign same projection as hotspotmask raster to MPA shapefile
poly_MPA <- spTransform(poly_MPA, crs(hotspotmask))

#Plot MPA shapefile and hotspot raster together
plot(poly_MPA, col='lightblue', border='blue')
plot(hotspotmask, add=TRUE)

############################################################
#PART VI. RASTER CLIPPING
#############################################################

#Load west coast shapefile

poly_coast<- readOGR(dsn=path.expand("/Users/Madi/Documents/UCSB Bren/ResilienSeas/Export_Output_2"), layer="Export_Output_2")


#poly_coast <- readOGR(dsn=path.expand("/Users/rttaylorburnscom/github/resilienseas/Export_Output_2"), layer="Export_Output_2")


#Set same projection as rasters
poly_coast <- spTransform(poly_coast, crs(aragonite_raster_prj))

#Use reverse mask to clip aragonite and hotspot rasters to clip to the coast
aragonite_clipped <- mask(aragonite_raster_prj, poly_coast, inverse = TRUE,progress='text')
mapview(aragonite_clipped)
plot(aragonite_clipped)

hotspot_clipped <- mask(hotspotmask, poly_coast, inverse = TRUE,progress='text')
mapview(hotspot_clipped)
plot(hotspot_clipped)


##############################################################
#PART VII. ZONAL STATISTICS
#############################################################

#Calculate mean aragonite saturation state for each MPA and export as data frame
aragonite_mean<- raster::extract(aragonite_clipped, poly_MPA, fun=mean, na.rm=TRUE, df=TRUE)
View(aragonite_mean)
colnames(aragonite_mean) <- c("OBJECTID", "ARAGONITE_MEAN")

#Replace "OBJECTID" with sequenced list to remove duplicates and change from factor to integer form
View(poly_MPA@data[,1])
poly_MPA@data[,1] <- seq(1, length(poly_MPA@data[,1]))
View(poly_MPA@data)

#Join newly calculated aragonite mean to spatial data frame based on OBJECTID
poly_MPA@data <- poly_MPA@data %>% 
  left_join(aragonite_mean, by = 'OBJECTID')
View(poly_MPA@data)

plot(poly_MPA,col=poly_MPA@data[,6])
mapview(poly_MPA)


#############################################################
#TRASH CODE
#############################################################
#Alternative hotspot visualization
#p = aragonitekrige['var1.pred']
#v = aragonitekrige['var1.var']

#p_r = raster(p)
#v_r = raster(v)

#p_lo = p_r - p_r * 0.9 * v_r
#plot(p_lo)

#plot(p_lo < 1)


#Clipping experiments
#plot(hotspotmask)
#plot(ocean2, add=TRUE)

####Clip hotspot mask to land
#Dir <- "G:/RasterClip/finalocean"
#ocean <- readOGR(dsn = Dir, layer = "finalocean")
#ocean2 <-spTransform(ocean, crs(hotspotmask))
#ocean@proj4string
#hotspotclipped <- mask(hotspotmask, ocean2)

#plot(hotspotclipped, col='lightblue')
#plot(ocean2)
#plot(poly_MPA, col='blue')
#plot(ocean, add=TRUE)
#plot(hotspotmask, add=TRUE)
#hotspotclipped
#plot(poly_MPA, add=TRUE)

#plot(ocean, col='lightblue')
#plot(hotspotmask, col='blue', add=TRUE)
#plot(hotspotmask, col='blue')
#plot(ocean, add=TRUE)

leaflet() %>% 
  addTiles() %>%
  addProviderTiles('Esri.OceanBasemap') %>% 
  addRasterImage(aragonite_clipped, colors = pal) %>% 
  addLegend(
    pal = pal, values = values(aragonite_clipped),
    title = "Aragonite Saturation State")

pal <- colorNumeric(c("#FFFFCC", "#41B6C4", "#0C2C84"), values(aragonite_clipped),na.color = "transparent")

