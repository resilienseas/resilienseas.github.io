# load packages ----
install.packages("rlang")
install.packages("here")
install.packages("tibble")
install.packages("sdmpredictors")
install.packages("tidyverse")
install.packages("dismo")
install.packages("deldir")


library(sdmpredictors) 
library(rlang)
library(tibble)
library(tidyverse)
library(here)
library(dismo)
library(deldir)

# explore datasets in the package
list_datasets()

# Explore layers in a dataset
list_layers()

# Explore names of layers in dataset
list<- list_layers("Bio-ORACLE")

# BO_chlomean
# BO_dissox
# BO_sstmean
# BO_salinity
# BO2_chlomean_bdmin

######### layer manipulation

# SEA SURFACE TEMPERATURE ----

# setup datadir for sdmpredictors
dir_sdmdata <- here("data/sdmpredictors")
if (!dir.exists(dir_sdmdata)) dir.create(dir_sdmdata, recursive = T)

# load mean SST layer w/o projection
SST <- load_layers("BO_sstmean", equalarea = F, datadir=dir_sdmdata)

# project to CA Albers NAD 83 EPSG 6414, 10 kilometers
# NOTE: should setup a single raster to snap all other rasters to and feed into "to" argument projectRaster(from, to)
SST <- projectRaster(SST, crs=CRS('+init=EPSG:6414'), res=10000, method="ngb")

# crop raster to fit the Eastern Pacific
NEpacific <- extent(-670000, 350000, -885000, 1400000) 
# names the extent - this is just an estimate ***need to coordinate extent with Rae based on Gap analysis feedback!

SSTcrop <- crop(SST, NEpacific) # crops SST layer according to NEpacific extent

# generate a nice color ramp and plot the map
my.colors = colorRampPalette(c("#5E85B8","#EDF0C0","#C13127"))
plot(SSTcrop,col=my.colors(1000),axes=FALSE, box=FALSE)
plot(inventorycoords, add=TRUE)
title(cex.sub = 1.25, sub = "SST (C)")

###SST RANGE

# load mean SST layer w/o projection
SSTrange <- load_layers("BO_sstrange", equalarea = F, datadir=dir_sdmdata)

# project to CA Albers NAD 83 EPSG 6414, 10 kilometers
# NOTE: should setup a single raster to snap all other rasters to and feed into "to" argument projectRaster(from, to)
SSTrange <- projectRaster(SSTrange, crs=CRS('+init=EPSG:6414'), res=10000, method="ngb")

# crop raster to fit the Eastern Pacific
NEpacific <- extent(-670000, 350000, -885000, 1400000) 
# names the extent - this is just an estimate ***need to coordinate extent with Rae based on Gap analysis feedback!

SSTrangecrop <- crop(SSTrange, NEpacific) # crops SST layer according to NEpacific extent

# generate a nice color ramp and plot the map
my.colors = colorRampPalette(c("#5E85B8","#EDF0C0","#C13127"))
plot(SSTrangecrop,col=my.colors(1000),axes=FALSE, box=FALSE)
title(cex.sub = 1.25, sub = "SST range (C)")

################################################################

## DISSOLVED OXYGEN

DO <- load_layers("BO_dissox", equalarea = F, datadir=dir_sdmdata)
# load Dissolved Oxygen Data W/O projection

# project to CA Albers NAD 83 EPSG 6414, 10 kilometers
# NOTE: should setup a single raster to snap all other rasters to and feed into "to" argument projectRaster(from, to)
DO <- projectRaster(DO, crs=CRS('+init=EPSG:6414'), res=10000, method="ngb")

DOcrop <- crop(DO, NEpacific) # crops DO layer according to NEpacific extent

# generate a nice color ramp and plot the map
my.colors = colorRampPalette(c("#5E85B8","#EDF0C0","#C13127"))
plot(DOcrop,col=my.colors(1000),axes=FALSE, box=FALSE)
title(cex.sub = 1.25, sub = "DO (C)")

####DO RANGE

# load mean SST layer w/o projection
DOrange <- load_layers("BO2_dissoxrange_bdmin", equalarea = F, datadir=dir_sdmdata)

# project to CA Albers NAD 83 EPSG 6414, 10 kilometers
# NOTE: should setup a single raster to snap all other rasters to and feed into "to" argument projectRaster(from, to)
DOrange <- projectRaster(DOrange, crs=CRS('+init=EPSG:6414'), res=10000, method="ngb")


DOrangecrop <- crop(DOrange, NEpacific) # crops SST layer according to NEpacific extent

# generate a nice color ramp and plot the map
my.colors = colorRampPalette(c("#5E85B8","#EDF0C0","#C13127"))
plot(DOrangecrop,col=my.colors(1000),axes=FALSE, box=FALSE)
title(cex.sub = 1.25, sub = "DO range")


###############################################################
#GAP ANALYSIS#
###############################################################

#import inventory
oahfocus <- read_csv(here("oahfocus.csv"))

#isolate coordinate columns
coords<-cbind.data.frame(oahfocus$Longitude, oahfocus$Latitude)

#remove duplicate locations
deduped.coords<-unique(coords)

#create spatial points objects
inventorycoords <- SpatialPoints(deduped.coords, CRS("+proj=longlat +ellps=WGS84"))
inventorycoords <- spTransform(inventorycoords, CRS('+init=EPSG:6414'))

#check to make sure projections match
plot(SSTcrop,col=my.colors(1000),axes=FALSE, box=FALSE)
plot(inventorycoords, add=TRUE)
title(cex.sub = 1.25, sub = "inventory")

#create voronoi polygons
vor<-voronoi(inventorycoords)

#plot polygons by id number
spplot(vor, "id")

#rasterize polygons
vorraster<- rasterize(vor, SSTcrop, "id")

#plot rasterized polygons
plot(vorraster, col=my.colors(1000))

#extract sst value for each monitoring site cell
sitesst<- raster::extract(SSTcrop, inventorycoords, method='simple', df=TRUE)

#rename column names of sitesst
colnames(sitesst)<-c("id", "SST")

#make sure inventory points and polygons are in same order?

#substitute polygon id for monitoring site sea surface temerature of that polygon
polygonsst<-subs(vorraster@data@values, sitesst, by=sitesst$id, which=sitesst$SST)



####################GAP ANALYSIS#################
distance<-distanceFromPoints(variability,inventorycoords)
plot(distance)
gaps <- setValues(distance, (getValues(distance)*getValues(variability)))
plot(gaps)

binarygaps <- setValues(gaps, (getValues(distance)*getValues(variability)) > 6000)

plot(binarygaps)

library(mapview) 
#install.packages("mapview")
mapview(gaps)
