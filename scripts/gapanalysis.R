# gap analysis

# load packages ----
install.packages("rlang")
install.packages("here")
install.packages("tibble")
install.packages("sdmpredictors")
install.packages("tidyverse")
install.packages("dismo")
install.packages("deldir")
install.packages("mapview")

library(sdmpredictors) 
library(rlang)
library(tibble)
library(tidyverse)
library(here)
library(dismo)
library(deldir)
library(mapview)

# explore sdmpredictors ----
list_datasets() %>% View()

# explore layers in a dataset
list_layers()


# explore names of layers in dataset
list_layers("Bio-ORACLE") %>% View()

list_layers("MARSPEC") %>% View()
list_layers("WorldClim") %>% View()


# explore names of layers in dataset
list<- list_layers("Bio-ORACLE")

# layer manipulation ----

# sea surface temperature

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

# sst range

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

# dissolved oxygen

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

# dissolved oxygen range

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

# prep inventory----

# import inventory
oahfocus <- read_csv(here("oahfocus.csv"))

# isolate coordinate columns
coords<-cbind.data.frame(oahfocus$Longitude, oahfocus$Latitude)

# remove duplicate locations
deduped.coords<-unique(coords)

# create spatial points objects
inventorycoords <- SpatialPoints(deduped.coords, CRS("+proj=longlat +ellps=WGS84"))
inventorycoords <- spTransform(inventorycoords, CRS('+init=EPSG:6414'))

# check to make sure projections match
plot(SSTcrop,col=my.colors(1000),axes=FALSE, box=FALSE)
plot(inventorycoords, add=TRUE)
title(cex.sub = 1.25, sub = "inventory")

# create voronoi polygons
vor<-voronoi(inventorycoords)

# plot polygons by id number
spplot(vor, "id")

# rasterize polygons
vorraster<- rasterize(vor, SSTcrop, "id")

# plot rasterized polygons
plot(vorraster, col=my.colors(1000))

# substitution process ----

# sst

# extract sst value for each monitoring site cell
sitesst<- raster::extract(SSTcrop, inventorycoords, method='simple', df=TRUE)

# rename column names of sitesst
colnames(sitesst)<-c("id", "SST")

# make sure inventory points and polygons are in same order?

# substitute polygon id for monitoring site sea surface temerature of that polygon
polygonsst<-subs(vorraster@data@values, sitesst, by=sitesst$id, which=sitesst$SST)

# sst range

# extract sst range value for each monitoring site cell
sitesstrange<- raster::extract(SSTrangecrop, inventorycoords, method='simple', df=TRUE)

# rename column names of sitesstrange
colnames(sitesstrange)<-c("id", "SSTrange")

# make sure inventory points and polygons are in same order?

# substitute polygon id for monitoring site sea surface temerature of that polygon
polygonsstrange<-subs(vorraster@data@values, sitesstrange, by=sitesstrange$id, which=sitesstrange$SSTrange)

# do

# extract do value for each monitoring site cell
sitedo<- raster::extract(DOcrop, inventorycoords, method='simple', df=TRUE)

# rename column names of sitedo
colnames(sitedo)<-c("id", "DO")

# make sure inventory points and polygons are in same order?

# substitute polygon id for monitoring site sea surface temerature of that polygon
polygondo<-subs(vorraster@data@values, sitedo, by=sitedo$id, which=sitedo$DO)

# do range

# extract do range value for each monitoring site cell
sitedorange<- raster::extract(DOrangecrop, inventorycoords, method='simple', df=TRUE)

# rename column names of sitedorange
colnames(sitedorange)<-c("id", "DOrange")

# make sure inventory points and polygons are in same order?

# substitute polygon id for monitoring site sea surface temerature of that polygon
polygondorange<-subs(vorraster@data@values, sitedorange, by=sitedorange$id, which=sitedorange$DOrange)

# gap analysis ----

# get distance to nearest monitoring site
distance<-distanceFromPoints(variability,inventorycoords)

# plot distance
plot(distance)

#define gaps = distance * ((diffmeans)+(diffranges*diffmeans))
#gaps <- setValues(distance, (getValues(distance)*getValues(variability)))

#plot gaps
#plot(gaps)

#create binary gaps
#binarygaps <- setValues(gaps, (getValues(distance)*getValues(variability)) > 6000)

#plot binary gaps
#plot(binarygaps)

#mapview
#mapview(gaps)
