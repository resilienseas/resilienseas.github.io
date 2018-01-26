
##########################################################
#CREATE VARIABILITY LAYER#
##########################################################

# Load packages ----

install.packages("rlang")
install.packages("here")
install.packages("tibble")
install.packages("sdmpredictors")

library(sdmpredictors) 
library(rlang)
library(tibble)
library(tidyverse)
library(here)

######### Package and Layers exploration

# Explore datasets in the package
list_datasets()

# Explore layers in a dataset
list_layers()

# Explore names of layers in dataset
list_layers("Bio-ORACLE")

# BO_chlomean
# BO_dissox
# BO_sstmean
# BO_salinity
# BO2_chlomean_bdmin

######### Layer manipulation

# SEA SURFACE TEMPERATURE ----

# setup datadir for sdmpredictors
dir_sdmdata <- here("data/sdmpredictors")
if (!dir.exists(dir_sdmdata)) dir.create(dir_sdmdata, recursive = T)

# load mean SST layer w/o projection
SST <- load_layers("BO_sstmean", equalarea = F, datadir=dir_sdmdata)

# project to CA Albers NAD 83 EPSG 6414, 10 kilometers
# NOTE: should setup a single raster to snap all other rasters to and feed into "to" argument projectRaster(from, to)
SST <- projectRaster(SST, crs=CRS('+init=EPSG:6414'), res=10000, method="ngb")

# Crop raster to fit the Eastern Pacific
NEpacific <- extent(-670000, 350000, -885000, 1400000) 
# names the extent - this is just an estimate ***need to coordinate extent with Rae based on Gap analysis feedback!

SSTcrop <- crop(SST, NEpacific) # crops SST layer according to NEpacific extent

# Generate a nice color ramp and plot the map
my.colors = colorRampPalette(c("#5E85B8","#EDF0C0","#C13127"))
plot(SSTcrop,col=my.colors(1000),axes=FALSE, box=FALSE)
title(cex.sub = 1.25, sub = "SST (C)")


### SST variability

sstvar=terrain(SSTcrop, opt='slope', neighbors = 8)
#get slope of SST

# Generate a nice color ramp and plot the map
my.colors = colorRampPalette(c("#5E85B8","#EDF0C0","#C13127"))
plot(sstvar,col=my.colors(1000),axes=FALSE, box=FALSE)
title(cex.sub = 1.25, sub = "SST variability(C)")

# Trying using unstack() function to isolate individual layers in variability rasterstacks

################################################################

## DISSOLVED OXYGEN

DO <- load_layers("BO_dissox", equalarea = F, datadir=dir_sdmdata)
# Load Dissolved Oxygen Data W/O projection

# project to CA Albers NAD 83 EPSG 6414, 10 kilometers
# NOTE: should setup a single raster to snap all other rasters to and feed into "to" argument projectRaster(from, to)
DO <- projectRaster(DO, crs=CRS('+init=EPSG:6414'), res=10000, method="ngb")

DOcrop <- crop(DO, NEpacific) # crops DO layer according to NEpacific extent

# Generate a nice color ramp and plot the map
my.colors = colorRampPalette(c("#5E85B8","#EDF0C0","#C13127"))
plot(DOcrop,col=my.colors(1000),axes=FALSE, box=FALSE)
title(cex.sub = 1.25, sub = "DO (C)")

### DO variability

dovar=terrain(DOcrop, opt='slope')
#get slope of DO

# Generate a nice color ramp and plot the map
my.colors = colorRampPalette(c("#5E85B8","#EDF0C0","#C13127"))
plot(dovar,col=my.colors(1000),axes=FALSE, box=FALSE)
title(cex.sub = 1.25, sub = "SST variability(C)")

###############################################################

#combine sst variability and do variability

donorm = dovar/maxValue(dovar)

sstnorm = sstvar/maxValue(sstvar)

variability = donorm+sstnorm

# Generate a nice color ramp and plot the map
my.colors = colorRampPalette(c("#5E85B8","#EDF0C0","#C13127"))
plot(variability,col=my.colors(1000),axes=FALSE, box=FALSE)
title(cex.sub = 1.25, sub = "normalized variability")

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
