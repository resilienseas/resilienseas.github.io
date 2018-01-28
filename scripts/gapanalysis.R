# gap analysis!


# load packages ----
if (!require(pacman)) install.packages("pacman")
library(pacman)
p_load(
  tidyverse, here, 
  raster,
  sdmpredictors, dismo, 
  deldir, 
  mapview)

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
oahfocus <- read_csv(here("data/oahfocus.csv"))

#oahfocus<-subset(oahfocus, DiscCarbPmtr>1 | ISCarbPmtr > 1)

measperyr<-oahfocus$`Meas/Yr`

oahfocus<-subset(oahfocus, measperyr > 365)


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
mapview(vor)

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
polygonsst <- subs(vorraster, sitesst, by="id", which="SST")
plot(polygonsst, col=my.colors(1000))

# sst range

# extract sst range value for each monitoring site cell
sitesstrange<- raster::extract(SSTrangecrop, inventorycoords, method='simple', df=TRUE)

# rename column names of sitesstrange
colnames(sitesstrange)<-c("id", "SSTrange")

# make sure inventory points and polygons are in same order?

# substitute polygon id for monitoring site sea surface temerature of that polygon
polygonsstrange<-subs(vorraster, sitesstrange, by="id", which="SSTrange")
plot(polygonsstrange, col=my.colors(1000))

# do

# extract do value for each monitoring site cell
sitedo<- raster::extract(DOcrop, inventorycoords, method='simple', df=TRUE)

# rename column names of sitedo
colnames(sitedo)<-c("id", "DO")

# make sure inventory points and polygons are in same order?

# substitute polygon id for monitoring site sea surface temerature of that polygon
polygondo<-subs(vorraster, sitedo, by="id", which="DO")
plot(polygondo, col=my.colors(1000))

# do range

# extract do range value for each monitoring site cell
sitedorange<- raster::extract(DOrangecrop, inventorycoords, method='simple', df=TRUE)

# rename column names of sitedorange
colnames(sitedorange)<-c("id", "DOrange")

# make sure inventory points and polygons are in same order?

# substitute polygon id for monitoring site sea surface temerature of that polygon
polygondorange<-subs(vorraster, sitedorange, by="id", which="DOrange")
plot(polygondorange, col=my.colors(1000))

# spatial + temporal variation ----

# variation = (imean - amean) + (imean - amean)*(irange - arange)
# where i = cell in raster of study area and a = cell containing nearest monitoring site

# sst variation

# sst mean
sstmeandiff <- abs(SSTcrop - polygonsst)
plot(SSTcrop)

plot(polygonsst)
plot(inventorycoords, add = TRUE)

mapview(polygonsst)
mapview(inventorycoords)
plot(sstmeandiff)
mapview(sstmeandiff)

# sst range
sstrangediff <- abs(SSTrangecrop - polygonsstrange)
plot(sstrangediff)
mapview(sstrangediff)
mapview(SSTrangecrop)

# sst combine
sstvariation <- sstmeandiff+(sstmeandiff*sstrangediff)
plot(sstvariation)

# do variation

# do mean
domeandiff <- abs(DOcrop - polygondo)
plot(domeandiff)

# do range
dorangediff <- abs(DOrangecrop - polygondorange)
plot(dorangediff)

# do combine
dovariation <- domeandiff + (domeandiff*dorangediff)
plot(dovariation)

#total variation
variation <- (sstvariation*dovariation)
plot(variation)

mapview(variation)

# gap analysis ----

# get distance to nearest monitoring site
distance<-distanceFromPoints(variation,inventorycoords)

# plot distance
plot(distance)

# define gaps = distance * ((diffmeans)+(diffranges*diffmeans))
gaps <- setValues(distance, (getValues(distance)*sqrt(getValues(variation))))

# plot gaps
plot(gaps)

# create binary gaps
binarygaps <- setValues(gaps, (getValues(distance)*getValues(variation)) > 6000)

# plot binary gaps
plot(binarygaps)

# mapview
mapview(gaps)

mapview(SSTcrop)



