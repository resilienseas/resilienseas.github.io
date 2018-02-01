# gap analysis!

#test
# load packages ----
if (!require(pacman)) install.packages("pacman")
library(pacman)
p_load(
  tidyverse, here, 
  raster,
  sdmpredictors, dismo, 
  deldir, 
  mapview)

# layer manipulation ----

# setup datadir for sdmpredictors
dir_sdmdata <- here("data/sdmpredictors")
if (!dir.exists(dir_sdmdata)) dir.create(dir_sdmdata, recursive = T)

# sst

# load mean SST layer w/o projection
SST <- load_layers("BO_sstmean", equalarea = F, datadir=dir_sdmdata)

# project to CA Albers NAD 83 EPSG 6414, 10 kilometers
# NOTE: should setup a single raster to snap all other rasters to and feed into "to" argument projectRaster(from, to)
SST <- projectRaster(SST, crs=CRS('+init=EPSG:6414'), res=10000, method="ngb")

# crop raster to fit the Eastern Pacific
NEpacific <- extent(-670000, 350000, -885000, 1400000) 
# names the extent - this is just an estimate ***need to coordinate extent with Rae based on Gap analysis feedback!

SSTcrop <- crop(SST, NEpacific) # crops SST layer according to NEpacific extent

plot(SSTcrop)
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

# dissolved oxygen

DO <- load_layers("BO_dissox", equalarea = F, datadir=dir_sdmdata)
# load Dissolved Oxygen Data W/O projection

# project to CA Albers NAD 83 EPSG 6414, 10 kilometers
# NOTE: should setup a single raster to snap all other rasters to and feed into "to" argument projectRaster(from, to)
DO <- projectRaster(DO, crs=CRS('+init=EPSG:6414'), res=10000, method="ngb")

DOcrop <- crop(DO, NEpacific) # crops DO layer according to NEpacific extent

# dissolved oxygen range

# load mean SST layer w/o projection
DOrange <- load_layers("BO2_dissoxrange_bdmin", equalarea = F, datadir=dir_sdmdata)

# project to CA Albers NAD 83 EPSG 6414, 10 kilometers
# NOTE: should setup a single raster to snap all other rasters to and feed into "to" argument projectRaster(from, to)
DOrange <- projectRaster(DOrange, crs=CRS('+init=EPSG:6414'), res=10000, method="ngb")

DOrangecrop <- crop(DOrange, NEpacific) # crops SST layer according to NEpacific extent

# prep inventory----

# import inventory
oahfocus <- read_csv(here("data/oahfocus.csv"))

#oahfocus<-subset(oahfocus, DiscCarbPmtr>1 | ISCarbPmtr > 1)

#measperyr<-oahfocus$`Meas/Yr`

#oahfocus<-subset(oahfocus, measperyr > 365)

# isolate coordinate columns
coords<-cbind.data.frame(oahfocus$Longitude, oahfocus$Latitude)

# remove duplicate locations
deduped.coords<-unique(coords)

# create spatial points objects
inventorycoords <- SpatialPoints(deduped.coords, CRS("+proj=longlat +ellps=WGS84"))
inventorycoords <- spTransform(inventorycoords, CRS('+init=EPSG:6414'))

# create voronoi polygons
vor<-voronoi(inventorycoords)

# rasterize polygons
vorraster<- rasterize(vor, SSTcrop, "id")

# substitution process ----

# sst

# extract sst value for each monitoring site cell
sitesst<- raster::extract(SSTcrop, inventorycoords, method='simple', df=TRUE)

# rename column names of sitesst
colnames(sitesst)<-c("id", "SST")

# make sure inventory points and polygons are in same order?

# substitute polygon id for monitoring site sea surface temerature of that polygon
polygonsst <- subs(vorraster, sitesst, by="id", which="SST", subsWithNA=FALSE)

# sst range

# extract sst range value for each monitoring site cell
sitesstrange<- raster::extract(SSTrangecrop, inventorycoords, method='simple', df=TRUE)

# rename column names of sitesstrange
colnames(sitesstrange)<-c("id", "SSTrange")

# make sure inventory points and polygons are in same order?

# substitute polygon id for monitoring site sea surface temerature of that polygon
polygonsstrange<-subs(vorraster, sitesstrange, by="id", which="SSTrange")

# do

# extract do value for each monitoring site cell
sitedo<- raster::extract(DOcrop, inventorycoords, method='simple', df=TRUE)

# rename column names of sitedo
colnames(sitedo)<-c("id", "DO")

# make sure inventory points and polygons are in same order?

# substitute polygon id for monitoring site sea surface temerature of that polygon
polygondo<-subs(vorraster, sitedo, by="id", which="DO")

# do range

# extract do range value for each monitoring site cell
sitedorange<- raster::extract(DOrangecrop, inventorycoords, method='simple', df=TRUE)

# rename column names of sitedorange
colnames(sitedorange)<-c("id", "DOrange")

# make sure inventory points and polygons are in same order?

# substitute polygon id for monitoring site sea surface temerature of that polygon
polygondorange<-subs(vorraster, sitedorange, by="id", which="DOrange")

# spatial + temporal variation ----

# variation = (imean - amean) + (imean - amean)*(irange - arange)
# where i = cell in raster of study area and a = cell containing nearest monitoring site

# sst variation

# sst mean
sstmeandiff <- abs(SSTcrop - polygonsst)

# sst range
sstrangediff <- abs(SSTrangecrop - polygonsstrange)

# sst combine
sstvariation <- sstmeandiff+(sstmeandiff*sstrangediff)

# do variation

# do mean
domeandiff <- abs(DOcrop - polygondo)

# do range
dorangediff <- abs(DOrangecrop - polygondorange)

# do combine
dovariation <- domeandiff + (domeandiff*dorangediff)

#total variation
variation <- (sstvariation*dovariation)

# gap analysis ----

# get distance to nearest monitoring site
distance<-distanceFromPoints(variation,inventorycoords)

## investigate capability of this function to pull measurements from nearest monitoring site as well....

# plot distance
plot(distance)

# define gaps = distance * ((diffmeans)+(diffranges*diffmeans))
gaps <- setValues(distance, (getValues(distance)*(getValues(variation))))

# create binary gaps
binarygaps <- setValues(gaps, (getValues(distance)*getValues(variation)) > 6000)

# plot binary gaps
plot(binarygaps)

# mapview
mapview(gaps)

my.colors = colorRampPalette(c("#5E85B8","#C13127"))

#pal <- colorBin(my.colors, values(gaps), pretty = FALSE, na.color = "transparent")

#leaflet() %>% 
#  addTiles() %>%
#  addProviderTiles('Esri.OceanBasemap') %>% 
#  addRasterImage(gaps, colors = pal) %>% 
#  addLegend(
#    pal = binpal, values = values(gaps),
#    title = "Monitoring Gaps")



