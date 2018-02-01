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

# custom R package: oatools
devtools::load_all(here("../oatools")) # for use while developing
# library(oatools) # devtools::install_github("resilinseas/oatools")

# paths & variables ----
dir_data        <- here("data")
dir_sdmdata_old <- here("data/sdmpredictors")
dir_cache       <- here("cache")
dir_sdmdata     <- here("cache/sdmpredictors")

SST_tif <- here("data/sst_mean.tif")
DO_tif  <- here("data/do_mean.tif")

# reorganize dirs so "cache" is always local and ignored by git, vs all in "data" tracked by git & pushed to github
if (!dir.exists(dir_data))    dir.create(dir_data)
if (!dir.exists(dir_cache))   dir.create(dir_cache)
if (!dir.exists(dir_sdmdata) & dir.exists(dir_sdmdata_old))
  file.rename(dir_sdmdata_old, dir_sdmdata)
if (!dir.exists(dir_sdmdata)) dir.create(dir_sdmdata)

# explore sdmpredictors ----
# commenting out unused exploratory commands
# list_datasets() %>% View()
# list_layers()
# list_layers("Bio-ORACLE") %>% View()
# list_layers("MARSPEC") %>% View()
# list_layers("WorldClim") %>% View()
# list<- list_layers("Bio-ORACLE")

# layer manipulation ----

# extent of NE Pacific study area, for cropping rasters
ext_study <- extent(-670000, 350000, -885000, 1400000)
crs_study <- '+init=EPSG:6414'

# sea surface temperature
# devtools::load_all(here("../oatools")) # for use while developing
r_sst_mean <- lyr_to_tif(
  lyr = "BO_sstmean", 
  tif = here("data/sst_mean.tif"),
  crs = crs_study,
  dir_sdm_cache = dir_sdmdata,
  extent_crop   = ext_study) #, redo=T)
plot_raster(r_sst_mean, "SST (C)")

# test gap filling before/after:
# sum(is.na(raster::getValues(r_sst_mean))) # n before: 11,835
# "                                         # n  after: 11,250

# sst range
r_sst_range <- lyr_to_tif(
  lyr = "BO_sstrange", 
  tif = here("data/sst_range.tif"),
  crs = crs_study,
  dir_sdm_cache = dir_sdmdata,
  extent_crop   = ext_study)
plot_raster(r_sst_range, "SST range (C)")

# dissolved oxygen
r_do_mean <- lyr_to_tif(
  lyr = "BO_dissox", 
  tif = here("data/do_mean.tif"),
  crs = crs_study,
  dir_sdm_cache = dir_sdmdata,
  extent_crop   = ext_study)
plot_raster(r_dissox, "DO (C)") # TODO: DO units? not Celsius (C), usually (mg/L), or parts per million (ppm) or in micromoles (umol)

# load mean SST layer w/o projection
r_do_range <- lyr_to_tif(
  lyr = "BO2_dissoxrange_bdmin", 
  tif = here("data/do_range.tif"),
  crs = crs_study,
  dir_sdm_cache = dir_sdmdata,
  extent_crop   = ext_study)
plot_raster(r_dissox, "DO range")

# TODO: convert above into loop over layers, loading into a raster stack and outputting plots

# prep inventory----

# import inventory
oahfocus <- read_csv(here("data/oahfocus.csv"))

#oahfocus<-subset(oahfocus, DiscCarbPmtr>1 | ISCarbPmtr > 1)

measperyr<-oahfocus$`Meas/Yr`

oahfocus<-subset(oahfocus, measperyr > 365)

# isolate coordinate columns
coords <- cbind.data.frame(oahfocus$Longitude, oahfocus$Latitude)

# remove duplicate locations
deduped.coords<-unique(coords)

# create spatial points objects
inventorycoords <- SpatialPoints(deduped.coords, CRS("+proj=longlat +ellps=WGS84"))
inventorycoords <- spTransform(inventorycoords, CRS('+init=EPSG:6414'))

# check to make sure projections match

# devtools::load_all(here("../oatools")) # for use while developing
plot_raster(r_sst_mean, "SST (C) + inventory")
plot(inventorycoords, add=TRUE)

# create voronoi polygons
vor <-voronoi(inventorycoords)

# rasterize polygons
vorraster<- rasterize(vor, r_sst_mean, "id")

# plot rasterized polygons
plot_raster(vorraster, "vorraster")
mapview(vorraster)

# substitution process ----

# sst

# extract sst value for each monitoring site cell
sitesst<- raster::extract(r_sst_mean, inventorycoords, method='simple', df=TRUE)

# rename column names of sitesst
colnames(sitesst)<-c("id", "SST")

# make sure inventory points and polygons are in same order?

# substitute polygon id for monitoring site sea surface temerature of that polygon
polygonsst <- subs(vorraster, sitesst, by="id", which="SST", subsWithNA=FALSE)

fill.na<- function(polygonsst){
  if(is.na(polygonsst)){
    return(round(mean(x, na.rm=TRUE),0))
  }else{
    return(round(polygonsst),0)
  }
}

polygonsst<-focal(polygonsst, w = matrix(1, 228, 102), fun = fill.na, pad = TRUE, na.rm = FALSE)


mapview(polygonsst)

# sst range

# extract sst range value for each monitoring site cell
sitesstrange<- raster::extract(r_sst_range, inventorycoords, method='simple', df=TRUE)

# rename column names of sitesstrange
colnames(sitesstrange)<-c("id", "SSTrange")

# make sure inventory points and polygons are in same order?

# substitute polygon id for monitoring site sea surface temerature of that polygon
polygonsstrange<-subs(vorraster, sitesstrange, by="id", which="SSTrange")

# do

# extract do value for each monitoring site cell
sitedo<- raster::extract(r_do_mean, inventorycoords, method='simple', df=TRUE)

# rename column names of sitedo
colnames(sitedo)<-c("id", "DO")

# make sure inventory points and polygons are in same order?

# substitute polygon id for monitoring site sea surface temerature of that polygon
polygondo<-subs(vorraster, sitedo, by="id", which="DO")

# do range

# extract do range value for each monitoring site cell
sitedorange<- raster::extract(r_do_range, inventorycoords, method='simple', df=TRUE)

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
mapview(sstmeandiff)

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

pal <- colorBin(my.colors, values(gaps), pretty = FALSE, na.color = "transparent")

leaflet() %>% 
  addTiles() %>%
  addProviderTiles('Esri.OceanBasemap') %>% 
  addRasterImage(gaps, colors = pal) %>% 
  addLegend(
    pal = binpal, values = values(gaps),
    title = "Monitoring Gaps")



