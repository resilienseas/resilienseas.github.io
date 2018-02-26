# gap analysis

# load packages ----
if (!require(pacman)) install.packages("pacman")
library(pacman)
p_load(
  tidyverse, here, glue,
  raster,
  sdmpredictors, dismo, 
  deldir, 
  mapview,
  tmap,
  ggplot2)

# custom R package: oatools
devtools::load_all(here("../oatools")) # for developing
#library(oatools) # devtools::install_github("resilinseas/oatools") # for eventual production

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

r_sst_mean_nofill <- lyr_to_tif(
  lyr = "BO_sstmean", 
  tif = here("data/sst_mean.tif"),
  crs = crs_study,
  dir_sdm_cache = dir_sdmdata,
  extent_crop   = ext_study, 
  redo=T, fill_na=FALSE)

r_sst_mean <- lyr_to_tif(
  lyr = "BO_sstmean", 
  tif = here("data/sst_mean.tif"),
  crs = crs_study,
  dir_sdm_cache = dir_sdmdata,
  extent_crop   = ext_study, 
  redo=T, fill_na=TRUE, fill_window=11)

n_na_nofill <- sum(is.na(raster::getValues(r_sst_mean_nofill)))
n_na        <- sum(is.na(raster::getValues(r_sst_mean)))

# test gap filling before/after:
# sum(is.na(raster::getValues(r_sst_mean))) # n before: 11,835
# "                                         # n  after: 11,250

# sst range
r_sst_range_nofill <- lyr_to_tif(
  lyr = "BO_sstrange", 
  tif = here("data/sst_range.tif"),
  crs = crs_study,
  dir_sdm_cache = dir_sdmdata,
  extent_crop   = ext_study, 
  redo=T, fill_na=FALSE)

r_sst_range <- lyr_to_tif(
  lyr = "BO_sstrange", 
  tif = here("data/sst_range.tif"),
  crs = crs_study,
  dir_sdm_cache = dir_sdmdata,
  extent_crop   = ext_study, 
  redo=T, fill_na=TRUE, fill_window=11)
plot_raster(r_sst_range, "SST range (C)")

# dissolved oxygen
r_do_mean_nofill <- lyr_to_tif(
  lyr = "BO_dissox", 
  tif = here("data/do_mean.tif"),
  crs = crs_study,
  dir_sdm_cache = dir_sdmdata,
  extent_crop   = ext_study, 
  redo=T, fill_na=FALSE)

r_do_mean <- lyr_to_tif(
  lyr = "BO_dissox", 
  tif = here("data/do_mean.tif"),
  crs = crs_study,
  dir_sdm_cache = dir_sdmdata,
  extent_crop   = ext_study, 
  redo=T, fill_na=TRUE, fill_window=11)
plot_raster(r_do_mean, "DO") 

#do range
r_do_range_nofill <- lyr_to_tif(
  lyr = "BO2_dissoxrange_bdmin", 
  tif = here("data/do_range.tif"),
  crs = crs_study,
  dir_sdm_cache = dir_sdmdata,
  extent_crop   = ext_study, 
  redo=T, fill_na=FALSE)

r_do_range <- lyr_to_tif(
  lyr = "BO2_dissoxrange_bdmin", 
  tif = here("data/do_range.tif"),
  crs = crs_study,
  dir_sdm_cache = dir_sdmdata,
  extent_crop   = ext_study, 
  redo=T, fill_na=TRUE, fill_window=11)
plot_raster(r_do_range, "DO range")

# TODO: convert above into loop over layers, loading into a raster stack and outputting plots

# prep inventory----

# import inventory
oahfocus <- read_csv(here("data/oahfocus.csv"))

carbcomplete<-subset(oahfocus, DiscCarbPmtr>1 | ISCarbPmtr > 1)

incomplete <- subset(oahfocus, DiscCarbPmtr<2 & ISCarbPmtr < 2)

measperyr<-oahfocus$`Meas/Yr`

highfrequency<-subset(oahfocus, measperyr > 364)

lowfrequency <- subset(oahfocus, measperyr < 365)

# isolate coordinate columns
coords <- cbind.data.frame(oahfocus$Longitude, oahfocus$Latitude)

carbcompletecoords <- cbind.data.frame(carbcomplete$Longitude, carbcomplete$Latitude)

incompletecoords <- cbind.data.frame(incomplete$Longitude, incomplete$Latitude)

highfrequencycoords <- cbind.data.frame(highfrequency$Longitude, highfrequency$Latitude)

lowfrequencycoords <- cbind.data.frame(lowfrequency$Longitude, lowfrequency$Latitude)

# remove duplicate locations
deduped.coords<-unique(coords)

deduped.carbcomplete <- unique(carbcompletecoords)

deduped.incomplete <- unique(incompletecoords)

deduped.highfrequency <- unique(highfrequencycoords)

deduped.lowfrequency <- unique(lowfrequencycoords)

# create spatial points objects
inventorycoords <- SpatialPoints(deduped.coords, CRS("+proj=longlat +ellps=WGS84"))
inventorycoords <- spTransform(inventorycoords, CRS('+init=EPSG:6414'))

carbcompletecoords <- SpatialPoints(deduped.carbcomplete, CRS("+proj=longlat +ellps=WGS84"))
carbcompletecoords <- spTransform(carbcompletecoords, CRS('+init=EPSG:6414'))

incompletecoords <- SpatialPoints(deduped.incomplete, CRS("+proj=longlat +ellps=WGS84"))
incompletecoords <- spTransform(incompletecoords, CRS('+init=EPSG:6414'))

highfreqcoords <- SpatialPoints(deduped.highfrequency, CRS("+proj=longlat +ellps=WGS84"))
highfreqcoords <- spTransform(highfreqcoords, CRS('+init=EPSG:6414'))

lowfreqcoords <- SpatialPoints(deduped.lowfrequency, CRS("+proj=longlat +ellps=WGS84"))
lowfreqcoords <- spTransform(lowfreqcoords, CRS('+init=EPSG:6414'))

# check to make sure projections match

# devtools::load_all(here("../oatools")) # for use while developing
plot_raster(r_sst_mean, "SST (C) + inventory")
plot(inventorycoords, add=TRUE)

# create voronoi polygons
vor <-voronoi(inventorycoords)

carbcompletevor <- voronoi(carbcompletecoords)
incompletevor <- voronoi(incompletecoords)
highfreqvor <- voronoi(highfreqcoords)
lowfreqvor <- voronoi(lowfreqcoords)


# rasterize polygons
vorraster<- rasterize(vor, r_sst_mean, "id")
carbcompletevorraster<- rasterize(carbcompletevor, r_sst_mean, "id")
incompletevorraster<- rasterize(incompletevor, r_sst_mean, "id")
highfreqvorraster<- rasterize(highfreqvor, r_sst_mean, "id")
lowfreqvorraster<- rasterize(lowfreqvor, r_sst_mean, "id")


# plot rasterized polygons
plot_raster(vorraster, "vorraster")
mapview(vorraster)

# substitution process ----

# sst

# extract sst value for each monitoring site cell
sitesst<- raster::extract(r_sst_mean, inventorycoords, method='simple', df=TRUE)
carbcompletesitesst<- raster::extract(r_sst_mean, carbcompletecoords, method='simple', df=TRUE)
highfreqsitesst<- raster::extract(r_sst_mean, highfreqcoords, method='simple', df=TRUE)

# rename column names of sitesst
colnames(sitesst)<-c("id", "SST")
colnames(carbcompletesitesst)<-c("id", "SST")
colnames(highfreqsitesst)<-c("id", "SST")

# substitute polygon id for monitoring site sea surface temerature of that polygon
polygonsst <- subs(vorraster, sitesst, by="id", which="SST", subsWithNA=FALSE)
carbcompletepolygonsst <- subs(carbcompletevorraster, sitesst, by="id", which="SST", subsWithNA=FALSE)
highfreqpolygonsst <- subs(highfreqvorraster, sitesst, by="id", which="SST", subsWithNA=FALSE)

# sst range

# extract sst range value for each monitoring site cell
sitesstrange<- raster::extract(r_sst_range, inventorycoords, method='simple', df=TRUE)
carbcompletesitesstrange<- raster::extract(r_sst_range, carbcompletecoords, method='simple', df=TRUE)
highfreqsitesstrange<- raster::extract(r_sst_range, highfreqcoords, method='simple', df=TRUE)

# rename column names of sitesstrange
colnames(sitesstrange)<-c("id", "SSTrange")
colnames(carbcompletesitesstrange)<-c("id", "SSTrange")
colnames(highfreqsitesstrange)<-c("id", "SSTrange")

# substitute polygon id for monitoring site sea surface temerature of that polygon
polygonsstrange<-subs(vorraster, sitesstrange, by="id", which="SSTrange", subsWithNA=FALSE)
carbcompletepolygonsstrange <- subs(carbcompletevorraster, sitesstrange, by="id", which="SSTrange", subsWithNA=FALSE)
highfreqpolygonsstrange <- subs(highfreqvorraster, sitesstrange, by="id", which="SSTrange", subsWithNA=FALSE)

# do

# extract do value for each monitoring site cell
sitedo<- raster::extract(r_do_mean, inventorycoords, method='simple', df=TRUE)
carbcompletesitedo<- raster::extract(r_do_mean, carbcompletecoords, method='simple', df=TRUE)
highfreqsitedo<- raster::extract(r_do_mean, highfreqcoords, method='simple', df=TRUE)

# rename column names of sitedo
colnames(sitedo)<-c("id", "DO")
colnames(carbcompletesitedo)<-c("id", "DO")
colnames(highfreqsitedo)<-c("id", "DO")

# substitute polygon id for monitoring site sea surface temerature of that polygon
polygondo<-subs(vorraster, sitedo, by="id", which="DO")
carbcompletepolygondo<-subs(carbcompletevorraster, carbcompletesitedo, by="id", which="DO")
highfreqpolygondo<-subs(highfreqvorraster, highfreqsitedo, by="id", which="DO")


#####   DID I DO THIS RIGHT? 

# do range

# extract do range value for each monitoring site cell
sitedorange<- raster::extract(r_do_range, inventorycoords, method='simple', df=TRUE)

# rename column names of sitedorange
colnames(sitedorange)<-c("id", "DOrange")

# substitute polygon id for monitoring site sea surface temerature of that polygon
polygondorange<-subs(vorraster, sitedorange, by="id", which="DOrange")

mapview(polygondorange)

# spatial + temporal variation ----

# variation = (imean - amean) + (imean - amean)*(irange - arange)
# where i = cell in raster of study area and a = cell containing nearest monitoring site

# sst variation

# sst mean
sstmeandiff <- abs(r_sst_mean_nofill - polygonsst)
mapview(sstmeandiff)


# sst range
sstrangediff <- abs(r_sst_range_nofill - polygonsstrange)
mapview(sstrangediff)

# sst combine
sstvariation <- sstmeandiff+(sstmeandiff*sstrangediff)

# do variation

# do mean
domeandiff <- abs(r_do_mean_nofill - polygondo)
mapview(domeandiff)

# do range
dorangediff <- abs(r_do_range_nofill - polygondorange)
mapview(dorangediff)

# do combine
dovariation <- domeandiff + (domeandiff*dorangediff)

#total variation
variation <- (sstvariation*dovariation)

# gap analysis ----

# get distance to nearest monitoring site
distance<-distanceFromPoints(variation,inventorycoords)

## investigate capability of this function to pull measurements from nearest monitoring site as well....

# plot distance
#plot(distance)

# define gaps = distance * ((diffmeans)+(diffranges*diffmeans))
gaps <- setValues(distance, (getValues(distance)*(getValues(variation))))

#test clip of raster to coast shapefile
poly_coast<- readOGR(dsn=path.expand("/Users/Madi/Documents/UCSB Bren/ResilienSeas/Export_Output_2"), layer="Export_Output_2")
poly_coast <- spTransform(poly_coast, crs(gaps))
gaps_clipped <- mask(gaps, poly_coast, inverse = TRUE,progress='text')

# create binary gaps
severegaps <- setValues(gaps, (getValues(distance)*getValues(variation)) > quantile(gaps, (.99)))


lowprioritygaps<-setValues(gaps, (getValues(distance)*getValues(variation)) > quantile(gaps, (.75)))

finalgaps<- severegaps+lowprioritygaps

plot(finalgaps)

# plot binary gaps
mapview(severegaps)

# mapview
#mapview(gaps)


#leaflet ----
#my.colors = colorRampPalette(c("#5E85B8","#C13127"))

#colors including specification for N/A values
#pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(gaps),na.color = "transparent")

#pal <- colorRampPalette(c("#0C2C84", "#41B6C4", "#FFFFCC"))


#leaflet() %>% 
#  addTiles() %>%
#  addProviderTiles('Esri.OceanBasemap') %>% 
#  addRasterImage(gaps, colors = pal) %>% 
#  addLegend(
#    pal = pal, values = values(gaps),
#    title = "Monitoring Gaps")

#tmap----

pal <- colorRampPalette(c("green", "yellow", "red"))


tm_shape(finalgaps)+
  tm_raster(palette = pal(3), colorNA = NULL)+
  tm_layout(main.title = "High Frequency Data Gap Severity", main.title.size = 1, bg.color = "white", main.title.position = c("center", "top"), legend.show = TRUE, legend.position = c("right", "center"), fontfamily = "serif", fontface = "bold")+
tm_shape(lowfreqcoords)+
  tm_dots(col = "black")+
tm_shape(highfreqcoords)+
  tm_dots(col = "gray")


tmap_mode("view")
last_map()
