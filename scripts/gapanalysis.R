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
  ggplot2,
  rgdal,
  gstat,
  usdm)

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
 list_layers("WorldClim") %>% View()
 list<- list_layers("Bio-ORACLE")

# layer manipulation ----

# extent of NE Pacific study area, for cropping rasters
ext_study <- extent(-670000, 340000, -650000, 1210000)
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

r_sst_min_nofill <- lyr_to_tif(
  lyr = "BO_sstmin", 
  tif = here("data/sst_min.tif"),
  crs = crs_study,
  dir_sdm_cache = dir_sdmdata,
  extent_crop   = ext_study, 
  redo=T, fill_na=FALSE)

r_sst_min <- lyr_to_tif(
  lyr = "BO_sstmin", 
  tif = here("data/sst_min.tif"),
  crs = crs_study,
  dir_sdm_cache = dir_sdmdata,
  extent_crop   = ext_study, 
  redo=T, fill_na=TRUE, fill_window=11)
plot_raster(r_sst_min, "SST min (C)")

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

#do min
r_do_min_nofill <- lyr_to_tif(
  lyr = "BO2_dissoxmin_bdmin", 
  tif = here("data/do_min.tif"),
  crs = crs_study,
  dir_sdm_cache = dir_sdmdata,
  extent_crop   = ext_study, 
  redo=T, fill_na=FALSE)

r_do_min <- lyr_to_tif(
  lyr = "BO2_dissoxmin_bdmin", 
  tif = here("data/do_min.tif"),
  crs = crs_study,
  dir_sdm_cache = dir_sdmdata,
  extent_crop   = ext_study, 
  redo=T, fill_na=TRUE, fill_window=11)
plot_raster(r_do_min_nofill, "DO min")

#juranek aragonite
j0 = 9.242*10^-1
j1 = 4.492*10^-3
j2 = 9.40 * 10^-4
jo2r = 140
jtr = 8

juranekarag <- j0 + j1 * (r_do_min-jo2r) + j2 * (r_do_min-jo2r) * (r_sst_min-jtr)
plot_raster(juranekarag, "Juranek Aragonite")

juranekarag_nofill <- j0 + j1 * (r_do_min_nofill-jo2r) + j2 * (r_do_min_nofill-jo2r) * (r_sst_min_nofill-jtr)
plot_raster(juranekarag_nofill, "Juranek Aragonite")

#alin aragonite
a0 = 1.112
a1 = 9.59*10^-3
a2 = 3.54*10^-3
a3 = 5.91*10^-4
ao2r = 138.46
atr = 10.28

alinarag <- a0 + a1 * (r_sst_min-atr) + a2 * (r_do_min-ao2r) + a3 * (r_sst_min-atr) * (r_do_min-ao2r)

alinarag_nofill <- a0 + a1 * (r_sst_min_nofill-atr) + a2 * (r_do_min_nofill-ao2r) + a3 * (r_sst_min_nofill-atr) * (r_do_min_nofill-ao2r)

modeldifference <- juranekarag-alinarag

tm_shape(juranekarag)+
  tm_raster(palette = pal(5), colorNA = NULL, title = "Test")+
  tm_layout(main.title = "Test", main.title.size = 1, bg.color = "white", main.title.position = c("center", "top"), legend.show = TRUE, legend.position = c("right", "center"), fontfamily = "serif", fontface = "bold")+ 
  tm_layout(basemaps = c('OpenStreetMap'))

juranekdf <- as.data.frame(juranekarag_nofill, xy = TRUE)

juranekdf <- juranekdf %>% #remove N/A values
  mutate(layer=replace(layer, layer==-999.000, NA)) %>%
  na.omit(juranekdf)

coordinates(juranekdf)<-  ~ x + y #transform into spatial points

juranekvar <- variogram(layer~1, juranekdf)

plot(juranekvar)

juranekfit <- fit.variogram(juranekvar, vgm(psill = 0.2, model = "Exp", range = 7e+05, nugget = 0.05))

plot(juranekvar, juranekfit)

alindf <- as.data.frame(alinarag_nofill, xy = TRUE)

alindf <- alindf %>% #remove N/A values
  mutate(layer=replace(layer, layer==-999.000, NA)) %>%
  na.omit(alindf)

coordinates(alindf)<-  ~ x + y #transform into spatial points

alinvar <- variogram(layer~1, alindf)

plot(alinvar)

alinfit <- fit.variogram(alinvar, vgm(psill = 0.06, model = "Exp", range = 6*10^5, nugget = 0.01))

plot(alinvar, alinfit)

# prep inventory----

# import inventory
inventory <- read_csv(here("data/inventory.csv"))

#remove non OAH focus entries
oahfocus <- subset(inventory, OAHFocus == "OA" | OAHFocus == "H" | OAHFocus == "OAH")

#quantify frequencies
unique(oahfocus$MeasFreq)

oahfocus$MeasFreq[oahfocus$MeasFreq =="Once"] <- 0
oahfocus$MeasFreq[oahfocus$MeasFreq == 10] <- 52560
oahfocus$MeasFreq[oahfocus$MeasFreq =="< 6 hours"] <- 1460
oahfocus$MeasFreq[oahfocus$MeasFreq == 60] <- 8760
oahfocus$MeasFreq[oahfocus$MeasFreq =="Daily"] <- 365
oahfocus$MeasFreq[oahfocus$MeasFreq ==30] <- 17520
oahfocus$MeasFreq[oahfocus$MeasFreq == 20] <- 26280
oahfocus$MeasFreq[oahfocus$MeasFreq == 15] <- 35040
oahfocus$MeasFreq[oahfocus$MeasFreq =="Quarterly"] <- 4
oahfocus$MeasFreq[oahfocus$MeasFreq =="Annual"] <- 1
oahfocus$MeasFreq[oahfocus$MeasFreq =="Monthly"] <- 12
oahfocus$MeasFreq[oahfocus$MeasFreq == 5] <- 105120
oahfocus$MeasFreq[oahfocus$MeasFreq == 6] <- 87600
oahfocus$MeasFreq[oahfocus$MeasFreq =="Semi-annual"] <- 2
oahfocus$MeasFreq[oahfocus$MeasFreq == 180] <- 2920
oahfocus$MeasFreq[oahfocus$MeasFreq == 2] <- 262800
oahfocus$MeasFreq[oahfocus$MeasFreq == 0.25] <- 2102400
oahfocus$MeasFreq[oahfocus$MeasFreq == 3] <- 175200
oahfocus$MeasFreq[oahfocus$MeasFreq == 1] <- 525600
oahfocus$MeasFreq[oahfocus$MeasFreq == 120] <- 2920
oahfocus$MeasFreq[oahfocus$MeasFreq =="Bi-weekly"] <- 26
oahfocus$MeasFreq[oahfocus$MeasFreq == 360] <- 1460
oahfocus$MeasFreq[oahfocus$MeasFreq == 720] <- 730
oahfocus$MeasFreq[oahfocus$MeasFreq =="Seasonally"] <- 1
oahfocus$MeasFreq[oahfocus$MeasFreq =="1/4 second"] <- 126144000
oahfocus$MeasFreq[oahfocus$MeasFreq =="Bi-monthly"] <- 6
oahfocus$MeasFreq[oahfocus$MeasFreq =="5  Years"] <- 0.2
oahfocus$MeasFreq[oahfocus$MeasFreq =="Bi-weekly"] <- 26
oahfocus$MeasFreq[oahfocus$MeasFreq =="Variable"] <- 0
oahfocus$MeasFreq[oahfocus$MeasFreq =="Decadal"] <- 0.1
oahfocus$MeasFreq[oahfocus$MeasFreq =="Biennial"] <- 0.5
oahfocus$MeasFreq[oahfocus$MeasFreq =="Weekly"] <- 52
oahfocus$MeasFreq[oahfocus$MeasFreq =="Triennial"] <- 0.33333
oahfocus$MeasFreq[oahfocus$MeasFreq =="Trimester"] <- 3

unique(oahfocus$MeasFreq)

#remove NA coordinates

oahfocus <- oahfocus[!is.na(oahfocus$Latitude), ]
oahfocus <- oahfocus[!is.na(oahfocus$Longitude), ]

#remove spaces and transform to numeric
gsub(" ", "", oahfocus$Latitude)
gsub(" ", "", oahfocus$Longitude)
gsub("'<ca>'", "", oahfocus$Longitude)
oahfocus$Longitude<-as.numeric(oahfocus$Longitude)
oahfocus$Latitude<-as.numeric(oahfocus$Latitude)


#subsets
carbcomplete<-subset(oahfocus, DisCrbPmtr>1 | ISCrbPmtr > 1)
incomplete <- subset(oahfocus, DisCrbPmtr<2 & ISCrbPmtr < 2 & AssetType == "Mooring" | AssetType == "Shoreside Sensor" | AssetType == "Samplesite" | AssetType == "Shoreside sensor" | AssetType == NA)

highfrequency<-subset(oahfocus, MeasFreq > 364)
highfreqcarbcomplete<-subset(oahfocus, MeasFreq > 364 & DisCrbPmtr>1 | MeasFreq > 364 & ISCrbPmtr > 1)
lowfrequency <- subset(oahfocus, MeasFreq < 365 & AssetType == "Mooring" | AssetType == "Shoreside Sensor" | AssetType == "Samplesite" | AssetType == "Shoreside sensor" | AssetType == NA)

# isolate coordinate columns
coords <- cbind.data.frame(oahfocus$Longitude, oahfocus$Latitude)
carbcompletecoords <- cbind.data.frame(carbcomplete$Longitude, carbcomplete$Latitude)
incompletecoords <- cbind.data.frame(incomplete$Longitude, incomplete$Latitude)
highfrequencycoords <- cbind.data.frame(highfrequency$Longitude, highfrequency$Latitude)
lowfrequencycoords <- cbind.data.frame(lowfrequency$Longitude, lowfrequency$Latitude)
highfreqcarbcompletecoords <- cbind.data.frame(highfreqcarbcomplete$Longitude, highfreqcarbcomplete$Latitude)

# remove duplicate locations
deduped.coords<-unique(coords)
deduped.carbcomplete <- unique(carbcompletecoords)
deduped.incomplete <- unique(incompletecoords)
deduped.highfrequency <- unique(highfrequencycoords)
deduped.lowfrequency <- unique(lowfrequencycoords)
deduped.highfreqcarbcomplete <- unique(highfreqcarbcompletecoords)

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

highfreqcarbcompletecoords <- SpatialPoints(deduped.highfreqcarbcomplete, CRS("+proj=longlat +ellps=WGS84"))
highfreqcarbcompletecoords <- spTransform(highfreqcarbcompletecoords, CRS('+init=EPSG:6414'))

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
carbcompletepolygonsst <- subs(carbcompletevorraster, carbcompletesitesst, by="id", which="SST", subsWithNA=FALSE)
highfreqpolygonsst <- subs(highfreqvorraster, highfreqsitesst, by="id", which="SST", subsWithNA=FALSE)

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
carbcompletepolygonsstrange <- subs(carbcompletevorraster, carbcompletesitesstrange, by="id", which="SSTrange", subsWithNA=FALSE)
highfreqpolygonsstrange <- subs(highfreqvorraster, highfreqsitesstrange, by="id", which="SSTrange", subsWithNA=FALSE)

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

# do range

# extract do range value for each monitoring site cell
sitedorange<- raster::extract(r_do_range, inventorycoords, method='simple', df=TRUE)
carbcompletesitedorange<- raster::extract(r_do_range, carbcompletecoords, method='simple', df=TRUE)
highfreqsitedorange<- raster::extract(r_do_range, highfreqcoords, method='simple', df=TRUE)

# rename column names of sitedorange
colnames(sitedorange)<-c("id", "DOrange")
colnames(carbcompletesitedorange)<-c("id", "DO")
colnames(highfreqsitedorange)<-c("id", "DO")

# substitute polygon id for monitoring site sea surface temerature of that polygon
polygondorange<-subs(vorraster, sitedorange, by="id", which="DOrange")
carbcompletepolygondorange<-subs(carbcompletevorraster, carbcompletesitedorange, by="id", which="DO")
highfreqpolygondorange<-subs(highfreqvorraster, highfreqsitedorange, by="id", which="DO")

# OA LAYER

# extract sst range value for each monitoring site cell
sitearag<- raster::extract(alinarag, inventorycoords, method='simple', df=TRUE)
carbcompletesitearag<- raster::extract(alinarag, carbcompletecoords, method='simple', df=TRUE)
highfreqsitearag<- raster::extract(alinarag, highfreqcoords, method='simple', df=TRUE)

# rename column names of sitesstrange
colnames(sitearag)<-c("id", "Arag")
colnames(carbcompletesitearag)<-c("id", "Arag")
colnames(highfreqsitearag)<-c("id", "Arag")

# substitute polygon id for monitoring site sea surface temerature of that polygon
polygonarag<-subs(vorraster, sitearag, by="id", which="Arag", subsWithNA=FALSE)
carbcompletepolygonarag <- subs(carbcompletevorraster, carbcompletesitearag, by="id", which="Arag", subsWithNA=FALSE)
highfreqpolygonarag <- subs(highfreqvorraster, highfreqsitearag, by="id", which="Arag", subsWithNA=FALSE)

tm_shape(polygondorange)+
  tm_raster(palette = pal(5), colorNA = NULL, title = "Test")+
  tm_layout(main.title = "Test", main.title.size = 1, bg.color = "white", main.title.position = c("center", "top"), legend.show = TRUE, legend.position = c("right", "center"), fontfamily = "serif", fontface = "bold")+ 
  tm_layout(basemaps = c('OpenStreetMap'))

#vij = (xi-xj)^2/2

vij <- (juranekarag_nofill-polygonarag)^2/2

plot(vij)

maxValue(vij)

# inverse y=(s)*(1-e^-(x/r)^2) is x = -r*ln((s-y)/s))
#psill = 0.2, model = "Exp", range = 7e+05, nugget = 0.05

#find distance

m <- (2.567094 - vij)/2.567094                                               
m <- log(m)

m <- -1 * m

distance <- 7e+05 * (m)

plot(distance)

tm_shape(distance)+
  tm_raster(palette = pal(5), colorNA = NULL, title = "Test")+
  tm_layout(main.title = "Test", main.title.size = 1, bg.color = "white", main.title.position = c("center", "top"), legend.show = TRUE, legend.position = c("right", "center"), fontfamily = "serif", fontface = "bold")+ 
  tm_layout(basemaps = c('OpenStreetMap'))


# normalization process ----
r_sst_mean_nofill_norm<-r_sst_mean_nofill/maxValue(r_sst_mean_nofill)
r_sst_range_nofill_norm<-r_sst_range_nofill/maxValue(r_sst_range_nofill)
r_do_mean_nofill_norm<-r_do_mean_nofill/maxValue(r_do_mean_nofill)
r_do_range_nofill_norm<-r_do_range_nofill/maxValue(r_do_range_nofill)

polygonsst<-polygonsst/maxValue(r_sst_mean_nofill)
carbcompletepolygonsst<-carbcompletepolygonsst/maxValue(r_sst_mean_nofill)
highfreqpolygonsst<-highfreqpolygonsst/maxValue(r_sst_mean_nofill)

polygonsstrange<-polygonsstrange/maxValue(r_sst_range_nofill)
carbcompletepolygonsstrange<-carbcompletepolygonsstrange/maxValue(r_sst_range_nofill)
highfreqpolygonsstrange<-highfreqpolygonsstrange/maxValue(r_sst_range_nofill)

polygondo<-polygondo/maxValue(r_do_mean_nofill)
carbcompletepolygondo<-carbcompletepolygondo/maxValue(r_do_mean_nofill)
highfreqpolygondo<-highfreqpolygondo/maxValue(r_do_mean_nofill)

polygondorange<-polygondorange/maxValue(r_do_range_nofill)
carbcompletepolygondorange<-carbcompletepolygondorange/maxValue(r_do_range_nofill)
highfreqpolygondorange<-highfreqpolygondorange/maxValue(r_do_range_nofill)

# spatial + temporal variation ----

# variation = (imean - amean) + (imean - amean)*(irange - arange)
# where i = cell in raster of study area and a = cell containing nearest monitoring site

# sst variation

# sst mean
sstmeandiff <- abs(r_sst_mean_nofill_norm - polygonsst)
carbcompletesstmeandiff <- abs(r_sst_mean_nofill_norm - carbcompletepolygonsst)
highfreqsstmeandiff <- abs(r_sst_mean_nofill_norm - highfreqpolygonsst)

# sst range
sstrangediff <- abs(r_sst_range_nofill_norm - polygonsstrange)
carbcompletesstrangediff <- abs(r_sst_range_nofill_norm - carbcompletepolygonsstrange)
highfreqsstrangediff <- abs(r_sst_range_nofill_norm - highfreqpolygonsstrange)

# do variation

# do mean
domeandiff <- abs(r_do_mean_nofill_norm - polygondo)
carbcompletedomeandiff <- abs(r_do_mean_nofill_norm - carbcompletepolygondo)
highfreqdomeandiff <- abs(r_do_mean_nofill_norm - highfreqpolygondo)


# do range
dorangediff <- abs(r_do_range_nofill_norm - polygondorange)
carbcompletedorangediff <- abs(r_do_range_nofill_norm - carbcompletepolygondorange)
highfreqdorangediff <- abs(r_do_range_nofill_norm - highfreqpolygondorange)

# gap analysis ----

distanceweight = 10^-6
temporalweight = 10

#oceanographic dissimilarity

spatial <-sstmeandiff^2+domeandiff^2
temporal <- sstrangediff^2+dorangediff^2
          

dissimilarity <- sqrt((sstmeandiff^2+domeandiff^2)+temporalweight*(sstrangediff^2+dorangediff^2))

carbcompletedissimilarity<- sqrt((carbcompletesstmeandiff^2+carbcompletedomeandiff^2)+temporalweight*(carbcompletesstrangediff^2+carbcompletedorangediff^2))

highfreqdissimilarity <- sqrt((highfreqsstmeandiff^2+highfreqdomeandiff^2)+temporalweight*(highfreqsstrangediff^2+highfreqdorangediff^2))

###sensitivity analysis to determine if this has a huge impact or not. if there is a future impact then thats an issue for future research. 

distance<-distanceFromPoints(dissimilarity, inventorycoords)*distanceweight
carbcompletedistance<-distanceFromPoints(carbcompletedissimilarity, carbcompletecoords)*distanceweight
highfreqdistance<-distanceFromPoints(highfreqdissimilarity, highfreqcoords)*distanceweight

gap<-setValues(distance, sqrt((getValues(distance)^2+(getValues(dissimilarity)^2))))
carbcompletegap<-setValues(carbcompletedistance, sqrt((getValues(carbcompletedistance)^2+(getValues(carbcompletedissimilarity)^2))))
highfreqgap<-setValues(highfreqdistance, sqrt((getValues(highfreqdistance)^2+(getValues(highfreqdissimilarity)^2))))

#FOR JOURNAL
tmap_mode("view")

pal <- colorRampPalette(c("royalblue2", "white", "red"))

tm_shape(domeandiff)+
  tm_raster(palette = pal(5), colorNA = NULL, title = "Difference in DO Mean <br>from Nearest Monitoring")+
  tm_layout(main.title = "Difference in DO Mean from Nearest Monitoring", main.title.size = 1, bg.color = "white", main.title.position = c("center", "top"), legend.show = TRUE, legend.position = c("right", "center"), fontfamily = "serif", fontface = "bold")+ 
  tm_layout(basemaps = c('OpenStreetMap'))

tm_shape(sstrangediff)+
  tm_raster(palette = pal(5), colorNA = NULL, title = "Difference in SST Range <br>from Nearest Monitoring")+
  tm_layout(main.title = "Difference in SST Range from Nearest Monitoring", main.title.size = 1, bg.color = "white", main.title.position = c("center", "top"), legend.show = TRUE, legend.position = c("right", "center"), fontfamily = "serif", fontface = "bold")+ 
  tm_layout(basemaps = c('OpenStreetMap'))

tm_shape(sstmeandiff)+
  tm_raster(palette = pal(5), colorNA = NULL, title = "Difference in SST Mean <br>from Nearest Monitoring")+
  tm_layout(main.title = "Difference in SST mean from nearest monitoring", main.title.size = 1, bg.color = "white", main.title.position = c("center", "top"), legend.show = TRUE, legend.position = c("right", "center"), fontfamily = "serif", fontface = "bold")+ 
  tm_layout(basemaps = c('OpenStreetMap'))


tm_shape(spatial)+
  tm_raster(palette = pal(5), colorNA = NULL, title = "Spatial Dissimilarity")+
  tm_layout(main.title = "Difference in DO rangefrom nearest monitoring", main.title.size = 1, bg.color = "white", main.title.position = c("center", "top"), legend.show = TRUE, legend.position = c("right", "center"), fontfamily = "serif", fontface = "bold")+ 
  tm_layout(basemaps = c('OpenStreetMap'))

tm_shape(temporal)+
  tm_raster(palette = pal(5), colorNA = NULL, title = "Temporal Dissimilarity")+
  tm_layout(main.title = "Difference in DO rangefrom nearest monitoring", main.title.size = 1, bg.color = "white", main.title.position = c("center", "top"), legend.show = TRUE, legend.position = c("right", "center"), fontfamily = "serif", fontface = "bold")+ 
  tm_layout(basemaps = c('OpenStreetMap'))




tm_shape(dissimilarity)+
  tm_raster(palette = pal(4), colorNA = NULL, title = "Oceanographic Dissimilarity")+
  tm_layout(main.title = "Oceanographic Dissimilarity", main.title.size = 1, bg.color = "white", main.title.position = c("center", "top"), legend.show = TRUE, legend.position = c("right", "center"), fontfamily = "serif", fontface = "bold")+ 
  tm_layout(basemaps = c('OpenStreetMap'))

tm_shape(gap)+
  tm_raster(palette = pal(10), colorNA = NULL, title = "Data Gap Severity", breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0, 1.2, 1.4, 1.6, 1.8, 2.0, 2.2, 2.4), auto.palette.mapping = FALSE)+
  tm_layout(main.title = "Ocean Acificitation Data Gaps", main.title.size = 1, bg.color = "white", main.title.position = c("center", "top"), legend.show = TRUE, legend.position = c(0.5, 0.4), legend.title.size = 1, fontfamily = "serif", fontface = "bold")+ 
  tm_layout(basemaps = c('OpenStreetMap'))+
  tm_shape(inventorycoords)+
  tm_dots(col = "black")+  
  tm_shape(vor)+
  tm_polygons()

tm_shape(carbcompletegap)+
  tm_raster(palette = pal(10), colorNA = NULL, title = "Data Gap Severity", breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0, 1.2, 1.4, 1.6, 1.8, 2.0, 2.2, 2.4), auto.palette.mapping = FALSE)+
  tm_layout(main.title = "Carbonate Complete Data Gaps", main.title.size = 1, bg.color = "white", main.title.position = c("center", "top"), legend.show = TRUE, legend.position = c(0.5, 0.4), legend.title.size = 1,fontfamily = "serif", fontface = "bold")+ 
  tm_layout(basemaps = c('OpenStreetMap'))+
  tm_shape(incompletecoords)+
  tm_dots(col = "black")

tm_shape(carbcompletegap)+
  tm_raster(palette = pal(10), colorNA = NULL, title = "Carbonate Complete Data Gaps", breaks = c(0, 0.4, 0.8, 1.2, 1.6, 2.0, 2.4), auto.palette.mapping = FALSE)+
  tm_layout(main.title = "Data Gap Severity", main.title.size = 1, bg.color = "white", main.title.position = c("center", "top"), legend.show = TRUE, legend.position = c("right", "center"), fontfamily = "serif", fontface = "bold")+ 
  tm_layout(basemaps = c('OpenStreetMap'))+
  tm_shape(carbcompletecoords)+
  tm_dots(col = "black")+  
  tm_shape(carbcompletevor)+
  tm_polygons()


tm_shape(highfreqgap)+
  tm_raster(palette = pal(10), colorNA = NULL, title = "Data Gap Severity", breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0, 1.2, 1.4, 1.6, 1.8, 2.0, 2.2, 2.4), auto.palette.mapping = FALSE)+
  tm_layout(main.title = "High Frequency Data Gaps", main.title.size = 1, bg.color = "white", main.title.position = c("center", "top"), legend.show = TRUE, legend.position = c(0.5, 0.4), legend.title.size = 1, fontfamily = "serif", fontface = "bold")+ 
  tm_layout(basemaps = c('OpenStreetMap'))+
  tm_shape(lowfreqcoords)+
  tm_dots(col = "black")













#FOR CLIENTS
severegaps <- setValues(distance, sqrt((getValues(distance)^2+(getValues(dissimilarity)^2)))) > quantile(gap, (.999))
highprioritygaps <- setValues(distance, sqrt((getValues(distance)^2+(getValues(dissimilarity)^2)))) > quantile(gap, (.99))
lowprioritygaps<-setValues(distance, sqrt((getValues(distance)^2+(getValues(dissimilarity)^2)))) > quantile(gap, (.75))
finalgaps<- severegaps+lowprioritygaps+highprioritygaps

carbcompleteseveregaps <- setValues(carbcompletedistance, sqrt((getValues(carbcompletedistance)^2+(getValues(carbcompletedissimilarity)^2)))) > quantile(carbcompletegap, (.999))
carbcompletehighprioritygaps<- setValues(carbcompletedistance, sqrt((getValues(carbcompletedistance)^2+(getValues(carbcompletedissimilarity)^2)))) > quantile(carbcompletegap, (.99))
carbcompletelowprioritygaps<- setValues(carbcompletedistance, sqrt((getValues(carbcompletedistance)^2+(getValues(carbcompletedissimilarity)^2)))) > quantile(carbcompletegap, (.75))
carbcompletefinalgaps<- carbcompleteseveregaps + carbcompletelowprioritygaps+carbcompletehighprioritygaps

highfreqseveregaps <- setValues(highfreqdistance, sqrt((getValues(highfreqdistance)^2+(getValues(highfreqdissimilarity)^2)))) > quantile(highfreqgap, (.999))
highfreqhighprioritygaps<-setValues(highfreqdistance, sqrt((getValues(highfreqdistance)^2+(getValues(highfreqdissimilarity)^2)))) > quantile(highfreqgap, (.99))
highfreqlowprioritygaps<-setValues(highfreqdistance, sqrt((getValues(highfreqdistance)^2+(getValues(highfreqdissimilarity)^2)))) > quantile(highfreqgap, (.75))
highfreqfinalgaps<- highfreqseveregaps+highfreqlowprioritygaps+highfreqhighprioritygaps



#save rasters for gap shiny app
writeRaster(finalgaps, filename = "/Users/rttaylorburnscom/github/resilienseas.github.io/fullgaps", format="GTiff", overwrite=TRUE)

writeRaster(carbcompletefinalgaps, filename = "/Users/rttaylorburnscom/github/resilienseas.github.io/carbcomplete_gaps", format="GTiff", overwrite=TRUE)

writeRaster(highfreqfinalgaps, filename = "/Users/rttaylorburnscom/github/resilienseas.github.io/highfreq_gaps", format="GTiff", overwrite=TRUE)


#test clip of raster to coast shapefile
poly_coast<- readOGR(dsn=path.expand("Export_Output_2"), layer="Export_Output_2")
poly_coast <- spTransform(poly_coast, crs(gaps))
gaps_clipped <- mask(gaps, poly_coast, inverse = TRUE,progress='text')

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

tmap_mode("view")

pal <- colorRampPalette(c("slateblue4", "slateblue", "plum", "orangered2"))

tm_shape(finalgaps)+
  tm_raster(palette = pal(3))

tm_shape(inventorycoords)+
  tm_dots(col = "black")+
  tm_shape(highfreqcoords)+
  tm_dots(col = "black")+
  tm_shape(highfreqcarbcompletecoords)+
  tm_dots(col = "black")+
  tm_layout(basemaps = c('OpenStreetMap'), basemaps.alpha = 1)

tm_shape(finalgaps)+
  tm_raster(palette = pal(4), colorNA = NULL, breaks = c(-0.1, 0.1, 1.1, 2.1, 4.1), title = "Ocean Acidification Data Gaps", labels = c("Sufficient Data", "Low Priority Gaps", "High Priority Gaps", "Severe Gaps"))+
  tm_layout(main.title = "Data Gap Severity", main.title.size = 1, bg.color = "white", main.title.position = c("center", "top"), legend.show = TRUE, legend.position = c("right", "center"), fontfamily = "serif", fontface = "bold")+ 
  tm_layout(basemaps = c('OpenStreetMap'))+
  tm_legend()+
  tm_shape(inventorycoords)+
  tm_dots(col = "black")

tm_shape(carbcompletefinalgaps)+
  tm_raster(palette = pal(4), colorNA = NULL, breaks = c(-0.5, 0.5, 1.5, 2.5, 3.5), title = "Aragonite Measurement Data Gaps", labels = c("Sufficient Data", "Low Priority Gaps", "High Priority Gaps", "Severe Gaps"))+
  tm_layout(main.title = "Data Gap Severity", main.title.size = 1, bg.color = "white", main.title.position = c("center", "top"), legend.show = TRUE, legend.position = c("right", "center"), fontfamily = "serif", fontface = "bold")+
  tm_layout(basemaps = c('OpenStreetMap'))+
  tm_shape(incompletecoords)+
  tm_dots(col = "black")

tm_shape(highfreqfinalgaps)+
  tm_raster(palette = pal(4), colorNA = NULL, breaks = c(-0.5, 0.5, 1.5, 2.5, 3.5), title = "High Frequency Data Gaps", labels = c("Sufficient Data", "Low Priority Gaps", "High Priority Gaps", "Severe Gaps"))+
  tm_layout(main.title = "Data Gap Severity", main.title.size = 1, bg.color = "white", main.title.position = c("center", "top"), legend.show = TRUE, legend.position = c("right", "center"), fontfamily = "serif", fontface = "bold")+
  tm_layout(basemaps = c('OpenStreetMap'))+
tm_shape(highfreqcoords)+
  tm_dots(col = "black")

#tmap_mode("view")
#last_map()

#tmap_mode("plot")

###### saving maps for use in storymap

inventory_map <- tm_shape(inventorycoords)+
  tm_dots(col = "black")+
  tm_layout(basemaps = c('OpenStreetMap'), basemaps.alpha = 1)

save_tmap(inventory_map, filename="visualizations/inventory_map.png", width = 1920, height = 1080)
# even though the mode is set to view, this function saves the "plot" view version of the map

save_tmap(inventory_map, "inventory_map.html")
# saves inventory_map as an html file -- still trying to figure out if we can integrate this w/ arcgis online

# convert inventory coords to spdf object
inventorycoords_spdf <- SpatialPointsDataFrame(inventorycoords, deduped.coords)
#dir.create("shapefiles") # create a directory for inventorycoords shapefiles
writeOGR(inventorycoords_spdf, dsn="shapefiles", layer="inventorycoords_spdf", driver="ESRI Shapefile") # export inventorycoords as shapefile

highfreqcoords_spdf <- SpatialPointsDataFrame(highfreqcoords, deduped.highfrequency)
writeOGR(highfreqcoords_spdf, dsn="shapefiles", layer="highfreqcoords_spdf", driver="ESRI Shapefile") # export highfreqcoords as shapefile

highfreqcarbcompletecoords_spdf <- SpatialPointsDataFrame(highfreqcarbcompletecoords, deduped.highfreqcarbcomplete)
writeOGR(highfreqcarbcompletecoords_spdf, dsn="shapefiles", layer="highfreqcarbcompletecoords_spdf", driver="ESRI Shapefile") # export highfreqcoords as shapefile


#data viz final project

pal <- colorRampPalette(c("white", "red"))

poly_coast<- readOGR(dsn=path.expand("Export_Output_2"), layer="Export_Output_2")
poly_coast <- spTransform(poly_coast, crs(distance))

distance_clipped <- mask(distance, dissimilarity,progress='text')

distance90 <- rasterToPolygons(distance_clipped, fun = function(distance_clipped){distance_clipped > quantile((distance_clipped), (.90))})

distance80 <- rasterToPolygons(distance_clipped, fun = function(distance_clipped){distance_clipped > quantile((distance_clipped), (.80)) & distance_clipped < quantile((distance_clipped), (.90))})

distance70 <- rasterToPolygons(distance_clipped, fun = function(distance_clipped){distance_clipped > quantile((distance_clipped), (.70)) & distance_clipped < quantile((distance_clipped), (.80))})


distance60 <- rasterToPolygons(distance_clipped, fun = function(distance_clipped){distance_clipped > quantile((distance_clipped), (.60)) & distance_clipped < quantile((distance_clipped), (.70))})


distance50 <- rasterToPolygons(distance_clipped, fun = function(distance_clipped){distance_clipped > quantile((distance_clipped), (.50)) & distance_clipped < quantile((distance_clipped), (.60))})


distance40 <- rasterToPolygons(distance_clipped, fun = function(distance_clipped){distance_clipped > quantile((distance_clipped), (.40)) & distance_clipped < quantile((distance_clipped), (.50))})

distance30 <- rasterToPolygons(distance_clipped, fun = function(distance_clipped){distance_clipped > quantile((distance_clipped), (.30)) & distance_clipped < quantile((distance_clipped), (.40))})

distance20 <- rasterToPolygons(distance_clipped, fun = function(distance_clipped){distance_clipped > quantile((distance_clipped), (.20)) & distance_clipped < quantile((distance_clipped), (.30))})


distance10 <- rasterToPolygons(distance_clipped, fun = function(distance_clipped){distance_clipped > quantile((distance_clipped), (.10)) & distance_clipped < quantile((distance_clipped), (.20))})

distance0 <- rasterToPolygons(distance_clipped, fun = function(distance_clipped){distance_clipped < quantile((distance_clipped), (.10))})


distance45 <- rasterToPolygons(distance_clipped, fun = function(distance_clipped){distance_clipped > quantile((distance_clipped), (.45)) & distance_clipped < quantile((distance_clipped), (.50))})


distance40 <- rasterToPolygons(distance_clipped, fun = function(distance_clipped){distance_clipped > quantile((distance_clipped), (.40)) & distance_clipped < quantile((distance_clipped), (.45))})


distance35 <- rasterToPolygons(distance_clipped, fun = function(distance_clipped){distance_clipped > quantile((distance_clipped), (.35)) & distance_clipped < quantile((distance_clipped), (.40))})

distance30 <- rasterToPolygons(distance_clipped, fun = function(distance_clipped){distance_clipped > quantile((distance_clipped), (.30)) & distance_clipped < quantile((distance_clipped), (.35))})

distance25 <- rasterToPolygons(distance_clipped, fun = function(distance_clipped){distance_clipped > quantile((distance_clipped), (.25)) & distance_clipped < quantile((distance_clipped), (.30))})

distance20 <- rasterToPolygons(distance_clipped, fun = function(distance_clipped){distance_clipped > quantile((distance_clipped), (.20)) & distance_clipped < quantile((distance_clipped), (.25))})

distance15 <- rasterToPolygons(distance_clipped, fun = function(distance_clipped){distance_clipped > quantile((distance_clipped), (.15)) & distance_clipped < quantile((distance_clipped), (.20))})

distance10 <- rasterToPolygons(distance_clipped, fun = function(distance_clipped){distance_clipped > quantile((distance_clipped), (.10)) & distance_clipped < quantile((distance_clipped), (.15))})

distance5 <- rasterToPolygons(distance_clipped, fun = function(distance_clipped){distance_clipped > quantile((distance_clipped), (.05)) & distance_clipped < quantile((distance_clipped), (.10))})


tm_shape(dissimilarity)+
  tm_raster(palette = pal(1000), legend.show = TRUE, breaks = c(0, 200, 600, 800, 1000), title = "Data Gaps", labels = c("Sufficient Data", "Low Priority Gaps", "High Priority Gaps", "Severe Gaps"))+
  tm_shape(distance90)+
  tm_fill(col = "white", alpha = 0.0)+
  tm_shape(distance80)+
  tm_fill(col = "white", alpha = 0.10)+
  tm_shape(distance70)+
  tm_fill(col = "white", alpha = 0.20)+
  tm_shape(distance60)+
  tm_fill(col = "white", alpha = 0.30)+
  tm_shape(distance50)+
  tm_fill(col = "white", alpha = 0.40)+
  tm_shape(distance40)+
  tm_fill(col = "white", alpha = 0.50)+
  tm_shape(distance30)+
  tm_fill(col = "white", alpha = 0.60)+
  tm_shape(distance20)+
  tm_fill(col = "white", alpha = 0.70)+
  tm_shape(distance10)+
  tm_fill(col = "white", alpha = 0.80)+
  tm_shape(distance0)+
  tm_fill(col = "white", alpha = 0.90)+ 
  tm_layout(basemaps = c('OpenStreetMap'))


tm_shape(distance90)+
  tm_fill(col = "white", alpha = 0.0)+
  tm_shape(distance80)+
  tm_fill(col = "white", alpha = 0.10)+
  tm_shape(distance70)+
  tm_fill(col = "white", alpha = 0.20)+
  tm_shape(distance60)+
  tm_fill(col = "white", alpha = 0.30)+
  tm_shape(distance50)+
  tm_fill(col = "white", alpha = 0.40)+
  tm_shape(distance40)+
  tm_fill(col = "white", alpha = 0.50)+
  tm_shape(distance30)+
  tm_fill(col = "white", alpha = 0.60)+
  tm_shape(distance20)+
  tm_fill(col = "white", alpha = 0.70)+
  tm_shape(distance10)+
  tm_fill(col = "white", alpha = 0.80)+
  tm_shape(distance0)+
  tm_fill(col = "white", alpha = 0.90)+
  tm_layout(basemaps = c('OpenStreetMap'))+
  tm_shape(inventorycoords)+
  tm_dots(col = "black", size = 0.001)

pal <- colorRampPalette(c("white", "blue"))


tm_shape(dissimilarity)+
  tm_raster(palette = pal(100), legend.show = TRUE, breaks = c(0, 200, 400, 600, 800), title = "Oceanographic Variability", labels = c("Low", "Moderate", "High", "Extreme"))+
  tm_layout(basemaps = c('OpenStreetMap'))

tm_shape(dissimilarity)+
  tm_raster(palette = pal(100), title = "Oceanographic Variability")+
  tm_layout(basemaps = c('OpenStreetMap'))



###sensitivity analysis

distanceweight = c(0.2*10^-6, 0.4*10^-6, 0.6*10^-6, 0.8*10^-6, 10^-6, 1.2*10^-6, 1.4*10^-6, 1.6*10^-6, 1.8*10^-6, 2*10^-6)
temporalweight = c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20)


rastersensitivity <- list()

for(i in 1:length(distanceweight)){
  for(j in 1:length(temporalweight)){
    dissimilarity <- sqrt((sstmeandiff^2+domeandiff^2)+temporalweight[j]*(sstrangediff^2+dorangediff^2))
    distance<-distanceFromPoints(dissimilarity, inventorycoords)*distanceweight[i]
    gap<-setValues(distance, sqrt((getValues(distance)^2+(getValues(dissimilarity)^2))))
    severegaps <- setValues(distance, sqrt((getValues(distance)^2+(getValues(dissimilarity)^2)))) > quantile(gap, (.25))
    name = paste(temporalweight[j], distanceweight[i], sep = "_")
    rastersensitivity[[name]] = severegaps
  }
}

sensitivitystack <- stack(rastersensitivity[[1]])
for(i in 2:length(rastersensitivity)) sensitivitystack <- addLayer(sensitivitystack, rastersensitivity[[i]])


sum <- sum(sensitivitystack)
plot(sum)
freq(sum)
