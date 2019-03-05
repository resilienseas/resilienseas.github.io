---
title: "manuscript_gap_analysis"
author: "Rae"
date: "12/16/2018"
output:
  word_document: default
  pdf_document: default
  html_document:
    df_print: paged
---


```r
knitr::opts_chunk$set(echo = TRUE)
```

## Setup
Load packages, set cache, define study area


```r
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
  usdm,
  knitr)

# custom R package: oatools
devtools::load_all(here("../oatools")) # for developing
```

```
## Loading oatools
```

```
## Warning in setup_ns_exports(pkg, export_all): Objects listed as exports,
## but not present in namespace: find_gaps
```

```r
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

# extent of NE Pacific study area, for cropping rasters
ext_study <- extent(-670000, 340000, -650000, 1210000)
crs_study <- '+init=EPSG:6414'

# sea surface temperature
# devtools::load_all(here("../oatools")) # for use while developing

r_sst_min_nofill <- lyr_to_tif(
  lyr = "BO_sstmin", 
  tif = here("data/sst_min.tif"),
  crs = crs_study,
  dir_sdm_cache = dir_sdmdata,
  extent_crop   = ext_study, 
  redo=F, fill_na=FALSE)
```

```
## lyr_to_tif() messages...
```

```
##   tif found, so reading
```

```r
r_sst_min <- lyr_to_tif(
  lyr = "BO_sstmin", 
  tif = here("data/sst_min.tif"),
  crs = crs_study,
  dir_sdm_cache = dir_sdmdata,
  extent_crop   = ext_study, 
  redo=F, fill_na=TRUE, fill_window=11)
```

```
## lyr_to_tif() messages...
##   tif found, so reading
```

```r
n_na_nofill <- sum(is.na(raster::getValues(r_sst_min_nofill)))
n_na        <- sum(is.na(raster::getValues(r_sst_min)))
```

## Create SST/DO min/max rasters from bio-oracle data
Min is used here to determine a worst case scenario for aragonite saturation state, and max is used with min to determine aragonite saturation state range


```r
r_sst_max_nofill <- lyr_to_tif(
  lyr = "BO_sstmax", 
  tif = here("data/sst_max.tif"),
  crs = crs_study,
  dir_sdm_cache = dir_sdmdata,
  extent_crop   = ext_study, 
  redo=F, fill_na=FALSE)
```

```
## lyr_to_tif() messages...
```

```
##   tif found, so reading
```

```r
r_sst_max <- lyr_to_tif(
  lyr = "BO_sstmax", 
  tif = here("data/sst_min.tif"),
  crs = crs_study,
  dir_sdm_cache = dir_sdmdata,
  extent_crop   = ext_study, 
  redo=F, fill_na=TRUE, fill_window=11)
```

```
## lyr_to_tif() messages...
##   tif found, so reading
```

```r
#do min
r_do_min_nofill <- lyr_to_tif(
  lyr = "BO2_dissoxmin_bdmin", 
  tif = here("data/do_min.tif"),
  crs = crs_study,
  dir_sdm_cache = dir_sdmdata,
  extent_crop   = ext_study, 
  redo=F, fill_na=FALSE)
```

```
## lyr_to_tif() messages...
##   tif found, so reading
```

```r
r_do_min <- lyr_to_tif(
  lyr = "BO2_dissoxmin_bdmin", 
  tif = here("data/do_min.tif"),
  crs = crs_study,
  dir_sdm_cache = dir_sdmdata,
  extent_crop   = ext_study, 
  redo=F, fill_na=TRUE, fill_window=11)
```

```
## lyr_to_tif() messages...
##   tif found, so reading
```

```r
r_do_max_nofill <- lyr_to_tif(
  lyr = "BO2_dissoxmax_bdmin", 
  tif = here("data/do_max.tif"),
  crs = crs_study,
  dir_sdm_cache = dir_sdmdata,
  extent_crop   = ext_study, 
  redo=F, fill_na=FALSE)
```

```
## lyr_to_tif() messages...
##   tif found, so reading
```

```r
r_do_max <- lyr_to_tif(
  lyr = "BO2_dissoxmax_bdmin", 
  tif = here("data/do_max.tif"),
  crs = crs_study,
  dir_sdm_cache = dir_sdmdata,
  extent_crop   = ext_study, 
  redo=F, fill_na=TRUE, fill_window=11)
```

```
## lyr_to_tif() messages...
##   tif found, so reading
```

## Use Juranek 2009 model to create ocean acidification layer for the study region
No fill is used for creating a variogram as it only includes cells in the ocean. Fill is used for creating final maps, so that all coastal cells are included in the final analysis. 


```r
#juranek aragonite
j0 = 9.242*10^-1
j1 = 4.492*10^-3
j2 = 9.40 * 10^-4
jo2r = 140
jtr = 8

juranekarag <- j0 + j1 * (r_do_min-jo2r) + j2 * (r_do_min-jo2r) * (r_sst_min-jtr)

juranekarag_nofill <- j0 + j1 * (r_do_min_nofill-jo2r) + j2 * (r_do_min_nofill-jo2r) * (r_sst_min_nofill-jtr)
```

##Repeat using Alin 2012 model


```r
#alin aragonite
a0 = 1.112
a1 = 9.59*10^-3
a2 = 3.54*10^-3
a3 = 5.91*10^-4
ao2r = 138.46
atr = 10.28

alinarag <- a0 + a1 * (r_sst_min-atr) + a2 * (r_do_min-ao2r) + a3 * (r_sst_min-atr) * (r_do_min-ao2r)

alinarag_nofill <- a0 + a1 * (r_sst_min_nofill-atr) + a2 * (r_do_min_nofill-ao2r) + a3 * (r_sst_min_nofill-atr) * (r_do_min_nofill-ao2r)
```

## Compare model outputs
Here we see a zonal difference between the models, and a bit of a latitudinal difference, especially in the California Bight region

```r
modeldifference <- juranekarag-alinarag
```

## Create variograms for each of the models

```r
juranekdf <- as.data.frame(juranekarag_nofill, xy = TRUE)

juranekdf <- juranekdf %>% #remove N/A values
  mutate(layer=replace(layer, layer==-999.000, NA)) %>%
  na.omit(juranekdf)

coordinates(juranekdf)<-  ~ x + y #transform into spatial points

juranekvar <- variogram(layer~1, juranekdf)

alindf <- as.data.frame(alinarag_nofill, xy = TRUE)

alindf <- alindf %>% #remove N/A values
  mutate(layer=replace(layer, layer==-999.000, NA)) %>%
  na.omit(alindf)

coordinates(alindf)<-  ~ x + y #transform into spatial points

alinvar <- variogram(layer~1, alindf)
```

##Loess for Alin Variogram

```r
alinvar.lo <- loess(alinvar$dist ~ alinvar$gamma, alinvar)
```

##Prep Inventory
These are the same steps from our GP paper, which we use to create an OA layer in which all the cells in our study site are assigned the aragonite saturation state measured at the nearest monitoring point. 

```r
# import inventory
inventory <- read_csv(here("data/inventory.csv"))
```

```
## Parsed with column specification:
## cols(
##   .default = col_character(),
##   Longitude = col_double(),
##   StartYr = col_integer(),
##   DisCrbPmtr = col_integer(),
##   ISCrbPmtr = col_integer()
## )
```

```
## See spec(...) for full column specifications.
```

```
## Warning in rbind(names(probs), probs_f): number of columns of result is not
## a multiple of vector length (arg 1)
```

```
## Warning: 5 parsing failures.
## row # A tibble: 5 x 5 col     row col       expected               actual                   file     expected   <int> <chr>     <chr>                  <chr>                    <chr>    actual 1  1135 Longitude no trailing characters "\xca"                   '/Users… file 2  1845 StartYr   an integer             Resumed sampling in 2002 '/Users… row 3  2319 StartYr   an integer             Resumed sampling in 2002 '/Users… col 4  2410 StartYr   an integer             Resumed sampling in 2002 '/Users… expected 5  2450 StartYr   an integer             Resumed sampling in 2016 '/Users…
```

```r
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

# create voronoi polygons
vor <-voronoi(inventorycoords)
carbcompletevor <- voronoi(carbcompletecoords)
incompletevor <- voronoi(incompletecoords)
highfreqvor <- voronoi(highfreqcoords)
lowfreqvor <- voronoi(lowfreqcoords)

# rasterize polygons
vorraster<- rasterize(vor, r_sst_min, "id")
carbcompletevorraster<- rasterize(carbcompletevor, r_sst_min, "id")
incompletevorraster<- rasterize(incompletevor, r_sst_min, "id")
highfreqvorraster<- rasterize(highfreqvor, r_sst_min, "id")
lowfreqvorraster<- rasterize(lowfreqvor, r_sst_min, "id")
```

##Create OA layer
Here we assign each cell the aragonite value from the nearest monitoring site

```r
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
```

## Determine semivariance


```r
#vij = (xi-xj)^2/2

vij <- (alinarag_nofill-polygonarag)^2/2

carbcompletevij <- (alinarag_nofill-carbcompletepolygonarag)^2/2

highfreqvij <- (alinarag_nofill-highfreqpolygonarag)^2/2
```

## Use Loess to create equation


```r
predictalinvar <- predict(alinvar.lo, newdata = data.frame(distance = alinvar$dist))

#uniroot(function(x, vij) predict(alinvar.lo, newdata = data.frame(distance = x)) - vij, c(0, 1000000000000) , vij = vij)

# I had a really hard time getting this step to work... I didn't succeed, and got error messages about the f(lower) and f(upper) arguments. Maybe you could help trouble shoot this?
```

## Find Oceanographic distance
This is done as an alternative to predict/uniroot since I hit a roadblock there. I chose the Alin variogram, and created a linear model that describes the relationship between distance and semivariance, and applied it to the semivariance rasters


```r
alinmodel <- lm(alinvar$dist ~ alinvar$gamma, data = alinvar)

intercept <- coef(alinmodel)[1]
slope <- coef(alinmodel)[2]

oceanographicdistance = slope * vij + intercept
carbcompleteoceanographicdistance = slope * carbcompletevij + intercept
highfreqoceanographicdistance = slope * highfreqvij + intercept
```

## Geographic distance
Determine geographic distance from the nearest monitoring point

```r
distance<-distanceFromPoints(oceanographicdistance, inventorycoords)
carbcompletedistance<-distanceFromPoints(carbcompleteoceanographicdistance, carbcompletecoords)
highfreqdistance<-distanceFromPoints(highfreqoceanographicdistance, highfreqcoords)
```

## Temporal Variation
This step determines the range in aragonite saturation state across the region by using max values of T and DO to find the max aragonite saturation state, and min values to find the min aragonite saturation state. The difference is the range. I also normalized this step. One problem I have here is that due to the coefficients (atr and ao2r) there are some places in the study region where using the max values of do and sst gives you are smaller value for aragonite saturation state than the min values do. This is because when you use the max values, for some raster cells the a3 term has a positive component and a negative component leading to a negative sign on the a3 term, whereas when you use the min values those raster cells have two negative components, leading to a positive sign on the a3 term. So I used the abs value of the range, but I'm not sure this makes sense, and maybe there is a better way to find the range. Using the range rasters for SST/DO is a possibility, but that method yields a really different result. 


```r
alinaragmax <- a0 + a1 * (r_sst_max-atr) + a2 * (r_do_max-ao2r) + a3 * (r_sst_max-atr) * (r_do_max-ao2r)

alinaragrange = abs(alinaragmax - alinarag)

alinaragrange <- alinaragrange/maxValue(alinaragrange)

#alternatively: alinaragrange <- a0 + a1 * (r_sst_range - atr) + a2 * (r_do_range - ao2r) + a3 * (r_sst_range - atr) * (r_do_range - ao2r)
```

## Find Gaps
Here I used the euclidean distance to combine the oceanographic distance and the geographic distance. I used a weighting factor of on the oceanographic distance, which is the max value of distance / max value of oceanographic distance, so that the two parameters combine equally. I multiplied each distance by the range of the aragonite saturation state, such that locations with equal distances will be "gappier" if their aragonite saturation state has a large range. This is to account for temporal variation. 


```r
weight = maxValue(distance)/maxValue(oceanographicdistance)

gap<-alinaragrange^2*(setValues(distance, sqrt((getValues(distance)^2+(getValues(weight*oceanographicdistance)^2)))))

carbcompletegap<-alinaragrange^2*setValues(carbcompletedistance, sqrt((getValues(carbcompletedistance)^2+(getValues(weight*carbcompleteoceanographicdistance)^2))))

highfreqgap<-alinaragrange^2*setValues(highfreqdistance, sqrt((getValues(highfreqdistance)^2+(getValues(weight*highfreqoceanographicdistance)^2))))
```

## Map Gaps
Visualize gaps using tmap


```r
tmap_mode("view")
```

```
## tmap mode set to interactive viewing
```

```r
pal <- colorRampPalette(c("royalblue2", "white", "red"))

tm_shape(oceanographicdistance)+
  tm_raster(palette = pal(10), colorNA = NULL, title = "Oceanographic Distance", auto.palette.mapping = FALSE, breaks = c(0, 5000000, 10000000, 15000000, 20000000))+
  tm_layout(main.title = "Ocean Acificitation Data Gaps", main.title.size = 1, bg.color = "white", main.title.position = c("center", "top"), legend.show = TRUE, legend.position = c(0.5, 0.4), legend.title.size = 1, fontfamily = "serif", fontface = "bold")+
  tm_layout(basemaps = c('OpenStreetMap'))
```

```
## PhantomJS not found. You can install it with webshot::install_phantomjs(). If it is installed, please make sure the phantomjs executable can be found via the PATH variable.
```

```
## Warning in normalizePath(f2): path[1]="./webshot248263d68ff6.png": No such
## file or directory
```

```
## Warning in file(con, "rb"): cannot open file './webshot248263d68ff6.png':
## No such file or directory
```

```
## Error in file(con, "rb"): cannot open the connection
```