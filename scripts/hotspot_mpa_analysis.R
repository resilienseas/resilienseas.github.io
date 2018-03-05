#Ocean Acidifcation Hotspot Interpolation and MPA Impact Analysis

#load packages----
if (!require(pacman)) install.packages("pacman")
library(pacman)
p_load(
  tidyverse, here, 
  sf, gstat, raster, 
  mapview)


#prepare dataset----
#load data and re-name columns
aragonite_data <- read_csv(here("data/WCOAC_2013_test.csv"))
colnames(aragonite_data) <- c("Date", "Time", "Lat", "Long", "Pressure", "OmegaAr")

#remove N/A values and filter for surface level obeservations (working on this part still)
aragonite_data <- aragonite_data %>%
  mutate(OmegaAr=replace(OmegaAr, OmegaAr==-999.000, NA)) %>%
  na.omit(aragonite_data) #%>% 
  #filter(aragonite_data, Pressure > 5)
  
#select for pressure <5 db to remove all observations made at lower than surface depth
aragonite_data<- aragonite_data[aragonite_data[, 5]<5,]

#transform to spatial data set
coordinates(aragonite_data)<-  ~ Long + Lat

#remove observations taken at same coordinate point
zd<-zerodist(aragonite_data)
aragonite_data<-aragonite_data[-zd[,2],]


#interpolation via simple kriging----
#look for anisotropy and create variogram of aragonite values
aragonite_var<-variogram(OmegaAr ~1, data=aragonite_data, alpha=c(0, 45, 90, 135))
plot(aragonite_var)

#fit a model to the values based on estimated nugget, sill, and range, and anisotropy
aragonite_fit<-fit.variogram(aragonite_var,model=vgm(nugget=0.2,psill=1,range=2,model="Exp", anis=c(0, 0.3)))

#get extent of cruise observations and increase ROI by one degree in each direction
extent <- bbox(aragonite_data)
long<-seq(extent[1,1],extent[1,2],length=388)
lat<-seq(extent[2,1],extent[2,2],length=1000)

#create grid for interpolation surface
aragonite_grid<-expand.grid(long,lat)
colnames(aragonite_grid)<- c("long", "lat")
coordinates(aragonite_grid) <- ~ long + lat
gridded(aragonite_grid)=TRUE

#run kriging on interpolation grid, based on best fit model
aragonitekrige<-krige(OmegaAr ~ 1, aragonite_data, newdata=aragonite_grid, model=aragonite_fit)


#create continuous aragonite raster----
#transform krige object to raster, set CRS and re-project to California Teale Albers Equal Area
aragonite_raster<-raster(aragonitekrige, layer=1, values=TRUE)
projection(aragonite_raster) <- CRS("+proj=longlat +datum=WGS84")
aragonite_raster_proj <- projectRaster(aragonite_raster, crs=CRS('+init=EPSG:6414'),method="ngb")


#create hotspot threshold mask----
thresholds <- c(0,1,1, 1,1.7,1.7, 1.7,2,2, 2,10,NA)
thresholdsmatrix <- matrix(thresholds, ncol=3, byrow=TRUE)
hotspotmask <- reclassify(aragonite_raster_proj, thresholdsmatrix)


#coast and mpa shapfiles----
#load west coast shapefile and re-project to EPSG 6414 (Cal Teale Albers)
westcoast <- st_read(dsn = "/Users/Madi/Documents/UCSB Bren/ResilienSeas/Export_Output_2")
westcoast_proj <- st_transform(westcoast, "+init=epsg:6414")
westcoast_sp <- as(westcoast_proj, "Spatial")

#load MPA shapefile and re-project to EPSG 6414 (Cal Teale Albers)
wc_mpas <- st_read(dsn = "/Users/Madi/Documents/UCSB Bren/ResilienSeas/all_mpas_update", layer = "all_mpas_update")
wc_mpas_proj <- st_transform(wc_mpas, "+init=epsg:6414")
wc_mpas_proj[,1] <- seq(1, length(wc_mpas_proj[,1]))
wc_mpas_sp <- as(wc_mpas_proj, "Spatial")


#clipping rasters to coast shapefile----
#clipping continuous aragonite layer
aragonite_clipped <- mask(aragonite_raster_proj, westcoast_sp, inverse = TRUE, progress='text')
mapview(aragonite_clipped)

#leaflet for continuous aragonite raster
pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(aragonite_clipped),na.color = "transparent")

leaflet() %>% 
  addTiles() %>%
  addProviderTiles('Esri.OceanBasemap') %>% 
  addRasterImage(aragonite_clipped, colors = pal) %>% 
  addLegend(
    pal = pal, values = values(aragonite_clipped),
    title = "Aragonite Saturation State")

#clipping hotspot mask
hotspotmask_proj <- projectRaster(hotspotmask, crs=CRS('+init=EPSG:6414'),method="ngb")
hotspot_clipped <- mask(hotspotmask, westcoast_sp, inverse = TRUE)
mapview(hotspot_clipped)


#zonal statistics----
poly_MPA@data[,1] <- seq(1, length(poly_MPA@data[,1]))

aragonite_mean<- raster::extract(aragonite_clipped, wc_mpas_proj, fun=mean, na.rm=TRUE, df=TRUE)


tm_shape(aragonite_data)+
  tm_dots(col = "black", size = 0.003)+
  tm_layout(basemaps = c('OpenStreetMap'), basemaps.alpha = 0.5)
