#sensitivity analysis

#first run gap analysis

#create matrix of weights
xy <- matrix(nrow = 10, ncol = 10)
distanceweight = c(10^-5, 10^-6, 10^-7, 10^-8, 10^-9, 10^-10, 10^-11, 10^-12, 10^-13, 10^-14)
temporalweight = c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20)
x = distanceweight
y = temporalweight
xy[i,j] <- paste(x[i], y[j])
apply(xy, 1, FUN=paste, x, y, sep =",")

#oceanographic dissimilarity

for(xy)

dissimilarity <- sqrt((sstmeandiff^2+domeandiff^2)+y*(sstrangediff^2+dorangediff^2))

distance<-distanceFromPoints(dissimilarity, inventorycoords)*x

gap<-setValues(distance, sqrt((getValues(distance)^2+(getValues(dissimilarity)^2))))

highprioritygaps <- setValues(distance, sqrt((getValues(distance)^2+(getValues(dissimilarity)^2)))) > quantile(gap, (.99))




#test clip of raster to coast shapefile
poly_coast<- readOGR(dsn=path.expand("Export_Output_2"), layer="Export_Output_2")
poly_coast <- spTransform(poly_coast, crs(gaps))
gaps_clipped <- mask(gaps, poly_coast, inverse = TRUE,progress='text')

