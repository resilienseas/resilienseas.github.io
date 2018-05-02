#sensitivity analysis

#first run gap analysis

#create matrix of weights
#xy <- matrix(nrow = 10, ncol = 10)
distanceweight = c(10^-5, 10^-6, 10^-7, 10^-8, 10^-9, 10^-10, 10^-11, 10^-12, 10^-13, 10^-14)
temporalweight = c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20)

#x = distanceweight
#y = temporalweight
#xy[i,j] <- paste(x[i], y[j])
#apply(xy, 1, FUN=paste, x, sep =",")

#create list of 100 rasters of top 1% of gaps

rastersensitivity <- list()

for(i in 1:length(distanceweight)){
  for(j in 1:length(temporalweight)){
    
    dissimilarity <- sqrt((sstmeandiff^2+domeandiff^2)+temporalweight[j]*(sstrangediff^2+dorangediff^2))
    
    distance<-distanceFromPoints(dissimilarity, inventorycoords)*distanceweight[i]
    
    gap<-setValues(distance, sqrt((getValues(distance)^2+(getValues(dissimilarity)^2))))
    
    highprioritygaps <- setValues(distance, sqrt((getValues(distance)^2+(getValues(dissimilarity)^2)))) > quantile(gap, (.99))
     
    name = paste(temporalweight[j], distanceweight[i], sep = "_")
    rastersensitivity[[name]] = highprioritygaps
  }
}

#transform to raster stack
sensitivitystack <- stack(rastersensitivity[[1]])
for(i in 2:length(rastersensitivity)) sensitivitystack <- addLayer(sensitivitystack, rastersensitivity[[i]])

###ADD UP VALUES
sum <- sum(sensitivitystack)

plot(sum)

freq(sum)

overlap <- setValues(sum, getValues(sum == 100))

plot(overlap)

freq(overlap)

##top 25% gaps 8% sensitive
