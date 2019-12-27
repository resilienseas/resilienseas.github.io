####Manuscript Maps

tmap_mode("view")

setwd("/Users/raetaylor-burns/downloads")

pal <- colorRampPalette(c("red", "white", "royalblue2"))

arag <- crop(arag, ext_study)

#breaks = c(1.6, 1.7, 1.8, 1.9, 2.0, 2.1, 2.2, 2.3, 2.4, 2.5, 2.6)

aragonite <- tm_shape(arag_clipped)+
  tm_raster(palette = pal(8), colorNA = NULL, alpha = 1, auto.palette.mapping = FALSE, legend.show = FALSE, breaks = c(1.6, 1.7, 1.8, 1.9, 2.0, 2.1, 2.2, 2.3, 2.4, 2.5, 2.6))+
  tm_layout(main.title = "Aragonite Saturation State", main.title.size = 1, bg.color = "white", main.title.position = c("center", "top"), fontfamily = "serif", fontface = "bold")+
  tm_shape(poly_coast)+
  tm_polygons()+
  tm_shape(Canada)+
  tm_polygons()+
  tm_grid(projection = "longlat", lwd = 0.1, n.x = 3, n.y = 6, labels.inside.frame = FALSE, labels.size = 0.6)
tmap_options(max.raster = c(plot = 45955000, view = 45955000))

aragonitelegend <- tm_shape(arag_clipped)+
  tm_raster(title = "Aragonite Saturation State", palette = pal(10), colorNA = NULL, alpha = 1, auto.palette.mapping = FALSE, legend.show = TRUE, breaks = c(1.6, 1.7, 1.8, 1.9, 2.0, 2.1, 2.2, 2.3, 2.4, 2.5, 2.6))+
  tm_layout(main.title = "Aragonite Saturation State", main.title.size = 1, bg.color = "white", main.title.position = c("center", "top"), fontfamily = "serif", fontface = "bold", legend.only = TRUE, legend.just = "center")+
  tm_shape(poly_coast)+
  tm_polygons()+
  tm_shape(Canada)+
  tm_polygons()+
  tm_grid(projection = "longlat", lwd = 0.1, n.x = 3, n.y = 6, labels.inside.frame = FALSE, labels.size = 0.6)
tmap_options(max.raster = c(plot = 45955000, view = 45955000))

tmap_save(aragonite, "aragonite.png", width = 3000, height = 4000, dpi = 500)
tmap_save(aragonitelegend, "aragonitelegend.png", width = 3000, height = 4000, dpi = 500)

pal <- colorRampPalette(c("royalblue2", "white", "red"))

# uncertaintyplot <- tm_shape(uncertainty_clipped)+
#   tm_raster(palette = pal(8), colorNA = NULL, alpha = 1, auto.palette.mapping = FALSE, legend.show = FALSE, breaks = c(0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08))+
#   tm_layout(main.title = "Uncertainty in Aragonite Saturation State", main.title.size = 1, bg.color = "white", main.title.position = c("center", "top"), fontfamily = "serif", fontface = "bold")+
#   tm_shape(poly_coast)+
#   tm_polygons()+
#   tm_shape(Canada)+
#   tm_polygons()+
#   tm_grid(projection = "longlat", lwd = 0.1, n.x = 3, n.y = 6, labels.inside.frame = FALSE, labels.size = 0.6)
# tmap_options(max.raster = c(plot = 45955000, view = 45955000))
# 
# uncertaintylegend <- tm_shape(uncertainty_clipped)+
#   tm_raster(title = "Uncertainty", palette = pal(10), colorNA = NULL, alpha = 1, auto.palette.mapping = FALSE, legend.show = TRUE, breaks = c(0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08))+
#   tm_layout(main.title = "Uncertainty", main.title.size = 1, bg.color = "white", main.title.position = c("center", "top"), fontfamily = "serif", fontface = "bold", legend.only = TRUE, legend.just = "center")+
#   tm_shape(poly_coast)+
#   tm_polygons()+
#   tm_shape(Canada)+
#   tm_polygons()+
#   tm_grid(projection = "longlat", lwd = 0.1, n.x = 3, n.y = 6, labels.inside.frame = FALSE, labels.size = 0.6)
# tmap_options(max.raster = c(plot = 45955000, view = 45955000))
# 
# tmap_save(uncertaintyplot, "uncertainty.png", width = 3000, height = 4000, dpi = 500)
# tmap_save(uncertaintylegend, "uncertaintylegend.png", width = 3000, height = 4000, dpi = 500)

pal <- colorRampPalette(c("royalblue1", "white", "red"))

oceanographicdist <- tm_shape(oceanographicdistance)+
  tm_raster(palette = pal(10), colorNA = 'grey87', alpha = 1, auto.palette.mapping = FALSE, legend.show = FALSE, breaks = c(0, 25, 50, 75, 100, 125, 150, 175, 200, 225, 250, 275, 300, 325, 350, 375, 400))+
  tm_layout(main.title = "Oceanographic Distance (km)", main.title.size = 1, bg.color = "white", main.title.position = c("center", "top"), fontfamily = "serif", fontface = "bold")+
  tm_shape(poly_coast)+
  tm_polygons()+
  tm_shape(Canada)+
  tm_polygons()+
  tm_grid(projection = "longlat", lwd = 0.1, n.x = 3, n.y = 6, labels.inside.frame = FALSE, labels.size = 0.6)+
  tm_shape(inventorycoords)+
  tm_dots(size = 0.02)
tmap_options(max.raster = c(plot = 45955000, view = 45955000))

oceanographicdistlegend <- tm_shape(oceanographicdistance)+
  tm_raster(title = "Aragonite Saturation State", palette = pal(10), colorNA = 'grey87', alpha = 1, auto.palette.mapping = FALSE, legend.show = TRUE, breaks = c(0, 25, 50, 75, 100, 125, 150, 175, 200, 225, 250, 275, 300, 325, 350, 375, 400))+
  tm_layout(main.title = "Oceanographic Distance (km)", main.title.size = 1, bg.color = "white", main.title.position = c("center", "top"), fontfamily = "serif", fontface = "bold", legend.only = TRUE, legend.just = "center")+
  tm_shape(poly_coast)+
  tm_polygons()+
  tm_shape(Canada)+
  tm_polygons()+
  tm_grid(projection = "longlat", lwd = 0.1, n.x = 3, n.y = 6, labels.inside.frame = FALSE, labels.size = 0.6)
tmap_options(max.raster = c(plot = 45955000, view = 45955000))

tmap_save(oceanographicdist, "oceanographicdistance.png", width = 3000, height = 4000, dpi = 500)
tmap_save(oceanographicdistlegend, "oceanographicdistancelegend.png", width = 3000, height = 4000, dpi = 500)

pal <- colorRampPalette(c("royalblue2", "white", "red"))

fullgap <- tm_shape(gap)+
  tm_raster(palette = pal(10), colorNA = 'grey87', alpha = 0.8, auto.palette.mapping = FALSE, legend.show = FALSE, breaks = c(0, 25, 50, 75, 100, 125, 150, 175, 200, 225, 250, 275, 300, 325, 350, 375))+
  tm_layout(main.title = "Gaps", fontfamily = "serif", fontface = "bold", main.title.size = 0.7, main.title.position = "center", outer.margins = c(0.05, 0.05, 0.05, 0.05))+
  tm_shape(poly_coast)+
  tm_polygons()+
  tm_shape(Canada)+
  tm_polygons()+
  tm_shape(inventorycoords)+
  tm_dots()+
  tm_grid(projection = "longlat", lwd = 0.1, n.x = 3, n.y = 6, labels.inside.frame = FALSE, labels.size = 0.6)+
  tmap_options(max.raster = c(plot = 45955000, view = 45955000))

fullgaplegend <- tm_shape(gap)+
  tm_raster(palette = pal(10), colorNA = 'grey87', alpha = 0.8, auto.palette.mapping = FALSE, breaks = c(0, 25, 50, 75, 100, 125, 150, 175, 200, 225, 250, 275, 300, 325, 350, 375))+
  tm_layout(main.title = "Gaps", fontfamily = "serif", fontface = "bold", main.title.size = 0.7, main.title.position = "center", outer.margins = c(0.05, 0.05, 0.05, 0.05), legend.only = TRUE)+
  tm_shape(poly_coast)+
  tm_polygons()+
  tm_shape(Canada)+
  tm_polygons()+
  tm_shape(inventorycoords)+
  tm_dots()+
  tm_grid(projection = "longlat", lwd = 0.1, n.x = 3, n.y = 6, labels.inside.frame = FALSE, labels.size = 0.6)+
  tmap_options(max.raster = c(plot = 45955000, view = 45955000))


tmap_save(fullgap, "gap.png", width = 2000, height = 4000, dpi = 500)
tmap_save(fullgaplegend, "gaplegend.png", width = 3000, height = 2000, dpi = 500)

#carbonate complete inventory gaps 

ccgap <- tm_shape(carbcompletegap)+
  tm_raster(palette = pal(10), colorNA = 'grey87', alpha = 0.8, auto.palette.mapping = FALSE, legend.show = FALSE,breaks = c(0, 25, 50, 75, 100, 125, 150, 175, 200, 225, 250, 275, 300, 325, 350, 375))+
  tm_layout(main.title = "Aragonite Measurement Gaps", fontfamily = "serif", fontface = "bold", main.title.size = 0.7, main.title.position = "center", outer.margins = c(0.05, 0.05, 0.05, 0.05))+
  tm_shape(poly_coast)+
  tm_polygons()+
  tm_shape(Canada)+
  tm_polygons()+  
  tm_shape(incompletecoords)+
  tm_dots()+
  tm_grid(projection = "longlat", lwd = 0.1, n.x = 3, n.y = 6, labels.inside.frame = FALSE, labels.size = 0.6)+
  tmap_options(max.raster = c(plot = 45955000, view = 45955000))

ccgaplegend <- tm_shape(carbcompletegap)+
  tm_raster(palette = pal(10), colorNA = 'grey87', alpha = 0.8, auto.palette.mapping = FALSE, breaks = c(0, 25, 50, 75, 100, 125, 150, 175, 200, 225, 250, 275, 300, 325, 350, 375))+
  tm_layout(main.title = "Aragonite Measurement Gaps", fontfamily = "serif", fontface = "bold", main.title.size = 0.7, main.title.position = "center", outer.margins = c(0.05, 0.05, 0.05, 0.05), legend.only =  TRUE)+
  tm_shape(poly_coast)+
  tm_polygons()+
  tm_shape(Canada)+
  tm_polygons()+  
  tm_shape(inventorycoords)+
  tm_dots()+
  tm_shape(incompletecoords)+
  tm_dots(col = "red")+
  tm_grid(projection = "longlat", lwd = 0.1, n.x = 3, n.y = 6, labels.inside.frame = FALSE, labels.size = 0.6)+
  tmap_options(max.raster = c(plot = 45955000, view = 45955000))

tmap_save(ccgap, "cc.png", width = 2000, height = 4000, dpi = 500)
tmap_save(ccgaplegend, "cclegend.png", width = 3000, height = 2000, dpi = 500)

# high frequency inventory gaps

hfgap <- tm_shape(highfreqgap)+
  tm_raster(palette = pal(10), colorNA = 'grey87', alpha = 0.8, auto.palette.mapping = FALSE, legend.show = FALSE, breaks = c(0, 25, 50, 75, 100, 125, 150, 175, 200, 225, 250, 275, 300, 325, 350, 375))+
  tm_layout(main.title = "Daily Monitoring Gaps", fontfamily = "serif", fontface = "bold", main.title.size = 0.7, main.title.position = "center", outer.margins = c(0.05, 0.05, 0.05, 0.05))+
  tm_shape(poly_coast)+
  tm_polygons()+
  tm_shape(Canada)+
  tm_polygons()+
  tm_shape(highfreqcoords)+
  tm_dots()+
  tm_grid(projection = "longlat", lwd = 0.1, n.x = 3, n.y = 6, labels.inside.frame = FALSE, labels.size = 0.6)+
  tmap_options(max.raster = c(plot = 45955000, view = 45955000))


hfgaplegend <- tm_shape(highfreqgap)+
  tm_raster(palette = pal(10), colorNA = 'grey87', alpha = 0.8, auto.palette.mapping = FALSE, breaks = c(0, 25, 50, 75, 100, 125, 150, 175, 200, 225, 250, 275, 300, 325, 350, 375))+
  tm_layout(main.title = "Daily Monitoring Gaps", fontfamily = "serif", fontface = "bold", main.title.size = 0.7, main.title.position = "center", outer.margins = c(0.05, 0.05, 0.05, 0.05), legend.only = TRUE)+
  tm_shape(poly_coast)+
  tm_polygons()+
  tm_shape(Canada)+
  tm_polygons()+
  tm_shape(highfreqcoords)+
  tm_dots()+
  tm_grid(projection = "longlat", lwd = 0.1, n.x = 3, n.y = 6, labels.inside.frame = FALSE, labels.size = 0.6)+
  tmap_options(max.raster = c(plot = 45955000, view = 45955000))

tmap_save(hfgap, "hf.png", width = 2000, height = 4000, dpi = 500)
tmap_save(hfgaplegend, "hflegend.png", width = 3000, height = 2000, dpi = 500)


