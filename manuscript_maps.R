
####Manuscript Maps

tmap_mode("view")

setwd("/Users/raetaylor-burns/downloads")

pal <- colorRampPalette(c("red", "white", "royalblue2"))

aragonite <- tm_shape(arag)+
  tm_raster(palette = pal(10), colorNA = 'grey87', alpha = 1, auto.palette.mapping = FALSE, legend.show = FALSE)+
  tm_layout(main.title = "Aragonite Saturation State", main.title.size = 1, bg.color = "white", main.title.position = c("center", "top"), fontfamily = "serif", fontface = "bold")+
  tm_shape(poly_coast)+
  tm_polygons()+
  tm_shape(Canada)+
  tm_polygons()+
  tm_grid(projection = "longlat", lwd = 0.1, n.x = 3, n.y = 6, labels.inside.frame = FALSE, labels.size = 0.6)
tmap_options(max.raster = c(plot = 45955000, view = 45955000))

aragonitelegend <- tm_shape(arag)+
  tm_raster(title = "Aragonite Saturation State", palette = pal(10), colorNA = 'grey87', alpha = 1, auto.palette.mapping = FALSE, legend.show = TRUE)+
  tm_layout(main.title = "Aragonite Saturation State", main.title.size = 1, bg.color = "white", main.title.position = c("center", "top"), fontfamily = "serif", fontface = "bold", legend.only = TRUE, legend.just = "center")+
  tm_shape(poly_coast)+
  tm_polygons()+
  tm_shape(Canada)+
  tm_polygons()+
  tm_grid(projection = "longlat", lwd = 0.1, n.x = 3, n.y = 6, labels.inside.frame = FALSE, labels.size = 0.6)
tmap_options(max.raster = c(plot = 45955000, view = 45955000))

tmap_save(aragonite, "aragonite.png", width = 3000, height = 4000, dpi = 500)
tmap_save(aragonitelegend, "aragonitelegend.png", width = 3000, height = 4000, dpi = 500)

pal <- colorRampPalette(c("royalblue1", "white", "red"))

aragonitediscrepancy <- tm_shape(abs(arag-polygonarag))+
  tm_raster(palette = pal(10), colorNA = 'grey87', alpha = 1, auto.palette.mapping = FALSE, legend.show = FALSE)+
  tm_layout(main.title = "Aragonite Saturation State Discrepancy", main.title.size = 1, bg.color = "white", main.title.position = c("center", "top"), fontfamily = "serif", fontface = "bold")+
  tm_shape(poly_coast)+
  tm_polygons()+
  tm_shape(Canada)+
  tm_polygons()+
  tm_grid(projection = "longlat", lwd = 0.1, n.x = 3, n.y = 6, labels.inside.frame = FALSE, labels.size = 0.6)+
  tm_shape(inventorycoords)+
  tm_dots(size = 0.02)
tmap_options(max.raster = c(plot = 45955000, view = 45955000))

aragonitediscrepancylegend <- tm_shape(abs(arag-polygonarag))+
  tm_raster(title = "Aragonite Saturation State", palette = pal(10), colorNA = 'grey87', alpha = 1, auto.palette.mapping = FALSE, legend.show = TRUE)+
  tm_layout(main.title = "Aragonite Saturation State", main.title.size = 1, bg.color = "white", main.title.position = c("center", "top"), fontfamily = "serif", fontface = "bold", legend.only = TRUE, legend.just = "center")+
  tm_shape(poly_coast)+
  tm_polygons()+
  tm_shape(Canada)+
  tm_polygons()+
  tm_grid(projection = "longlat", lwd = 0.1, n.x = 3, n.y = 6, labels.inside.frame = FALSE, labels.size = 0.6)
tmap_options(max.raster = c(plot = 45955000, view = 45955000))

aragoniterange <- tm_shape(abs(aragrange))+
  tm_raster(palette = pal(10), colorNA = 'grey87', alpha = 1, auto.palette.mapping = FALSE, legend.show = FALSE)+
  tm_layout(main.title = "Aragonite Saturation State Range", main.title.size = 1, bg.color = "white", main.title.position = c("center", "top"), fontfamily = "serif", fontface = "bold")+
  tm_shape(poly_coast)+
  tm_polygons()+
  tm_shape(Canada)+
  tm_polygons()+
  tm_grid(projection = "longlat", lwd = 0.1, n.x = 3, n.y = 6, labels.inside.frame = FALSE, labels.size = 0.6)+
  tm_shape(inventorycoords)+
  tm_dots(size = 0.02)
tmap_options(max.raster = c(plot = 45955000, view = 45955000))  

aragoniterangelegend <- tm_shape(aragrange)+
  tm_raster(title = "Aragonite Saturation State Range", palette = pal(10), colorNA = 'grey87', alpha = 1, auto.palette.mapping = FALSE, legend.show = TRUE)+
  tm_layout(main.title = "Aragonite Saturation State Range", main.title.size = 1, bg.color = "white", main.title.position = c("center", "top"), fontfamily = "serif", fontface = "bold", legend.only = TRUE, legend.just = "center")+
  tm_shape(poly_coast)+
  tm_polygons()+
  tm_shape(Canada)+
  tm_polygons()+
  tm_grid(projection = "longlat", lwd = 0.1, n.x = 3, n.y = 6, labels.inside.frame = FALSE, labels.size = 0.6)
tmap_options(max.raster = c(plot = 45955000, view = 45955000))

oceanographicdist <- tm_shape(oceanographicdistance)+
  tm_raster(palette = pal(10), colorNA = 'grey87', alpha = 1, auto.palette.mapping = FALSE, legend.show = FALSE)+
  tm_layout(main.title = "Oceanographic Distance", main.title.size = 1, bg.color = "white", main.title.position = c("center", "top"), fontfamily = "serif", fontface = "bold")+
  tm_shape(poly_coast)+
  tm_polygons()+
  tm_shape(Canada)+
  tm_polygons()+
  tm_grid(projection = "longlat", lwd = 0.1, n.x = 3, n.y = 6, labels.inside.frame = FALSE, labels.size = 0.6)+
  tm_shape(inventorycoords)+
  tm_dots(size = 0.02)
tmap_options(max.raster = c(plot = 45955000, view = 45955000))  

oceanographicdistlegend <- tm_shape(oceanographicdistance)+
  tm_raster(title = "Oceanographic Distance", palette = pal(10), colorNA = 'grey87', alpha = 1, auto.palette.mapping = FALSE, legend.show = TRUE)+
  tm_layout(main.title = "Oceanographic Distance", main.title.size = 1, bg.color = "white", main.title.position = c("center", "top"), fontfamily = "serif", fontface = "bold", legend.only = TRUE, legend.just = "center")+
  tm_shape(poly_coast)+
  tm_polygons()+
  tm_shape(Canada)+
  tm_polygons()+
  tm_grid(projection = "longlat", lwd = 0.1, n.x = 3, n.y = 6, labels.inside.frame = FALSE, labels.size = 0.6)
tmap_options(max.raster = c(plot = 45955000, view = 45955000))

tmap_save(aragonitediscrepancy, "aragonitediscrepancy.png", width = 3000, height = 4000, dpi = 500)
tmap_save(aragonitediscrepancylegend, "aragonitediscrepancylegend.png", width = 3000, height = 4000, dpi = 500)
tmap_save(aragoniterange, "aragoniterange.png", width = 3000, height = 4000, dpi = 500)
tmap_save(aragoniterangelegend, "aragoniterangelegend.png", width = 3000, height = 4000, dpi = 500)
tmap_save(oceanographicdist, "oceanographicdistance.png", width = 3000, height = 4000, dpi = 500)
tmap_save(oceanographicdistlegend, "oceanographicdistancelegend.png", width = 3000, height = 4000, dpi = 500)


pal <- colorRampPalette(c("royalblue2", "white", "red"))

fullgap <- tm_shape(gap)+
  tm_raster(palette = pal(10), colorNA = 'grey87', alpha = 0.8, auto.palette.mapping = FALSE, legend.show = FALSE,  breaks = c(0, 150, 300, 450, 600, 750))+
  tm_layout(main.title = "Effective Distance to Nearest Monitoring (km)", fontfamily = "serif", fontface = "bold", main.title.size = 1)+
  tm_shape(poly_coast)+
  tm_polygons()+
  tm_shape(Canada)+
  tm_polygons()+
  tm_grid(projection = "longlat", lwd = 0.1, n.x = 3, n.y = 6, labels.inside.frame = FALSE, labels.size = 0.6)+
  tmap_options(max.raster = c(plot = 45955000, view = 45955000))

channelislandfullgap <- crop(gap, ext_study_channelislands)

channelislandgap <- tm_shape(channelislandfullgap)+
  tm_raster(palette = pal(10), colorNA = 'grey87', alpha = 0.8, auto.palette.mapping = FALSE, legend.show = FALSE,  breaks = c(0, 150, 300, 450, 600, 750))+
  tm_layout(fontfamily = "serif", fontface = "bold")+
  tm_shape(poly_coast)+
  tm_polygons()+
  tm_shape(Canada)+
  tm_polygons()+
  tm_shape(inventorycoords)+
  tm_dots(size = 0.05)+
  tm_grid(projection = "longlat", lwd = 0.1, n.x = 3, n.y = 6, labels.inside.frame = FALSE, labels.size = 0.6)+
  tmap_options(max.raster = c(plot = 45955000, view = 45955000))

pugetsoundfullgap <- crop(gap, ext_study_pugetsound)

pugetsoundgap <- tm_shape(pugetsoundfullgap)+
  tm_raster(palette = pal(10), colorNA = 'grey87', alpha = 0.8, auto.palette.mapping = FALSE, legend.show = FALSE,  breaks = c(0, 150, 300, 450, 600, 750))+
  tm_layout(fontfamily = "serif", fontface = "bold")+
  tm_shape(poly_coast)+
  tm_polygons()+
  tm_shape(Canada)+
  tm_polygons()+
  tm_shape(inventorycoords)+
  tm_dots(size = 0.05)+
  tm_grid(projection = "longlat", lwd = 0.1, n.x = 3, n.y = 6, labels.inside.frame = FALSE, labels.size = 0.6)+
  tmap_options(max.raster = c(plot = 45955000, view = 45955000))

oregoncoastfullgap <- crop(gap, ext_study_oregoncoast)

oregongap <- tm_shape(oregoncoastfullgap)+
  tm_raster(palette = pal(10), colorNA = 'grey87', alpha = 0.8, auto.palette.mapping = FALSE, legend.show = FALSE,  breaks = c(0, 150, 300, 450, 600, 750))+
  tm_layout(fontfamily = "serif", fontface = "bold")+
  tm_shape(poly_coast)+
  tm_polygons()+
  tm_shape(Canada)+
  tm_polygons()+
  tm_shape(inventorycoords)+
  tm_dots(size = 0.05)+
  tm_grid(projection = "longlat", lwd = 0.1, n.x = 3, n.y = 6, labels.inside.frame = FALSE, labels.size = 0.6)+
  tmap_options(max.raster = c(plot = 45955000, view = 45955000))

gaplegend <- tm_shape(gap)+
  tm_raster(title = "Effective Distance to Nearest Monitoring (km)", palette = pal(10), colorNA = 'grey87', alpha = 1, auto.palette.mapping = FALSE, legend.show = TRUE)+
  tm_layout(main.title = "Effective Distance to Nearest Monitoring (km)", main.title.size = 1, bg.color = "white", main.title.position = c("center", "top"), fontfamily = "serif", fontface = "bold", legend.only = TRUE, legend.just = "center")+
  tm_shape(poly_coast)+
  tm_polygons()+
  tm_shape(Canada)+
  tm_polygons()+
  tm_grid(projection = "longlat", lwd = 0.1, n.x = 3, n.y = 6, labels.inside.frame = FALSE, labels.size = 0.6)
tmap_options(max.raster = c(plot = 45955000, view = 45955000))


tmap_save(fullgap, "gap.png", width = 2000, height = 4000, dpi = 500)
tmap_save(channelislandgap, "channelislandgap.png", width = 3800, height = 2500, dpi = 500)
tmap_save(pugetsoundgap, "pugetsoundgap.png", width = 3800, height = 2500, dpi = 500)
tmap_save(oregongap, "oregongap.png", width = 3800, height = 2500, dpi = 500)
tmap_save(gaplegend, "gaplegend.png", width = 3000, height = 2000, dpi = 500)

#carbonate complete inventory gaps 

ccgap <- tm_shape(carbcompletegap)+
  tm_raster(palette = pal(10), colorNA = 'grey87', alpha = 0.8, breaks = c(0, 150, 300, 450, 600, 750), legend.show = FALSE)+
  tm_layout(main.title = "Effective Distance to Nearest Aragonite Monitoring (km)", fontfamily = "serif", fontface = "bold", main.title.size = 1)+
  tm_shape(poly_coast)+
  tm_polygons()+
  tm_shape(Canada)+
  tm_polygons()+
  tm_grid(projection = "longlat", lwd = 0.1, n.x = 3, n.y = 6, labels.inside.frame = FALSE, labels.size = 0.6)+
  tmap_options(max.raster = c(plot = 45955000, view = 45955000))

channelislandcarbcompgap <- crop(carbcompletegap, ext_study_channelislands)

channelislandccgap <- tm_shape(channelislandcarbcompgap)+
  tm_raster(palette = pal(10), colorNA = 'grey87', alpha = 0.8, auto.palette.mapping = FALSE, legend.show = FALSE,  breaks = c(0, 150, 300, 450, 600, 750))+
  tm_layout()+
  tm_shape(poly_coast)+
  tm_polygons()+
  tm_shape(Canada)+
  tm_polygons()+
  tm_shape(incompletecoords)+
  tm_dots(size = 0.05)+
  tmap_options(max.raster = c(plot = 45955000, view = 45955000))

pugetsoundcarbcompgap <- crop(carbcompletegap, ext_study_pugetsound)

pugetsoundccgap <- tm_shape(pugetsoundcarbcompgap)+
  tm_raster(palette = pal(10), colorNA = 'grey87', alpha = 0.8, auto.palette.mapping = FALSE, legend.show = FALSE,  breaks = c(0, 150, 300, 450, 600, 750))+
  tm_layout()+
  tm_shape(poly_coast)+
  tm_polygons()+
  tm_shape(Canada)+
  tm_polygons()+
  tm_shape(incompletecoords)+
  tm_dots(size = 0.05)+
  tmap_options(max.raster = c(plot = 45955000, view = 45955000))

oregoncoastcarbcompgap <- crop(carbcompletegap, ext_study_oregoncoast)

oregonccgap <- tm_shape(oregoncoastcarbcompgap)+
  tm_raster(palette = pal(10), colorNA = 'grey87', alpha = 0.8, auto.palette.mapping = FALSE, legend.show = FALSE,  breaks = c(0, 150, 300, 450, 600, 750))+
  tm_layout()+
  tm_shape(poly_coast)+
  tm_polygons()+
  tm_shape(Canada)+
  tm_polygons()+
  tm_shape(incompletecoords)+
  tm_dots(size = 0.05)+
  tmap_options(max.raster = c(plot = 45955000, view = 45955000))

tmap_save(ccgap, "ccgaps.png", width = 3000, height = 4000, dpi = 500)
tmap_save(channelislandccgap, "channelislandccgap.png", width = 3800, height = 2500, dpi = 500)
tmap_save(pugetsoundccgap, "pugetsoundccgap.png", width = 3800, height = 2500, dpi = 500)
tmap_save(oregonccgap, "oregonccgap.png", width = 3800, height = 2500, dpi = 500)

# high frequency inventory gaps

highfreqgap <- crop(highfreqgap, ext_study)

hfgap <- tm_shape(highfreqgap)+
  tm_raster(palette = pal(10), colorNA = 'grey87', alpha = 0.8, auto.palette.mapping = FALSE,  breaks = c(0, 150, 300, 450, 600, 750), legend.show = FALSE)+
  tm_layout(main.title = "Effective Distance to Nearest Daily Monitoring (km)", fontfamily = "serif", fontface = "bold", main.title.size = 1)+
  tm_shape(poly_coast)+
  tm_polygons()+
  tm_shape(Canada)+
  tm_polygons()+
  tm_grid(projection = "longlat", lwd = 0.1, n.x = 3, n.y = 6, labels.inside.frame = FALSE, labels.size = 0.6)+
  tmap_options(max.raster = c(plot = 45955000, view = 45955000))

channelislandhighfreqgap <- crop(highfreqgap, ext_study_channelislands)

channelislandhfgap <- tm_shape(channelislandhighfreqgap)+
  tm_raster(palette = pal(10), colorNA = 'grey87', alpha = 0.8, auto.palette.mapping = FALSE, legend.show = FALSE,  breaks = c(0, 150, 300, 450, 600, 750))+
  tm_layout()+
  tm_shape(poly_coast)+
  tm_polygons()+
  tm_shape(Canada)+
  tm_polygons()+
  tm_shape(lowfreqcoords)+
  tm_dots(size = 0.05)+
  tmap_options(max.raster = c(plot = 45955000, view = 45955000))

pugetsoundhighfreqgap <- crop(highfreqgap, ext_study_pugetsound)

pugetsoundhfgap <- tm_shape(pugetsoundhighfreqgap)+
  tm_raster(palette = pal(10), colorNA = 'grey87', alpha = 0.8, auto.palette.mapping = FALSE, legend.show = FALSE,  breaks = c(0, 150, 300, 450, 600, 750))+
  tm_layout()+
  tm_shape(poly_coast)+
  tm_polygons()+
  tm_shape(Canada)+
  tm_polygons()+
  tm_shape(lowfreqcoords)+
  tm_dots(size = 0.05)+
  tmap_options(max.raster = c(plot = 45955000, view = 45955000))

oregoncoasthighfreqgap <- crop(highfreqgap, ext_study_oregoncoast)

oregonhfgap <- tm_shape(oregoncoasthighfreqgap)+
  tm_raster(palette = pal(10), colorNA = 'grey87', alpha = 0.8, auto.palette.mapping = FALSE, legend.show = FALSE,  breaks = c(0, 150, 300, 450, 600, 750))+
  tm_layout()+
  tm_shape(poly_coast)+
  tm_polygons()+
  tm_shape(Canada)+
  tm_polygons()+
  tm_shape(lowfreqcoords)+
  tm_dots(size = 0.05)+
  tmap_options(max.raster = c(plot = 45955000, view = 45955000))

tmap_save(hfgap, "hfgaps.png", width = 4000, height = 4000, dpi = 500)
tmap_save(channelislandhfgap, "channelislandhfgap.png", width = 3800, height = 2500, dpi = 500)
tmap_save(pugetsoundhfgap, "pugetsoundhfgap.png", width = 3800, height = 2500, dpi = 500)
tmap_save(oregonhfgap, "oregonhfgap.png", width = 3800, height = 2500, dpi = 500)
