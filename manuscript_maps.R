####Manuscript Maps

tmap_mode("view")

setwd("/Users/raetaylor-burns/downloads")

pal <- colorRampPalette(c("red", "white", "royalblue2"))

arag <- crop(arag, ext_study)

aragonite <- tm_shape(arag)+
  tm_raster(palette = pal(10), colorNA = NULL, alpha = 1, auto.palette.mapping = FALSE, legend.show = FALSE)+
  tm_layout(main.title = "Aragonite Saturation State", main.title.size = 1, bg.color = "white", main.title.position = c("center", "top"), fontfamily = "serif", fontface = "bold")+
  tm_shape(poly_coast)+
  tm_polygons()+
  tm_shape(Canada)+
  tm_polygons()+
  tm_grid(projection = "longlat", lwd = 0.1, n.x = 3, n.y = 6, labels.inside.frame = FALSE, labels.size = 0.6)
tmap_options(max.raster = c(plot = 45955000, view = 45955000))

aragonitelegend <- tm_shape(arag)+
  tm_raster(title = "Aragonite Saturation State", palette = pal(10), colorNA = NULL, alpha = 1, auto.palette.mapping = FALSE, legend.show = TRUE)+
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

polygonarag <- crop(polygonarag, ext_study)

aragonitediscrepancy <- tm_shape(abs(arag-polygonarag))+
  tm_raster(palette = pal(10), colorNA = NULL, alpha = 1, auto.palette.mapping = FALSE, legend.show = FALSE)+
  tm_layout(main.title = "Aragonite Saturation State Discrepancy", main.title.size = 1, bg.color = "white", main.title.position = c("center", "top"), fontfamily = "serif", fontface = "bold")+
  tm_shape(poly_coast)+
  tm_polygons()+
  tm_shape(Canada)+
  tm_polygons()+
  tm_grid(projection = "longlat", lwd = 0.1, n.x = 3, n.y = 6, labels.inside.frame = FALSE, labels.size = 0.6)+
  tm_shape(inventorycoords)+
  tm_dots(size = 0.02)
tmap_options(max.raster = c(plot = 45955000, view = 45955000))

pugetsoundaragdiscrepancy <- crop(abs(arag-polygonarag), ext_study_pugetsound)

pugetsoundaragdisc <- tm_shape(pugetsoundaragdiscrepancy)+
  tm_raster(palette = pal(10), colorNA = NULL, alpha = 0.8, auto.palette.mapping = FALSE, legend.show = FALSE)+
  tm_layout(fontfamily = "serif", fontface = "bold")+
  tm_shape(poly_coast)+
  tm_polygons()+
  tm_shape(Canada)+
  tm_polygons()+
  tm_shape(inventorycoords)+
  tm_dots(size = 0.05)+
  tm_grid(projection = "longlat", lwd = 0.1, n.x = 3, n.y = 6, labels.inside.frame = FALSE, labels.size = 0.6)+
  tmap_options(max.raster = c(plot = 45955000, view = 45955000))

aragonitediscrepancylegend <- tm_shape(abs(arag-polygonarag))+
  tm_raster(title = "Aragonite Saturation State", palette = pal(10), colorNA = NULL, alpha = 1, auto.palette.mapping = FALSE, legend.show = TRUE)+
  tm_layout(main.title = "Aragonite Saturation State", main.title.size = 1, bg.color = "white", main.title.position = c("center", "top"), fontfamily = "serif", fontface = "bold", legend.only = TRUE, legend.just = "center")+
  tm_shape(poly_coast)+
  tm_polygons()+
  tm_shape(Canada)+
  tm_polygons()+
  tm_grid(projection = "longlat", lwd = 0.1, n.x = 3, n.y = 6, labels.inside.frame = FALSE, labels.size = 0.6)
tmap_options(max.raster = c(plot = 45955000, view = 45955000))

oceanographicdistance <- crop(oceanographicdistance, ext_study)

oceanographicdist <- tm_shape(oceanographicdistance)+
  tm_raster(palette = pal(10), colorNA = NULL, alpha = 1, auto.palette.mapping = FALSE, legend.show = FALSE)+
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
  tm_raster(title = "Oceanographic Distance", palette = pal(10), colorNA = NULL, alpha = 1, auto.palette.mapping = FALSE, legend.show = TRUE)+
  tm_layout(main.title = "Oceanographic Distance", main.title.size = 1, bg.color = "white", main.title.position = c("center", "top"), fontfamily = "serif", fontface = "bold", legend.only = TRUE, legend.just = "center")+
  tm_shape(poly_coast)+
  tm_polygons()+
  tm_shape(Canada)+
  tm_polygons()+
  tm_grid(projection = "longlat", lwd = 0.1, n.x = 3, n.y = 6, labels.inside.frame = FALSE, labels.size = 0.6)
tmap_options(max.raster = c(plot = 45955000, view = 45955000))

tmap_save(aragonitediscrepancy, "aragonitediscrepancy.png", width = 3000, height = 4000, dpi = 500)
tmap_save(pugetsoundaragdisc, "pugetsoundaragonitediscrepancy.png", width = 3000, height = 4000, dpi = 500)
tmap_save(aragonitediscrepancylegend, "aragonitediscrepancylegend.png", width = 3000, height = 4000, dpi = 500)
tmap_save(aragoniterange, "aragoniterange.png", width = 3000, height = 4000, dpi = 500)
tmap_save(aragoniterangelegend, "aragoniterangelegend.png", width = 3000, height = 4000, dpi = 500)
tmap_save(oceanographicdist, "oceanographicdistance.png", width = 3000, height = 4000, dpi = 500)
tmap_save(oceanographicdistlegend, "oceanographicdistancelegend.png", width = 3000, height = 4000, dpi = 500)

pal <- colorRampPalette(c("royalblue2", "white", "red"))

fullgap <- tm_shape(gap)+
  tm_raster(palette = pal(10), colorNA = NULL, alpha = 0.8, auto.palette.mapping = FALSE, legend.show = FALSE,  breaks = c(0, 100, 200, 300, 400, 500, 600))+
  tm_layout(main.title = "Effective Distance to Nearest Monitoring (km)", fontfamily = "serif", fontface = "bold", main.title.size = 1)+
  tm_shape(poly_coast)+
  tm_polygons()+
  tm_shape(Canada)+
  tm_polygons()+
  tm_grid(projection = "longlat", lwd = 0.1, n.x = 3, n.y = 6, labels.inside.frame = FALSE, labels.size = 0.6)+
  tmap_options(max.raster = c(plot = 45955000, view = 45955000))

socalfullgap <- crop(gap, ext_study_socal)

socalgap <- tm_shape(socalfullgap)+
  tm_raster(palette = pal(10), colorNA = NULL, alpha = 0.8, auto.palette.mapping = FALSE, legend.show = FALSE, breaks = c(0, 50, 100, 150, 200, 250, 300))+
  tm_layout(fontfamily = "serif", fontface = "bold")+
  tm_shape(poly_coast)+
  tm_polygons()+
  tm_shape(Canada)+
  tm_polygons()+
  tm_shape(inventorycoords)+
  tm_dots(size = 0.05)+
  tmap_options(max.raster = c(plot = 45955000, view = 45955000))

cencalfullgap <- crop(gap, ext_study_cencal)

cencalgap <- tm_shape(cencalfullgap)+
  tm_raster(palette = pal(10), colorNA = NULL, alpha = 0.8, auto.palette.mapping = FALSE, legend.show = FALSE,  breaks = c(0, 50, 100, 150, 200, 250, 300, 350))+
  tm_layout(fontfamily = "serif", fontface = "bold")+
  tm_shape(poly_coast)+
  tm_polygons()+
  tm_shape(Canada)+
  tm_polygons()+
  tm_shape(inventorycoords)+
  tm_dots(size = 0.05)+
  tmap_options(max.raster = c(plot = 45955000, view = 45955000))

norcalfullgap <- crop(gap, ext_study_norcal)

norcalgap <- tm_shape(norcalfullgap)+
  tm_raster(palette = pal(10), colorNA = NULL, alpha = 0.8, auto.palette.mapping = FALSE, legend.show = FALSE,  breaks = c(0, 50, 100, 150))+
  tm_layout(fontfamily = "serif", fontface = "bold")+
  tm_shape(poly_coast)+
  tm_polygons()+
  tm_shape(Canada)+
  tm_polygons()+
  tm_shape(inventorycoords)+
  tm_dots(size = 0.05)+
  tmap_options(max.raster = c(plot = 45955000, view = 45955000))

orfullgap <- crop(gap, ext_study_orcoast)

orgap <- tm_shape(orfullgap)+
  tm_raster(palette = pal(10), colorNA = NULL, alpha = 0.8, auto.palette.mapping = FALSE, legend.show = FALSE,  breaks = c(0, 50, 100))+
  tm_layout(fontfamily = "serif", fontface = "bold")+
  tm_shape(poly_coast)+
  tm_polygons()+
  tm_shape(Canada)+
  tm_polygons()+
  tm_shape(inventorycoords)+
  tm_dots(size = 0.05)+
  tmap_options(max.raster = c(plot = 45955000, view = 45955000))

wafullgap <- crop(gap, ext_study_wacoast)

wagap <- tm_shape(wafullgap)+
  tm_raster(palette = pal(10), colorNA = NULL, alpha = 0.8, auto.palette.mapping = FALSE, legend.show = FALSE,  breaks = c(0, 100, 200, 300, 400, 500, 600))+
  tm_layout(fontfamily = "serif", fontface = "bold")+
  tm_shape(poly_coast)+
  tm_polygons()+
  tm_shape(Canada)+
  tm_polygons()+
  tm_shape(inventorycoords)+
  tm_dots(size = 0.05)+
  tmap_options(max.raster = c(plot = 45955000, view = 45955000))

fullgaplegend <- tm_shape(fullgap)+
  tm_raster(title = "Effective Distance to Nearest Monitoring (km)", palette = pal(10), colorNA = NULL, alpha = 1, auto.palette.mapping = FALSE, legend.show = TRUE)+
  tm_layout(main.title = "Effective Distance to Nearest Monitoring (km)", main.title.size = 1, bg.color = "white", main.title.position = c("center", "top"), fontfamily = "serif", fontface = "bold", legend.only = TRUE, legend.just = "center")+
  tm_shape(poly_coast)+
  tm_polygons()+
  tm_shape(Canada)+
  tm_polygons()+
tmap_options(max.raster = c(plot = 45955000, view = 45955000))

socalfullgaplegend <- tm_shape(socalfullgap)+
  tm_raster(title = "Effective Distance to Nearest Monitoring (km)", palette = pal(10), colorNA = NULL, alpha = 1, auto.palette.mapping = FALSE, legend.show = TRUE)+
  tm_layout(main.title = "Effective Distance to Nearest Monitoring (km)", main.title.size = 1, bg.color = "white", main.title.position = c("center", "top"), fontfamily = "serif", fontface = "bold", legend.only = TRUE, legend.just = "center")+
  tm_shape(poly_coast)+
  tm_polygons()+
  tm_shape(Canada)+
  tm_polygons()+
  tmap_options(max.raster = c(plot = 45955000, view = 45955000))

cencalfullgaplegend <- tm_shape(cencalfullgap)+
  tm_raster(title = "Effective Distance to Nearest Monitoring (km)", palette = pal(10), colorNA = NULL, alpha = 1, auto.palette.mapping = FALSE, legend.show = TRUE)+
  tm_layout(main.title = "Effective Distance to Nearest Monitoring (km)", main.title.size = 1, bg.color = "white", main.title.position = c("center", "top"), fontfamily = "serif", fontface = "bold", legend.only = TRUE, legend.just = "center")+
  tm_shape(poly_coast)+
  tm_polygons()+
  tm_shape(Canada)+
  tm_polygons()+
  tmap_options(max.raster = c(plot = 45955000, view = 45955000))

norcalfullgaplegend <- tm_shape(norcalfullgap)+
  tm_raster(title = "Effective Distance to Nearest Monitoring (km)", palette = pal(10), colorNA = NULL, alpha = 1, auto.palette.mapping = FALSE, legend.show = TRUE)+
  tm_layout(main.title = "Effective Distance to Nearest Monitoring (km)", main.title.size = 1, bg.color = "white", main.title.position = c("center", "top"), fontfamily = "serif", fontface = "bold", legend.only = TRUE, legend.just = "center")+
  tm_shape(poly_coast)+
  tm_polygons()+
  tm_shape(Canada)+
  tm_polygons()+
  tmap_options(max.raster = c(plot = 45955000, view = 45955000))

orfullgaplegend <- tm_shape(orfullgap)+
  tm_raster(title = "Effective Distance to Nearest Monitoring (km)", palette = pal(10), colorNA = NULL, alpha = 1, auto.palette.mapping = FALSE, legend.show = TRUE)+
  tm_layout(main.title = "Effective Distance to Nearest Monitoring (km)", main.title.size = 1, bg.color = "white", main.title.position = c("center", "top"), fontfamily = "serif", fontface = "bold", legend.only = TRUE, legend.just = "center")+
  tm_shape(poly_coast)+
  tm_polygons()+
  tm_shape(Canada)+
  tm_polygons()+
  tmap_options(max.raster = c(plot = 45955000, view = 45955000))

wafullgaplegend <- tm_shape(wafullgap)+
  tm_raster(title = "Effective Distance to Nearest Monitoring (km)", palette = pal(10), colorNA = NULL, alpha = 1, auto.palette.mapping = FALSE, legend.show = TRUE)+
  tm_layout(main.title = "Effective Distance to Nearest Monitoring (km)", main.title.size = 1, bg.color = "white", main.title.position = c("center", "top"), fontfamily = "serif", fontface = "bold", legend.only = TRUE, legend.just = "center")+
  tm_shape(poly_coast)+
  tm_polygons()+
  tm_shape(Canada)+
  tm_polygons()+
  tmap_options(max.raster = c(plot = 45955000, view = 45955000))

tmap_save(fullgap, "gap.png", width = 2000, height = 4000, dpi = 500)
tmap_save(socalgap, "socalgap.png", width = 5000, height = 3300, dpi = 500)
tmap_save(cencalgap, "cencalgap.png", width = 3000, height = 3500, dpi = 500)
tmap_save(norcalgap, "norcalgap.png", width = 3000, height = 3500, dpi = 500)
tmap_save(orgap, "orgap.png", width = 3000, height = 3500, dpi = 500)
tmap_save(wagap, "wagap.png", width = 4000, height = 3500, dpi = 500)

tmap_save(gaplegend, "gaplegend.png", width = 3000, height = 2000, dpi = 500)
tmap_save(socalfullgaplegend, "socalgaplegend.png", width = 3000, height = 2000, dpi = 500)
tmap_save(cencalfullgaplegend, "cencalgaplegend.png", width = 3000, height = 2000, dpi = 500)
tmap_save(norcalfullgaplegend, "norcalgaplegend.png", width = 3000, height = 2000, dpi = 500)
tmap_save(orfullgaplegend, "orgaplegend.png", width = 3000, height = 2000, dpi = 500)
tmap_save(wafullgaplegend, "wagaplegend.png", width = 3000, height = 2000, dpi = 500)

#carbonate complete inventory gaps 

ccgap <- tm_shape(carbcompletegap)+
  tm_raster(palette = pal(10), colorNA = NULL, alpha = 0.8, breaks = c(0, 100, 200, 300, 400, 500, 600), legend.show = FALSE)+
  tm_layout(main.title = "Effective Distance to Nearest Aragonite Monitoring (km)", fontfamily = "serif", fontface = "bold", main.title.size = 1)+
  tm_shape(poly_coast)+
  tm_polygons()+
  tm_shape(Canada)+
  tm_polygons()+
  tm_grid(projection = "longlat", lwd = 0.1, n.x = 3, n.y = 6, labels.inside.frame = FALSE, labels.size = 0.6)+
  tmap_options(max.raster = c(plot = 45955000, view = 45955000))

socalcarbcompgap <- crop(carbcompletegap, ext_study_socal)

socalccgap <- tm_shape(socalcarbcompgap)+
  tm_raster(palette = pal(10), colorNA = NULL, alpha = 0.8, auto.palette.mapping = FALSE, legend.show = FALSE,  breaks = c(0, 50, 100, 150, 200, 250, 300))+
  tm_layout()+
  tm_shape(poly_coast)+
  tm_polygons()+
  tm_shape(Canada)+
  tm_polygons()+
  tm_shape(incompletecoords)+
  tm_dots(size = 0.05)+
  tmap_options(max.raster = c(plot = 45955000, view = 45955000))

cencalcarbcompgap <- crop(carbcompletegap, ext_study_cencal)

cencalccgap <- tm_shape(cencalcarbcompgap)+
  tm_raster(palette = pal(10), colorNA = NULL, alpha = 0.8, auto.palette.mapping = FALSE, legend.show = FALSE,  breaks = c(0, 50, 100, 150, 200, 250, 300, 350, 400))+
  tm_layout()+
  tm_shape(poly_coast)+
  tm_polygons()+
  tm_shape(Canada)+
  tm_polygons()+
  tm_shape(incompletecoords)+
  tm_dots(size = 0.05)+
  tmap_options(max.raster = c(plot = 45955000, view = 45955000))

norcalcarbcompgap <- crop(carbcompletegap, ext_study_norcal)

norcalccgap <- tm_shape(norcalcarbcompgap)+
  tm_raster(palette = pal(10), colorNA = NULL, alpha = 0.8, auto.palette.mapping = FALSE, legend.show = FALSE,  breaks = c(0, 25, 50, 75, 100, 125))+
  tm_layout()+
  tm_shape(poly_coast)+
  tm_polygons()+
  tm_shape(Canada)+
  tm_polygons()+
  tm_shape(incompletecoords)+
  tm_dots(size = 0.05)+
  tmap_options(max.raster = c(plot = 45955000, view = 45955000))

orcarbcompgap <- crop(carbcompletegap, ext_study_orcoast)

orccgap <- tm_shape(orcarbcompgap)+
  tm_raster(palette = pal(10), colorNA = NULL, alpha = 0.8, auto.palette.mapping = FALSE, legend.show = FALSE,  breaks = c(0, 25, 50, 75, 100, 125, 150))+
  tm_layout()+
  tm_shape(poly_coast)+
  tm_polygons()+
  tm_shape(Canada)+
  tm_polygons()+
  tm_shape(incompletecoords)+
  tm_dots(size = 0.05)+
  tmap_options(max.raster = c(plot = 45955000, view = 45955000))

wacarbcompgap <- crop(carbcompletegap, ext_study_wacoast)

waccgap<-tm_shape(wacarbcompgap)+
  tm_raster(palette = pal(10), colorNA = NULL, alpha = 0.8, auto.palette.mapping = FALSE, legend.show = FALSE,  breaks = c(0, 250, 500, 750, 1000, 1250))+
  tm_layout(fontfamily = "serif", fontface = "bold")+
  tm_shape(poly_coast)+
  tm_polygons()+
  tm_shape(Canada)+
  tm_polygons()+
  tm_shape(inventorycoords)+
  tm_dots(size = 0.05)+
  tm_grid(projection = "longlat", lwd = 0.1, n.x = 3, n.y = 6, labels.inside.frame = FALSE, labels.size = 0.6)+
  tmap_options(max.raster = c(plot = 45955000, view = 45955000))

ccgaplegend <- tm_shape(carbcompletegap)+
  tm_raster(title = "Effective Distance to Nearest Monitoring (km)", palette = pal(10), colorNA = NULL, alpha = 1, auto.palette.mapping = FALSE, legend.show = TRUE)+
  tm_layout(main.title = "Effective Distance to Nearest Monitoring (km)", main.title.size = 1, bg.color = "white", main.title.position = c("center", "top"), fontfamily = "serif", fontface = "bold", legend.only = TRUE, legend.just = "center")+
  tm_shape(poly_coast)+
  tm_polygons()+
  tm_shape(Canada)+
  tm_polygons()+
  tm_grid(projection = "longlat", lwd = 0.1, n.x = 3, n.y = 6, labels.inside.frame = FALSE, labels.size = 0.6)
tmap_options(max.raster = c(plot = 45955000, view = 45955000))

socalccgaplegend <- tm_shape(socalcarbcompgap)+
  tm_raster(title = "Effective Distance to Nearest Monitoring (km)", palette = pal(10), colorNA = NULL, alpha = 1, auto.palette.mapping = FALSE, legend.show = TRUE)+
  tm_layout(main.title = "Effective Distance to Nearest Monitoring (km)", main.title.size = 1, bg.color = "white", main.title.position = c("center", "top"), fontfamily = "serif", fontface = "bold", legend.only = TRUE, legend.just = "center")+
  tm_shape(poly_coast)+
  tm_polygons()+
  tm_shape(Canada)+
  tm_polygons()+
  tm_grid(projection = "longlat", lwd = 0.1, n.x = 3, n.y = 6, labels.inside.frame = FALSE, labels.size = 0.6)
tmap_options(max.raster = c(plot = 45955000, view = 45955000))

cencalccgaplegend <- tm_shape(cencalcarbcompgap)+
  tm_raster(title = "Effective Distance to Nearest Monitoring (km)", palette = pal(10), colorNA = NULL, alpha = 1, auto.palette.mapping = FALSE, legend.show = TRUE)+
  tm_layout(main.title = "Effective Distance to Nearest Monitoring (km)", main.title.size = 1, bg.color = "white", main.title.position = c("center", "top"), fontfamily = "serif", fontface = "bold", legend.only = TRUE, legend.just = "center")+
  tm_shape(poly_coast)+
  tm_polygons()+
  tm_shape(Canada)+
  tm_polygons()+
  tm_grid(projection = "longlat", lwd = 0.1, n.x = 3, n.y = 6, labels.inside.frame = FALSE, labels.size = 0.6)
tmap_options(max.raster = c(plot = 45955000, view = 45955000))

norcalccgaplegend <- tm_shape(norcalcarbcompgap)+
  tm_raster(title = "Effective Distance to Nearest Monitoring (km)", palette = pal(10), colorNA = NULL, alpha = 1, auto.palette.mapping = FALSE, legend.show = TRUE)+
  tm_layout(main.title = "Effective Distance to Nearest Monitoring (km)", main.title.size = 1, bg.color = "white", main.title.position = c("center", "top"), fontfamily = "serif", fontface = "bold", legend.only = TRUE, legend.just = "center")+
  tm_shape(poly_coast)+
  tm_polygons()+
  tm_shape(Canada)+
  tm_polygons()+
  tm_grid(projection = "longlat", lwd = 0.1, n.x = 3, n.y = 6, labels.inside.frame = FALSE, labels.size = 0.6)
tmap_options(max.raster = c(plot = 45955000, view = 45955000))

orccgaplegend <- tm_shape(orcarbcompgap)+
  tm_raster(title = "Effective Distance to Nearest Monitoring (km)", palette = pal(10), colorNA = NULL, alpha = 1, auto.palette.mapping = FALSE, legend.show = TRUE)+
  tm_layout(main.title = "Effective Distance to Nearest Monitoring (km)", main.title.size = 1, bg.color = "white", main.title.position = c("center", "top"), fontfamily = "serif", fontface = "bold", legend.only = TRUE, legend.just = "center")+
  tm_shape(poly_coast)+
  tm_polygons()+
  tm_shape(Canada)+
  tm_polygons()+
  tm_grid(projection = "longlat", lwd = 0.1, n.x = 3, n.y = 6, labels.inside.frame = FALSE, labels.size = 0.6)
tmap_options(max.raster = c(plot = 45955000, view = 45955000))

waccgaplegend <- tm_shape(wacarbcompgap)+
  tm_raster(title = "Effective Distance to Nearest Monitoring (km)", palette = pal(10), colorNA = NULL, alpha = 1, auto.palette.mapping = FALSE, legend.show = TRUE)+
  tm_layout(main.title = "Effective Distance to Nearest Monitoring (km)", main.title.size = 1, bg.color = "white", main.title.position = c("center", "top"), fontfamily = "serif", fontface = "bold", legend.only = TRUE, legend.just = "center")+
  tm_shape(poly_coast)+
  tm_polygons()+
  tm_shape(Canada)+
  tm_polygons()+
  tm_grid(projection = "longlat", lwd = 0.1, n.x = 3, n.y = 6, labels.inside.frame = FALSE, labels.size = 0.6)
tmap_options(max.raster = c(plot = 45955000, view = 45955000))

tmap_save(ccgap, "cc.png", width = 2000, height = 4000, dpi = 500)
tmap_save(socalccgap, "socalcc.png", width = 5000, height = 3300, dpi = 500)
tmap_save(cencalccgap, "cencalcc.png", width = 3000, height = 3500, dpi = 500)
tmap_save(norcalccgap, "norcalcc.png", width = 3000, height = 3500, dpi = 500)
tmap_save(orccgap, "orcc.png", width = 3000, height = 3500, dpi = 500)
tmap_save(waccgap, "wacc.png", width = 4000, height = 3500, dpi = 500)

tmap_save(ccgaplegend, "cclegend.png", width = 3000, height = 2000, dpi = 500)
tmap_save(socalccgaplegend, "socalcclegend.png", width = 3000, height = 2000, dpi = 500)
tmap_save(cencalccgaplegend, "cencalcclegend.png", width = 3000, height = 2000, dpi = 500)
tmap_save(norcalccgaplegend, "norcalcclegend.png", width = 3000, height = 2000, dpi = 500)
tmap_save(orccgaplegend, "orcclegend.png", width = 3000, height = 2000, dpi = 500)
tmap_save(waccgaplegend, "wacclegend.png", width = 3000, height = 2000, dpi = 500)

# high frequency inventory gaps

hf_ext_study <- extent(-470000, 340000, -580000, 1215000)

highfreqgap <- crop(highfreqgap, hf_ext_study)

hfgap <- tm_shape(highfreqgap)+
  tm_raster(palette = pal(10), colorNA = 'gray30', alpha = 0.8, auto.palette.mapping = FALSE,  breaks = c(0, 250, 500, 750, 1000, 1250), legend.show = FALSE)+
  tm_layout(main.title = "Effective Distance to Nearest Daily Monitoring (km)", fontfamily = "serif", fontface = "bold", main.title.size = 1)+
  tm_shape(poly_coast)+
  tm_polygons()+
  tm_shape(Canada)+
  tm_polygons()+
  tm_grid(projection = "longlat", lwd = 0.1, n.x = 3, n.y = 6, labels.inside.frame = FALSE, labels.size = 0.6)+
  tmap_options(max.raster = c(plot = 45955000, view = 45955000))

socalhighfreqgap <- crop(highfreqgap, ext_study_socal)

socalhfgap <- tm_shape(socalhighfreqgap)+
  tm_raster(palette = pal(10), colorNA = 'gray30', alpha = 0.8, auto.palette.mapping = FALSE, legend.show = FALSE,  breaks = c(0, 50, 100, 150, 200, 250, 300))+
  tm_layout()+
  tm_shape(poly_coast)+
  tm_polygons()+
  tm_shape(Canada)+
  tm_polygons()+
  tm_shape(lowfreqcoords)+
  tm_dots(size = 0.05)+
  tmap_options(max.raster = c(plot = 45955000, view = 45955000))

cencalhighfreqgap <- crop(highfreqgap, ext_study_cencal)

cencalhfgap <- tm_shape(cencalhighfreqgap)+
  tm_raster(palette = pal(10), colorNA = 'gray30', alpha = 0.8, auto.palette.mapping = FALSE, legend.show = FALSE,  breaks = c(0, 50, 100, 150, 200, 250, 300, 350, 400, 450))+
  tm_layout()+
  tm_shape(poly_coast)+
  tm_polygons()+
  tm_shape(Canada)+
  tm_polygons()+
  tm_shape(lowfreqcoords)+
  tm_dots(size = 0.05)+
  tmap_options(max.raster = c(plot = 45955000, view = 45955000))

norcalhighfreqgap <- crop(highfreqgap, ext_study_norcal)

norcalhfgap <- tm_shape(norcalhighfreqgap)+
  tm_raster(palette = pal(10), colorNA = 'gray30', alpha = 0.8, auto.palette.mapping = FALSE, legend.show = FALSE,  breaks = c(0, 50, 100, 150, 200, 250))+
  tm_layout()+
  tm_shape(poly_coast)+
  tm_polygons()+
  tm_shape(Canada)+
  tm_polygons()+
  tm_shape(lowfreqcoords)+
  tm_dots(size = 0.05)+
  tmap_options(max.raster = c(plot = 45955000, view = 45955000))

pugetsoundhighfreqgap <- crop(highfreqgap, ext_study_pugetsound)

orhighfreqgap <- crop(highfreqgap, ext_study_orcoast)

orhfgap <- tm_shape(orhighfreqgap)+
  tm_raster(palette = pal(10), colorNA = 'gray30', alpha = 0.8, auto.palette.mapping = FALSE, legend.show = FALSE,  breaks = c(0, 50, 100, 150))+
  tm_layout()+
  tm_shape(poly_coast)+
  tm_polygons()+
  tm_shape(Canada)+
  tm_polygons()+
  tm_shape(lowfreqcoords)+
  tm_dots(size = 0.05)+
  tmap_options(max.raster = c(plot = 45955000, view = 45955000))

wahighfreqgap <- crop(highfreqgap, ext_study_wacoast)

wahfgap <- tm_shape(wahighfreqgap)+
  tm_raster(palette = pal(10), colorNA = 'gray30', alpha = 0.8, auto.palette.mapping = FALSE, legend.show = FALSE,  breaks = c(0, 250, 500, 750, 1000, 1250))+
  tm_layout()+
  tm_shape(poly_coast)+
  tm_polygons()+
  tm_shape(Canada)+
  tm_polygons()+
  tm_shape(lowfreqcoords)+
  tm_dots(size = 0.05)+
  tmap_options(max.raster = c(plot = 45955000, view = 45955000))

hfgaplegend <- tm_shape(highfreqgap)+
  tm_raster(title = "Effective Distance to Nearest Monitoring (km)", palette = pal(10), colorNA = 'gray30', alpha = 1, auto.palette.mapping = FALSE, legend.show = TRUE)+
  tm_layout(main.title = "Effective Distance to Nearest Monitoring (km)", main.title.size = 1, bg.color = "white", main.title.position = c("center", "top"), fontfamily = "serif", fontface = "bold", legend.only = TRUE, legend.just = "center")+
  tm_shape(poly_coast)+
  tm_polygons()+
  tm_shape(Canada)+
  tm_polygons()+
  tm_grid(projection = "longlat", lwd = 0.1, n.x = 3, n.y = 6, labels.inside.frame = FALSE, labels.size = 0.6)
tmap_options(max.raster = c(plot = 45955000, view = 45955000))

socalhfgaplegend <- tm_shape(socalhighfreqgap)+
  tm_raster(title = "Effective Distance to Nearest Monitoring (km)", palette = pal(10), colorNA = NULL, alpha = 1, auto.palette.mapping = FALSE, legend.show = TRUE)+
  tm_layout(main.title = "Effective Distance to Nearest Monitoring (km)", main.title.size = 1, bg.color = "white", main.title.position = c("center", "top"), fontfamily = "serif", fontface = "bold", legend.only = TRUE, legend.just = "center")+
  tm_shape(poly_coast)+
  tm_polygons()+
  tm_shape(Canada)+
  tm_polygons()+
  tm_grid(projection = "longlat", lwd = 0.1, n.x = 3, n.y = 6, labels.inside.frame = FALSE, labels.size = 0.6)
tmap_options(max.raster = c(plot = 45955000, view = 45955000))

cencalhfgaplegend <- tm_shape(cencalhighfreqgap)+
  tm_raster(title = "Effective Distance to Nearest Monitoring (km)", palette = pal(10), colorNA = NULL, alpha = 1, auto.palette.mapping = FALSE, legend.show = TRUE)+
  tm_layout(main.title = "Effective Distance to Nearest Monitoring (km)", main.title.size = 1, bg.color = "white", main.title.position = c("center", "top"), fontfamily = "serif", fontface = "bold", legend.only = TRUE, legend.just = "center")+
  tm_shape(poly_coast)+
  tm_polygons()+
  tm_shape(Canada)+
  tm_polygons()+
  tm_grid(projection = "longlat", lwd = 0.1, n.x = 3, n.y = 6, labels.inside.frame = FALSE, labels.size = 0.6)
tmap_options(max.raster = c(plot = 45955000, view = 45955000))

norcalhfgaplegend <- tm_shape(norcalhighfreqgap)+
  tm_raster(title = "Effective Distance to Nearest Monitoring (km)", palette = pal(10), colorNA = NULL, alpha = 1, auto.palette.mapping = FALSE, legend.show = TRUE)+
  tm_layout(main.title = "Effective Distance to Nearest Monitoring (km)", main.title.size = 1, bg.color = "white", main.title.position = c("center", "top"), fontfamily = "serif", fontface = "bold", legend.only = TRUE, legend.just = "center")+
  tm_shape(poly_coast)+
  tm_polygons()+
  tm_shape(Canada)+
  tm_polygons()+
  tm_grid(projection = "longlat", lwd = 0.1, n.x = 3, n.y = 6, labels.inside.frame = FALSE, labels.size = 0.6)
tmap_options(max.raster = c(plot = 45955000, view = 45955000))

orhfgaplegend <- tm_shape(orhighfreqgap)+
  tm_raster(title = "Effective Distance to Nearest Monitoring (km)", palette = pal(10), colorNA = NULL, alpha = 1, auto.palette.mapping = FALSE, legend.show = TRUE)+
  tm_layout(main.title = "Effective Distance to Nearest Monitoring (km)", main.title.size = 1, bg.color = "white", main.title.position = c("center", "top"), fontfamily = "serif", fontface = "bold", legend.only = TRUE, legend.just = "center")+
  tm_shape(poly_coast)+
  tm_polygons()+
  tm_shape(Canada)+
  tm_polygons()+
  tm_grid(projection = "longlat", lwd = 0.1, n.x = 3, n.y = 6, labels.inside.frame = FALSE, labels.size = 0.6)
tmap_options(max.raster = c(plot = 45955000, view = 45955000))

wahfgaplegend <- tm_shape(wahighfreqgap)+
  tm_raster(title = "Effective Distance to Nearest Monitoring (km)", palette = pal(10), colorNA = NULL, alpha = 1, auto.palette.mapping = FALSE, legend.show = TRUE)+
  tm_layout(main.title = "Effective Distance to Nearest Monitoring (km)", main.title.size = 1, bg.color = "white", main.title.position = c("center", "top"), fontfamily = "serif", fontface = "bold", legend.only = TRUE, legend.just = "center")+
  tm_shape(poly_coast)+
  tm_polygons()+
  tm_shape(Canada)+
  tm_polygons()+
  tm_grid(projection = "longlat", lwd = 0.1, n.x = 3, n.y = 6, labels.inside.frame = FALSE, labels.size = 0.6)
tmap_options(max.raster = c(plot = 45955000, view = 45955000))


tmap_save(hfgap, "hf.png", width = 2000, height = 4000, dpi = 500)
tmap_save(socalhfgap, "socalhf.png", width = 5000, height = 3300, dpi = 500)
tmap_save(cencalhfgap, "cencalhf.png", width = 3000, height = 3500, dpi = 500)
tmap_save(norcalhfgap, "norcalhf.png", width = 3000, height = 3500, dpi = 500)
tmap_save(orhfgap, "orhf.png", width = 3000, height = 3500, dpi = 500)
tmap_save(wahfgap, "wahf.png", width = 4000, height = 3500, dpi = 500)

tmap_save(hfgaplegend, "hflegend.png", width = 3000, height = 2000, dpi = 500)
tmap_save(socalhfgaplegend, "socalhflegend.png", width = 3000, height = 2000, dpi = 500)
tmap_save(cencalhfgaplegend, "cencalhflegend.png", width = 3000, height = 2000, dpi = 500)
tmap_save(norcalhfgaplegend, "norcalhflegend.png", width = 3000, height = 2000, dpi = 500)
tmap_save(orhfgaplegend, "orhflegend.png", width = 3000, height = 2000, dpi = 500)
tmap_save(wahfgaplegend, "wahflegend.png", width = 3000, height = 2000, dpi = 500)

