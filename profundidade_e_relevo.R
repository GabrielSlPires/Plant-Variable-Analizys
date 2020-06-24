setwd('~/Unicamp/Projeto Xilema/Clima')

library(raster)
library(geobr)
library(rasterVis)
library(fields)
library(sf)

#profundidade de raiz
root <- raster("root_depth/TIFF/América do Sul.tif")

#Relevo
r1 <- raster("relevo_sp/srtm_26_16/srtm_26_16.tif")
r2 <- raster("relevo_sp/srtm_26_17/srtm_26_17.tif")
r3 <- raster("relevo_sp/srtm_27_16/srtm_27_16.tif")
r4 <- raster("relevo_sp/srtm_27_17/srtm_27_17.tif")
r5 <- raster("relevo_sp/srtm_27_18/srtm_27_18.tif")
r6 <- raster("relevo_sp/srtm_28_17/srtm_28_17.tif")

relevo <- r1 %>% 
  mosaic(r2, fun=mean) %>% 
  mosaic(r3, fun=mean) %>% 
  mosaic(r4, fun=mean) %>% 
  mosaic(r5, fun=mean) %>% 
  mosaic(r6, fun=mean)

#cores
cores <- colorRampPalette(c("white", "darkblue","blue","lightblue",
                            "darkgreen","green","yellow",
                            "orange","red","darkred"))

col.relevo <- colorRampPalette(c('#bcd2a4','#89d2a4','#28a77e',
                                 '#ddb747','#fecf5b','#da9248',
                                 '#b75554','#ad7562','#b8a29a',
                                 '#9f9e98'))
#intervalos
my.at <- c(0, 0.0001, 0.2, 0.5, 1, 2, 5, 10, 20, 40)
my.brks <- seq(0,40,5)
myColorkey <- list(at=my.brks, ## where the colors change
                   labels=list(
                     at=my.brks,
                     labes=my.at[-2],
                     space="right"## where to print labels
                   ))


#São Paulo / root depth
sp <- read_state(code_state = 'SP', year = 2018)
r.sp <- crop(root, sp)
top.sp <- mask(r.sp, sp)
p <- levelplot(top.sp, margin = FALSE,
               col.regions = cores(100),
               at = my.at, colorkey = myColorkey,
               xlab="Latitude",
               ylab="Longitude",
               main="Profundidade de raízes no estado de São Paulo")
l <- latticeExtra::layer(sp.lines(as(sp$geom,Class = 'Spatial')))
#pdf(paste0("root_depth/Mapas/Mapa_",'SP',"_.pdf"))
p+l
#dev.off()

#São Paulo / Relevo
rel.sp <- crop(relevo, sp)
top.rel.sp <- mask(rel.sp, sp)
p <- levelplot(top.rel.sp, margin = FALSE,
               cuts = 80,
               col.regions = col.relevo(90),
               xlab="Latitude",
               ylab="Longitude",
               main="Relevo de São Paulo")
l <- latticeExtra::layer(sp.lines(as(sp$geom,Class = 'Spatial')))
#pdf(paste0("root_depth/Mapas/Mapa_",'SP',"_.pdf"))
p+l
#dev.off()
lookup_muni("Campinas")
#Campinas / root depth
cps <- read_municipality(3509502,2018)
r.cps <- crop(root, cps)
top.cps <- mask(r.cps, cps)
a <- levelplot(top.cps, margin = FALSE,
               col.regions = cores(100),
               at = my.at, colorkey = myColorkey,
               xlab="Latitude",
               ylab="Longitude",
               main="Profundidade de raízes em Campinas")
l <- latticeExtra::layer(sp.lines(as(cps$geom,Class = 'Spatial')))
#pdf(paste0("root_depth/Mapas/Mapa_",'SP',"_.pdf"))
a+l
#dev.off()

#Campinas / Relevo
rel.cps <- crop(relevo, cps)
top.rel.cps <- mask(rel.cps, cps)
p <- levelplot(top.rel.cps, margin = FALSE,
               cuts = 80,
               col.regions = col.relevo(90),
               xlab="Latitude",
               ylab="Longitude",
               main="Relevo da mesorregião de Campinas",
               alpha.regions = 1)
a+l
p+l
p+l+a
l <- latticeExtra::layer(sp.lines(as(cps$geom,Class = 'Spatial')))
#pdf(paste0("root_depth/Mapas/Mapa_",'SP',"_.pdf"))
p+l
#dev.off()
