setwd('~/Caminho...')

library(plyr)
library(dplyr)
library(ncdf4)
library(raster)
library(rasterVis)

australia <- paste0("root_depth/","maxroot_Australia_CF",".nc")
a.sul <- paste0("root_depth/","maxroot_South_America_CF",".nc")
africa <- paste0("root_depth/","maxroot_Africa_CF",".nc")
a.norte <- paste0("root_depth/","maxroot_North_America_CF",".nc")
europa <- paste0("root_depth/","maxroot_Europe_and_Asia_CF",".nc")

files <- c(australia, a.sul, africa, a.norte, europa)#Caminhos dos arquivos
titles <- c("Austr�lia", "Am�rica do Sul", "Africa", "Am�rica do Norte", "Europa e �sia")#Nomes que aparecer�o nos gr�ficos

#Paleta de cores para o mapa
cores <- colorRampPalette(c("white", "darkblue","blue","lightblue",
                            "darkgreen","green","yellow",
                            "orange","red","darkred"))

#Definir intervalos onde as cores mudam
my.at <- c(0, 0.0001, 0.2, 0.5, 1, 2, 5, 10, 20, 40)
my.brks <- seq(0,40,5)
myColorkey <- list(at=my.brks, #Aonde as cores mudam
                   labels=list(
                     at=my.brks,
                     labes=my.at[-2] #Aonde as legendas aparecem
                   ), space="right") #Posi��o da legenda ("right","left","top","bottom")


#Loop para todos os continentes
for(i in 2:length(files)){
  #Abrir o arquivo
  nc.in <- nc_open(files[i])
  #atribui��o das vari�veis
  fillvalue <- ncatt_get(nc.in,"root_depth", "_FillValue")  
  lon <- ncvar_get(nc.in, "lon")
  lat <- ncvar_get(nc.in, "lat")
  rtd <- ncvar_get(nc.in, "root_depth")
  #Colocar "NA" no formato do R
  rtd[rtd == fillvalue$value] <- NA
  
  #Consolidar as informa��es em um arquivo raster
  r <- raster(t(rtd),
              xmn=min(lon), xmx=max(lon),
              ymn=min(lat), ymx=max(lat)) %>%
    flip('y')
  
  #Salvar o arquivo raster para an�lise futuras
  #O nome do arquivo ser� o nome em "titles" com a exten��o ".tif"
  writeRaster(r, paste0("root_depth/TIFF/",titles[i]), "GTiff", overwrite=TRUE)
  
  #Salvar o gr�fico em PDF  
  pdf(paste0("root_depth/Mapas/Mapa_",titles[i],"_.pdf"))
  print(levelplot(r, margin = FALSE, col.regions = cores(100),
            at = my.at, colorkey = myColorkey,
            xlab="Latitude",
            ylab="Longitude",
            main=paste0("Profundidade de Ra�zes (m) na ",titles[i])))
  dev.off()
  
  #Limpar vari�veis para o p�ximo loop
  nc_close(nc.in)
  rm(r, nc.in, fillvalue, lon, lat, rtd)
}
