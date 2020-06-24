setwd('~/Unicamp/Projeto Xilema/Clima')

library(xlsx)
library(raster)
library(geobr)
library(rasterVis)
library(fields)
library(sf)
library(maps)
library(ggplot2)
library(reshape2)
library(rgbif)


#Dados de Liu 2019
g_liu <- read.xlsx('raw files/liu2019.xlsx', 'full_data')

#tabela de coordenadas de Liu 2019
#Filtrar valores com coordendas
g_liu <- g_liu[is.na(g_liu$Xnew) != TRUE,]
x <- c(g_liu$Xnew)
y <- c(g_liu$Ynew)
name <- c(g_liu$Species)

liu_points <- data.frame(x, y, name)
coordinates(liu_points) <- ~ x + y


australia <- paste0("root_depth/","TIFF/Austrália",".tif")
a.sul <- paste0("root_depth/","TIFF/América do Sul",".tif")
africa <- paste0("root_depth/","TIFF/Africa",".tif")
a.norte <- paste0("root_depth/","TIFF/América do Norte",".tif")
europa <- paste0("root_depth/","TIFF/Europa e Ásia",".tif")

files <- c(australia, a.sul, africa, a.norte, europa)
titles <- c("Austrália", "América do Sul", "Africa", "América do Norte", "Europa e Ásia")

roots <- list()

for(i in 1:length(files)){
  roots[[i]] <- raster(files[i])
}

cores <- colorRampPalette(c("white", "darkblue","blue","lightblue",
                            "darkgreen","green","yellow",
                            "orange","red","darkred"))

  
my.at <- c(0, 0.0001, 0.2, 0.5, 1, 2, 5, 10, 20, 40)
my.brks <- seq(0,40,5)
myColorkey <- list(at=my.brks, ## where the colors change
                   labels=list(
                     at=my.brks,
                     labes=my.at[-2] ## where to print labels
                   ), space="right")


l <- latticeExtra::layer(sp.points(liu_points, cex = 1.5, col = "black", pch = 16))

plots <- list()
for(i in 1:length(files)){
  p <- levelplot(roots[[i]], margin = FALSE,
                 col.regions = cores(100),
                 at = my.at, colorkey = myColorkey,
                 xlab="Latitude",
                 ylab="Longitude",
                 main=paste("Localização dos indivíduos analisados \n por Liu et. al 2019 na",titles[i]))
  plots[[i]] <- p+l
}

plots[[1]]
plots[[2]]
plots[[3]]
plots[[4]]
plots[[5]]

g_liu$raiz <- rowSums(data.frame(raster::extract(roots[[1]],liu_points),
                                 raster::extract(roots[[2]],liu_points),
                                 raster::extract(roots[[3]],liu_points),
                                 raster::extract(roots[[4]],liu_points),
                                 raster::extract(roots[[5]],liu_points)),
                        na.rm = TRUE)


data.cor <- g_liu[,c(10:29,35)]#Colunas com classe = num
rownames(data.cor) <- as.character(g_liu[,1])#Coluna com nome das espécies

correlacao<-data.frame(matrix(ncol = 2, nrow = 0))
colnames(correlacao) <- c("Variáveis", "Correlações")

for(i in c(1:21)) {
  for (j in c(1:21)) {
    variaveis <- paste0(colnames(data.cor)[i], "/", colnames(data.cor)[j])
    df<-data.frame(variaveis, cor(data.cor[i],data.cor[j],use="complete.obs"), row.names = NULL)
    colnames(df) <- c("Variáveis", "Correlações")
    correlacao<-rbind(correlacao,df)
  }
}

write.csv(correlacao,"Correlação - Liu 2019.csv", row.names = FALSE)
