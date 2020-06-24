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



#profundidade de raiz
root <- raster("root_depth/TIFF/América do Sul.tif")

#mean p50
g_choat <- read.xlsx('raw files/choat2012.xlsx', 'Table S1')

brasil <- g_choat[g_choat$Country == "Brasil",2:8]

g_liu <- read.xlsx('raw files/liu2019.xlsx', 'full_data')


world<-map_data('world')
m <- ggplot(legend=FALSE) +
  geom_polygon( data=world, aes(x=long, y=lat,group=group)) 

#Disperção de indivíduos analisados por Choat et. al 2012
m + geom_point(data=g_choat,aes(Longitude,Latitude),colour="yellow",size=2)+
  labs(title = "Disperção de indivíduos analisados por Choat et. al 2012")
#Disperção de indivíduos analisados por Liu et. al 2019
m + geom_point(data=g_liu,aes(Xnew,Ynew),colour="yellow",size=2)+
  labs(title = "Disperção de indivíduos analisados por Liu et. al 2019")


#Tabela de coordenadas de Choat 2012
x <- c(g_choat$Longitude)
y <- c(g_choat$Latitude)
name <- c(g_choat$Species)

choat_points <- data.frame(x, y, name)
coordinates(choat_points) <- ~ x + y

#tabela de coordenadas de Liu 2019
#Filtrar valores com coordendas
g_liu <- g_liu[is.na(g_liu$Xnew) != TRUE,]
x <- c(g_liu$Xnew)
y <- c(g_liu$Ynew)
name <- c(g_liu$Species)

liu_points <- data.frame(x, y, name)
coordinates(liu_points) <- ~ x + y


#Paleta de cores profundidade de raízes
cores <- colorRampPalette(c("white", "darkblue","blue","lightblue",
                            "darkgreen","green","yellow",
                            "orange","red","darkred"))

#intervalos de cores profundidade de raízes
my.at <- c(0, 0.0001, 0.2, 0.5, 1, 2, 5, 10, 20, 40)
my.brks <- seq(0,40,5)
myColorkey <- list(at=my.brks, ## where the colors change
                   labels=list(
                     at=my.brks,
                     labes=my.at[-2] ## where to print labels
                   ))

#Localização dos indivíduos anaalizados
p <- levelplot(root, margin = FALSE,
               col.regions = cores(100),
               at = my.at, colorkey = myColorkey,
               xlab="Latitude",
               ylab="Longitude",
               main="Localização dos indivíduos analisados \n por Choat et. al 2012")
l <- latticeExtra::layer(sp.lines(as(sp$geom,Class = 'Spatial')))
#Analisados por Choat 2012
l1 <- latticeExtra::layer(sp.points(choat_points, cex = 1.5, col = "black", pch = 16))
p+l1
# Pesquisar: plotar com multiplos layers -> p+l+l1
#Analisasos por Liu 2019
l2 <- latticeExtra::layer(sp.points(liu_points, cex = 1.5, col = "black", pch = 16))
p+l2

#Extraindo valores de profundidade de raízes
g_choat$raiz <- raster::extract(root,choat_points)
g_liu$raiz <- raster::extract(root,liu_points)

#Analise de correlação de variveis com a profundidade de raízes em choat 2012
cor(g_choat$raiz,g_choat$Ï.50..Mpa., use="complete.obs")
cor(g_choat$raiz,g_choat$Ï.88..Mpa., use="complete.obs")
cor(g_choat$raiz,g_choat$MAT..mean.annual.temperature...Â.C., use="complete.obs")
cor(g_choat$raiz,g_choat$MAP..mean.annual.precipitation...mm., use="complete.obs")
cor(g_choat$raiz,g_choat$AI..aridity.index., use="complete.obs")
cor(g_choat$raiz,g_choat$Ï.50.safety.margin..Mpa., use="complete.obs")
cor(g_choat$raiz,g_choat$Ï.88.safety.margin..Mpa., use="complete.obs")
cor(g_choat$raiz,g_choat$Elevation..m., use="complete.obs")

str(data.pca)

data.pca <- g_liu[,c(10:29,35)]#Colunas com classe = num
rownames(data.pca) <- as.character(g_liu[,1])#Coluna com nome das espécies

pca <- prcomp(t(data.pca), scale. = TRUE, na.action = na.omit)

pca.var <- pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)

barplot(pca.var.per, main="Scree Plot", xlab="Principal Component", ylab="Percent Variation")

plot(pca$x[,1],pca$x[,2])
#------ ainda em teste
result.pca <- merge(data.frame("species"=rownames(pca$x),
                               X=pca$x[,1],
                               Y=pca$x[,2],
                               Z=pca$x[,3]),
                    g_p50[,c(1,8,9,10,11)],
                    by = "species")

ggplot(result.pca, aes(x= X, y= Y, colour= family)) +
  geom_point()+
  xlab(paste("PC1 - ", pca.var.per[1], "%", sep="")) +
  ylab(paste("PC2 - ", pca.var.per[2], "%", sep="")) +
  theme_bw() +
  ggtitle("My PCA Graph")


ggplot(g_p50, aes(x= tpm, y= wd, colour= p50))+
  geom_point()+
  geom_smooth(method = lm)

cor.test(g_p50$wd, g_p50$tpm)

#------- até aqui



ggplot(g_choat, aes(raiz, Ï.50..Mpa., colour = Location )) +
  geom_point()+
  theme(legend.position = "none")+
  geom_smooth(method='lm')+
  xlab("Produndiade do Sistema Radicular")+
  ylab("P50")+
  labs(title = "Usando disperção de espécies de Choat et al. 2012")
  

teste_choat <- melt(g_choat[,c(3,4,5,7,8,9,10,11,12,13,24)], id.vars="raiz")



ggplot(teste_choat, aes(raiz, value, fill=variable)) + 
  geom_point()+
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "left",
        panel.background = element_rect(fill = "white", colour = "grey10"),
        panel.border = element_rect(linetype = "solid", fill=NA),
        panel.grid.major.y = element_line(colour = "grey50"),
        panel.grid.minor.y = element_line(colour = "grey50"),
        plot.title = element_text(size = rel(2)))+
  facet_wrap(~variable)+
  geom_smooth(method = "lm")+
  xlab("Produndiade do Sistema Radicular")+
  ylab("value")+
  labs(title = "Variáveis de Choat et al 2012")
  
  

ggplot(liu2, aes(raiz, name)) + 
  geom_point()+
  geom_smooth(method = "lm")+
  xlab("Produndiade do Sistema Radicular")+
  ylab("P50")+
  labs(title = "Usando disperção de espécies de Liu et al. 2019")


ggplot(teste_choat, aes(raiz, value)) + 
  geom_boxplot()
  geom_smooth(method = "lm")+
  xlab("Produndiade do Sistema Radicular")+
  ylab("P50")+
  labs(title = "Usando disperção de espécies de Liu et al. 2019")



###find longitude/latitude
start.time <- Sys.time()
distribution <- occ_search(scientificName = as.character(brasil$Species),
                           fields = c('species','decimalLatitude','decimalLongitude'),
                           hasCoordinate = TRUE,
                           return = 'data')
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

distribution <- na.omit(distribution[!grepl("no data found", distribution)])
distribution <- dplyr::bind_rows(distribution)
distribution <- distribution[!duplicated(distribution[,c('species','decimalLatitude', 'decimalLongitude')]),]
distribution <- data.frame(distribution)
names(distribution)[names(distribution) == "decimalLongitude"] <- "lon"
names(distribution)[names(distribution) == "decimalLatitude"] <- "lat"

dista <- merge(brasil, distribution, by.x = "Species", by.y = "species") 

m + geom_point(data=distribution,aes(lon,lat, colour = species),size=2)

x <- c(dista$lon)
y <- c(dista$lat)
name <- c(dista$Species)

distb <- data.frame(x, y, name) %>% 
  na.omit
coordinates(distb) <- ~ x + y


dista$raiz <- raster::extract(root,distb)

ggplot(dista, aes(raiz, dista$Ï.50..Mpa.))+
  geom_point()+
  xlab("Produndiade do Sistema Radicular")+
  ylab("P50")+
  labs(title = "Usando disperção de espécies do GBIF")+
  geom_smooth(method = "lm")


