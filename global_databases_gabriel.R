setwd('~/Unicamp/Projeto Xilema/Clima')

#istall.packages("devtools") # if necessary
#devtools::install_github("ropenscilabs/datastorr")
#devtools::install_github("wcornwell/taxonlookup")

#devtools::install_github("thomasp85/patchwork")

library(patchwork)#Somar objetos ggplot

library(taxonlookup)
library(xlsx)
library(ggplot2)
library(rgbif)
library(plyr)
library(dplyr)
library(ncdf4)
library(raster)
library(rasterVis)


library("mapr")

#mean p50
g_choat <- read.xlsx('raw files/choat2012.xlsx', 'Table S1')

#mean p50 (include Choat 2012)
g_pausas <- read.xlsx('raw files/pausas2016.xlsx', 'Table S1')

#raw p50 (from Xylem Functional Trait database plus new entries, but filtered by tree heigth)
g_liu <- read.xlsx('raw files/liu2019.xlsx', 'full_data')

#mean wood density
g_chave <- read.xlsx('raw files/chave2009.xlsx', 'Data')

#add pit membrane thickness from XFT (needed to add Li et al. 2016)
g_li <- read.csv('pm_thickness.csv', header=T, sep=',', as.is=T)

#------------------------

#append liu and pausas, without NA values
g_p50_all <- na.omit(rbind(data.frame(g_liu$Species,g_liu$P50, fix.empty.names = FALSE),
                           data.frame(g_pausas$Species,g_pausas$Mean.Ï.50..with.all...MPa.,
                                      fix.empty.names = FALSE)))
colnames(g_p50_all) <- c('species', 'p50')

#calculate species mean p50
g_p50 <- merge(aggregate(x = g_p50_all$p50, by = list(g_p50_all$species), FUN = mean),
               aggregate(x = g_p50_all$p50, by = list(g_p50_all$species), FUN = sd),
               by = "Group.1")
colnames(g_p50) <- c("species", "p50", "p50_sd")

#mean wood density and SD
g_wd <- merge(aggregate(x = g_chave$Wood.density..g.cm.3...oven.dry.mass.fresh.volume,
                        by = list(g_chave$Binomial), FUN = mean),
              aggregate(x = g_chave$Wood.density..g.cm.3...oven.dry.mass.fresh.volume,
                        by = list(g_chave$Binomial), FUN = sd),
              by = "Group.1")
colnames(g_wd) <- c("species", "wd", "wd_sd")

#mean pit membrane thickness and SD
g_tpm <- merge(aggregate(x = g_li$pm_thickness, by = list(g_li$species), FUN = mean),
               aggregate(x = g_li$pm_thickness, by = list(g_li$species), FUN = sd),
               by = "Group.1")
colnames(g_tpm) <- c("species", "tpm", "tpm_sd")

#------------------------

#merge with p50
g_p50 <- merge(g_p50, g_wd, by = "species")
g_p50 <- merge(g_p50, g_tpm, by = "species")




#include group and families
g_id <- lookup_table(as.character(g_p50$species), by_species=TRUE)
#rowname as column
g_id$species <- rownames(g_id)
#merge with p50 database
g_p50 = merge(g_p50, g_id, by = "species", all.x = TRUE)


###find longitude/latitude
start.time <- Sys.time()
distribution <- occ_search(scientificName = as.character(g_p50$species),
                          fields = c('species','decimalLatitude','decimalLongitude'),
                          hasCoordinate = TRUE,
                          return = 'data')
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


#remove data not found
distribution <- na.omit(distribution[!grepl("no data found", distribution)])
distribution <- dplyr::bind_rows(distribution)
distribution <- distribution[!duplicated(distribution[,c('species','decimalLatitude', 'decimalLongitude')]),]
distribution <- data.frame(distribution)
names(distribution)[names(distribution) == "decimalLongitude"] <- "lon"
names(distribution)[names(distribution) == "decimalLatitude"] <- "lat"
 
#g_p50 <- merge(g_p50, distribution, by = "species") 


#Split Angiosperm and Gyminosperm data
g_angi <- g_p50[g_p50$group == "Angiosperm",]
g_gimi <- g_p50[g_p50$group == "Gyminosperm",]



#write.csv(distribution, "distribution.csv")

#distribution <- read.csv('distribution.csv', header=T, sep=',', as.is=T)

coordinates(distribution) <- ~ lon + lat

distribution$raiz <- rowSums(data.frame(raster::extract(roots[[1]],distribution),
                                        raster::extract(roots[[2]],distribution),
                                        raster::extract(roots[[3]],distribution),
                                        raster::extract(roots[[4]],distribution),
                                        raster::extract(roots[[5]],distribution)),
                             na.rm = TRUE)

g_root <- merge(aggregate(x = distribution$raiz, by = list(distribution$species), FUN = mean),
                aggregate(x = distribution$raiz, by = list(distribution$species), FUN = sd),
                by = "Group.1")
colnames(g_root) <- c("species", "root", "root_sd")

g_p50 <- merge(g_p50, g_root, by = "species") 


world<-map_data('world')
m <- ggplot(legend=FALSE) +
  geom_polygon( data=world, aes(x=long, y=lat,group=group)) 

#Disperção de indivíduos
m + geom_point(data = as.data.frame(distribution),aes(lon,lat,colour=species),size=1)+
  labs(title = "Disperção de espécies analisados")+
  xlab("Longitude")+
  ylab("Latitude")


ggplot(as.data.frame(distribution),aes(species,raiz))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(width = 0.2)+
  ylim(0,20)+
  theme(axis.text.x = element_text(angle = 90, hjust=1, vjust=0.2))

ggplot(as.data.frame(distribution),aes(raiz))+
  geom_histogram()+
  facet_wrap(~species)+
  xlim(0,30)+
  ylim(0,75)
  theme(axis.text.x = element_text(angle = 90, hjust=1, vjust=0.2))


#g_p50_bkp <- g_p50  

g_p50 <- g_p50_bkp[,g_p50_bkp$species != "Ceanothus cuneatus" |
                     g_p50_bkp$species != "Ceanothus oliganthus" |
                     g_p50_bkp$species != "Frangula californica" |
                     g_p50_bkp$species != "Olea europaea" |
                     g_p50_bkp$species != "Quercus ilex" |
                     g_p50_bkp$species != "Rhamnus crocea" |
                     g_p50_bkp$species != "Umbellularia californica"]


corr_eqn <- function(x,y, digits = 2) {
  corr_coef <- round(cor(x, y), digits = digits)
  paste("italic(r) == ", corr_coef)
}


label_tpm = data.frame(x = 3, y = 3, label = corr_eqn(g_p50$root, g_p50$tpm))
label_p50 = data.frame(x = 3, y = 3, label = corr_eqn(g_p50$root, g_p50$p50))
label_wd = data.frame(x = 3, y = 3, label = corr_eqn(g_p50$root, g_p50$wd))

p_tpm <- ggplot(g_p50, aes(x= root, y= tpm))+
  geom_point()+
  geom_pointrange(aes(xmax=root+root_sd,xmin=root-root_sd))+
  geom_smooth(method = lm)+
  geom_text(data = label_tpm, aes(x = x, y = 293*y,
                                  label = label),parse = TRUE)+
  ggtitle("Expessura das pit membranas")+
  xlab("Root Depth")+
  ylab("Pit Membrane Thickness")

p_p50 <- ggplot(g_p50, aes(x= root, y= p50))+
  geom_point()+
  geom_smooth(method = lm)+
  geom_text(data = label_p50, aes(x = x, y = y,
                                  label = label), parse = TRUE)+
  ggtitle("P50")+
  xlab("Root Depth")+
  ylab("P50")

p_wd <- ggplot(g_p50, aes(x= root, y= wd))+
  geom_point()+
  geom_smooth(method = lm)+
  geom_text(data = label_wd, aes(x = x, y = y,
                                 label = label), parse = TRUE)+
  ggtitle("Densidade da madeira")+
  xlab("Root Depth")+
  ylab("Wood Density")

p_tpm + p_p50 + p_wd

#------------------------

data.pca <- g_p50[,c(2,4,6,12)]
rownames(data.pca) <- g_p50[,1]

pca <- prcomp(data.pca, scale. = TRUE)

pca.var <- pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)

barplot(pca.var.per, main="Scree Plot", xlab="Principal Component", ylab="Percent Variation")

result.pca <- merge(data.frame("species"=rownames(pca$x),
                               w=pca$x[,1],
                               X=pca$x[,2],
                               Y=pca$x[,3],
                               Z=pca$x[,4]),
                    g_p50[,c(1,8,9,10,11)],
                    by = "species")

ggplot(result.pca, aes(x= X, y= Y, colour= family)) +
  geom_point()+
  xlab(paste("PC1 - ", pca.var.per[1], "%", sep="")) +
  ylab(paste("PC4 - ", pca.var.per[4], "%", sep="")) +
  theme_bw() +
  ggtitle("My PCA Graph")

#https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html
 

 #find root_depth for this locations
 
tmp<-data.frame(x=g_p50[,12],
           y=g_p50[,13],
           z=g_p50[,2])

raster(g_p50)

library("reshape2")

acast(tmp,x~y,value.var='z')


#Continuar aqui------------------------------------------------------------------------------------------


africa <- paste("root_depth/", "maxroot_Europe_and_Asia_CF", ".nc", sep = "")
ncin <- nc_open(africa)
lon <- ncvar_get(ncin, "lon")
nlon <- dim(lon)
lat <- ncvar_get(ncin, "lat", verbose = F)
nlat <- dim(lat)
t <- ncvar_get(ncin, "root_depth")
lonlat <- expand.grid(lon, lat)
t_vector <- as.vector(rtd)
t_matrix <- matrix(t_vector, nrow = dim(lon) * dim(lat))#, ncol = nt)
root_depth <- data.frame(cbind(lonlat, t_matrix)) #options(width = 110)

head(root_depth)





 # #change to data.table and delete NA
 root_depth <- data.table(root_depth)
 #change column names
 names(root_depth)[names(root_depth) == "Var1"] <- "lon"
 names(root_depth)[names(root_depth) == "Var2"] <- "lat"
 names(root_depth)[names(root_depth) == "t_matrix"] <- "root_depth"
 #round lat and lon for 2 digits
 root_depth$lat <- round(root_depth$lat, digits = 2)
 root_depth$lon <- round(root_depth$lon, digits = 2)
 # #calculate mean root_depth at the same lat-lon
 root_depth1 <- setDT(root_depth)[ , .(mean_root_depth = mean(root_depth)), by = c("lon","lat")]
 #round to 2 digits the lat-lon from distribution database
 distribution$lat <- round(distribution$lat, digits = 2)
 distribution$lon <- round(distribution$lon, digits = 2)
 # merge distribution with root_depth
 root1 = merge(distribution, root_depth1, by = c("lon", "lat"))
 #root1 <- na.omit(root1)
 write.csv(root1,'C:/Users/Dri/Documents/OneDrive/Documentos/Projetos/Global_databases/databases/europe_asia.csv')

 ## merge .csv files
 library(dplyr)

 files<- list.files(path = 'C:/Users/Dri/Documents/OneDrive/Documentos/Projetos/Global_databases/databases/', pattern = "*.csv", full.names = T)
 root_depth_all <- sapply(files, read.csv, simplify = FALSE) %>%
   bind_rows(.id = "id")
 write.csv(root_depth_all,'C:/Users/Dri/Documents/OneDrive/Documentos/Projetos/Global_databases/databases/root_depth_p50_database.csv')

setwd('C:/Users/Dri/Documents/OneDrive/Documentos/Projetos/Global_databases/databases')
root_depth <- read.csv('root_depth_p50_database.csv', header=T, sep=',', as.is=T)

#and merge with p50_wd database
p50_root = merge(root_depth, p50_database, by = "species", all.x = FALSE)
head(p50_root)

# graph from raw values
library("ggpubr")
ggscatter(p50_root, x = "mean_root_depth", y = "p50", xlim=c(0,100),  ylim=c(0,-15),
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "root_depth", ylab = "p50")

############# VER SE USAR DADOS BRUTOS É MELHOR QUE MEDIA
root_depth1 <- aggregate(x = root_depth$mean_root_depth, by = list(root_depth$species), FUN = mean, na.rm = TRUE)
names(root_depth1)[names(root_depth1) == "Group.1"] <- "species"
names(root_depth1)[names(root_depth1) == "x"] <- "root_depth"
head(root_depth1)

p50 <- aggregate(x = p50_database$p50, by = list( p50_database$species), FUN = mean, na.rm = TRUE)
names(p50)[names(p50) == "Group.1"] <- "species"
names(p50)[names(p50) == "x"] <- "root_depth"
head(p50)
p50_mean1 = merge(root_depth1, p50, by = "species", all.x = FALSE)
head(p50_mean1)
names(p50_mean1)[names(p50_mean1) == "root_depth.x"] <- "mean_root_depth"
names(p50_mean1)[names(p50_mean1) == "root_depth.y"] <- "p50"

library("ggpubr")
ggscatter(p50_mean1, x = "mean_root_depth", y = "p50", xlim=c(0,15),  ylim=c(0,-15),
          # add = "reg.line", conf.int = TRUE, 
          # cor.coef = TRUE, cor.method = "pearson",
          xlab = "root_depth", ylab = "p50")


#quantile regression p50 x root_depth
library(quantreg)
library(dplyr)
#eliminar primeiros pontos para ver se dá regressão quantílica... só que não...
p50_mean2 <- p50_mean1 %>% filter(p50_mean1$mean_root_depth > 4)
head(p50_mean2)

fit1 <- rq(p50_mean2$p50 ~ p50_mean2$mean_root_depth, tau= c(0.05, .25, .5, .75, .85, .9, .91, .95))
summary(fit1, se='nid')

plot(p50_mean2$mean_root_depth,p50_mean2$p50,cex=.25,type="n",xlab="Root depth (m)", ylab=expression(paste(psi[50]," (MPa)")))
points(p50_mean2$mean_root_depth,p50_mean2$p50,cex=.5,col="blue")
abline(rq(p50_mean2$p50~p50_mean2$mean_root_depth,tau=.05),col="blue")
abline(lm(p50_mean2$p50~p50_mean2$mean_root_depth),lty=2,col="red") #the dreaded ols line
taus <- c(.05,.1,.25,.75,.90,.95)
for( i in 1:length(taus)){
  abline(rq(p50_mean2$p50~p50_mean2$mean_root_depth,tau=taus[i]),col="gray")
}

#quantile regression p50 x wood density
head(p50_wd)

fit2 <- rq(p50_wd$p50 ~ p50_wd$wd_mean, tau= c(0.05, .25, .5, .75, .85, .9, .91, .95))
summary(fit2, se='nid')

plot(p50_wd$wd_mean,p50_wd$p50,cex=.25,type="n",xlab="Lignin content (%)", ylab=expression(paste(psi[50]," (MPa)")))
points(p50_wd$wd_mean,p50_wd$p50,cex=.5,col="blue")
abline(rq(p50_wd$p50~p50_wd$wd_mean,tau=.05),col="blue")
abline(lm(p50_wd$p50~p50_wd$wd_mean),lty=2,col="red") #the dreaded ols line
taus <- c(.05,.1,.25,.75,.90,.95)
for( i in 1:length(taus)){
  abline(rq(p50_wd$p50~p50_wd$wd_mean,tau=taus[i]),col="black")
}

##### usar essas duas equações para filtrar espécies com P50 estimado de WD ????????????????????

##################
##Soil database

system("wget http://webarchive.iiasa.ac.at/Research/LUC/External-World-soil-database/HWSD_Data/HWSD_RASTER.zip")
require(raster)
hwsd <- raster("C:/Users/Dri/Documents/OneDrive/Documentos/Projetos/Global_databases/r/soil_database/hwsd.bil")
proj4string(hwsd) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
save(hwsd, file = "hwsd.RData")


install.packages("remotes")
remotes::install_github("dlebauer/rhwsd")

library(rhwsd)
lat <- 44
lon <- -80
gridsize <- 0.1
ans <- extract.one(box = c(44, 44.5, -88.5, -88))


