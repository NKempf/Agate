#----------------------------------------------------------------------------------------------------------------------#
#                                   Agata - Statistical calculation improvement                                        #
#----------------------------------------------------------------------------------------------------------------------#

# 21.12.2018

# Improve statistical calculation of indicators

# Packages nécessaires
#---------------------
library(rgdal) # Input shapefile
library(rgeos) # Map tools
library(tidyverse) # Tidy data
library(fst) # Read partial data
library(fstplyr) # dplyr for fst object

source("Other programs/Zonage/Agate - Cartographie fct.R",encoding = "UTF-8")
source("Other programs/StatZonage/Agate - Statistics Zonage_v4.R",encoding = "UTF-8")

# Statistical summary about cities and territory
if(file.exists("Data/Statistiques Zonage/StatRegCom_rp14_filo14.RData")){
  load("Data/Statistiques Zonage/StatRegCom_rp14_filo14.RData")  # Real data
  com.stat <- statCom_rp14_filo14
  dep.stat <- statReg_rp14_filo14
}else{
  load("Data/Stats/Region and cities/region_stat.RData") # Fake region and cities stat
}

# O. Selection des bases de travail (donnees reelles ou fausses)
#---------------------------------------------------------------
# Ril
rilPath <- ifelse(file.exists("Data/Ril/ril15.fst"),"Data/Ril/ril15.fst","Data/Ril/FakeRil.fst")
# RP
rpiPath <- ifelse(file.exists("Data/Rp/rp13i.fst"),"Data/Rp/rp13i.fst","Data/Rp/FakeRpi.fst")
rplPath <- ifelse(file.exists("Data/Rp/rp13l.fst"),"Data/Rp/rp13l.fst","Data/Rp/FakeRpl.fst")
# Filosofi
filoPath <- ifelse(file.exists("Data/Filosofi/filo14.fst"),"Data/Filosofi/filo14.fst","Data/Filosofi/FakeFilo.fst")

# I. Preparation du zonage
#-------------------------
# I.1. Creation de la variable zonage
zonage <- readOGR(dsn = "../Data/Maps/QPV/qpv.shp")[1:2,]
zonage@data <- zonage@data %>% 
  rename(idZonage = CODE_QP)
zonage <- spTransform(zonage, "+init=epsg:3857")

# I.2. Creation de l'identifiant idZonage
zonage@data$idZonage <- zonage@data$idZonage

# II. Logements géolocalisés du RIL 
#----------------------------------
load("Data/Maps/Cities/cities.RData") # Cities map
# II.1 Communes dans lesquelles se trouvent une ou plusieurs zones
zoneInter <- gIntersects(zonage,com.dom,byid = TRUE)
test <- apply(zoneInter, 1, function(x){
  test <- sum(x)
  return(ifelse(test>0,TRUE,FALSE))}) 
com.dom.select <- com.dom@data$Codgeo[test]

# II.2. Chargement des logements du RIL dans les communes d'interets
ril <- read_fst(rilPath) %>% 
  select(idx,x,y) %>% 
  mutate(com = substr(idx,1,5)) %>%
  filter(com %in% com.dom.select)

# II.3. Transformation du ril en objet spatial
coordinates(ril) <- ~x+y
ril@proj4string <- CRS("+init=epsg:3857")

# III. Ajout de la zone aux données du recensement
#-------------------------------------------------

# III.1. Zone dans laquelle chaque logement se situe
incProgress(amount = 0.1,message = "Zone dans laquelle chaque logement se situe")
pts.sp <- zonaPts(pts.sp = ril,zonage = zonage)

# III.2. Ajout de la zone aux données du rp individu
# Note : pour des raisons de performances, les données du RP sont préalablement filtrées selon les communes étudiées
incProgress(amount = 0.2,message = "Ajout de la zone aux données du RP")

rpi <- read_fst(rpiPath) %>% 
  filter(idx %in% ril@data$idx) %>% 
  left_join(pts.sp@data[,c("idx","idZonage")], "idx") %>% 
  mutate(idZonage = ifelse(is.na(idZonage),"Hors zonage", idZonage))

# III.3. Ajout de la zone aux données du rp logement
rpl <- read_fst(rplPath) %>% 
  filter(idx %in% ril@data$idx) %>% 
  left_join(pts.sp@data[,c("idx","idZonage")], "idx") %>% 
  mutate(idZonage = ifelse(is.na(idZonage),"Hors zonage", idZonage))

# IV. Ajout de la zone aux données fiscales
#------------------------------------------

# IV.1. Chargement des données fiscales filtrées selon les communes d'intérêts
incProgress(amount = 0.3,message = "Ajout des données fiscales")
filo <- read_fst(filoPath) %>% 
  filter(com %in% com.dom.select)

# IV.2. Transformation des données en objet spatial
filo.sp <- SpatialPointsDataFrame(coords = filo[,c("x","y")],data = filo,proj4string = CRS("+init=epsg:3857"))

# IV.3. Zone dans laquelle chaque foyer fiscal se situe
filo.sp <- zonaPts(pts.sp = filo.sp,zonage = zonage)

# IV.4. Ajout de la zone aux données fiscales
typmen.label <- c("famille monoparentale","couple sans enfant","couple avec enfant(s)","menage complexe",
                  "femme seule","homme seul")

filo <- filo.sp@data %>% 
  left_join(data.frame(unique(rpl[,c("com","com.lib")])),"com") %>% 
  mutate(dep = substr(com,1,3),
         idZonage = ifelse(is.na(idZonage),"Hors zonage", idZonage),
         typmenR.lib = factor(typmenR,labels = typmen.label))
rm(filo.sp)

# Enregistrement des bases nécessaires
save(zonage,rpi,rpl,filo,file="Data/Tmp/FakeDataImproveCalcul.RData")



# V. Calcul des indicateurs statistiques
#---------------------------------------
incProgress(amount = 0.4,message = "Calcul des statistiques")

# V.1. Statistiques dans la zone
rv$statZone <- statistics_zone(rpi = rpi,rpl = rpl,filo = filo,group_var = c("idZonage"))

# V.2. Statistiques communales hors zone
rv$statHZone <- statistics_zone(rpi = rpi,rpl = rpl,filo = filo,group_var = c("com","com.lib","idZonage"))





