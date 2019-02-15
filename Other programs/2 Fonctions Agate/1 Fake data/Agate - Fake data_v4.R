#--------------------------------------------------------------------------------------------------------------------------------#
#                     Agate - RP and Filo Fake data                                                                              #
#--------------------------------------------------------------------------------------------------------------------------------#

# MAJ : 15.12.2018

# Nicolas Kempf

# Fake data with same model data as real data. 

### Working directory 
# Paramètre automatiquement le dossier dans lequel se trouve le programme comme espace de travail
# Ne fonction que pour une version de R supérieure à 3.3
# setwd(dir=dirname(rstudioapi::getActiveDocumentContext()$path))
# getwd()

t1 <- Sys.time()

# Packages nécessaires
#---------------------
library(sp) # Objets spatiaux
# library(rgdal) # Lecture d'objets spatiaux
library(rgeos) # Transformation objets spatiaux
library(tidyverse) # transform data
library(fst) # Partial data
# library(writexl) # Export des données en excel

# Fonctions utiles
#-----------------
source ("Other programs/Fake data/Agate - Fake data fct.R")
source("Other programs/Zonage/Agate - Cartographie fct.R")
# source("Other programs/StatZonage/Agate - Statistics Zonage_v3.R")

# I. Import des fausses bases
#-------------------------------------------------------------------------------------------------
load("Data/fakePts.RData")
load("Data/fakeData.Rdata")

# Modification de la variable identifiant idx (il faut que les 5 premiers caracteres soient le code commune)
pts.fake@data %>% 
  mutate(idx = paste(com,idx,sep="_")) -> pts.fake@data

# Filosofi
#---------
# typmen.label <- c("famille monoparentale","couple sans enfant","couple avec enfant(s)","menage complexe",
#                   "femme seule","homme seul")
# filo <- as_tibble(filo) %>%
#   # filter(com %in% lstCom) %>%
#   mutate(typmenR.lib = factor(typmenR,labels = typmen.label))

# II. Construction des fausses bases de données
#-------------------------------------------------------------------------------------------------
lst_var <- c("idx","dep","com","com.lib")
rpi <- cbind(pts.fake@data[,lst_var],faking_data(rpi,nrow(pts.fake)))
rpl <- cbind(pts.fake@data[,lst_var],faking_data(rpl,nrow(pts.fake)))

# Attention penser à ajouter les variables x et y 
filo <- cbind(pts.fake@data[,c(lst_var,"x","y")],faking_data(filo,nrow(pts.fake))) %>% 
  select(-com.lib)


# III. Enregistrement au format RData et fst
#--------------------------------------------------------------------------------------------------

# III.1. Fake Ril
fakeRil <- pts.fake@data
write_fst(x = fakeRil,path = "Data/Ril/FakeRil.fst")

# III.2. RP individu
write_fst(x = rpi,path = "Data/Rp/FakeRpi.fst")

# III.3. Rp logement
write_fst(x = rpl,path = "Data/Rp/FakeRpl.fst")

# III.4. Filosofi
write_fst(x = filo,path = "Data/Filosofi/FakeFilo.fst")


# IV. Test de la chaine avec les fausses données
#----------------------------------------------------------------------------------------------------

library(fstplyr) # dplyr for fst object

source("Other programs/StatZonage/Agate - Statistics Zonage_v4.R",encoding = "UTF-8")
source("Other programs/Zonage/Agate - Cartographie fct.R",encoding = "UTF-8")

load("Data/Maps/Cities/cities.RData") # Carte des villes des DOM
load("Data/QPV/qpvFake.RData") # Carte des qpv comme exemple de zone

qpv_stat.fake@data %>% 
  select(CODE_QP,NOM_QP) %>% 
  rename(idZonage = CODE_QP) -> qpv_stat.fake@data

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
zonage <- qpv_stat.fake
zonage <- spTransform(zonage, "+init=epsg:3857")

# I.2. Creation de l'identifiant idZonage
# zonage@data$idZonage <- zonage@data$idZonage

# II. Logements géolocalisés du RIL 
#----------------------------------

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
# incProgress(amount = 0.1,message = "Zone dans laquelle chaque logement se situe")
pts.sp <- zonaPts(pts.sp = ril,zonage = zonage)

# III.2. Ajout de la zone aux données du rp individu
# Note : pour des raisons de performances, les données du RP sont préalablement filtrées selon les communes étudiées
# incProgress(amount = 0.2,message = "Ajout de la zone aux données du RP")

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
# incProgress(amount = 0.3,message = "Ajout des données fiscales")
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

# V. Calcul des indicateurs statistiques
#---------------------------------------
# incProgress(amount = 0.4,message = "Calcul des statistiques")

# V.1. Statistiques dans la zone
statZone <- statistics_zone(rpi = rpi,rpl = rpl,filo = filo,group_var = c("idZonage"))

# V.2. Statistiques communales hors zone
statHZone <- statistics_zone(rpi = rpi,rpl = rpl,filo = filo,group_var = c("com","com.lib","idZonage"))

statZone$tFilo.I.1








