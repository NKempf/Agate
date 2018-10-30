# Modification du calcul de statistiques pour gain de temps.
# Idee 1 : detecter la ou les communes pour ne charger que les données correspondantes
# Idee 2 : charger les données du

library(rgdal) # Chargement de shape
library(rgeos) # Opérations cartographiques
library(fst) # Lecture partielle de table
library(fstplyr) # Dplyr pour les tables fst

source("Other programs/Zonage/Agate - Cartographie fct.R",encoding = "UTF-8")


# chargement des communes
load("Data/Maps/Cities/cities.RData")
# load("Data/Maps/Region/region.RData")

# Chargement des polygones de tests
zoneTest <- readOGR("Data/Maps/Infra test/POLYGON.shp",stringsAsFactors = FALSE)

# Changement du systeme de projection
zoneTest <- spTransform(zoneTest, "+init=epsg:3857")

# Tester l'intersection entre les communes et les zones test
zoneInter <- gIntersects(zoneTest,com.dom,byid = TRUE)

# Selection des communes dans lesquelles se trouvent une zone de test
test <- apply(zoneInter, 1, function(x){
  test <- sum(x)
  return(ifelse(test>0,TRUE,FALSE))}) 
com.dom.select <- com.dom@data$Codgeo[test] 
   
# Chargement des points du RIL des communes concernées
ril <- read_fst("Data/Ril/ril15.fst") %>% 
  select(idx,x,y) %>% 
  mutate(com = substr(idx,1,5)) %>%
  filter(com %in% com.dom.select)

# Transformation du ril en objet spatial
coordinates(ril) <- ~x+y
ril@proj4string <- CRS("+init=epsg:3857")

# Creation de la variable zonage
zonage <- zoneTest

# Creation de l'identifiant idZonage
zonage@data$idZonage <- zonage@data$id

# Determine pour chaque point dans quel zone il se situe
# incProgress(amount = 0.1,message = "Appariement entre la zone et les points")
pts.sp <- zonaPts(pts.sp = ril,zonage = zonage)

# I. Traitement cartographique avec les vraies données
#-----------------------------------------------------
# incProgress(amount = 0.4,message = "Ajout des données du RP")

# RP individu (chargement des données et ajout de la variable idZonage)
rpi <- read_fst("Data/Rp/rp14i.fst") %>% 
  filter(idx %in% ril@data$idx) %>% 
  left_join(pts.sp@data[,c("idx","idZonage")], "idx") %>% 
  mutate(idZonage = ifelse(is.na(idZonage),"Hors zonage", idZonage))

# Rp logement
rpl <- read_fst("Data/Rp/rp14l.fst") %>% 
  filter(idx %in% ril@data$idx) %>% 
  left_join(pts.sp@data[,c("idx","idZonage")], "idx") %>% 
  mutate(idZonage = ifelse(is.na(idZonage),"Hors zonage", idZonage))

# Fichiers fiscaux
filo <- read_fst("Data/Filosofi/filo14.fst") %>% 
  filter(com %in% com.dom.select)
filo.sp <- SpatialPointsDataFrame(coords = filo[,c("x","y")],data = filo,proj4string = CRS("+init=epsg:3857"))
filo.sp <- zonaPts(pts.sp = filo.sp,zonage = zonage)
filo <- filo.sp@data %>% 
  left_join(data.frame(unique(rpl[,c("com","com.lib")])),"com") %>% 
  mutate(dep = substr(com,1,3),
         idZonage = ifelse(is.na(idZonage),"Hors zonage", idZonage))
rm(filo.sp)








