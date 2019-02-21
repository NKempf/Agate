library(rgdal)
library(tidyverse)

zonage <- readOGR(dsn = "Data/QPV/qpv.shp",encoding = "UTF-8",stringsAsFactors = FALSE)
zonage@data <- zonage@data %>% 
  rename(Codgeo = CODE_QP,
         Libgeo = NOM_QP) %>% 
  select(Codgeo,Libgeo) %>% 
  mutate(dep = substr(Codgeo,3,5))
zonage <- spTransform(zonage, "+init=epsg:3857")
qpv.mar <- zonage[zonage@data$dep == "972",]

qpv.mar@proj4string
qpv.mar@data$Libgeo <- c("Frange Ouest Du Centre-Ville Agglomere","Quartiers Sainte-Therese – Bon Air","Quartier Volga Plage",
  "Zone De Cite Lacroix Trou Terre Pointe Lynch","Tombolo City","Quartiers Ouest","Quartier Haut Dillon")



writeOGR(obj = qpv.mar,dsn = "../Agate cartes/QPV martinique/qpv972.shp",layer = "qpv972",driver = "ESRI Shapefile",overwrite_layer = T)
ogrDrivers()


test <- qpv.mar@data


# I. Bruits Guadeloupe
#--------------------------------------------------------------------------------------------------------------------------------------------
zonage <- readOGR(dsn = "../Agate cartes/KaruGeo/Bruits971_3.shp",stringsAsFactors = FALSE,encoding = "utf-8")


colnames(zonage@data)
zonage@data <- zonage@data %>% 
  select(Codgeo,Libgeo) %>% 
  mutate(Codgeo = paste0("b_",Codgeo))


writeOGR(obj = zonage,dsn = "../Agate cartes/QPV martinique/bruit971.shp",layer = "bruit971",driver = "ESRI Shapefile",
         encoding = "windows-1252",overwrite_layer = T)
