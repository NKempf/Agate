#----------------------------------------------------------------------------------------------------------------------#
#                                   Agate - Statistical calculation for predefine area                                 #
#----------------------------------------------------------------------------------------------------------------------#

# 17.02.2019

# Statistical indicators for predefine area

# Packages nécessaires
#---------------------
library(rgdal) # Input shapefile
library(rgeos) # Map tools
library(tidyverse) # Tidy data
library(fst) # Read partial data
library(fstplyr) # dplyr for fst object
library(rlang) # Non standard evaluation

# Fonctions supplémentaires
#--------------------------
source("Other programs/2 Fonctions Agate/2 Cartographie/Agate - Cartographie fct.R",encoding = "UTF-8")
source("Other programs/2 Fonctions Agate/3 Indicateurs statistiques/Agate - Statistics Zonage_v6.R",encoding = "UTF-8")
source("Other programs/1 Maintenance/4 Indicateurs stat pour zone predefinie/Fonctions/Agate - Indicators predefine area fct.R")

ril.an <- "15"


# I. Villes
#----------------------------------------------------------------------------------------------------------------------

# I.1. Import des données
#------------------------
load("Data/Maps/Cities/cities.RData")
zonage <- com.dom
zonage@data <- zonage@data %>% 
  rename(idZonage = Codgeo,
         idZonage.name = Libgeo) %>% 
  select(idZonage,idZonage.name)
zonage <- spTransform(zonage, "+init=epsg:3857")

# I.2. Calcul des indicateurs
#----------------------------
indStat13 <- indStat_RegCities(zonage = zonage,var = "com",rp.an = "13",filo.an = "14",ril.an = ril.an)
indStat14 <- indStat_RegCities(zonage = zonage,var = "com",rp.an = "14",filo.an = "14",ril.an = ril.an)
indStat15 <- indStat_RegCities(zonage = zonage,var = "com",rp.an = "14",filo.an = "15",ril.an = ril.an)

# I.3. Objet final
#-----------------
df <- indStat13$indicateur_stat %>% 
  bind_rows(indStat14$indicateur_stat %>% filter(source %in% c("rpi14","rpl14")),
            indStat15$indicateur_stat %>% filter(source == "filo15")) %>% 
  left_join(zonage@data,by = "idZonage")

pyr <- indStat13$pyramide_detail %>% 
  bind_rows(indStat14$pyramide_detail %>% filter(source %in% c("rpi14","rpl14")),
            indStat15$pyramide_detail %>% filter(source == "filo15")) %>% 
  left_join(zonage@data,by = "idZonage")

pyr.tr <- indStat13$pyramide_tr %>% 
  bind_rows(indStat14$pyramide_tr %>% filter(source %in% c("rpi14","rpl14")),
            indStat15$pyramide_tr %>% filter(source == "filo15")) %>% 
  left_join(zonage@data,by = "idZonage")

c_lorenz <- indStat13$c_lorenz %>% 
  bind_rows(indStat14$c_lorenz %>% filter(source %in% c("rpi14","rpl14")),
            indStat15$c_lorenz %>% filter(source == "filo15")) %>% 
  left_join(zonage@data,by = "idZonage")

indStat.cities <- list(indicateur_stat = df,pyramide_detail = pyr,
                       pyramide_tr = pyr.tr,c_lorenz = c_lorenz)

# I.4. Enregsitrement
#--------------------
save(indStat.cities,file = "Data/Stats/Prefine aera/Real/RData/com_stat.RData")

# II. Region
#----------------------------------------------------------------------------------------------------------------------

# II.1. Import des données
#------------------------
load("Data/Maps/Region/region.RData")
zonage <- dep.dom
zonage@data <- zonage@data %>% 
  rename(idZonage = Codgeo,
         idZonage.name = Libgeo) %>% 
  select(idZonage,idZonage.name)
zonage <- spTransform(zonage, "+init=epsg:3857")

# II.2. Calcul des indicateurs
#-----------------------------
indStat13 <- indStat_RegCities(zonage = zonage,var = "dep",rp.an = "13",filo.an = "14",ril.an = ril.an)
indStat14 <- indStat_RegCities(zonage = zonage,var = "dep",rp.an = "14",filo.an = "14",ril.an = ril.an)
indStat15 <- indStat_RegCities(zonage = zonage,var = "dep",rp.an = "14",filo.an = "15",ril.an = ril.an)

# II.3. Objet final
#------------------
df <- indStat13$indicateur_stat %>% 
  bind_rows(indStat14$indicateur_stat %>% filter(source %in% c("rpi14","rpl14")),
            indStat15$indicateur_stat %>% filter(source == "filo15")) %>% 
  left_join(zonage@data,by = "idZonage")

pyr <- indStat13$pyramide_detail %>% 
  bind_rows(indStat14$pyramide_detail %>% filter(source %in% c("rpi14","rpl14")),
            indStat15$pyramide_detail %>% filter(source == "filo15")) %>% 
  left_join(zonage@data,by = "idZonage")

pyr.tr <- indStat13$pyramide_tr %>% 
  bind_rows(indStat14$pyramide_tr %>% filter(source %in% c("rpi14","rpl14")),
            indStat15$pyramide_tr %>% filter(source == "filo15")) %>% 
  left_join(zonage@data,by = "idZonage")

c_lorenz <- indStat13$c_lorenz %>% 
  bind_rows(indStat14$c_lorenz %>% filter(source %in% c("rpi14","rpl14")),
            indStat15$c_lorenz %>% filter(source == "filo15")) %>% 
  left_join(zonage@data,by = "idZonage")

indStat.dep <- list(indicateur_stat = df,pyramide_detail = pyr,
                       pyramide_tr = pyr.tr,c_lorenz = c_lorenz)

# II.4. Enregsitrement
#--------------------
save(indStat.dep,file = "Data/Stats/Prefine aera/Real/RData/dep_stat.RData")

# III. QPV
#----------------------------------------------------------------------------------------------------------------------

# III.1. Import des données
#------------------------
zonage <- readOGR(dsn = "Data/QPV/qpv.shp")
zonage@data <- zonage@data %>% 
  rename(idZonage = CODE_QP,
         idZonage.name = NOM_QP) %>% 
  select(idZonage,idZonage.name)
zonage <- spTransform(zonage, "+init=epsg:3857")

# III.2. Calcul des indicateurs
#-----------------------------
indStat13 <- indStat_AutZOn(zonage = zonage,rp.an = "13",filo.an = "14",ril.an = ril.an)
indStat14 <- indStat_AutZOn(zonage = zonage,rp.an = "14",filo.an = "14",ril.an = ril.an)
indStat15 <- indStat_AutZOn(zonage = zonage,rp.an = "14",filo.an = "15",ril.an = ril.an)

# III.3. Objet final
#------------------
df <- indStat13$indicateur_stat %>% 
  bind_rows(indStat14$indicateur_stat %>% filter(source %in% c("rpi14","rpl14")),
            indStat15$indicateur_stat %>% filter(source == "filo15")) %>% 
  left_join(zonage@data,by = "idZonage")

pyr <- indStat13$pyramide_detail %>% 
  bind_rows(indStat14$pyramide_detail %>% filter(source %in% c("rpi14","rpl14")),
            indStat15$pyramide_detail %>% filter(source == "filo15")) %>% 
  left_join(zonage@data,by = "idZonage")

pyr.tr <- indStat13$pyramide_tr %>% 
  bind_rows(indStat14$pyramide_tr %>% filter(source %in% c("rpi14","rpl14")),
            indStat15$pyramide_tr %>% filter(source == "filo15")) %>% 
  left_join(zonage@data,by = "idZonage")

c_lorenz <- indStat13$c_lorenz %>% 
  bind_rows(indStat14$c_lorenz %>% filter(source %in% c("rpi14","rpl14")),
            indStat15$c_lorenz %>% filter(source == "filo15")) %>% 
  left_join(zonage@data,by = "idZonage")

indStat.qpv <- list(indicateur_stat = df,pyramide_detail = pyr,
                    pyramide_tr = pyr.tr,c_lorenz = c_lorenz)

# III.4. Enregsitrement
#--------------------
save(indStat.qpv,file = "Data/Stats/Prefine aera/Real/realQpv_stat.RData")

# IV. Ilot99
#----------------------------------------------------------------------------------------------------------------------

# V. Enregistrement en fst
#----------------------------------------------------------------------------------------------------------------------

# V.1. indicateur_stat
#---------------------
indicateur_stat <- bind_rows(indStat.dep$indicateur_stat %>% mutate(zone.predefine = 1),
                             indStat.cities$indicateur_stat %>% mutate(zone.predefine = 2),
                             indStat.qpv$indicateur_stat%>% mutate(zone.predefine = 3)) %>% 
  select(zone.predefine,source,idZonage,idZonage.name,domaine,categorie,indicateur,type.indicateur,value)

write_fst(indicateur_stat,"Data/Stats/Prefine aera/Real/fst/indicateur_stat.fst",compress = 100)

# V.2. pyramide
#--------------
# TODO

