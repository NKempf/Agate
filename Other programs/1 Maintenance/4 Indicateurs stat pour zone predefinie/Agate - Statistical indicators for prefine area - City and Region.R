#----------------------------------------------------------------------------------------------------------------------#
#                                   Agate - Statistical calculation for predefine area : City and Region               #
#----------------------------------------------------------------------------------------------------------------------#

# 02.04.2019

# Statistical indicators for predefine area

# Packages nécessaires
#---------------------
library(rgdal) # Input shapefile
library(rgeos) # Map tools
library(tidyverse) # Tidy data
library(fst) # Read partial data
library(fstplyr) # dplyr for fst object
library(rlang) # non standard evaluation

# Fonction necessaire
source("Other programs/1 Maintenance/4 Indicateurs stat pour zone predefinie/Fonctions/Agate - Indicators predefine area fct.R",encoding = "UTF-8")

# Listes des indicateurs statistiques
load("Data/Liste indicateurs statistiques/lstIndicateur.RData")

group_var <- c("idZonage","idZonage.name")
an <- c("13","14","15")

# I. City and Region
#-------------------------------------------------------------------------------------------------------------------------

# Region
load("data/Maps/Region/region.RData")
dep.dom@data <- dep.dom@data %>% 
  rename(idZonage = Codgeo,
         idZonage.name = Libgeo) %>% 
  select(idZonage,idZonage.name)

region <- lapply(an, indStat_RegCity,var.pred = "dep",zonage = dep.dom,
               lstCategorie = lstCategorie, lstIndicateur = lstIndicateur,pred.zone = 1,group_var = group_var)

# City
load("data/Maps/Cities/cities.RData")
city <- com.dom
city@data <- city@data %>% 
  rename(idZonage = Codgeo,
         idZonage.name = Libgeo) %>% 
  select(idZonage,idZonage.name)

city <- lapply(an, indStat_RegCity,var.pred = "com",zonage = city,pred.zone = 2,
                 lstCategorie = lstCategorie, lstIndicateur = lstIndicateur,group_var = group_var)

# II. Infracity predefine zone
#---------------------------------------------------------------------------------------------------------------------------

# QPV
zonage <- readOGR(dsn = "Data/QPV/qpv.shp",encoding = "UTF-8",stringsAsFactors = FALSE)
zonage@data <- zonage@data %>% 
  rename(idZonage = CODE_QP,
         idZonage.name = NOM_QP) %>% 
  select(idZonage,idZonage.name)
zonage <- spTransform(zonage, "+init=epsg:3857")

t1 <- Sys.time()
qpv <- lapply(an, agate_statRp,
       zone.pred = 3,zoneType = "QPV",zonage = zonage,group_var = group_var,
       com.dom = com.dom,rpi.weight = "IPONDI.cal",rpl.weight = "IPONDL.cal")
Sys.time() - t1

# III.Tables finales
#--------------------------------------------------------------------------------------------------------------------------
df.zone <- bind_rows(lapply(c(1,2,3), function(x) bind_rows(region[[x]][[1]],city[[x]][[1]],
                                                            qpv[[x]][[1]] %>% filter(type.indicateur == "valeur.diffusable"))))
pyramide <- bind_rows(lapply(c(1,2,3), function(x) bind_rows(region[[x]][[2]],city[[x]][[2]],qpv[[x]][[2]])))

save(df.zone,pyramide,file="data/Stats/Prefine aera/Real/RData/predefineStat.RData")
write_fst(df.zone,"Data/Stats/Prefine aera/Real/fst/indicateur_stat.fst",compress = 100)
write_fst(pyramide,"Data/Stats/Prefine aera/Real/fst/pyramide.fst",compress = 100)


