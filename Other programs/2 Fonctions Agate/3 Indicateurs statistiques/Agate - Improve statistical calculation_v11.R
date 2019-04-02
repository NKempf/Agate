#----------------------------------------------------------------------------------------------------------------------#
#                                   Agata - Statistical calculation improvement                                        #
#----------------------------------------------------------------------------------------------------------------------#

# 01.04.2019

# Improve statistical calculation of indicators and display them into cool datatable (package DT)
# Ajout des travaux de Baptiste Raimbaud
# Suppression du RIL au profil du rp niveau adresse géolocalisé

# Packages nécessaires
#---------------------
library(rgdal) # Input shapefile
library(rgeos) # Map tools
library(tidyverse) # Tidy data
library(fst) # Read partial data
library(fstplyr) # dplyr for fst object
library(DT) # Interactive datatable
library(openxlsx) # Export en excel
library(rlang) # non standard evaluation
library(easySdcTable) # Statistical disclosure

# Fonctions supplémentaires
source("Other programs/2 Fonctions Agate/3 Indicateurs statistiques/Agate - Statistics Zonage_v9.R",encoding = "UTF-8")

# I. Import des données
#---------------------------------------------------------------------------------------------------------------------------------------------
# Zonage
zonage <- readOGR(dsn = "Data/QPV/qpv.shp",encoding = "UTF-8",stringsAsFactors = FALSE)[1:2,]
zonage@data <- zonage@data %>% 
  rename(idZonage = CODE_QP,
         idZonage.name = NOM_QP) %>% 
  select(idZonage,idZonage.name)

# Villes
load("Data/Maps/Cities/cities.RData") # Cities map

# Listes des indicateurs statistiques
load("Data/Liste indicateurs statistiques/lstIndicateur.RData")

# Paramètres fonction
# rp.an <- "13"
# zonage <- zonage
# com.dom <- com.dom
# zoneType <- "QPV"
# zone.pred <- 3
# group_var <- c("idZonage","idZonage.name") # Attention utilisé plusieurs fois
# rpi.weight = "IPONDI.cal"
# rpl.weight = "IPONDL.cal"
# seuil_qualite_appariement <- 30
# seuil_diffusion_qualite <- 5 # Seuil de diffusion de la valeur du coefficient de variation
# seuil_secret_stat <- 11 # Seuil fixé à 11 observations minimum par case conformément aux recommandations d'utilisation des fichiers filosofi

# II. Agate statistiques du Recensement de la population
#--------------------------------------------------------------------------------------------------------------------------------------------
t1 <- Sys.time()
agate.stat <- agate_statRp(rp.an = "13",zone.pred = 4,zoneType = "QPV",zonage = zonage,group_var = c("idZonage","idZonage.name"),
                           com.dom = com.dom,rpi.weight = "IPONDI.cal",rpl.weight = "IPONDL.cal")
Sys.time() - t1

# IX. Enregistrement temporaire pour test
#----------------------------------------
df.zone <- agate.stat$df.zone
zonage.com <- agate.stat$zonage.com
pyramide <- agate.stat$pyramide

save(zonage,df.zone,pyramide,zonage.com,file = "Data/Tmp/qpv_stat_tmp.RData")


