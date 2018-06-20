#--------------------------------------------------------------------------------------------------------------------------------#
#                     Agate - RP and Filo Fake data                                                                              #
#--------------------------------------------------------------------------------------------------------------------------------#

# MAJ : 19.06.2018

# Nicolas Kempf

# Statistiques descriptives par îlots

### Working directory 
# Paramètre automatiquement le dossier dans lequel se trouve le programme comme espace de travail
# Ne fonction que pour une version de R supérieure à 3.3
# setwd(dir=dirname(rstudioapi::getActiveDocumentContext()$path))
# getwd()
t1 <- Sys.time()
# Packages nécessaires
#---------------------
library(sp) # Objets spatiaux
library(rgdal) # Lecture d'objets spatiaux
library(rgeos) # Transformation objets spatiaux
library(tidyverse) # transform data
library(writexl) # Export des données en excel

# Fonctions utiles
#-----------------
source ("Other programs/Fake data/Agate - Fake data fct.R")
source("Other programs/Zonage/Agate - Cartographie fct.R")
source("Other programs/StatZonage/Agate - Statistics Zonage_v3.R")


# I. Import des bases
#-------------------------------------------------------------------------------------------------
load("Data/fakePts.RData")
load("Data/fakeData.Rdata")
load("Data/QPV/qpvFake.RData")

# Filosofi
#---------
typmen.label <- c("famille monoparentale","couple sans enfant","couple avec enfant(s)","menage complexe",
                  "femme seule","homme seul")

# filo <- as_tibble(filo) %>% 
#   # filter(com %in% lstCom) %>%
#   mutate(typmenR.lib = factor(typmenR,labels = typmen.label))


# II. Traitements cartographiques
#--------------------------------------------------------------------------------------------------

qpv_stat.fake@data <- qpv_stat.fake@data[,c("CODE_QP","NOM_QP")]

# Nommage temporaire des objets en vue de la création d'une fonction
filo <- filo
pts.sp <- pts.fake
zonage <- qpv_stat.fake
rpi <- rpi
rpl <- rpl
idZonage <- "CODE_QP"
nomRapport <- "AgateAqpvFake"

# Creation de l'identifiant idZonage
zonage@data$idZonage <- zonage@data[,idZonage]

# Determine pour chaque point dans quel zone il se situe
pts.sp <- zonaPts(pts.sp = pts.sp,zonage = zonage)

# Faking data
lst_var <- c("idx","idZonage","dep","com","com.lib")
rpi <- cbind(pts.sp@data[,lst_var],faking_data(rpi,nrow(pts.sp)))
rpl <- cbind(pts.sp@data[,lst_var],faking_data(rpl,nrow(pts.sp)))
filo <- cbind(pts.sp@data[,lst_var],faking_data(filo,nrow(pts.sp)))

# Attention placer ce bout de code ici (pas avant car sinon le temps de calcul explose)
filo <- as_tibble(filo) %>%
  # filter(com %in% lstCom) %>%
  mutate(typmenR.lib = factor(typmenR,labels = typmen.label))

# III. Statistiques descriptives
#--------------------------------------------------------------------------------------------------------------------------------------------------------------
StatZona <- statistics_zone(rpi = rpi,rpl = rpl,filo = filo,group_var = c("com","com.lib","idZonage"))
class(filo$idZonage)
table(filo$idZonage)



# IV. Sortie excel
#---------------------------------------------------------------------------------------------------------------------------------------------------------------
write_xlsx(StatZona,path = "Output/Agate_stat.xlsx")





Sys.time() - t1









