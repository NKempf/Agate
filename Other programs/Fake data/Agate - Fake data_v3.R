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

# Packages nécessaires
#---------------------
library(sp) # Objets spatiaux
library(rgdal) # Lecture d'objets spatiaux
library(tidyverse) # transform data
library(writexl) # Export des données en excel

# Fonctions utiles
#-----------------
source ("Other programs/Fake data/Agate - Fake data fct.R")
source("Other programs/Zonage/AppliShiny - Cartographie.R")


# I. Import des bases
#-------------------------------------------------------------------------------------------------
load("Data/fakePts.RData")
load("Data/fakeData.Rdata")
load("Data/QPV/qpvFake.RData")

# Maintenant, on va determiner pour chaque point dans quel zonage il se situe.

# II. Traitements cartographiques
#--------------------------------------------------------------------------------------------------


# Nommage temporaire des objets en vue de la création d'une fonction
filo <- filo
ril <- pts.971
zonage <- qpv_stat.fake
rpi <- rpi
rpl <- rpl
idZonage <- "CODE_QP"
nomRapport <- "AgateAqpvFake"



### Préparation du fonc de carte zonage
zonage@data$idZonage <- zonage@data[,idZonage]

### Jointure spatiale ril et zonage ###
# Determine pour chaque logement du ril s'il se trouve dans le zonage. Si oui, lequel.
ril <- zonaRil(ril = ril,zonage = zonage)




# Nommage temporaire des objets en vue de la création d'une fonction
filo <- filo14.disp
ril <- rilhab15
zonage <- qpv
rpi <- rp13i
rpl <- rp13l
idZonage <- "CODE_QP"
nomRapport <- "QpvRapport2013"









