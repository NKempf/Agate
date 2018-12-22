#--------------------------------------------------------------------------------------------------------------------------------#
#                     Agate - RP and Filo Fake data                                                                              #
#--------------------------------------------------------------------------------------------------------------------------------#

# MAJ : 3.06.2018

# Nicolas Kempf

# Statistiques descriptives par îlots

### Working directory 
# Paramètre automatiquement le dossier dans lequel se trouve le programme comme espace de travail
# Ne fonction que pour une version de R supérieure à 3.3
setwd(dir=dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Packages nécessaires
#---------------------
library(sp) # Objets spatiaux
library(tidyverse) # transform data
library(writexl) # Export des données en excel

# I. Import des données
#-------------------------------------------------------------------------------------------------
load("../Bases/R/comDom.RData")

com.tmp <- com[as.character(com$insee) %in% c("97101","97102"),]

test <- spatial_fakedata(zone = com.tmp,1000)


# II. Merge spatial data and fake.data
#----------------------------------------------------------------------------------------------------
load("../Bases/R/fakeData.Rdata")

rpi <- data.frame(df,rpi)
rpl <- data.frame(df,rpl)
filo <- data.frame(df,filo)

# 















