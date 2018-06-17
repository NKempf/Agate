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
library(rgdal) # Lecture d'objets spatiaux
library(tidyverse) # transform data
library(writexl) # Export des données en excel

source ("Other programs/Fake data/Agate - Fake data fct.R")



# I. Import des bases
#-------------------------------------------------------------------------------------------------


