#-------------------------------------------------------------------------------------#
#                AppliShiny - Estimation de la qualite du RP infracommunal            #
#-------------------------------------------------------------------------------------#

# Nicolas Kempf

# Derniere MAJ : 20.07.2018

# Ce programme utilise les travaux de Lionel Delta pour calculer la qualite des donnees du recensement à l'infracommunal.   

# Ce programme est enregistré avec l'encodage UTF-8. Si les caractères spéciaux sautent, penser à parametrer RStudio avec
# l'encodage UTF-8 (Tools/Global option/Saving/default text encoding)

### Working directory 
# Paramètre automatiquement le dossier dans lequel se trouve le programme comme espace de travail
# Ne fonction que pour une version de R supérieure à 3.3
setwd(dir=dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Library indispensables
#-----------------------
library(rgdal)
library(rgeos) 
library(tidyverse)
# library(plyr)
# library(rgeos)
library(sampling)
# library(data.table)

# Fonctions utiles
#-----------------
source("../QPV prog/Catalogue fonctions/AMV - Fonctions Carthographiques_v2.R",encoding = "UTF-8")
source("Catalogue de fonctions/Agate - Fonction qualite RP infra_v5.R",encoding = "UTF-8")

# I. Import des bases
#----------------------------------------------------------------------------------------------------------------------------------------------------------

# I.1. RP niveau adresses
#------------------------
load("../../Bdd/RData/Qualite/rpa13.RData")


# I.2. RP niveau logement et individu
#------------------------------------
load("../../Bdd/RData/Rp/rp13.RData")
# Ajout d'autres variables
rpl_rpa <- rp13l %>% 
  select(idrp,INPCM) %>% 
  group_by(idrp) %>% 
  summarise(INPCM = sum(INPCM)) %>% 
  ungroup()

# Ajout des nouvelles variables à la table adresse
rpa <- rpa13 %>% 
  left_join(rpl_rpa,c("idx"="idrp")) %>% 
  mutate(INPCM = ifelse(is.na(INPCM),0,INPCM))

# I.3. RIL
#---------
load("../../Bdd/RData/Ril/ril15.RData")

# I.4. Zonage
#------------
load("../../Bdd/RData/Qpv/qpv.RData")
zonage@data$idZonage <- zonage@data$CODE_QP

# II. Qualité du RP dans la zone
#---------------------------------------------------------------------------------------------------------------------------------------------------------
# Parametres
zonage <- qpv
ril <- rilhab15
rpa <- rpa
Y <- "INPCM"
idZonage <- "idZonage"

# Ajout de la superficie à la couche Zonage
zonage <- superficieZon(zonage = zonage,idZonage = idZonage)

# Jointure spatiale
ril <- zonaRil(ril = ril,zonage = zonage)

# Ajout de la variable CODE_QP a la base rpa
rpa <- merge(rpa,ril@data[,c("idx","idZonage")],by="idx",all.x=T)

### Toute adresse non affectée à un zonage QPV donné est considérée hors du périmètre des
### quartiers Politique de la Ville.
rpa[,idZonage][is.na(rpa[,idZonage])] <- "horsZon"

# Renommage de l'identifiant du zonage
# rpa$idZonage <- as.character(rpa[,idZonage])

# Base sondage (special calage)
sondage <- sondageZon(rpa = rpa)

# Calcul de la precision analytique sans calage
RPStats <- precision_analytique_nc(rpa = rpa,Y = INPER,zonage = zonage,idZonage = "idZonage")
RPStats2 <- precision_analytique_nc(rpa = rpa,Y = INPCM,zonage = zonage,idZonage = "idZonage")

# Enregistrement en RData
save(RPstats,file="../../Bdd/RData/Temp/RPStats.RData")



