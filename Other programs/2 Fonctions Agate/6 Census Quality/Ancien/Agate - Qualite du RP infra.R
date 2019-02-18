#-------------------------------------------------------------------------------------#
#                AppliShiny - Estimation de la qualite du RP infracommunal            #
#-------------------------------------------------------------------------------------#

# Nicolas Kempf

# Derniere MAJ : 16.03.2018

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
library(sp)
library(rgdal)
library(plyr)

# Fonctions utiles
#-----------------
source("../QPV prog/Catalogue fonctions/AMV - Fonctions Carthographiques_v2.R")
source("Catalogue de fonctions/Agate - Fonction qualite RP infra.R")

#-----------------------------#
#   Chargement des bases      #
#-----------------------------#
# RP/RIL niveau adresses
load("../../Bdd/RData/Qualite/rpa13.RData")

# RIL
load("../../Bdd/RData/Ril/ril15.RData")

# Zonage
load("../../Bdd/RData/Qpv/qpv.RData")

# Parametres
zonage <- qpv
ril <- rilhab15
rpa <- rpa13
Y <- "INPER"
idZonage <- "CODE_QP"
eff_rilRp <- Eff_rilRp13

# Ajout de la superficie à la couche Zonage
zonage <- superficieZon(zonage = zonage,idZonage = idZonage)

### Jointure spatiale ril et zonage ###
# Determine pour chaque logement du ril s'il se trouve dans le zonage. Si oui, lequel.
ril <- zonaRil(ril = ril,zonage = zonage)

# Ajout de la variable CODE_QP a la base rpa
rpa <- merge(rpa,ril@data[,c("idx",idZonage)],by="idx",all.x=T)

# Renommage de l'identifiant du zonage
rpa$idZonage <- as.character(rpa[,idZonage])
rpa$com <- substr(rpa$idx,1,5)

### Toute adresse non affectée à un zonage QPV donné est considérée hors du périmètre des
### quartiers Politique de la Ville.
rpa[,idZonage][is.na(rpa[,idZonage])] <- "horsZon"

# Base sondage (special calage)
sondage <- sondageZon(eff_rilRp = eff_rilRp,rpa = rpa,idZonage = idZonage)

# Calcul de la precision analytique sans calage
RPStats <- precision_analytique_nc(rpa = rpa,Y = Y,idZonage = "idZonage")

# INPCM : Nombre de chômeurs du ménage 
qpv_RPstats_densite_chomeurs <- precision_analytique_nc_densite("INPCM")


# Enregistrement en RData
save(RPstats,file="../../Bdd/RData/Temp/RPStats.RData")



