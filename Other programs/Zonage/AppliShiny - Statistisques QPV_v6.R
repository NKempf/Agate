#-------------------------------------------------------------------------------------#
#                AppliShiny - Statistiques QPV                                        #
#-------------------------------------------------------------------------------------#

# MAJ : 29.05.2018

# Nicolas Kempf

# Ce programme calcule pour un zonage particulier (par exemple les QPV) une série de statistiques issues du Rp et de filosofi

# Ce programme est enregistré avec l'encodage UTF-8. Si les caractères spéciaux sautent, penser à parametrer RStudio avec
# l'encodage UTF-8 (Tools/Global option/Saving/default text encoding)

### Working directory 
# Paramètre automatiquement le dossier dans lequel se trouve le programme comme espace de travail
# Ne fonction que pour une version de R supérieure à 3.3
setwd(dir=dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Chargement du catalogue de fonctions spécifiques à l'outil
source("Catalogue fonctions/AppliShiny - Cartographie.R")
source("Catalogue fonctions/AppliShiny - Calcul statistiques_v2.R")
source("Catalogue fonctions/AppliShiny - Reporting QPV.R")

# Chargement dans R
library(rgdal) # Importer de nombreux formats de données spatiales
library(tidyverse) # Transformation des données
# library(plyr) # Transformation de tables

#-----------------------------------------------#
#             Import des cartes                 #
#-----------------------------------------------#

# Ril habitation
#---------------
load("../../Bdd/RData/Ril/ril15.RData")

# QPV
#----
load("../../Bdd/RData/Qpv/qpv.RData")


#-----------------------------------------------#
#             Import des bases de données       #
#-----------------------------------------------#

# Recensement de la population 2014
#----------------------------------
load("../../Bdd/RData/Rp/rp14.RData")
load("../../Bdd/RData/Rp/rp13.RData")

# Filosofi 2014
#--------------
load("../../Bdd/RData/Filosofi/filo14.Rdata")


# Nommage temporaire des objets en vue de la création d'une fonction
filo <- filo14.disp
ril <- rilhab15
zonage <- qpv
rpi <- rp13i
rpl <- rp13l
idZonage <- "CODE_QP"
nomRapport <- "QpvRapport2013"

#-----------------------------------------------#
#   Traitements statistiques et geographiques   #
#-----------------------------------------------#

### Préparation du fonc de carte zonage
zonage@data$idZonage <- zonage@data[,idZonage]

### Jointure spatiale ril et zonage ###
# Determine pour chaque logement du ril s'il se trouve dans le zonage. Si oui, lequel.
ril <- zonaRil(ril = ril,zonage = zonage)

### Fusion du ril avec le RP logement ###
# Objectif : obtenir l idZonage pour chaque logement du RP
rpl <- merge(rpl,ril@data[,c("idx","idZonage")],by="idx",all.x=T)

# Ajout des variables issues du zonage à la table RP individu
rpi <- merge(rpi,ril@data[,c("idx","idZonage")], by="idx",all.x=T)

### Jointure spatiale Filosofi et zonage
# COnvertion du fichier filosofi en spatialpointdataframe
filo.sp <- SpatialPointsDataFrame(coords = filo[,c("x","y")],data = filo,proj4string = CRS("+init=epsg:3857"))
# Determine pour chaque logement de filosofi  s'il se trouve dans le zonage. Si oui, lequel.
filo.sp <- zonaRil(ril = filo.sp,zonage = zonage)
filo <- filo.sp@data
rm(filo.sp)


# Enregistrement temporaire
# save(rpi,rpl,filo,filo14.disp.dico,file = "../../Bdd/RData/Temp/rpTidyStat.RData")


#-----------------------------------------------#
#           Calcul statistiques                 #
#-----------------------------------------------#

# Génère les tableaux des statistiques issues du RP pour un zonage à choisir
# Cette procédure génére un tableau par chaque statistique calculée et un tableau récapitulatif contenant
# toutes les statistiques d'interêts
# Attention : cette procédure renvoie une liste de data.frame
zona_statRP <- zonaStatRp(rpi = rpi,rpl = rpl,id = "idZonage")
# Statistiques communales
com_statRP <- zonaStatRp(rpi = rpi,rpl = rpl,id = "com")
# Statistiques communales hors zonage
Hzona_statRP <- zonaStatRp(rpi = rpi[is.na(rpi$idZonage),],rpl = rpl[is.na(rpi$idZonage),],id = "com")

# Statistiques des autres zonages (a l'exclusion du zonage considere) du departement. 
# L'idee est d'obtenir les indicateurs moyen des autres zonages par rapport au zonage d'etude. 
  # 1. Base speciale individu
  # rpiZ <- rpAutZon(rp = rpi,zonage = zonage)
  # # 2. Base speciale logement
  # rplZ <- rpAutZon(rp = rpl,zonage = zonage)
  # # 3. Calcul des statistiques pour les autres zonages
  # AutZon_statRP <- zonaStatRp(rpi = rpiZ,rpl = rplZ,id = "idZonageAutZon")
  # # 4. suppression des bases
  # rm(rpiZ,rplZ)
  
# Calcule les statistiques de filosofi
filoStat <- zonaStatFilo(filo = filo,id = "idZonage")

# Enregsitrement des bases dans une liste
base <- list(statCom = com_statRP$tStatRP,statCom_pyramid = com_statRP$t1d_pyramide,statCom_pyramidTr = com_statRP$t1e_pyramide,
             statHZone=Hzona_statRP$tStatRP,statHZone_pyramid=Hzona_statRP$t1d_pyramide,statHZone_pyramidTr=Hzona_statRP$t1d_pyramide,
             # statAutZon=AutZon_statRP$tStatRP,
             # rpi=rpi,rpl=rpl,
             filo=filo[,c("idx","idZonage","depcom","typmenR","nivviem")])

# Fusion des listes
statZona <- c(zona_statRP,filoStat,base)

#-----------------------------------------------#
#               Reporting                       #
#-----------------------------------------------#    

# Création du rapport excel des indicateurs Stats
chemin <- paste("../../Reporting/Qpv/",nomRapport,"_",as.character(Sys.Date()),".xlsx",sep="")
zonaQpvReporting(statZona,file = chemin,sourceInsee = "Insee, RP 2014") 

# Enregistrement de la carte
#writeOGR(obj = zonage2,dsn = "../../Cartes/QPV/qpv_stat.geojson",layer = "qpv_stat",driver = "GeoJSON")
# Changement du système de projection
#qpv_stat <- spTransform(qpv_stat, "+init=epsg:4326")
save(statZona,file="../../Bdd/RData/statTmp.RData")

#-----------------------------------------------#
#               Variable IdZonage RP            #
#-----------------------------------------------#  

# Enregistrement en rData d'une base contenant la variable du zonage (ici qpv) et l'identifiant du RP logement correspondant.
rpl13 <- rpl[,c("idx","idZonage")]
save(rpl13,file = "../../Output/rpl13_qpv.RData")


t2 <- Sys.time()

print(t2-t1)


  





