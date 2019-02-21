#-------------------------------------------------------------------------------------#
#                Agate - Estimation de la qualite du RP infracommunal                 #
#-------------------------------------------------------------------------------------#

# Nicolas Kempf & Baptiste Raimbaud

# Derniere MAJ : 20.02.2019

# Ce programme utilise les travaux de Lionel Delta et de Baptiste Raimbaud pour calculer la qualite des donnees du recensement à l'infracommunal.   

# Ce programme est enregistré avec l'encodage UTF-8. Si les caractères spéciaux sautent, penser à parametrer RStudio avec
# l'encodage UTF-8 (Tools/Global option/Saving/default text encoding)

# Library indispensables
#-----------------------
library(rgdal)
library(rgeos) 
library(tidyverse)
library(sampling)
library(fst)

# Fonctions utiles
#-----------------
source("Other programs/Zonage/Agate - Cartographie fct.R",encoding = "UTF-8")
source("Other programs/Quality/Agate - Census infra quality.R")

# I. Import des bases
#----------------------------------------------------------------------------------------------------------------------------------------------------------

# I.1. RP niveau adresses
#------------------------
load("Data/Rp/rpa13.RData")

# I.2. RP niveau logement et individu
#------------------------------------
rp13l <- read_fst("Data/Rp/rp13l.fst")
# Ajout d'autres variables
rpl_rpa <- rp13l %>% 
  select(idrp,INPCM) %>% 
  group_by(idrp) %>% 
  summarise(INPCM = sum(INPCM)) %>% 
  ungroup()

# Ajout des nouvelles variables à la table adresse
rpa13 <- rpa13 %>% 
  left_join(rpl_rpa,c("idx"="idrp")) %>% 
  mutate(INPCM = ifelse(is.na(INPCM),0,INPCM))

# I.3. RIL
#---------
load("Data/Ril/ril15.RData")

# I.4. Zonage
#------------
load("Data/QPV/qpvFake.RData")

qpv_stat.fake <- spTransform(qpv_stat.fake, "+init=epsg:3857")

qpv_stat.fake@data <- qpv_stat.fake@data %>% 
  select(CODE_QP,NOM_QP) %>% 
  mutate(NOM_QP = iconv(NOM_QP,to="ASCII//TRANSLIT"))


# II. Qualité du RP dans la zone
#---------------------------------------------------------------------------------------------------------------------------------------------------------
# Parametres
zonage <- qpv_stat.fake
zonage@data$idZonage <- zonage@data$CODE_QP
ril <- rilhab15
rpa <- rpa13
Y <- "INPCM"
idZonage <- "idZonage"

# Ajout de la superficie à la couche Zonage
zonage <- superficieZon(zonage = zonage,idZonage = idZonage)

# Jointure spatiale
ril <- zonaPts(pts.sp = ril,zonage = zonage)

# Ajout de la variable CODE_QP a la base rpa
rpa <- merge(rpa,ril@data[,c("idx","idZonage")],by="idx",all.x=T)

### Toute adresse non affectée à un zonage QPV donné est considérée hors du périmètre des
### quartiers Politique de la Ville.
rpa[,idZonage][is.na(rpa[,idZonage]) | rpa$idZonage == "Hors zonage"] <- "horsZon"

# Renommage de l'identifiant du zonage
# rpa$idZonage <- as.character(rpa[,idZonage])

# Base sondage (special calage)
sondage <- sondageZon(rpa = rpa)

# Calcul de la precision analytique sans calage
RPStats <- precision_analytique_nc(rpa = rpa,Y = INPER,zonage = zonage,idZonage = "idZonage",sondage = sondage) # Nombre de personne
RPStats2 <- precision_analytique_nc(rpa = rpa,Y = INPCM,zonage = zonage,idZonage = "idZonage") # Nombre de chômeur

# Enregistrement en RData
save(RPstats,file="../../Bdd/RData/Temp/RPStats.RData")

str(rpa13)

