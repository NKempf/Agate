#--------------------------------------------------------------------------------------------------------------------------------#
#                     Agate - RP and Filo Fake data First time                                                                             #
#--------------------------------------------------------------------------------------------------------------------------------#

# MAJ : 3.06.2018

# Nicolas Kempf

# Ce programme tranforme les vrai données du RP et filosofi en fake data qui seront utilisée ensuite dans la 
# version de démoinstration d'Agate

### Working directory 
# Paramètre automatiquement le dossier dans lequel se trouve le programme comme espace de travail
# Ne fonction que pour une version de R supérieure à 3.3
# setwd(dir=dirname(rstudioapi::getActiveDocumentContext()$path))
# getwd()

# Packages nécessaires
#---------------------
library(rgdal) # Chargement des objets spatiaux
library(tidyverse) # transform data
library(writexl) # Export des données en excel

source("Other programs/Fake data/Agate - Fake data fct.R")


# I. Import des données du RP et de filosofi
#-------------------------------------------------------------------------------------------------
load("../Bases/R/rp14.RData")

rpi <- rp14i %>% 
  select(-idx,-dep,-com,-com.lib,-ilot)
rpl <- rp14l %>% 
  select(-idx,-dep,-com,-com.lib,-ilot,-C_ANNEE_COL,-C_GEO,-C_IMM,-C_LOG)  

colnames(rpi)
colnames(rpl)

load("../Bases/R/filo14.Rdata")

filo <- filo14.disp %>% 
  select(-DIRNOSEQ,-qp,-idx,-x,-y,-com,-REG16)

# II. Faking data
#---------------------------------------------------------------------------------------------------
nbObs <- 2000
data.filter <- seq(1,nbObs,1)

# Simule des fausses données ayant la structure de vrai données
faking_data <- function(df){
  L <- lapply(colnames(df), function(x){
    return(sample(df[,x],replace = TRUE))
  })
  df2 <-  data.frame(do.call(data.frame, L))
  colnames(df2) <-  colnames(df)
  return(df2)
}

# RP individu
#------------
rpi <- faking_data(df = rpi[data.filter,])

# Rp logement
#------------
rpl <- faking_data(df = rpl[data.filter,])

# Filosofi
#---------
filo <- faking_data(df = filo[data.filter,])

# Enregistrement des fausses bases
save(rpi,rpl,filo,file = "../Bases/R/fakeData.Rdata")


# III. Creation d'une série de point aléatoire dans chaque département
#-------------------------------------------------------------------------------------------------

# Chargement de la carte des communes
com <- readOGR(dsn = "../Cartes/Communes/communeDOM.shp")
com <- spTransform(com, CRS("+init=epsg:3857"))

# III.1. Guadeloupe
#------------------
nObs <- 100000

# Chargement de la carte de Guadeloupe
dep971 <- readOGR(dsn = "../Cartes/971/admin-departement.shp")
dep971 <- spTransform(dep971, CRS("+init=epsg:3857"))

# Generation des points aléatoires 200000 pour la Guadeloupe
pts.971 <- spsample(dep971 , n = nObs,type = "random")

# Ajout du département et de la commune
pts.971 <- fake_zonaRil(zone = com,pts.alea = pts.971)

# III.2. Martinique
#------------------

# Chargement de la carte de Guadeloupe
dep972 <- readOGR(dsn = "../Cartes/972/admin-departement.shp")
dep972 <- spTransform(dep972, CRS("+init=epsg:3857"))

# Generation des points aléatoires 200000 pour la Guadeloupe
pts.972 <- spsample(dep972 , n = nObs,type = "random")

# Ajout du département et de la commune
pts.972 <- fake_zonaRil(zone = com,pts.alea = pts.972)

# III.3. Guyane
#--------------

# Chargement de la carte de Guadeloupe
dep973 <- readOGR(dsn = "../Cartes/973/admin-departement.shp")
dep973 <- spTransform(dep973, CRS("+init=epsg:3857"))

# Generation des points aléatoires 200000 pour la Guadeloupe
pts.973 <- spsample(dep973 , n = nObs,type = "random")

# Ajout du département et de la commune
pts.973 <- fake_zonaRil(zone = com,pts.alea = pts.973)

# IV. Fusion des bases
#-----------------------------------------------------------------------------------------------------------------
pts.fake <- rbind(pts.971,pts.972) %>% 
  rbind(pts.973)

# Identifiant fictif
pts.fake@data$idx <- paste0("id",rownames(pts.fake@data))

# 
# load("Data/fakeData.Rdata")
# 
# # Fake rpi
# rpi.fake <- faking_data(rpi,nrow(pts.fake))
# rpl.fake <- faking_data(rpl,nrow(pts.fake))
# filo.fake <- faking_data(filo,nrow(pts.fake))


# Enregistrement des points 
save(pts.fake,file="Data/fakePts.RData")





