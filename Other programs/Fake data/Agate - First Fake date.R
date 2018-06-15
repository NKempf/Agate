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
setwd(dir=dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Packages nécessaires
#---------------------
library(rgdal) # Chargement des objets spatiaux
library(tidyverse) # transform data
library(writexl) # Export des données en excel


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






