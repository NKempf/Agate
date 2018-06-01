#--------------------------------------------------------------------------------------------------------------------------------#
#                      Agate - Fonction calcul de statistiques selon un zonage                                                   #
#--------------------------------------------------------------------------------------------------------------------------------#

# MAJ : 29.05.2018

# Nicolas Kempf

# Ce programme calcule les statistiques issues du RP, de filosofi et prochainement : de Fideli et de la BPE. 

### Working directory 
# Paramètre automatiquement le dossier dans lequel se trouve le programme comme espace de travail
# Ne fonction que pour une version de R supérieure à 3.3
setwd(dir=dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

load(file="../../Bdd/RData/Temp/rpTidyStat.RData")

# load("../../Bdd/RData/Ancien/statTmp.RData")

# Libellé des communes
# comLib <- read.csv2("../../Bdd/RP13/RP13_Communes_DFA.csv",stringsAsFactors = F) %>% 
#   arrange(cod_mod)

library(tidyverse)
library(writexl)

source(file = "Agate - Statistics Zonage_v3.R")


# I. Préparation des tables
#--------------------------


# lstCom <- unique(rpi$com[!is.na(rpi$idZonage)])

# Selection des individus des communes dans lesquelles se trouve un zonage
# RP individu
#------------
rpi <- as_tibble(rpi) %>%
  # filter(com %in% lstCom) %>%
  mutate(idZonage = ifelse(is.na(idZonage),"Hors zonage", idZonage))

# Rp logement
#------------
rpl <- as_tibble(rpl) %>%
  # filter(com %in% lstCom) %>%
  mutate(idZonage = ifelse(is.na(idZonage),"Hors zonage", idZonage))

# Filosofi
#---------
typmen.label <- c("famille monoparentale","couple sans enfant","couple avec enfant(s)","menage complexe",
                  "femme seule","homme seul")

filo <- as_tibble(filo) %>% 
  # filter(com %in% lstCom) %>%
  mutate(idZonage = ifelse(is.na(idZonage),"Hors zonage", idZonage),
         typmenR.lib = factor(typmenR,labels = typmen.label),
         dep = substr(com,1,3))



StatZona <- statistics_zone(rpi = rpi,rpl = rpl,filo = filo,group_var = c("com","com.lib","idZonage"))

# Enregistrement en RData
save(StatZona,file="../../Bdd/RData/Temp/StatZonav2.RData")



View(test$tFilo.I.3)
