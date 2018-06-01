#--------------------------------------------------------------------------------------------------------------------------------#
#                      Agate - Statistiques au niveau Régional et communal                                                       #
#--------------------------------------------------------------------------------------------------------------------------------#

# MAJ : 30.05.2018

# Nicolas Kempf

# Ce programme calcule les statistiques issues du RP, de filosofi et prochainement : de Fideli et de la BPE au niveau Régional et communal
# Les résultats sont calculés et stockés en RData à chaque nouveau millesime d'une base

### Working directory 
# Paramètre automatiquement le dossier dans lequel se trouve le programme comme espace de travail
# Ne fonction que pour une version de R supérieure à 3.3
setwd(dir=dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Packages nécessaires
#---------------------
library(tidyverse) # transformation des données et calcul de statistiques descriptives simples.
library(writexl) # Export simple des données en excel

# Fonctions nécessaires
#----------------------
source("Agate - Statistics Zonage_v3.R")

# Chargement des bases
#---------------------
# RP 2013
load("../../Bdd/RData/Rp/rp13.RData")

# RP 2014
load("../../Bdd/RData/Rp/rp14.RData")

# Filosofi 2014
load("../../Bdd/RData/Filosofi/filo14.Rdata")

typmen.label <- c("famille monoparentale","couple sans enfant","couple avec enfant(s)","menage complexe",
                  "femme seule","homme seul")
filo14.disp <- filo14.disp %>% 
  mutate(dep = substr(com,1,3),
         typmenR.lib = factor(typmenR,labels = typmen.label))

# Filosofi 2015
load("../../Bdd/RData/Filosofi/filo15.Rdata")

filo15.disp <- filo15.disp %>% 
  mutate(dep = substr(com,1,3),
         typmenR.lib = factor(typmenR,labels = typmen.label))


# Statistiques régionales
#------------------------------------------------------------------------------------------------------------------------------------------------------------
statReg_rp14_filo15 <- statistics_zone(rpi = rp14i,rpl = rp14l,filo = filo15.disp,group_var = "dep")
statReg_rp14_filo14 <- statistics_zone(rpi = rp14i,rpl = rp14l,filo = filo14.disp,group_var = "dep")
statReg_rp13_filo14 <- statistics_zone(rpi = rp13i,rpl = rp13l,filo = filo14.disp,group_var = "dep")

# Statistiques communales
#------------------------------------------------------------------------------------------------------------------------------------------------------------
statCom_rp14_filo15 <- statistics_zone(rpi = rp14i,rpl = rp14l,filo = filo15.disp,group_var = c("dep","com","com.lib"))
statCom_rp14_filo14 <- statistics_zone(rpi = rp14i,rpl = rp14l,filo = filo14.disp,group_var = c("dep","com","com.lib"))
statCom_rp13_filo14 <- statistics_zone(rpi = rp13i,rpl = rp13l,filo = filo14.disp,group_var = c("dep","com","com.lib"))

# Enregistrement en RData
#------------------------------------------------------------------------------------------------------------------------------------------------------------
save(statReg_rp14_filo15,statCom_rp14_filo15,file="../../Bdd/RData/Statistiques Zonage/StatRegCom_rp14_filo15.RData")
save(statReg_rp14_filo14,statCom_rp14_filo14,file="../../Bdd/RData/Statistiques Zonage/StatRegCom_rp14_filo14.RData")
save(statReg_rp13_filo14,statCom_rp13_filo14,file="../../Bdd/RData/Statistiques Zonage/StatRegCom_rp13_filo14.RData")


# Export des données
#------------------------------------------------------------------------------------------------------------------------------------------------------------
writexl::write_xlsx(path = "../../Reporting/Statistiques sur une zone/Region/StatReg_rp14_filo15.xlsx",lapply(statReg_rp14_filo15,as.data.frame))
writexl::write_xlsx(path = "../../Reporting/Statistiques sur une zone/Region/StatReg_rp14_filo14.xlsx",lapply(statReg_rp14_filo14,as.data.frame))
writexl::write_xlsx(path = "../../Reporting/Statistiques sur une zone/Region/StatReg_rp13_filo14.xlsx",lapply(statReg_rp13_filo14,as.data.frame))


writexl::write_xlsx(path = "../../Reporting/Statistiques sur une zone/Commune/StatCom_rp14_filo15.xlsx",lapply(statCom_rp14_filo15,as.data.frame))
writexl::write_xlsx(path = "../../Reporting/Statistiques sur une zone/Commune/StatCom_rp14_filo14.xlsx",lapply(statCom_rp14_filo14,as.data.frame))
writexl::write_xlsx(path = "../../Reporting/Statistiques sur une zone/Commune/StatCom_rp13_filo14.xlsx",lapply(statCom_rp13_filo14,as.data.frame))

