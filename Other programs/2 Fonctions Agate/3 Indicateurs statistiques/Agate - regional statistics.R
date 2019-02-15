#---------------------------------------------------------------------------------------------------------------------------#
#                                             Agate - Regional statistics                                                   #
#---------------------------------------------------------------------------------------------------------------------------#

# 31.10.2018

# Nicolas Kempf

# Packages nécessaires
#---------------------
library(tidyverse) # transform data

# Fonctions utiles
#-----------------
source ("Other programs/Fake data/Agate - Fake data fct.R",encoding = "UTF-8")
# source("Other programs/Zonage/Agate - Cartographie fct.R")
source("Other programs/StatZonage/Agate - Statistics Zonage_v4.R",encoding = "UTF-8")
source("Other programs/Export Report/Agate - Export Excel fct.R",encoding = "UTF-8")


# I. Chargement des bases
#----------------------------------------------------------------------------------------------------------------------------------------------------------------
load("Data/Maps/Cities/cities.RData")
com.dom@data %>% 
  rename(com = Codgeo,
         com.lib = Libgeo) -> com.dom@data

# RP 2013
load("Data/Rp/rp13.RData")

# RP 2014
load("Data/Rp/rp14.RData")

# Filosofi 2014
load("Data/Filosofi/filo14.Rdata")

typmen.label <- c("famille monoparentale","couple sans enfant","couple avec enfant(s)","menage complexe",
                  "femme seule","homme seul")
filo14.disp <- filo14.disp %>% 
  mutate(dep = substr(com,1,3),
         typmenR.lib = factor(typmenR,labels = typmen.label)) %>% 
  left_join(com.dom@data[,c("com","com.lib")],by = "com")

# Filosofi 2015
load("Data/Filosofi/filo15.Rdata")

filo15.disp <- filo15.disp %>% 
  mutate(dep = substr(com,1,3),
         typmenR.lib = factor(typmenR,labels = typmen.label)) %>% 
  left_join(com.dom@data[,c("com","com.lib")],by = "com")


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
save(statReg_rp14_filo15,statCom_rp14_filo15,file="Data/Statistiques Zonage/StatRegCom_rp14_filo15.RData")
save(statReg_rp14_filo14,statCom_rp14_filo14,file="Data/Statistiques Zonage/StatRegCom_rp14_filo14.RData")
save(statReg_rp13_filo14,statCom_rp13_filo14,file="Data/Statistiques Zonage/StatRegCom_rp13_filo14.RData")



