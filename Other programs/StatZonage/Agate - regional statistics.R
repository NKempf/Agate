#---------------------------------------------------------------------------------------------------------------------------#
#                                             Agate - Regional statistics                                                   #
#---------------------------------------------------------------------------------------------------------------------------#

# 20.06.2018

# Nicolas Kempf

# Fake regional statistics

# Packages nécessaires
#---------------------
library(tidyverse) # transform data

# Fonctions utiles
#-----------------
source ("Other programs/Fake data/Agate - Fake data fct.R")
# source("Other programs/Zonage/Agate - Cartographie fct.R")
source("Other programs/StatZonage/Agate - Statistics Zonage_v3.R")

# I. Import des bases
#-------------------------------------------------------------------------------------------------
load("Data/fakePts.RData")
load("Data/fakeData.Rdata")

# Filosofi
#---------
typmen.label <- c("famille monoparentale","couple sans enfant","couple avec enfant(s)","menage complexe",
                  "femme seule","homme seul")

# Faking data
lst_var <- c("idx","dep","com","com.lib")
rpi <- cbind(pts.fake@data[,lst_var],faking_data(rpi,nrow(pts.fake)))
rpl <- cbind(pts.fake@data[,lst_var],faking_data(rpl,nrow(pts.fake)))
filo <- cbind(pts.fake@data[,lst_var],faking_data(filo,nrow(pts.fake)))

# Attention placer ce bout de code ici (pas avant car sinon le temps de calcul explose)
filo <- as_tibble(filo) %>%
  # filter(com %in% lstCom) %>%
  mutate(typmenR.lib = factor(typmenR,labels = typmen.label))

# III. Statistiques descriptives
#--------------------------------------------------------------------------------------------------------------------------------------------------------------
dep.stat <- statistics_zone(rpi = rpi,rpl = rpl,filo = filo,group_var = c("dep"))
com.stat <- statistics_zone(rpi = rpi,rpl = rpl,filo = filo,group_var = c("com","com.lib"))

# Enregistrement en RData
save(dep.stat,com.stat,file="Data/Stats/Region and cities/region_stat.RData")
