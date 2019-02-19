#----------------------------------------------------------------------------------------------------------------------#
#                                   Agate - HeatPoint                                                                  #
#----------------------------------------------------------------------------------------------------------------------#

# 18.02.2019

# Heatpoints selection
library(fst)
library(fstplyr)
library(sp)
library(tidyverse)

# Chargement du ril
# load(file = "Data/Ril/Ancien/ril15.RData")
ril <- read_fst("Data/Ril/ril_leger.fst")
heat.pts <- SpatialPointsDataFrame(coords = ril[,c("X","Y")],data = ril,proj4string = CRS("+init=epsg:3857"))

# Selection d'une adresse sur 10
heat.pts <- heat.pts[seq(from = 1,to = nrow(heat.pts@data),by = 10),]
heat.pts <-  spTransform(heat.pts,"+init=epsg:4326")

heat.pts <- heat.pts@data %>% 
  mutate(id = 1:nrow(heat.pts@data),
         x = heat.pts@coords[,1],
         y = heat.pts@coords[,2]) %>%
  rename(nb_log = NB_LOG) %>% 
  select(id,nb_log,x,y)

# Enregistrement
save(heat.pts,file = "Data/Maps/HeatPoint/heatpoints.Rdata")





