#--------------------------------------------------------------------------------------------------------------------------------#
#                 Agate - Tests de la fonction heatmap                                                                           #
#--------------------------------------------------------------------------------------------------------------------------------#

# MAJ : 02.12.2018

# Nicolas Kempf

# Problématique : Affichage dynamique des zones densement peuplées

# Objectif : utiliser la fonction heatmap du package leaflet.extras pour afficher les zones concentrant un nombre de points importants

#  Packages necessaires
library(sp)
library(rgdal) # Lecture objets spatiaux
library(tidyverse) # Transformation de données
library(leaflet) # Carte interactive
library(leaflet.extras) # Affichage d'une carte de chaleur

# Fonctions nécessaires
source ("Other programs/Fake data/Agate - Fake data fct.R",encoding = "utf-8")

# I. Test simple de la fonction heatmap
#--------------------------------------------------------------------------------------------------------------------------------

leaflet(quakes) %>% addProviderTiles(providers$CartoDB.DarkMatter) %>%
  setView( 178, -20, 5 ) %>%
  addHeatmap(lng = ~long, lat = ~lat, intensity = ~mag,
             blur = 20, max = 0.05, radius = 15)

# II. Utilisation avec fausses données
#--------------------------------------------------------------------------------------------------------------------------------

# Chargement des faux points
load("Data/fakePts.RData")
pts.fake@proj4string

# Reprojection des données
map <- spTransform(pts.fake, "+init=epsg:4326")

# Changement des x et y
map <- map@data %>% 
  mutate(x = map@coords[,1],
         y = map@coords[,2]) %>% 
  select(idx,x,y) %>% 
  as.data.frame()

# Affichage de la carte
leaflet(map) %>% addTiles() %>%
  fitBounds(lng1 = -65,lat1 = 18,lng2 = -45,lat2 = 3) %>% 
  addHeatmap(lng = ~x, lat = ~y,
             blur = 30, radius = 10)

# III. Utilisation avec des vraies données
#-------------------------------------------------------------------------------------------------------
load("../Data/RData/filo14.Rdata")

# Transformation en objet spatial
coordinates(filo14.disp) <- ~x+y
proj4string(filo14.disp) <- pts.fake@proj4string

# Reprojection des données
filo <- spTransform(filo14.disp, "+init=epsg:4326")

colnames(filo@data)

# Changement des x et y
map <- filo@data %>% 
  mutate(x = filo@coords[,1],
         y = filo@coords[,2]) %>% 
  select(DIRNOSEQ,nivviem,x,y) %>% 
  as.data.frame()

# Affichage de la carte (Fonctionne avec des vraies données)
leaflet(map) %>% addTiles() %>%
  fitBounds(lng1 = -65,lat1 = 18,lng2 = -45,lat2 = 3) %>% 
  addHeatmap(lng = ~x, lat = ~y,
             # intensity = ~nivviem,
             blur = 40, radius = 15)

