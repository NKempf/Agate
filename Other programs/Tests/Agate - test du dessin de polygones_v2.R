#--------------------------------------------------------------------------------------------------------------------------------#
#                 Agate - Tests du package leaflet.extras                                                                        #
#--------------------------------------------------------------------------------------------------------------------------------#

# MAJ : 10.11.2018

# Nicolas Kempf

# Problématique : Dessiner des zones doit être fait sur un site indépendant de l'application

# Objectif : ajouter la fonctionnalité de dessin d'un ou plusieurs polygones à l'application

# Solutions : 
#   1) Explorer le package leaflet.extras
#   2) Utiliser le package mapedit

# I. Exploration du package leaflet.extras
#-------------------------------------------------------------------------------------------------------------------------------------------------------------

# I.1. Dessiner une carte et récupérer la figure
#-----------------------------------------------

# Solution viable voir :
# https://github.com/bhaskarvk/leaflet.extras/issues/77
# https://github.com/rstudio/leaflet/issues/227
# https://www.screencast.com/t/16rrNV6Hel


library(leaflet)
library(leaflet.extras)

leaflet() %>%
  setView(0,0,2) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addDrawToolbar(
    targetGroup='draw',
    polylineOptions = FALSE,
    circleOptions = FALSE,
    markerOptions = FALSE,
    circleMarkerOptions = FALSE,
    editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))  

# %>% # Gestion des options de dessins
#   addLayersControl(overlayGroups = c('draw'), options = 
#                      layersControlOptions(collapsed=FALSE)) %>%
#   addStyleEditor()






