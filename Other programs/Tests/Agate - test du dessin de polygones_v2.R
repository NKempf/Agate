#--------------------------------------------------------------------------------------------------------------------------------#
#                 Agate - Tests du package leaflet.extras                                                                        #
#--------------------------------------------------------------------------------------------------------------------------------#

# MAJ : 13.11.2018

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

# The long and short of it is:
#   
# 1) Create reactive list of SpatialPolygons as they are drawn (converting based on the drawn coordinates) with 
# input$map_draw_new_feature
# 2) Observing input$map_draw_stop, a text input box is displayed, prompting the user to label their polygon, 
# which is saved in a reactive context
# 3) Labels are appended in a data frame based on aforementioned indexing (length of reactive list)
# An action button is employed to "finalize" polygons by creating a SpatialPolygonsDataFrame, which combines the reactive SpatialPolygons and the created data frame with labels.
# You can then use leafletProxy() to remove the drawn polygons and plot the same shapes as a single SPDF with labels, layerIds, etc. Hope this makes sense. I'm sure there are other more elegant solutions, but the point that I'm trying to make is that it's totally possible to assign layerIds after polygons are drawn. :)

library(leaflet.extras)
library(shiny)
library(tidyverse)
library(DT)

source("Other programs/Drawings/Agate - Drawings.R",encoding = "utf-8")

# I. UI
#----------------------------------------------------------------------------------------------------------------------------------
ui <- navbarPage("Agate",theme = "cosmo",collapsible=TRUE,
           
           # I. Interactive web map
           #-------------------------------------------------------------------------------------------------------------
           tabPanel("Carte",value="vis",
                    
                    #-----------------#
                    # I.1. Map object #
                    #-----------------#
                    div(class="outer",
                        # NB : Leaflet map need agate.css to fit windows
                        tags$head(includeCSS("www/agate.css")),
                        # I.1.1. Leaflet map
                        #-----------------
                        leafletOutput("leafmap", width = "100%", height = "100%"),
                        
                        # I.1.2. Statistical controls
                        #----------------------------
                        absolutePanel(top = 30, right = 20,height=200, width=400,
                                      # Affichage du logo Insee
                                      #img(src = "Logo_Insee.png", height = 72, width = 72,align="right"),
                                      
                                      wellPanel(
                                        DTOutput('x1')
                                      ),
                                      style = "opacity: 0.75; z-index: 1000;" # IMPORTANT : Absolute panel not hidden by tiles
                        )
                    ) # end div
))

# II. SERVER
#----------------------------------------------------------------------------------------------------------------------------------
server <- function(input, output, session) {
  output$leafmap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>% 
      fitBounds(lng1 = -65,lat1 = 18,lng2 = -45,lat2 = 3) %>% 
      addPolygons(data=zone,popup = ~name,group = 'draw') %>% 
      addDrawToolbar(
        targetGroup='draw',
        polylineOptions = FALSE,
        circleOptions = FALSE,
        markerOptions = FALSE,
        circleMarkerOptions = FALSE,
        editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions())) 
    # %>%
    #   addStyleEditor()
  })
  
  # Start of Drawing
  observeEvent(input$leafmap_draw_start, {
    print("Start of drawing")
    print(input$leafmap_draw_start)
  })
  
  # Stop of Drawing
  observeEvent(input$leafmap_draw_stop, {
    print("Stopped drawing")
    print(input$leafmap_draw_stop)
  })
  
  # New Feature
  observeEvent(input$leafmap_draw_new_feature, {
    print("New Feature")
    print(input$leafmap_draw_new_feature)
  })
  
  # Edited Features
  observeEvent(input$leafmap_draw_edited_features, {
    print("Edited Features")
    print(input$leafmap_draw_edited_features)
  })
  
  # Deleted features
  observeEvent(input$leafmap_draw_deleted_features, {
    print("Deleted Features")
    print(input$leafmap_draw_deleted_features)
  })
  
  # We also listen for draw_all_features which is called anytime
  # features are created/edited/deleted from the map
  observeEvent(input$leafmap_draw_all_features, {
    print("All Features")
    # print(input$leafmap_draw_all_features)

    # 1) Enregistrement de la liste des features
    liste <- input$leafmap_draw_all_features
    save(liste,file="Data/Tmp/leafLetDraw.RData")
    print("Figure sauvegardée !")

    # 2) Conversion features to spatialpolygones
    zone <- featureToSpatialPolygonDF(featureList = liste)
    save(zone,file = "Data/Tmp/polyTmp.RData")
    print("Polygones sauvegardés !!")

    print(str(zone))

  })
  
  # II.. Table attributaire
  #-----------------------------------------------------------------------------------------------------------
  
  # https://stackoverflow.com/questions/28274584/get-selected-row-from-datatable-in-shiny-app
  
  # Affichage de la table attributaire dans un DT
  output$x1 = renderDT(zone@data, selection = 'single', server = F, editable = T,rownames = FALSE)
  
  # Mise à jour de la DT
  proxy = dataTableProxy('x1')
  observeEvent(input$x1_cell_edit, {
    info = input$x1_cell_edit
    str(info)
    i = info$row
    j = info$col
    v = info$value
    zone@data[i, j] <<- DT::coerceValue(v, zone@data[i, j])
    replaceData(proxy, zone@data, resetPaging = FALSE)  # important
  })

  # Ligne selectionnée
  observeEvent(input$x1_rows_selected,{
    print(input$x1_rows_selected)
  })
}

# III. Launch App
#----------------------------------------------------------------------------------------------------------------------------------
shinyApp(ui, server)




