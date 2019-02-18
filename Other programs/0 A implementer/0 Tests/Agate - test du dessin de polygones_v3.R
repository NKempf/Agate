#--------------------------------------------------------------------------------------------------------------------------------#
#                 Agate - Tests du package leaflet.extras                                                                        #
#--------------------------------------------------------------------------------------------------------------------------------#

# MAJ : 02.12.2018

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
library(sp)

source("Other programs/Drawings/Agate - Drawings.R",encoding = "utf-8")

# I. UI
#----------------------------------------------------------------------------------------------------------------------------------

# Fonction permettant de supprimer l'ensemble des objets dessinés
scr <- tags$script(HTML(
  "
  Shiny.addCustomMessageHandler(
  'removeleaflet',
  function(x){
  console.log('deleting',x)
  // get leaflet map
  var map = HTMLWidgets.find('#' + x.elid).getMap();
  // remove
  map.removeLayer(map._layers[x.layerid])
  })
  "
))

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
                        scr,# Remove drawn shape
                        leafletOutput("leafmap", width = "100%", height = "100%"),
                        
                        # I.1.2. Statistical controls
                        #----------------------------
                        absolutePanel(top = 30, right = 20,height=200, width=400,
                                      # Affichage du logo Insee
                                      #img(src = "Logo_Insee.png", height = 72, width = 72,align="right"),
                                      
                                      wellPanel(
                                        DTOutput('x1'),
                                        actionButton(inputId = "ab_finish",label = "Finaliser figures"),
                                        actionButton(inputId = "ab_finish2",label = "Terminer")
                                      ),
                                      style = "opacity: 0.75; z-index: 1000;" # IMPORTANT : Absolute panel not hidden by tiles
                        )
                    ) # end div
))

# II. SERVER
#----------------------------------------------------------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # II.0. Valeur réactive
  #-------------------------------------------------------------------
  rv <- reactiveValues(AgateMap=NULL,
                       statZone=NULL,
                       statHZone=NULL,
                       qualityZone=NULL,
                       drawnshapes=NULL)
  
  # II.1. Carte de base
  #--------------------------------------------------------------------
  output$leafmap <- renderLeaflet({
    
    # load("Data/Tmp/polyTmp.RData")
    # rv$AgateMap <- zone
    
    # Boundary box
    # AgateMap.bbox <- as.data.frame(bbox(rv$AgateMap))
    leaflet() %>%
      addTiles() %>% 
      fitBounds(lng1 = -65,lat1 = 18,lng2 = -45,lat2 = 3) %>% 
      # fitBounds(lng1 = AgateMap.bbox$min[1],lat1 = AgateMap.bbox$max[2],lng2 = AgateMap.bbox$max[1],lat2 = AgateMap.bbox$min[2]) %>%
      # addPolygons(data=rv$AgateMap,popup = ~name,group = 'draw',layerId = ~id) %>% 
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
    # print("All Features")
    print(input$leafmap_draw_all_features)
    
    
    # 2.1. Liste des identifiants (leaflet.extras) des figures dessinées
    rv$drawnshapes <- lapply(
      input$leafmap_draw_all_features$features,
      function(ftr) {
        ftr$properties$`_leaflet_id`
      }
    )
    
    print(rv$drawnshapes)

  })
  
  # Finalisation des polygones dessinés
  observeEvent(input$ab_finish,{
    
    # 1) Enregistrement de la liste des features
    liste <- input$leafmap_draw_all_features
    save(liste,file="Data/Tmp/leafLetDraw2.RData")
    print("Figure sauvegardée !")
    
    # 2) Conversion features to spatialpolygones
    rv$AgateMap <- featureToSpatialPolygonDF(featureList = liste)
    
    # 1) Enregistrement de carte en RData
    #------------------------------------
    zone <- rv$AgateMap
    save(zone,file = "Data/Tmp/polyTmp.RData")
    print("Polygones sauvegardés !!")
    
    print(str(rv$AgateMap))
    
    
    # 3) Mise à jour de la carte leaflet
    #-----------------------------------
    if (!is.null(rv$AgateMap)) {
      # Boundary box
      AgateMap.bbox <- as.data.frame(bbox(rv$AgateMap))
      # Update leaflet
      leafletProxy("leafmap") %>%
        fitBounds(lng1 = AgateMap.bbox$min[1],lat1 = AgateMap.bbox$max[2],lng2 = AgateMap.bbox$max[1],lat2 = AgateMap.bbox$min[2]) 
      # %>%
      #   addPolygons(data=rv$AgateMap,opacity = 3,
      #               color = "green", stroke = TRUE, weight = 2,
      #               fill = TRUE, fillOpacity = 0.2,popup = ~paste(name),layerId = ~paste(id),group = "draw")
      
      output$x1 = renderDT({
        datatable(rv$AgateMap@data,rownames = FALSE, editable = TRUE,selection = "single")
      })
    }
  })
  
  # Finalisation des polygones dessinés (Fonctionne !)
  observeEvent(input$ab_finish2,{
    
    # 2) Suppresssion des figures dessinées
    #--------------------------------------
    # 2.2. Suppression des figures
    lapply(
      rv$drawnshapes,
      function(todelete) {
        session$sendCustomMessage(
          "removeleaflet",
          list(elid="leafmap", layerid=todelete)
        )
      }
    )
    
    # Boundary box
    AgateMap.bbox <- as.data.frame(bbox(rv$AgateMap))
    # Update leaflet
    leafletProxy("leafmap") %>%
      fitBounds(lng1 = AgateMap.bbox$min[1],lat1 = AgateMap.bbox$max[2],lng2 = AgateMap.bbox$max[1],lat2 = AgateMap.bbox$min[2]) %>%
      addPolygons(data=rv$AgateMap,opacity = 3,
                  color = "green", stroke = TRUE, weight = 2,
                  fill = TRUE, fillOpacity = 0.2,popup = ~paste(name),layerId = ~paste(idZonage))
    
    print(class(rv$AgateMap))
  })
  
  
  
  # II.. Table attributaire édition
  #-----------------------------------------------------------------------------------------------------------
  
  # https://stackoverflow.com/questions/28274584/get-selected-row-from-datatable-in-shiny-app
  
  # Affichage de la table attributaire dans un DT
  # output$x1 = renderDT({
  #   
  #   datatable(data.frame(id=NA,name=NA),rownames = FALSE, editable = TRUE)
  #   print(!is.null(rv$AgateMap))
  #   
  #   # if(!is.null(rv$AgateMap)){
  #   #   datatable(rv$AgateMap@data, selection = 'single', rownames = FALSE, editable = TRUE)
  #   # }else{
  #   #   
  #   # }
  #   
  # })
  
  # Mise à jour de la DT (Fonctionne !)
  proxy = dataTableProxy('x1')
  observeEvent(input$x1_cell_edit, {
    info = input$x1_cell_edit
    str(info)
    i = info$row
    j = info$col + 1
    v = info$value
    rv$AgateMap@data[i, j] <- DT::coerceValue(v, rv$AgateMap@data[i, j])
    replaceData(proxy, rv$AgateMap@data, resetPaging = FALSE)  # important
  })

  # Ligne selectionnée
  observeEvent(input$x1_rows_selected,{
    
    # 1) Selection du polygone
    mapSelect <- rv$AgateMap[input$x1_rows_selected,]
    
    # 2) Boundary Box du polygone
    mapSelect.bbox <- as.data.frame(bbox(mapSelect))
    
    # 3) Mise à jour de la carte
    leafletProxy("leafmap") %>%
      fitBounds(lng1 = mapSelect.bbox$min[1],lat1 = mapSelect.bbox$max[2],lng2 = mapSelect.bbox$max[1],lat2 = mapSelect.bbox$min[2])
    
    # 4) Affichage de la ligne
    print(input$x1_rows_selected)
  })
}

# III. Launch App
#----------------------------------------------------------------------------------------------------------------------------------
shinyApp(ui, server)




# Gestion de l'edition des cartes : plusieurs solutions : 
# 1) Trouver l'identifiant du polygon correspondant dans la feature pour réassocier correctement la couche cartographique modifiée 
# C'est la solution idéale car la solution la plus souple.
# 2) Faire la création de nouveau élément et l"édition de manière séparée avec deux boutons. Solution moins sympa mais je pense plus
# facile à coder.

# edit : 19.11.2018 : Fonctionnalité de creation de carte et de mise à jour de la table attributaire foncitonne désormais. Actuellement,
#  il faut créer des polygones, cliquer sur finaliser polygones, mettre à jour la table attributaire puis cliquer sur terminer. 
#  Soit 4 actions ce qui semble trop. La fonctionnalité de suppression des objets leaflet.extras a permis d'accéder à l'identifiant
#  des polygones attribués par leaflet.extras.
# Il faut désormais trouver comment associer cette identifiant avec celui des couches cartographiques.

# L'édition de polygones préalablement chargés ne créer par l'identifiant dans la feature associée. Il faut donc trouver un 
# moyen de mettre à jour l'identifiant de la feature.


# Derniere fonctionnalité à développer : Ajout des polygones dessinés à une éventuelle couche carto chargée par l'utilisateur




# Idée : ajouter heatmap pour indiquer ou se trouve la plus grande concentration de points





