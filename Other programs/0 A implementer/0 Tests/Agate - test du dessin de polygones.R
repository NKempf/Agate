#--------------------------------------------------------------------------------------------------------------------------------#
#                 Agate - Tests du package leaflet.extras                                                                        #
#--------------------------------------------------------------------------------------------------------------------------------#

# MAJ : 26.10.2018

# Nicolas Kempf

# Problématique : Dessiner des zones doit être fait sur un site indépendant de l'application

# Objectif : ajouter la fonctionnalité de dessin d'un ou plusieurs polygones à l'application

# Solutions : 
#   1) Explorer le package leaflet.extras
#   2) Utiliser le package mapedit

# I. Exploration du package leaflet.extras
#-------------------------------------------------------------------------------------------------------------------------------------------------------------

# Article : https://redoakstrategic.com/geoshaper/


library(shiny)
library(leaflet)
library(leaflet.extras)
library(sp)
library(geoshaper)

# source: https://opendata.socrata.com/dataset/Airport-Codes-mapped-to-Latitude-Longitude-in-the-/rxrh-4cxm
airports <- read.csv('Data/Tmp/Airport_Codes_mapped_to_Latitude_Longitude_in_the_United_States.csv')

# longitudinal coordinates in dataset are off, reverse to negative values to place them in the western hemisphere
airports$Longitude <- airports$Longitude - 2 * airports$Longitude

# generate second set of unique location IDs for second layer of selected locations
airports$secondLocationID <- paste(as.character(airports$locationID), "_selectedLayer", sep="")

coordinates <- SpatialPointsDataFrame(airports[,c('Longitude', 'Latitude')] , airports)

head(airports)


shinyApp(
  ui <- fluidPage(
    leafletOutput("mymap")
  ),
  
  server <- function(input, output) {
    
    ################################################# section one #################################################
    # list to store the selections for tracking
    data_of_click <- reactiveValues(clickedMarker = list())
    
    ################################################# section two #################################################
    # base map
    output$mymap <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        addCircles(data = airports,
                   radius = 1000,
                   lat = airports$Latitude,
                   lng = airports$Longitude,
                   fillColor = "white",
                   fillOpacity = 1,
                   color = "hotpink",
                   weight = 2,
                   stroke = T,
                   layerId = as.character(airports$locationID),
                   highlightOptions = highlightOptions(color = "mediumseagreen",
                                                       opacity = 1.0,
                                                       weight = 2,
                                                       bringToFront = TRUE)) %>%
        addDrawToolbar(
          targetGroup='Selected',
          polylineOptions=FALSE,
          markerOptions = FALSE,
          polygonOptions = drawPolygonOptions(shapeOptions=drawShapeOptions(fillOpacity = 0
                                                                            ,color = 'white'
                                                                            ,weight = 3)),
          rectangleOptions = drawRectangleOptions(shapeOptions=drawShapeOptions(fillOpacity = 0
                                                                                ,color = 'white'
                                                                                ,weight = 3)),
          circleOptions = drawCircleOptions(shapeOptions = drawShapeOptions(fillOpacity = 0
                                                                            ,color = 'white'
                                                                            ,weight = 3)),
          editOptions = editToolbarOptions(edit = FALSE, selectedPathOptions = selectedPathOptions()))
    })
    
    ############################################### section three #################################################
    observeEvent(input$mymap_draw_new_feature,{
      #Only add new layers for bounded locations
      found_in_bounds <- findLocations(shape = input$mymap_draw_new_feature
                                       , location_coordinates = coordinates
                                       , location_id_colname = "locationID")
      
      for(id in found_in_bounds){
        if(id %in% data_of_click$clickedMarker){
          # don't add id
        } else {
          # add id
          data_of_click$clickedMarker<-append(data_of_click$clickedMarker, id, 0)
        }
      }
      
      # look up airports by ids found
      selected <- subset(airports, locationID %in% data_of_click$clickedMarker)
      
      proxy <- leafletProxy("mymap")
      proxy %>% addCircles(data = selected,
                           radius = 1000,
                           lat = selected$Latitude,
                           lng = selected$Longitude,
                           fillColor = "wheat",
                           fillOpacity = 1,
                           color = "mediumseagreen",
                           weight = 3,
                           stroke = T,
                           layerId = as.character(selected$secondLocationID),
                           highlightOptions = highlightOptions(color = "hotpink",
                                                               opacity = 1.0,
                                                               weight = 2,
                                                               bringToFront = TRUE))
      
    })
    
    ############################################### section four ##################################################
    observeEvent(input$mymap_draw_deleted_features,{
      # loop through list of one or more deleted features/ polygons
      for(feature in input$mymap_draw_deleted_features$features){
        
        # get ids for locations within the bounding shape
        bounded_layer_ids <- findLocations(shape = feature
                                           , location_coordinates = coordinates
                                           , location_id_colname = "secondLocationID")
        
        
        # remove second layer representing selected locations
        proxy <- leafletProxy("mymap")
        proxy %>% removeShape(layerId = as.character(bounded_layer_ids))
        
        first_layer_ids <- subset(airports, secondLocationID %in% bounded_layer_ids)$locationID
        
        data_of_click$clickedMarker <- data_of_click$clickedMarker[!data_of_click$clickedMarker
                                                                   %in% first_layer_ids]
      }
    })
  },
  
  options = list(height = 400)
)


# https://stackoverflow.com/questions/48858581/get-coordinates-from-a-drawing-object-from-an-r-leaflet-map


library(leaflet.extras)

# Define UI 
ui <- fluidPage(
  leafletOutput("mymap",height=800)
)

# Define server logic 
server <- function(input, output) {
  
  output$mymap <- renderLeaflet(
    leaflet() %>%
      addProviderTiles("Esri.OceanBasemap",group = "Ocean Basemap") %>%
      setView(lng = -166, lat = 58.0, zoom = 5) %>%
      addDrawToolbar(
        targetGroup='draw',
        editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))  %>%
      addLayersControl(overlayGroups = c('draw'), options =
                         layersControlOptions(collapsed=FALSE))
  )
  
  observeEvent(input$mymap_draw_new_feature,{
    feature <- input$mymap_draw_new_feature
    
    print(feature)
    
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)


# II. Exploration du package map.edit
#-------------------------------------------------------------------------------------------------------------------------------------------------------------

library(mapview)
library(mapedit)

what_we_created <- mapview() %>%
  editMap()

mapview(what_we_created$finished)

# Conversion d'un objet sf to sp
test <- as(what_we_created$finished, "Spatial")
plot(test)


library(sf)

# simplified border for purpose of exercise
border <- st_as_sfc(
  "LINESTRING(-109.050197582692 31.3535554844322, -109.050197582692 31.3535554844322, -111.071681957692 31.3723176640684, -111.071681957692 31.3723176640684, -114.807033520192 32.509681296831, -114.807033520192 32.509681296831, -114.741115551442 32.750242384668, -114.741115551442 32.750242384668, -117.158107738942 32.5652527715121, -117.158107738942 32.5652527715121)"
) %>%
  st_set_crs(4326)


library(shiny)
edited_features <- runGitHub(
  "geojson.io", "timelyportfolio", ref="shiny"
)











