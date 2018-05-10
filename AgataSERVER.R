#---------------------------------------------------------------------------------------------------------------------------#
#                                             Agate - Server                                                                #
#---------------------------------------------------------------------------------------------------------------------------#

server <- function(input, output,session) {
  
# I. Interactive web map
#----------------------------------------------------------------------------------------------------------------------------
  
  # I.1. Initiate interactive web map
  #----------------------------------
  output$mymap <- renderLeaflet({
    leaflet("map") %>% addTiles()%>% 
      fitBounds(lng1 = -65,lat1 = 18,lng2 = -45,lat2 = 3) 
    # %>%
    #   addPolygons(data=qpv_stat,opacity = 3,
    #               color = "green", stroke = TRUE, weight = 2,
    #               fill = TRUE, fillOpacity = 0.2,popup = ~paste(NOM_QP),layerId = ~paste(CODE_QP))

  })
  
  # I.2. Mise à jour de la carte leaflet à chaque chargement de carte
  #-------------------------------------------------------------------
  observeEvent(input$file1,{
    
    if (!is.null(input$file1)) {
      # Update leaflet
      leafletProxy("map") %>%
        addPolygons(data=userMap(),opacity = 3,
                    color = "green", stroke = TRUE, weight = 2,
                    fill = F, fillOpacity = 0.2)
    }
  })
   
  
# II. Import user shapefile map
#-----------------------------------------------------------------------------------------------------------------------------------
  userMap <- eventReactive(input$file1,{
    myshape<- input$inputdata
    
    if (is.null(myshape)) 
      return(NULL)       
    
    dir<-dirname(myshape[1,4])
    for ( i in 1:nrow(myshape)) {
      file.rename(myshape[i,4], paste0(dir,"/",myshape[i,1]))
      }
    getshp <- list.files(dir, pattern="*.shp", full.names=TRUE)
    map <- readOGR(dsn = getshp)
    # Changement du système de projection
    map <- spTransform(map, "+init=epsg:4326")
    return(map)
  })
  
  
  
  
  
  

  
}
