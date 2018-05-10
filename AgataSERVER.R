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

  # I.2. Update leaflet with user map
  #----------------------------------
  observeEvent(input$file1,{
    
    if (!is.null(input$file1)) {
      # Boundary box
      userMap.bbox <- as.data.frame(bbox(userMap()))
      # Update leaflet
      leafletProxy("mymap") %>%
        fitBounds(lng1 = userMap.bbox$min[1],lat1 = userMap.bbox$max[2],lng2 = userMap.bbox$max[1],lat2 = userMap.bbox$min[2]) %>%
        addPolygons(data=userMap(),opacity = 3,
                                  color = "green", stroke = TRUE, weight = 2,
                                  fill = TRUE, fillOpacity = 0.2,popup = ~paste(NOM_QP),layerId = ~paste(CODE_QP))
        
        
        
        
        # addPolygons(data=qpv_stat,opacity = 3,
        #             color = "green", stroke = TRUE, weight = 2,
        #             fill = F, fillOpacity = 0.2,popup = ~paste(NOM_QP),layerId = ~paste(CODE_QP))
    }
  })
   
  
# II. Import user shapefile map
#-----------------------------------------------------------------------------------------------------------------------------------
  userMap <- eventReactive(input$file1,{
    myshape<- input$file1
    
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
  
  
  
# III. InfraCity statistical computation 
#-----------------------------------------------------------------------------------------------------------------------------------
  observeEvent(input$b_calcul, {
    
    t1 <- Sys.time()  
    # Test de bar de progression
    
    # Create 0-row data frame which will be used to store data
    dat <- data.frame(x = numeric(0), y = numeric(0))
    
    # withProgress calls can be nested, in which case the nested text appears
    # below, and a second bar is shown.
    withProgress(message = 'Generating data',style = "notification", detail = "part 0", value = 0, {
      for (i in 1:10) {
        # Each time through the loop, add another row of data. This a stand-in
        # for a long-running computation.
        dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
        
        # Increment the progress bar, and update the detail text.
        incProgress(0.1, detail = paste("part", i))
        
        # Pause for 0.1 seconds to simulate a long computation.
        Sys.sleep(0.1)
      }
    })
    
    Sys.sleep(0.1)
    
    # Pop-up indiquant la fin du calcul
    temps <- as.character(round(abs(difftime(t1,Sys.time(), units="secs")),2))
    
    shinyWidgets::sendSweetAlert(
      session = session, 
      title = "Terminé !", text = paste("Le calcul a été effectué en ",temps," secondes"), type = "success"
    )
    
    # Ferme automatiquement le bsmodal options avancées
    
  })
  
  

  
}
