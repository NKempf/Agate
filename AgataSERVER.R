#---------------------------------------------------------------------------------------------------------------------------#
#                                             Agate - Server                                                                #
#---------------------------------------------------------------------------------------------------------------------------#

source(file = "AgataSERVER_dashboard.R",encoding = "UTF-8")


server <- function(input, output,session) {
# 0. Reactive Values
#----------------------------------------------------------------------------------------------------------------------------
  rv <- reactiveValues(AgateMap=NULL)
  
   
# I. Interactive web map
#----------------------------------------------------------------------------------------------------------------------------
  
  # I.1. Initiate interactive web map
  #----------------------------------
  output$mymap <- renderLeaflet({
    leaflet("map") %>% addTiles()%>% 
      fitBounds(lng1 = -65,lat1 = 18,lng2 = -45,lat2 = 3) %>%
      addPolygons(data=qpv_stat,opacity = 3,
                  color = "green", stroke = TRUE, weight = 2,
                  fill = TRUE, fillOpacity = 0.2,popup = ~paste(NOM_QP),layerId = ~paste(CODE_QP))
    
    # cat("my output")
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
    }
  })
  
  # I.3. Open reactive dashboard on click
  #--------------------------------------
  observeEvent(input$mymap_shape_click,{
    toggleModal(session, "boxPopUp1", toggle = "toggle")
  })
  
  # I.4. Update view on map clicks
  #-------------------------------
  observeEvent(input$mymap_shape_click, { 
    
    # # Polygon selection
    # mapTmp <- userMap()[userMap()@data$id == input$mymap_shape_click$id,] 
    # centroidCoord <- as.data.frame(coordinates(rgeos::gCentroid(mapTmp)))
    # 
    # # Update view
    # leafletProxy("mymap") %>%
    #   setView(lng=centroidCoord$x, lat=centroidCoord$y, input$Map_zoom) 
    # %>% acm_defaults(p$lng, p$lat)
    
    
    # p <- input$Map_marker_click
    # proxy <- leafletProxy("Map")
    # if(p$id=="Selected"){
    #   proxy %>% removeMarker(layerId="Selected")
    # } else {
    #   proxy %>% setView(lng=p$lng, lat=p$lat, input$Map_zoom) %>% acm_defaults(p$lng, p$lat)
    # }
  })
   
  
# II. Import user shapefile map
#-----------------------------------------------------------------------------------------------------------------------------------
  
  # II.1. Upload ShapeFile
  userMap <- eventReactive(input$file1,{
    # !!! Solution temporaire !!!
    if(exists("qpv_stat")){
      map <- qpv_stat
    }else{
      myshape<- input$file1
      if (is.null(myshape)) 
        return(NULL)       
      
      dir<-dirname(myshape[1,4])
      for ( i in 1:nrow(myshape)) {
        file.rename(myshape[i,4], paste0(dir,"/",myshape[i,1]))
      }
      getshp <- list.files(dir, pattern="*.shp", full.names=TRUE)
      map <- readOGR(dsn = getshp,encoding = "UTF-8")
      # SIG update to fit leaflet
      map <- spTransform(map, "+init=epsg:4326")
    }
    return(map)
  })
  
  # II.2. Open Modal
  observeEvent(input$file1, {
    lst_var <- colnames(userMap()@data)
    # Update selectinput with usermap colnames
    updateSelectInput(session = session,inputId = "SI_id",choices = lst_var,selected = lst_var[1])
    updateSelectInput(session = session,inputId = "SI_name",choices = lst_var,selected = lst_var[2])
    # open bsmodal
    toggleModal(session, "bs_importShp", toggle = "toggle")

  })
  
  # II.3. Update AgateMap
  observeEvent(c(input$SI_id,input$SI_name), {
    if(input$SI_id != "Defaut"){
      rv$AgateMap <- userMap()
      rv$AgateMap@data$idAgate <- rv$AgateMap@data[,input$SI_id]
      rv$AgateMap@data$idAgate.name <- rv$AgateMap@data[,input$SI_name]
      print(rv$AgateMap@data[,c("idAgate","idAgate.name")])
    }
  })
  
  
  # 
  # userMap <- eventReactive(c(input$SI_id,input$SI_name),{
  #   map <- isolate(userMapTmp())
  #   map@data$idAgate <- NA
  #   print(map@data[,input$SI_id])
  #   
  #   if(input$SI_id != "Defaut"){
  #     map@data$idAgate <- map@data[,input$SI_id]
  #     map@data$idAgate.name <- map@data[,input$SI_name]
  #     print("je marche!")
  #   }
  #   print(input$SI_id)
  #   print(map@data$idAgate)
  #   print(map@data$idAgate.name)
  #   return(map)
  # })
  



  
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
  
# IV. Reactive Dashboard
#---------------------------------------------------------------------------------------------------------------------------------
  
  # IV.0.1 Filtered userdata
  #-------------------------
  qpv_filtre <- eventReactive(input$mymap_shape_click,{

    print(input$mymap_shape_click)

    if(is.null(input$mymap_shape_click)) return(NULL)
    # A modifier quand tout sera branché en amont
    df <- qpv_stat@data[qpv_stat@data$CODE_QP == input$mymap_shape_click$id,]
    return(df)
  })
  
  # IV.0.2 Reactive modal title
  #----------------------------
  output$modalTitle <- renderText({ 
    if(is.null(qpv_filtre())) {return("Informations sur la zone")}
    paste("Zone : ",qpv_filtre()$CODE_QP," - ",qpv_filtre()$NOM_QP,sep="") 
    })

  # IV.1. Total population infobox
  #-------------------------------
  output$IB_pop <- renderInfoBox({
    infoBox(title = "Population", value =
              format(round(qpv_filtre()$pop_qpv_p,digits = 0),digits = 9,decimal.mark=",", big.mark=" "),
            icon = icon("child"),color = "green", fill = TRUE
    )
  })

  # IV.2. Mean income infobox
  #--------------------------
  output$IB_rev <- renderInfoBox({
    infoBox(title = "Niveau de vie moyen", value =
              format(round(qpv_filtre()$nivvie.mean / 12,digits = 0),digits = 9,decimal.mark=",", big.mark=" "),
            icon = icon("eur"),color = "aqua", fill = TRUE,subtitle = "mensuel"
    )
  })

  # IV.3. Taux de chomeur infobox
  #------------------------------
  output$IB_chom <- renderInfoBox({
    infoBox(title = "Taux de chomeur", value =
              paste(format(round(qpv_filtre()$t_chom_15_64,digits = 0),digits = 9,decimal.mark=",", big.mark=" ")," %",sep=""),
            icon = icon("industry"),color = "orange", fill = TRUE
    )
  })

  # IV.4. BoxPlot du Niveau de vie
  #-------------------------------
  # renderPlotly() also understands ggplot2 objects!
  output$plotly1 <- renderPlotly({
    callModule(plotlyBoxplotIncome,"plotly1")
  })
   
  # IV.5. Pyramide des ages
  #------------------------
  output$plotly2 <- renderPlotly({
    callModule(plotlyAgedPyramid,"plotly2")
  })
  # IV.6 Informations about household
  #----------------------------------
  # output$plotly3 <- callModule(plotlyInfoPopulation)
  # 
  # # IV.7 Informations about housing
  # #--------------------------------
  # output$plotly4 <- callModule(plotlyInfoHousing)
  # 
  # # IV.8 Download Dashboard
  # #------------------------
  # output$dl_dash <- downloadHandler(
  #   # # For PDF output, change this to "report.pdf"
  #   # filename = "report.html",
  #   # content = function(file) {
  #   #   # Copy the report file to a temporary directory before processing it, in
  #   #   # case we don't have write permissions to the current working dir (which
  #   #   # can happen when deployed).
  #   #   tempReport <- file.path(getwd(), "shinyFlex_tmp.Rmd")
  #   #   file.copy("shinyFlex_v2.Rmd", tempReport, overwrite = TRUE)
  #   #   
  #   #   # Parametres transmis au fichier rmarkdown
  #   #   params <- list(n = qpv_filtre()$idZonage)
  #   #   
  #   #   # Knit the document, passing in the `params` list, and eval it in a
  #   #   # child of the global environment (this isolates the code in the document
  #   #   # from the code in this app).
  #   #   rmarkdown::render(tempReport, output_file = file,
  #   #                     params = params,
  #   #                     envir = new.env(parent = globalenv())
  #   #   )
  #   #   
  #   # }
  # )
  
  
}
