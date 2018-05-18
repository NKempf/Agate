#---------------------------------------------------------------------------------------------------------------------------#
#                                             Agate - Server                                                                #
#---------------------------------------------------------------------------------------------------------------------------#

source(file = "AgataSERVER_dashboard.R",encoding = "UTF-8")


server <- function(input, output,session) {
  
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
    
    # click <- input$mymap_shape_click
    
    # # Reactive dashboard pop-up
    # showModal(modalDialog(
    #   title = "You selected a marker!",
    #   tabPanel(
    #     title = "Synthèse",
    #     value = "page1",
        # # Add CSS files : use infobox from shinydashboard package into a shinyApp
        # includeCSS(path = "www/AdminLTE.css"),
        # includeCSS(path = "www/shinydashboard.css"),
        # fluidRow(
        #   infoBoxOutput(outputId = "IB_pop"),
        #   infoBoxOutput(outputId = "IB_rev"),
        #   infoBoxOutput(outputId = "IB_chom")
        # ),
        # plotlyOutput("plot"))
    # ))
    
    
    # Mise a jour de la liste
    # updateSelectInput(session = session,inputId = "SI_Poly",
    #                   choices = levels(factor(qpv_stat@data[,1])), 
    #                   selected = click$id)
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
  output$plotly1 <- callModule(plotlyBoxplotIncome)
  
  # output$plot <- renderPlotly({
  #   # Selection de la commune
  #   comZone <- statZona$filo$depcom[statZona$filo$idZonage %in% qpv_filtre()$idZonage][1]
  # 
  #   plot_ly(type = 'box') %>%
  #     # Boxplot d'un equipement
  #     add_boxplot(y = statZona$filo$nivviem[statZona$filo$idZonage %in% qpv_filtre()$idZonage],
  #                 boxpoints = FALSE,
  #                 marker = list(color = 'rgb(255,69,0)'),
  #                 line = list(color = 'rgb(255,69,0)'),
  #                 name = "Zonage") %>%
  #     # Boxplot des individus de la commune qui ne vivent pas le zonage
  #     add_boxplot(y = statZona$filo$nivviem[!(statZona$filo$idZonage %in% qpv_filtre()$idZonage) &
  #                                             statZona$filo$depcom %in% comZone],
  #                 name = "Hors zonage", boxpoints = FALSE,
  #                 marker = list(color = 'rgb(113,113,198)'),
  #                 line = list(color = 'rgb(113,113,198)')) %>%
  #     # Boxplot des individus de la commune
  #     add_boxplot(y = statZona$filo$nivviem[statZona$filo$depcom %in% comZone],
  #                 name = "Communal", boxpoints = FALSE,
  #                 marker = list(color = 'rgb(113,198,113)'),
  #                 line = list(color = 'rgb(113,198,113)')) %>%
  # 
  #     # Boxplot des individus des autres zonages du departement
  #     # add_boxplot(y = statZona$filo$nivviem[!(statZona$filo$idZonage %in% qpv_filtre()$idZonage) & !is.na(statZona$filo$idZonage)
  #     #                                       & substr(statZona$filo$depcom,1,3) %in% substr(comZone,1,3)],
  #     #             name = "Autres zonages", boxpoints = FALSE,
  #     #             marker = list(color = 'rgb(252,141,98)'),
  #     #             line = list(color = 'rgb(252,141,98)')) %>%
  # 
  #     # Reglage des axes
  #     layout(
  #       yaxis = list(range = c(0, 50000)))
  # })

  # VI.5. Pyramide des ages
  #------------------------
  output$plotly2 <- callModule(plotlyAgedPyramid)
  
  # output$plot2 <- renderPlotly({
  # 
  #   # Pyramide des ages
  #   agePyramid <- statZona$t1d_pyramide[statZona$t1e_pyramide$idZonage %in% qpv_filtre()$idZonage,]
  # 
  #   # Valeur des Hommes négatives
  #   agePyramid$pop[agePyramid$SEXE=="homme"] <- - agePyramid$pop[agePyramid$SEXE=="homme"]
  # 
  #   # Label pour plotly
  #   agePyramid$abs_pop <- paste(abs(agePyramid$pop)," ",agePyramid$SEXE,"s de ",
  #                               agePyramid$age," ans",sep="")
  # 
  #   # Affichage de la pyramide
  #   plot_ly(data = agePyramid,x= ~pop, y=~age,color=~SEXE,colors = c('#fb9a99','#a6cee3')) %>%
  #     add_bars(orientation = 'h', hoverinfo = 'text', text = ~abs_pop) %>%
  #     layout(bargap = 0.1, barmode = 'overlay',
  #            xaxis = list(title = "Population",tickmode = "array"),
  #            yaxis = list(title = "Age"))
  # })
  
  
  
}
