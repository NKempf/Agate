#---------------------------------------------------------------------------------------------------------------------------#
#                                             Agate - Server                                                                #
#---------------------------------------------------------------------------------------------------------------------------#

source(file = "AgataSERVER_dashboard.R",encoding = "UTF-8")


server <- function(input, output,session) {
# 0. Reactive Values
#----------------------------------------------------------------------------------------------------------------------------
  rv <- reactiveValues(AgateMap=NULL,
                       statZone=NULL,
                       statHZone=NULL)
  
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
  observeEvent(rv$AgateMap,{
    if (!is.null(rv$AgateMap@data$idZonage) & !is.null(rv$AgateMap@data$idZonage.name)) {
      # Boundary box
      AgateMap.bbox <- as.data.frame(bbox(rv$AgateMap))
      # Update leaflet
      leafletProxy("mymap") %>%
        fitBounds(lng1 = AgateMap.bbox$min[1],lat1 = AgateMap.bbox$max[2],lng2 = AgateMap.bbox$max[1],lat2 = AgateMap.bbox$min[2]) %>%
        addPolygons(data=rv$AgateMap,opacity = 3,
                    color = "green", stroke = TRUE, weight = 2,
                    fill = TRUE, fillOpacity = 0.2,popup = ~paste(idZonage.name),layerId = ~paste(idZonage))
    }
  })
  # observeEvent(input$file1,{
  #   if (!is.null(input$file1)) {
  #     # Boundary box
  #     userMap.bbox <- as.data.frame(bbox(userMap()))
  #     # Update leaflet
  #     leafletProxy("mymap") %>%
  #       fitBounds(lng1 = userMap.bbox$min[1],lat1 = userMap.bbox$max[2],lng2 = userMap.bbox$max[1],lat2 = userMap.bbox$min[2]) %>%
  #       addPolygons(data=userMap(),opacity = 3,
  #                                 color = "green", stroke = TRUE, weight = 2,
  #                                 fill = TRUE, fillOpacity = 0.2,popup = ~paste(NOM_QP),layerId = ~paste(CODE_QP))
  #   }
  # })
  
  # I.3. Open reactive dashboard on click
  #--------------------------------------
  observeEvent(input$mymap_shape_click,{
    if(!is.null(rv$statZone)){
      toggleModal(session, "boxPopUp1", toggle = "toggle")
    }
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
  observeEvent(userMap(), {
    
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
      rv$AgateMap@data$idZonage <- rv$AgateMap@data[,input$SI_id]
      rv$AgateMap@data$idZonage.name <- rv$AgateMap@data[,input$SI_name]
    }
  })
  
  # II.4. Update selectInput Comparison field
  observeEvent(rv$AgateMap, {
    updateSelectInput(session, "SI_comp", 
                    choices = c("Commune","Departement","HorsZone",unique(as.character(rv$AgateMap@data$idZonage))),
                    selected = c("Commune"))
  })
  
  # II.5. Edit map parameter
  observeEvent(input$b_paramCarte, {
    # open bsmodal
    toggleModal(session, "bs_importShp", toggle = "toggle")
  })
  

# III. InfraCity statistical computation 
#-----------------------------------------------------------------------------------------------------------------------------------
  observeEvent(input$b_calcul, {
    t1 <- Sys.time()
    
    withProgress(message = "Creation de l'identifiant",style = "notification", value = 0, {
    
    # I. Preparation du zonage
    #-------------------------
      # I.1. Creation de la variable zonage
      zonage <- rv$AgateMap
      zonage <- spTransform(zonage, "+init=epsg:3857")
      
      # I.2. Creation de l'identifiant idZonage
      zonage@data$idZonage <- zonage@data$idZonage
      
      
    # II. Logements géolocalisés du RIL 
    #----------------------------------
    
    # II.1 Communes dans lesquelles se trouvent une ou plusieurs zones
    zoneInter <- gIntersects(zonage,com.dom,byid = TRUE)
      test <- apply(zoneInter, 1, function(x){
        test <- sum(x)
        return(ifelse(test>0,TRUE,FALSE))}) 
    com.dom.select <- com.dom@data$Codgeo[test]
    
    # II.2. Chargement des logements du RIL dans les communes d'interets
    ril <- read_fst("Data/Ril/ril15.fst") %>% 
      select(idx,x,y) %>% 
      mutate(com = substr(idx,1,5)) %>%
      filter(com %in% com.dom.select)
    
    # II.3. Transformation du ril en objet spatial
    coordinates(ril) <- ~x+y
    ril@proj4string <- CRS("+init=epsg:3857")
      
    # III. Ajout de la zone aux données du recensement
    #-------------------------------------------------
    
    # III.1. Zone dans laquelle chaque logement se situe
    incProgress(amount = 0.1,message = "Zone dans laquelle chaque logement se situe")
    pts.sp <- zonaPts(pts.sp = ril,zonage = zonage)
      
    # III.2. Ajout de la zone aux données du rp individu
    # Note : pour des raisons de performances, les données du RP sont préalablement filtrées selon les communes étudiées
    incProgress(amount = 0.4,message = "Ajout de la zone aux données du RP")
    
    rpi <- read_fst("Data/Rp/rp14i.fst") %>% 
      filter(idx %in% ril@data$idx) %>% 
      left_join(pts.sp@data[,c("idx","idZonage")], "idx") %>% 
      mutate(idZonage = ifelse(is.na(idZonage),"Hors zonage", idZonage))
    
    # III.3. Ajout de la zone aux données du rp logement
    rpl <- read_fst("Data/Rp/rp14l.fst") %>% 
      filter(idx %in% ril@data$idx) %>% 
      left_join(pts.sp@data[,c("idx","idZonage")], "idx") %>% 
      mutate(idZonage = ifelse(is.na(idZonage),"Hors zonage", idZonage))
    
    # IV. Ajout de la zone aux données fiscales
    #------------------------------------------
    
    # IV.1. Chargement des données fiscales filtrées selon les communes d'intérêts
    incProgress(amount = 0.6,message = "Ajout des données fiscales")
    filo <- read_fst("Data/Filosofi/filo14.fst") %>% 
      filter(com %in% com.dom.select)
    
    # IV.2. Transformation des données en objet spatial
    filo.sp <- SpatialPointsDataFrame(coords = filo[,c("x","y")],data = filo,proj4string = CRS("+init=epsg:3857"))
    
    # IV.3. Zone dans laquelle chaque foyer fiscal se situe
    filo.sp <- zonaPts(pts.sp = filo.sp,zonage = zonage)
    
    # IV.4. Ajout de la zone aux données fiscales
    typmen.label <- c("famille monoparentale","couple sans enfant","couple avec enfant(s)","menage complexe",
                      "femme seule","homme seul")
    
    filo <- filo.sp@data %>% 
      left_join(data.frame(unique(rpl[,c("com","com.lib")])),"com") %>% 
      mutate(dep = substr(com,1,3),
             idZonage = ifelse(is.na(idZonage),"Hors zonage", idZonage),
             typmenR.lib = factor(typmenR,labels = typmen.label))
    rm(filo.sp)
    
    # V. Calcul des indicateurs statistiques
    #---------------------------------------
    incProgress(amount = 0.8,message = "Calcul des statistiques")
    
    # V.1. Statistiques dans la zone
    rv$statZone <- statistics_zone(rpi = rpi,rpl = rpl,filo = filo,group_var = c("idZonage"))
    
    # V.2. Statistiques communales hors zone
    rv$statHZone <- statistics_zone(rpi = rpi,rpl = rpl,filo = filo,group_var = c("com","com.lib","idZonage"))
      
    }) # Fermeture du withprogress

    # VI. Nettoyage
    #--------------
    
    # VI.1. Closing modal
    toggleModal(session, modalId = "bs_optad", toggle = "close")    
    
    # VI.2. Pop-up indiquant la fin du calcul
    temps <- as.character(round(abs(difftime(t1,Sys.time(), units="secs")),2))
    
    if(!is.null(rv$statZone)){
      shinyWidgets::sendSweetAlert(
        session = session, 
        title = "Terminé !", text = paste("Le calcul a été effectué en ",temps," secondes"), type = "success"
      )
    }
  }) # Close observeEvent
  
# IV. Reactive Dashboard
#---------------------------------------------------------------------------------------------------------------------------------
  
  # IV.0.1 Filtered userdata
  #-------------------------
  zone_filtre <- eventReactive(input$mymap_shape_click,{
    # print(input$mymap_shape_click)

    if(is.null(rv$AgateMap)) return(NULL)
    df <- rv$AgateMap@data[rv$AgateMap@data$idZonage == input$mymap_shape_click$id,]
    # df <- qpv_stat@data[qpv_stat@data$CODE_QP == input$mymap_shape_click$id,]
    return(df)
  })
  
  # IV.0.2 Reactive modal title
  #----------------------------
  output$modalTitle <- renderText({ 
    if(is.null(zone_filtre())) {return("Informations sur la zone")}
    
    print(zone_filtre())
    
    paste("Zone : ",zone_filtre()$idZonage," - ",rv$AgateMap@data$idZonage.name[rv$AgateMap@data$idZonage == zone_filtre()$idZonage],sep="") 
    # paste("Zone : ",zone_filtre()$idAgate," - ",zone_filtre()$idAgate.name,sep="") 
    })

  # IV.1. Total population infobox
  #-------------------------------
  output$IB_pop <- renderInfoBox({
    infoBox(title = "Population", value =
              format(round(with(rv$statZone$tRp.I.2 ,popZonage_p[idZonage == zone_filtre()$idZonage]),
                           digits = 0),digits = 9,decimal.mark=",", big.mark=" "),
            icon = icon("child"),color = "green", fill = TRUE
    )
  })

  # IV.2. Poverty infobox
  #----------------------
  output$IB_rev <- renderInfoBox({
    infoBox(title = "Taux  de pauvrete", value =
              paste(format(round(with(rv$statZone$tFilo.II.1, tx_pauv60.ind.metro[idZonage == zone_filtre()$idZonage]),
                           digits = 2),digits = 9,decimal.mark=",", big.mark=" "), " %", sep = "" ),
            icon = icon("hands-helping",lib = "font-awesome"),color = "aqua", fill = TRUE,subtitle = "seuil metro"
    )
  })

  # IV.3. Taux de chomeur infobox
  #------------------------------
  output$IB_chom <- renderInfoBox({
    infoBox(title = "Taux de chomeur", value =
              paste(format(round(with(rv$statZone$tRp.IV.1, part_p[TACT == "chomeur" & idZonage == zone_filtre()$idZonage]),
                                 digits = 0),digits = 9,decimal.mark=",", big.mark=" ")," %",sep=""),
            icon = icon("industry"),color = "orange", fill = TRUE
    )
  })

  # IV.4. BoxPlot du Niveau de vie
  #-------------------------------
  # renderPlotly() also understands ggplot2 objects!
  output$plotly1 <- renderPlotly({
    callModule(plotlyBoxplotIncome,"plotly1",rv$statZone,rv$statHZone,zone_filtre(),input$SI_comp)
  })
   
  # IV.5. Pyramide des ages
  #------------------------
  output$plotly2 <- renderPlotly({
    callModule(plotlyAgedPyramid,"plotly2",rv$statZone,rv$statHZone,zone_filtre(),input$SI_comp)
  })
  # IV.6 Informations about household
  #----------------------------------
  output$plotly3 <- renderPlotly({
    callModule(plotlyInfoPopulation,"plotly3")
  })
  # IV.7 Informations about housing
  #--------------------------------
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

  
# V. Statistics dataSet
#--------------------------------------------------------------------------------------------------------------------------------------
  
  # V.1. Update Tab selection
  #--------------------------
  observeEvent(rv$statZone, {
    if(!is.null(rv$statZone)){
      updateSelectInput(session, "SI_TabSelect", 
                        choices = rv$statZone$tab_lib,
                        # label = rv$statZone$tab_lib[names(rv$statZone$tab_lib) %in% names(rv$statZone)],
                        selected = rv$statZone$tab_lib[2])
    }
  })
  
  # V.2 Reactive value for selected dataset
  #----------------------------------------
  datasetInput <- reactive({
    
    tab.select <- names(rv$statZone)[rv$statZone$tab_lib == input$SI_TabSelect]
    
    switch(input$SI_ZoneSelect,
           Commune = {
             df <- com.stat[[tab.select]]
           },
           Departement = {
             df <- dep.stat[[tab.select]]
           },
           HorsZone = {
             df <- rv$statHZone[[tab.select]]
           },
           {
             df <- rv$statZone[[tab.select]]
           }
    )
    return(df)
  })
  
  # V.3. Reactive title
  #--------------------
  output$TO_titleTab <- renderText({ 
    # paste("Titre : ", input$SI_TabSelect)
    input$SI_TabSelect
  })
  
  # V.3. Display table
  #-------------------
  output$table = DT::renderDataTable(
    datasetInput(),
    extensions = 'Buttons',
    options = list(
      fixedColumns = TRUE,
      autoWidth = TRUE,
      ordering = FALSE,
      dom = 'lBfrtip',
      buttons = c('copy', I('colvis'))
    ),
    class = "display" #if you want to modify via .css
  )
  
  # V.4 Reactive value for report input
  #------------------------------------
  reportInput <- reactive({
    
    switch(input$SI_ZoneSelect,
           Commune = {
             df <- com.stat
           },
           Departement = {
             df <- dep.stat
           },
           HorsZone = {
             df <- rv$statHZone
           },
           {
             df <- rv$statZone
           }
    )
    
    print(df[1])
    print(names(df))
    
    
    return(df)
  })
  
  # V.5. Download report
  #---------------------
  output$DL_StatReport <- downloadHandler(
    filename = function() { paste(input$SI_ZoneSelect,"_Excelfile.xlsx",sep="")},
    
    content = function(file){
      # example_plot=plot(1:10,1:10)
      # Results_Workbook <- createWorkbook(type='xlsx')
      # # A=as.data.frame(matrix(2,2,2))
      # sheet.1 <- createSheet(Results_Workbook, sheetName = "Data frame")
      # addDataFrame(mtcars, sheet=sheet.1, startRow=4, 
      #              startColumn=2,row.names=FALSE)
      # setColumnWidth(sheet.1,colIndex=c(1:100),colWidth=30)
      # sheet.2 <- createSheet(Results_Workbook, sheetName = "Plot")
      # addDataFrame(rock, sheet=sheet.2, startRow=4, 
      #              startColumn=2,row.names=FALSE)
      # 
      # # ggsave("plot",example_plot, device="emf")
      # # addImage(file = "plot.emf", sheet = sheet.2, scale = 55,
      # #          startRow = 4, startColumn = 4)
      # saveWorkbook(Results_Workbook,file)
      
      report_stat_zone(tab_list = reportInput(),file = file)
    } 
  )
  
  
  
  
  
  
  
  
  
  
  
} # End Server
