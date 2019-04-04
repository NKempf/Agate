#---------------------------------------------------------------------------------------------------------------------------#
#                                             Agate - Server                                                                #
#---------------------------------------------------------------------------------------------------------------------------#

# source(file = "AgataSERVER_dashboard.R",encoding = "UTF-8")


server <- function(input, output,session) {
# 0. Reactive Values
#----------------------------------------------------------------------------------------------------------------------------
  rv <- reactiveValues(AgateMap=NULL,
                       df.zone=NULL,
                       source.an = "13"
                       # statZone=NULL,
                       # qualityZone=NULL,
                       # source = NULL, # Page Statistiques
                       # zone.etude = "QP971002",
                       # zone.compare = NULL,
                       # df.zone=NULL,
                       # pyramide = NULL,
                       # zonage.com = NULL,
                       # dash.indicateur = NULL,

                       ) # Page Statistiques
  
# I. Interactive web map
#----------------------------------------------------------------------------------------------------------------------------
  
  # I.1. Initiate interactive web map
  #----------------------------------
  output$mymap <- renderLeaflet({
    leaflet("map",data = heat.pts) %>% addTiles()%>% 
      fitBounds(lng1 = -65,lat1 = 18,lng2 = -45,lat2 = 3) %>% 
      addHeatmap(group = "heatpts",lng = ~x, lat = ~y,
                 # intensity = ~nivviem,
                 blur = 60, radius = 30) %>% 
      hideGroup("heatpts")
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

  # I.3. Open reactive dashboard on click
  #--------------------------------------
  observeEvent(input$mymap_shape_click,{
    if(!is.null(rv$df.zone)){
      toggleModal(session, "boxPopUp1", toggle = "toggle")
    }
  })
  
  # I.4. Update view on map clicks
  #-------------------------------
  observeEvent(input$mymap_shape_click, { 
    print(input$mymap_shape_click)
    # 1) Selection du polygone
    mapSelect <- rv$AgateMap[rv$AgateMap@data$idZonage == input$mymap_shape_click$id,]

    # 2) Boundary Box du polygone
    mapSelect.bbox <- as.data.frame(bbox(mapSelect))
    zoom_lng <- (mapSelect.bbox$max[1] - mapSelect.bbox$min[1])/2
    zoom_lat <- (mapSelect.bbox$max[2] - mapSelect.bbox$min[2])/2

    # 3) Mise à jour de la carte
    leafletProxy("mymap") %>%
      fitBounds(lng1 = mapSelect.bbox$min[1] - zoom_lng,
                lat1 = mapSelect.bbox$max[2] + zoom_lat,
                lng2 = mapSelect.bbox$max[1] + zoom_lng,
                lat2 = mapSelect.bbox$min[2] - zoom_lat)
  })
  
  # I.5. Heatpoints
  #----------------
  observeEvent(input$ms_heatpts, {
    
    if(input$ms_heatpts){
      leafletProxy("mymap") %>% 
        showGroup("heatpts")
    }else{
      leafletProxy("mymap") %>% 
        hideGroup("heatpts")
    }
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

    withProgress(message = "Creation de l'identifiant",style = "notification", value = 0, {

      incProgress(amount = 0.4,message = "Calcul des statistiques")
      agate.stat <- agate_statRp(rp.an = rv$source.an,zone.pred = 4,zoneType = "ZU",zonage = zonage,group_var = c("idZonage","idZonage.name"),
                                 com.dom = com.dom,rpi.weight = "IPONDI.cal",rpl.weight = "IPONDL.cal")

      # Paramètres Agate
      rv$df.zone <- agate.stat$df.zone
      # rv$pyramide <- agate.stat$pyramide
      # rv$source <- source
  })

    # VII. Nettoyage
    #--------------

    # VII.1. Closing modal
    toggleModal(session, modalId = "bs_optad", toggle = "close")

    # VII.2. Pop-up indiquant la fin du calcul
    temps <- as.character(round(abs(difftime(t1,Sys.time(), units="secs")),2))

    if(!is.null(rv$df.zone)){
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
  # zone_filtre <- eventReactive(input$mymap_shape_click,{
  #   # print(input$mymap_shape_click)
  # 
  #   if(is.null(rv$AgateMap)) return(NULL)
  #   df <- rv$AgateMap@data[rv$AgateMap@data$idZonage == input$mymap_shape_click$id,]
  #   # df <- qpv_stat@data[qpv_stat@data$CODE_QP == input$mymap_shape_click$id,]
  #   return(df)
  # })
  # 
  # # IV.0.2 Reactive modal title
  # #----------------------------
  # output$modalTitle <- renderText({ 
  #   if(is.null(zone_filtre())) {return("Informations sur la zone")}
  #   
  #   print(zone_filtre())
  #   
  #   paste("Zone : ",zone_filtre()$idZonage," - ",rv$AgateMap@data$idZonage.name[rv$AgateMap@data$idZonage == zone_filtre()$idZonage],sep="") 
  #   # paste("Zone : ",zone_filtre()$idAgate," - ",zone_filtre()$idAgate.name,sep="") 
  #   })
  # 
  # # IV.1. Total population infobox
  # #-------------------------------
  # output$IB_pop <- renderInfoBox({
  #   infoBox(title = "Population", value =
  #             format(round(with(rv$statZone$tRp.I.2 ,popZonage_p[idZonage == zone_filtre()$idZonage]),
  #                          digits = 0),digits = 9,decimal.mark=",", big.mark=" "),
  #           icon = icon("child"),color = "green", fill = TRUE
  #   )
  # })
  # 
  # # IV.2. Poverty infobox
  # #----------------------
  # output$IB_rev <- renderInfoBox({
  #   infoBox(title = "Taux  de pauvrete", value =
  #             paste(format(round(with(rv$statZone$tFilo.II.1, tx_pauv60.ind.metro[idZonage == zone_filtre()$idZonage]),
  #                          digits = 2),digits = 9,decimal.mark=",", big.mark=" "), " %", sep = "" ),
  #           icon = icon("hands-helping",lib = "font-awesome"),color = "aqua", fill = TRUE,subtitle = "seuil metro"
  #   )
  # })
  # 
  # # IV.3. Taux de chomeur infobox
  # #------------------------------
  # output$IB_chom <- renderInfoBox({
  #   infoBox(title = "Taux de chomeur", value =
  #             paste(format(round(with(rv$statZone$tRp.IV.1, part_p[TACT == "chomeur" & idZonage == zone_filtre()$idZonage]),
  #                                digits = 0),digits = 9,decimal.mark=",", big.mark=" ")," %",sep=""),
  #           icon = icon("industry"),color = "orange", fill = TRUE
  #   )
  # })
  # 
  # # IV.4. BoxPlot du Niveau de vie
  # #-------------------------------
  # # renderPlotly() also understands ggplot2 objects!
  # output$plotly1 <- renderPlotly({
  #   callModule(plotlyBoxplotIncome,"plotly1",rv$statZone,rv$statHZone,zone_filtre(),input$SI_comp)
  # })
  #  
  # # IV.5. Pyramide des ages
  # #------------------------
  # output$plotly2 <- renderPlotly({
  #   callModule(plotlyAgedPyramid,"plotly2",rv$statZone,rv$statHZone,zone_filtre(),input$SI_comp)
  # })
  # # IV.6 Informations about household
  # #----------------------------------
  # output$plotly3 <- renderPlotly({
  #   callModule(plotlyInfoPopulation,"plotly3")
  # })
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
  
  # V.1. Table utilisateur et source
  #---------------------------------
  # observe({
  #   rv$df.zone <- indStat$indicateur_stat
  #   rv$source <- unique(indStat$indicateur_stat$source)
  # })
  
  # V.2. MAJ liste des catégories
  #------------------------------
  # observeEvent(input$si_domaine,{
  #   cat <- lstCategorie$idCategorie[lstCategorie$idDomaine == input$si_domaine]
  #   names(cat) <- lstCategorie$labelCategorie[lstCategorie$idDomaine == input$si_domaine]
  #   updateSelectInput(session, "si_categorie",
  #                     choices = cat)
  # })
  # 
  # # V.3. Reactive data table
  # #-------------------------
  # observeEvent(c(input$si_categorie,input$si_domaine,input$si_zoneSelect),{
  #   
  #   if(input$si_categorie != ""){
  #     # V.3.1. Selection de la base de données
  #     if(input$si_zoneSelect == 4){
  #       df <- rv$df.zone %>% 
  #         select(source,domaine,categorie,com,idZonage,idZonage.name,indicateur,type.indicateur,value) %>%
  #         filter(type.indicateur != "part_np") %>% 
  #         filter(domaine == input$si_domaine & categorie == input$si_categorie)
  #     }else{
  #       df <- read_fst("Data/Stats/Prefine aera/Real/fst/indicateur_stat.fst") %>% 
  #         filter(zone.predefine == input$si_zoneSelect & domaine == input$si_domaine & 
  #                  categorie == input$si_categorie & source %in% rv$source) %>%
  #         filter(type.indicateur != "part_np") %>% 
  #         select(source,domaine,categorie,idZonage,idZonage.name,indicateur,type.indicateur,value)
  #     }
  #     
  #     # V.3.2. Libelles de colonnes du tableau
  #     type.ind <- typInd[typInd %in% c("idZonage","idZonage.name",unique(df$type.indicateur))]
  #     
  #     # V.3.3. Construction tableau
  #     df <- df %>%
  #       spread(key = type.indicateur, value = value) %>%
  #       left_join(lstIndicateur %>% select(nomIndicateur,labelIndicateur),c("indicateur" = "nomIndicateur")) %>% 
  #       mutate(indicateur = labelIndicateur) %>% 
  #       select(-domaine,-categorie,-labelIndicateur)
  #     
  #     # V.3.4. Titre du tableau
  #     output$TO_titleTab <- renderText({lstCategorie$titreTab[lstCategorie$idDomaine == input$si_domaine &
  #                                                               lstCategorie$idCategorie == input$si_categorie]})
  #     # V.3.5. Affichage du tableau
  #     output$table = renderDT(
  #       datatable(df,
  #                 colnames = type.ind,
  #                 extensions = 'Buttons',
  #                 options = list(
  #                   scrollX = TRUE,
  #                   # fixedColumns = TRUE,
  #                   # autoWidth = TRUE,
  #                   ordering = FALSE,
  #                   dom = 'lBfrtip',
  #                   buttons = c(I('colvis'),'excel')),
  #                 rownames= FALSE)
  #     )
  #   } # end if
    
  # })
  


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
  
  
# VI. Census quality
#--------------------------------------------------------------------------------------------------------------------------------------  
  
  # VI.1. MAJ Select input
  #-----------------------
  # observeEvent(rv$qualityZone,{
  #   
  #   print(is.null(rv$qualityZone))
  #   
  #   if(!is.null(rv$qualityZone)){
  #     QualiteVar<- rv$qualityZone %>% filter(!duplicated(Variable)) %>% select(Variable)
  #     QualiteZone<- rv$qualityZone %>% filter(!duplicated(zonage)) %>% select(zonage)
  # 
  #     updateSelectInput(session, "si_variable_qual",
  #                       choices = as.character(QualiteVar$Variable),
  #                       selected = as.character(QualiteVar$Variable[1])
  #     )
  # 
  #     updateSelectInput(session, "si_zone_qual",
  #                       choices = as.character(QualiteZone$zonage),
  #                       selected = as.character(QualiteZone$zonage[1])
  #     )
  #     
  #     
  #     
  #   }
  #   
  # })
  # 
  # 
  # 
  # 
  # # VI.1. Display data
  # #-------------------
  # 
  # # output$qualityTable = DT::renderDataTable(
  # #   datatable(rv$qualityZone,
  # #             extensions = 'Buttons',
  # #             options = list(
  # #               scrollX = TRUE,
  # #               # fixedColumns = TRUE,
  # #               # autoWidth = TRUE,
  # #               ordering = FALSE,
  # #               dom = 'lBfrtip',
  # #               buttons = c(I('colvis'),'excel', 'pdf')),
  # #             rownames= FALSE,
  # #             class = "display" #if you want to modify via .css
  # #             ) %>% formatStyle(
  # #     'CV_Y',
  # #     target = 'row',
  # #     # backgroundColor = styleEqual(c(0,28.8), c('blank', 'yellow'))
  # #     backgroundColor = styleInterval(c(15,30,100), c("blank","#fee8c8","#fdbb84","#e34a33"))
  # #   ) %>% 
  # #     formatCurrency(columns = 2:4, currency = "", interval = 3, mark = " ",digits = 0) 
  # # )
  # 
  # observeEvent(c(input$si_zone_qual,input$si_variable_qual,input$si_select_qual),{
  #   
  #   if(!is.null(rv$qualityZone)){
  #     
  #     if(input$si_select_qual=="Par Zone"){
  #       output$TO_titleTab_qual <- renderText(paste0("Toutes les variables sur ",input$si_zone_qual))
  #       
  #       df<-rv$qualityZone %>% filter(zonage==input$si_zone_qual)
  #       output$dt_qualite = renderDT(
  #         datatable(df,
  #                   #colnames = type.ind,
  #                   extensions = 'Buttons',
  #                   options = list(
  #                     scrollX = TRUE,
  #                     # fixedColumns = TRUE,
  #                     # autoWidth = TRUE,
  #                     ordering = FALSE,
  #                     dom = 'lBfrtip',
  #                     buttons = c(I('colvis'),'excel')),
  #                   rownames= FALSE)
  #       )
  #     } else if(input$si_select_qual=="Par Variable"){
  #       output$TO_titleTab_qual <- renderText(paste0(input$si_variable_qual," sur toutes les zones"))
  #       
  #       df<-rv$qualityZone %>% filter(Variable==input$si_variable_qual)
  #       output$dt_qualite = renderDT(
  #         datatable(df,
  #                   #colnames = type.ind,
  #                   extensions = 'Buttons',
  #                   options = list(
  #                     scrollX = TRUE,
  #                     # fixedColumns = TRUE,
  #                     # autoWidth = TRUE,
  #                     ordering = FALSE,
  #                     dom = 'lBfrtip',
  #                     buttons = c(I('colvis'),'excel')),
  #                   rownames= FALSE)
  #       )
  #     }
  #     else{
  #       output$TO_titleTab_qual <- renderText("Toutes les Variables sur toutes les zones")
  #       
  #       df<-rv$qualityZone
  #       output$dt_qualite = renderDT(
  #         datatable(df,
  #                   #colnames = type.ind,
  #                   extensions = 'Buttons',
  #                   options = list(
  #                     scrollX = TRUE,
  #                     # fixedColumns = TRUE,
  #                     # autoWidth = TRUE,
  #                     ordering = FALSE,
  #                     dom = 'lBfrtip',
  #                     buttons = c(I('colvis'),'excel')),
  #                   rownames= FALSE)
  #       )
  #     }# end if
  #     
  #   }# end if
  #   
  # })
  
  
  
  
  
} # End Server
