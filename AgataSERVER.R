#---------------------------------------------------------------------------------------------------------------------------#
#                                             Agate - Server                                                                #
#---------------------------------------------------------------------------------------------------------------------------#

source(file = "AgataSERVER_dashboard.R",encoding = "UTF-8")


server <- function(input, output,session) {
# 0. Reactive Values
#----------------------------------------------------------------------------------------------------------------------------
  rv <- reactiveValues(AgateMap=NULL,
                       statZone=NULL,
                       # statHZone=NULL,
                       qualityZone=NULL,
                       source = NULL, # Page Statistiques
                       df.zone=NULL) # Page Statistiques
  
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
    if(!is.null(rv$statZone)){
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
    t1 <- Sys.time()
    
    withProgress(message = "Creation de l'identifiant",style = "notification", value = 0, {
      
      # RP
      rp.an <- "13"
      rpi.path.string <- paste0("Data/Rp/rpi",rp.an,".fst")
      rpl.path.string <- paste0("Data/Rp/rpl",rp.an,".fst")
      rpa.path.string <- paste0("Data/Rp/rpa",rp.an,".fst")
      rpiPath <- ifelse(file.exists(rpi.path.string),rpi.path.string,"Data/Rp/FakeRpi.fst")
      rplPath <- ifelse(file.exists(rpl.path.string),rpl.path.string,"Data/Rp/FakeRpl.fst")
      rpaPath <- ifelse(file.exists(rpa.path.string),rpa.path.string,NA)
      
      # I. Preparation du zonage
      #-------------------------
      zonage <- rv$AgateMap
      zonage <- spTransform(zonage, "+init=epsg:3857")
      
      # II. Adresses géolocalisées
      #---------------------------

      # II.1 Communes dans lesquelles se trouvent une ou plusieurs zones
      zoneInter <- gIntersects(zonage,com.dom,byid = TRUE)
      test <- apply(zoneInter, 1, function(x){
        test <- sum(x)
        return(ifelse(test>0,TRUE,FALSE))}) 
      com.dom.select <- com.dom@data$Codgeo[test]
      
      # II.2. Adresses géolocalisées
      rpa <- read_fst(rpaPath) %>%
        filter(com %in% com.dom.select)
      
      rpa.geo <- rpa %>% 
        filter(!is.na(ril.millesime)) %>% 
        mutate(idx = C_IMM) %>% 
        select(idx,C_IMM,com,x,y,nb_logn.ril)
      
      # II.3. Transformation du ril en objet spatial
      coordinates(rpa.geo) <- ~x+y
      rpa.geo@proj4string <- CRS("+init=epsg:3857")
      
      # III. Données du recensement + identification de la zone
      #--------------------------------------------------------
      
      # III.1. Zone dans laquelle chaque logement se situe (MAJ : 19.03.2019)
      incProgress(amount = 0.1,message = "Zone dans laquelle chaque logement se situe")
      # rpa.geo <- rpa.geo[duplicated(rpa.geo@data$idx)==F,]
      pts.sp <- zonaPts(pts.sp = rpa.geo,zonage = zonage)
      pts.df <- pts.sp@data %>% 
        mutate(idZonage = ifelse(is.na(idZonage) | idZonage == "Hors zonage",paste0("horsZon",com), idZonage)) %>%   
        select(-idx,-com)
      
      # III.2. Ajout de la zone aux données du rp individu (MAJ : 19.03.2019)
      # Note : pour des raisons de performances, les données du RP sont préalablement filtrées selon les communes étudiées
      incProgress(amount = 0.2,message = "Ajout de la zone aux données du RP")
      
      rpi <- read_fst(rpiPath) %>% 
        filter(C_IMM %in% unique(pts.df$C_IMM)) %>% 
        left_join(pts.df, by = c("C_IMM")) %>% 
        mutate(idZonage = ifelse(is.na(idZonage) | idZonage == "Hors zonage",paste0("horsZon",com), idZonage),
               idZonage.name = ifelse(is.na(idZonage.name) & substr(idZonage,1,7) == "horsZon",paste0(com.lib," (hors zone)"),idZonage.name))
      
      # III.3. Ajout de la zone aux données du rp logement (MAJ : 19.03.2019)
      rpl <- read_fst(rplPath) %>% 
        filter(C_IMM %in% unique(pts.df$C_IMM)) %>% 
        left_join(pts.df, by = c("C_IMM")) %>% 
        mutate(idZonage = ifelse(is.na(idZonage) | idZonage == "Hors zonage",paste0("horsZon",com), idZonage),
               idZonage.name = ifelse(is.na(idZonage.name) & substr(idZonage,1,7) == "horsZon",paste0(com.lib," (hors zone)"),idZonage.name))
      
      # IV. Table multicommunes + bridage lié à la qualité de l'appariement
      #-----------------------------------------------------------------------
      seuil_qualite_appariement <- 30
      
      qualiteAppariement <- read_fst("Data/Rp/appariementRil_Rp.fst") %>% 
        filter(com %in% com.dom.select & an == rp.an) %>% 
        mutate(appariement_diff = ifelse(nonApparie.pct < seuil_qualite_appariement,TRUE,FALSE))
      
      zonage.com <- rpl %>% 
        group_by(dep,com,com.lib,idZonage,idZonage.name) %>% 
        summarise(freq=n()) %>% 
        ungroup() %>% 
        mutate(dep = substr(com,1,3)) %>% 
        left_join(qualiteAppariement,by="com")
      
      # VI. Calcul des indicateurs statistiques
      #---------------------------------------
      incProgress(amount = 0.4,message = "Calcul des statistiques")
      
      # VI.1. Statistiques dans la zone
      group_var <- c("idZonage","idZonage.name") # Attention utilisé plusieurs fois
      statZone <- statistics_zone(group_var = group_var,zone = zonage,rpi = rpi,rpl = rpl, 
                                  lstCategorie = lstCategorie,
                                  sourceRp = rp.an,
                                  rpi.weight = "IPONDI.cal",
                                  rpl.weight = "IPONDL.cal")
      
      # VI.2. Objets pour page "statistiques"
      df.zone <- statZone$indicateur_stat
      source <- unique(df.zone$source)
      
      # VII. Qualité des données du RP (Travaux Baptiste Raimbaud)
      #----------------------------------------------------------
      incProgress(amount = 0.5,message = "Qualité des données du rp")
      
      ril <- read_fst("Data/Ril/ril_leger.fst") %>% 
        select(idx,x,y,nb_logn) %>% 
        mutate(com = substr(idx,1,5)) %>%
        filter(com %in% com.dom.select)
      coordinates(ril) <- ~x+y
      ril@proj4string <- CRS("+init=epsg:3857")
      ril.geo <- ril[!duplicated(ril@data$idx),]
      ril.geo <- zonaPts(pts.sp = ril.geo,zonage = zonage)
      ril <- ril@data %>% 
        left_join(ril.geo@data %>% select(idx,idZonage),by="idx") %>% 
        mutate(idZonage = ifelse(is.na(idZonage) | idZonage == "Hors zonage",paste0("horsZon",com), idZonage))
      
      
      # VII.1. Chargement de la base adresses (MAJ : 26.03.2019)
      rpa.qualite <- rpa %>% 
        left_join(pts.df %>% select(C_IMM,idZonage,idZonage.name), by = c("C_IMM")) %>% 
        filter(ril.millesime == 1) %>% 
        mutate(IPOND = IPOND.cal)
      
      # VII.2. Liste des variables à calculer
      group_var.qualite <- c("INPER",lstIndicateur$qualiteIndicateur[paste0(lstIndicateur$nomVariable,lstIndicateur$nomIndicateur) %in% colnames(rpa.qualite) & 
                                                                       lstIndicateur$calculQualite == 1])
      
      # VII.3. Estimation de la qualité
      seuil_diffusion <- 5 # Seuil de diffusion de la valeur du coefficient de variation
      qualityZone <- Qlfinal(rpa.qualite,group_var.qualite,ril = ril) %>% 
        mutate(val.qualite = ifelse(CoefVariation <= seuil_diffusion & !is.nan(CoefVariation),EstVariable,IntervalConf.)) 
      Sys.time() - t1
      
      # VII.4. Ajout des données à la table finale
      df.zone <- qualityZone %>% 
        rename(idZonage = zonage,
               qualiteIndicateur = Variable) %>% 
        left_join(lstIndicateur %>% 
                    select(domaine,categorie,nomVariable,nomIndicateur,qualiteIndicateur,source),
                  by = "qualiteIndicateur") %>% # Ajout de variables
        left_join(zonage@data %>% 
                    select(idZonage,idZonage.name)) %>% 
        mutate(source = paste0(source,rp.an)) %>% 
        select(-qualiteIndicateur) %>% 
        gather("type.indicateur","value",-group_var,-nomVariable,-nomIndicateur,-domaine,-categorie,-source) %>% # Transformation de la base
        bind_rows(df.zone %>% mutate(value = as.character(value))) # Ajout des données sur la qualité dans un format simple a exploiter
      
      # VIII. Secret statistique
      #-------------------------
      incProgress(amount = 0.8,message = "Secret statistique")
      
      # Utiliser la règle des 11 observations minimum par case.
      seuil_secret_stat <- 11
      
      # VIII.1. Table de travail pour le secret statistique
      df.zone.secret <- df.zone %>% 
        filter(type.indicateur %in% c("freq","n"))
      
      # VIII.2. Catégorie à secretiser
      var.secret <- lstCategorie$nomVariable[lstCategorie$typeVar == "pct" & substr(lstCategorie$source,1,2) == "rp"]
      
      # VIII.3. Secret statistique
      indicateur.secret <- bind_rows(lapply(var.secret,secret_stat,df.zone.secret = df.zone.secret,seuil_secret_stat = seuil_secret_stat))
      Sys.time() - t1
      
      # VIII.4. Ajout du secret aux indicateurs calculés
      df.zone <- df.zone.secret %>% 
        left_join(indicateur.secret,by = c("idZonage","nomVariable","nomIndicateur")) %>% # Ajout de la variable diffusable
        mutate(value = ifelse(is.na(diff.secret),"diffusable",diff.secret),
               type.indicateur = "secret_stat") %>% 
        select(-diff.secret) %>% 
        bind_rows(df.zone)
      
      Sys.time() - t1
      
      # VIII.5 Valeur diffusable
      df.zone <- df.zone %>% 
        filter(type.indicateur %in% c("val.qualite","secret_stat")) %>% 
        spread(key = type.indicateur, value = value) %>% 
        mutate(secret_stat = ifelse(is.na(secret_stat),"n_diffusable",secret_stat),
               valeur.diffusable = ifelse(secret_stat == "diffusable",val.qualite,"c")) %>%  # c : données confidencielles
        select(-secret_stat,-val.qualite) %>% 
        gather("type.indicateur","value",-group_var,-nomVariable,-nomIndicateur,-domaine,-categorie,-source) %>% 
        bind_rows(df.zone)
      
      # Paramètres Agate
      rv$df.zone <- df.zone
      rv$statZone <- statZone
      rv$source <- source
  })

    # VII. Nettoyage
    #--------------
    
    # VII.1. Closing modal
    toggleModal(session, modalId = "bs_optad", toggle = "close")    
    
    # VII.2. Pop-up indiquant la fin du calcul
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
  observeEvent(rv$qualityZone,{
    
    print(is.null(rv$qualityZone))
    
    if(!is.null(rv$qualityZone)){
      QualiteVar<- rv$qualityZone %>% filter(!duplicated(Variable)) %>% select(Variable)
      QualiteZone<- rv$qualityZone %>% filter(!duplicated(zonage)) %>% select(zonage)

      updateSelectInput(session, "si_variable_qual",
                        choices = as.character(QualiteVar$Variable),
                        selected = as.character(QualiteVar$Variable[1])
      )

      updateSelectInput(session, "si_zone_qual",
                        choices = as.character(QualiteZone$zonage),
                        selected = as.character(QualiteZone$zonage[1])
      )
      
      
      
    }
    
  })
  
  
  
  
  # VI.1. Display data
  #-------------------
  
  # output$qualityTable = DT::renderDataTable(
  #   datatable(rv$qualityZone,
  #             extensions = 'Buttons',
  #             options = list(
  #               scrollX = TRUE,
  #               # fixedColumns = TRUE,
  #               # autoWidth = TRUE,
  #               ordering = FALSE,
  #               dom = 'lBfrtip',
  #               buttons = c(I('colvis'),'excel', 'pdf')),
  #             rownames= FALSE,
  #             class = "display" #if you want to modify via .css
  #             ) %>% formatStyle(
  #     'CV_Y',
  #     target = 'row',
  #     # backgroundColor = styleEqual(c(0,28.8), c('blank', 'yellow'))
  #     backgroundColor = styleInterval(c(15,30,100), c("blank","#fee8c8","#fdbb84","#e34a33"))
  #   ) %>% 
  #     formatCurrency(columns = 2:4, currency = "", interval = 3, mark = " ",digits = 0) 
  # )
  
  observeEvent(c(input$si_zone_qual,input$si_variable_qual,input$si_select_qual),{
    
    if(!is.null(rv$qualityZone)){
      
      if(input$si_select_qual=="Par Zone"){
        output$TO_titleTab_qual <- renderText(paste0("Toutes les variables sur ",input$si_zone_qual))
        
        df<-rv$qualityZone %>% filter(zonage==input$si_zone_qual)
        output$dt_qualite = renderDT(
          datatable(df,
                    #colnames = type.ind,
                    extensions = 'Buttons',
                    options = list(
                      scrollX = TRUE,
                      # fixedColumns = TRUE,
                      # autoWidth = TRUE,
                      ordering = FALSE,
                      dom = 'lBfrtip',
                      buttons = c(I('colvis'),'excel')),
                    rownames= FALSE)
        )
      } else if(input$si_select_qual=="Par Variable"){
        output$TO_titleTab_qual <- renderText(paste0(input$si_variable_qual," sur toutes les zones"))
        
        df<-rv$qualityZone %>% filter(Variable==input$si_variable_qual)
        output$dt_qualite = renderDT(
          datatable(df,
                    #colnames = type.ind,
                    extensions = 'Buttons',
                    options = list(
                      scrollX = TRUE,
                      # fixedColumns = TRUE,
                      # autoWidth = TRUE,
                      ordering = FALSE,
                      dom = 'lBfrtip',
                      buttons = c(I('colvis'),'excel')),
                    rownames= FALSE)
        )
      }
      else{
        output$TO_titleTab_qual <- renderText("Toutes les Variables sur toutes les zones")
        
        df<-rv$qualityZone
        output$dt_qualite = renderDT(
          datatable(df,
                    #colnames = type.ind,
                    extensions = 'Buttons',
                    options = list(
                      scrollX = TRUE,
                      # fixedColumns = TRUE,
                      # autoWidth = TRUE,
                      ordering = FALSE,
                      dom = 'lBfrtip',
                      buttons = c(I('colvis'),'excel')),
                    rownames= FALSE)
        )
      }# end if
      
    }# end if
    
  })
  
  
  
  
  
} # End Server
