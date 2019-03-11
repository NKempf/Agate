#---------------------------------------------------------------------------------------------------------------------------#
#                                             Agate - Server                                                                #
#---------------------------------------------------------------------------------------------------------------------------#

source(file = "AgataSERVER_dashboard.R",encoding = "UTF-8")


server <- function(input, output,session) {
# 0. Reactive Values
#----------------------------------------------------------------------------------------------------------------------------
  rv <- reactiveValues(AgateMap=NULL,
                       statZone=NULL,
                       statHZone=NULL,
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
      
    # 0. Selection des bases de travail (donnees reelles ou fausses)
    #---------------------------------------------------------------
      # Utilise les valeurs saisies par l'utilisateur
      # Ril
      ril.an <- "15"
      ril.path.string <- paste0("Data/Ril/ril",ril.an,".fst")
      # ril.path.string <- "Data/Ril/ril_leger.fst"
      rilPath <- ifelse(file.exists(ril.path.string),ril.path.string,"Data/Ril/FakeRil.fst")
      # RP
      rp.an <- substr(input$SI_Rp,3,4)
      rpi.path.string <- paste0("Data/Rp/rp",rp.an,"i.fst")
      rpl.path.string <- paste0("Data/Rp/rp",rp.an,"l.fst")
      rpiPath <- ifelse(file.exists(rpi.path.string),rpi.path.string,"Data/Rp/FakeRpi.fst")
      rplPath <- ifelse(file.exists(rpl.path.string),rpl.path.string,"Data/Rp/FakeRpl.fst")
      # Filosofi
      filo.an <- substr(input$SI_filo,3,4)
      filo.path.string <- paste0("Data/Filosofi/filo",filo.an,".fst")
      filoPath <- ifelse(file.exists(filo.path.string),filo.path.string,"Data/Filosofi/FakeFilo.fst")
    
    # I. Preparation du zonage
    #-------------------------
      # I.1. Creation de la variable zonage
      zonage <- rv$AgateMap
      zonage <- spTransform(zonage, "+init=epsg:3857")
      
      # I.2. Creation de l'identifiant idZonage
      # zonage@data$idZonage <- zonage@data$idZonage
      
    # II. Logements géolocalisés du RIL 
    #----------------------------------
    
    # II.1 Communes dans lesquelles se trouvent une ou plusieurs zones
    zoneInter <- gIntersects(zonage,com.dom,byid = TRUE)
      test <- apply(zoneInter, 1, function(x){
        test <- sum(x)
        return(ifelse(test>0,TRUE,FALSE))}) 
    com.dom.select <- com.dom@data$Codgeo[test]
    
    # II.2. Chargement des logements du RIL dans les communes d'interets
    ril <- read_fst(rilPath) %>% 
      select(idx,nb_logn,x,y) %>% 
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
    incProgress(amount = 0.2,message = "Ajout de la zone aux données du RP")
    
    rpi <- read_fst(rpiPath) %>% 
      filter(idx %in% ril@data$idx) %>% 
      left_join(pts.sp@data[,c("idx","idZonage")], "idx") %>% 
      mutate(idZonage = ifelse(is.na(idZonage),"Hors zonage", idZonage)) %>% 
      left_join(zonage@data %>% select(idZonage,idZonage.name),"idZonage")
    
    # III.3. Ajout de la zone aux données du rp logement
    rpl <- read_fst(rplPath) %>% 
      filter(idx %in% ril@data$idx) %>% 
      left_join(pts.sp@data[,c("idx","idZonage")], "idx") %>% 
      mutate(idZonage = ifelse(is.na(idZonage),"Hors zonage", idZonage)) %>% 
      left_join(zonage@data %>% select(idZonage,idZonage.name),"idZonage")
    
    # III.4. Table multicommunes
    zonage.com <- rpl %>% 
      group_by(dep,com,com.lib,idZonage,idZonage.name) %>% 
      summarise(freq=n())
    
    # IV. Ajout de la zone aux données fiscales
    #------------------------------------------
    
    # IV.1. Chargement des données fiscales filtrées selon les communes d'intérêts
    incProgress(amount = 0.3,message = "Ajout des données fiscales")
    filo <- read_fst(filoPath) %>% 
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
    
    str(filo)
    
    # V. Calcul des indicateurs statistiques
    #---------------------------------------
    incProgress(amount = 0.4,message = "Calcul des statistiques")
    
    # V.1. Statistiques dans la zone
    rv$statZone <- statistics_zone(group_var = c("com","idZonage","idZonage.name"),zone = zonage,rpi = rpi,rpl = rpl, filo = filo,
                               sourceRpi = paste0("rpi",rp.an),
                               sourceRpl = paste0("rpl",rp.an),
                               sourceFilo = paste0("filo",filo.an),
                               rpi.weight = "IPONDI",
                               rpl.weight = "IPONDL",
                               filo.weight = "nbpersm")
    
    # V.2. Objets pour page "statistiques"
    rv$df.zone <- rv$statZone$indicateur_stat
    rv$source <- unique(rv$df.zone$source)
    
    # VI. Qualité des données du RP
    #------------------------------
    incProgress(amount = 0.5,message = "Qualité des données du rp")
    
    ###### MAJ 20.02.2019 : Ajout des travaux de Baptiste Raimbaud
    # VI.1. Chargement de la base adresses
    rpa <- read_fst("Data/Tmp/rpa.fst") %>% 
      filter(idx %in% ril@data$idx) %>% 
      left_join(pts.sp@data[,c("idx","idZonage")], "idx") %>% 
      mutate(idZonage = ifelse(is.na(idZonage) | idZonage == "Hors zonage","horsZon", idZonage)) %>%
      mutate(idZonage = ifelse(idZonage=="horsZon",paste0(idZonage,com),idZonage))
    
    rilqualite <- pts.sp@data %>% mutate(idZonage = ifelse(is.na(idZonage) | idZonage == "Hors zonage","horsZon", idZonage)) %>%
      mutate(idZonage = ifelse(idZonage=="horsZon",paste0(idZonage,com),idZonage))
    
    # VI.2. Liste des variables à calculer
    group_var.qualite <- c("INPER","NbFemme / INPER","NbHomme / INPER","NbJeune / INPER","NbMoyen / INPER","NbVieux / INPER", #Territoire
                           "NbVieux2 / INPER","NbMoyen2 / INPER","NbVieux3 / INPER",
                           "ACTIF / NbAgeTravaille","- ACTIF / NbAgeTravaille","INPCM / ACTIF","- INPCM / ACTIF","HommeActif / NbHomme", # Emploi
                           "- HommeActif / NbHomme","FemmeActif / NbFemme","- FemmeActif / NbFemme","NbCadre / NbAgeTravaille","- NbCadre / NbAgeTravaille",
                           "NbEtudian1825 / INPER","NbEtudian0206 / INPER","NbEtudian0614 / Nb0614","NbDecrocheur / Nb1625","NbScole_15plus / NbScole", #Scolarité
                           "- NbEtudian1825 / INPER","- NbEtudian0206 / INPER","- NbEtudian0614 / Nb0614","- NbDecrocheur / Nb1625","- NbScole_15plus / NbScole",
                           "NbImmigre / INPER","- NbImmigre / INPER","NbEtranger / INPER","- NbEtranger / INPER", # Immigration
                           "CATL1 / X","CATL2 / X","CATL3 / X","CATL4 / X","NbLocataire / X","- NbLocataire / X","NblocHLM / X", #Logement
                           "- NblocHLM / X","NbAppartement / X","- NbAppartement / X",
                           "NbHLM / CATL1","Surface / CATL1","- Surface / CATL1","NbBain / CATL1","NbEAU / CATL1","NbEGOUL / CATL1", # Residence principal
                           "- NbHLM / CATL1","- NbBain / CATL1","- NbEAU / CATL1","- NbEGOUL / CATL1"
    )
    TablePassage <- data.frame(group_var.qualite)
    TablePassage$LibVar <- c("population","femme","homme","[0,20)" ,"[20,65)","[65,120]","[75,120]","[20,60)","[60,75)", #Territoire
                             "actif","inactif","chomeur","actifocc","actif homme","inactif homme","actif femme","inactif femme","cadre_prof_inter","autre", #Emploi
                             "etudi[18,25)","etudi[2,6)","etudi","decrocheur","nScola_15plus","n_etudi[18,25)","n_etudi[2,6)","n_etudi","n_decrocheur","autre", #Scolarité
                             "immigre","non_immigre","etranger","francais", #Immigration
                             "CATL_1","CATL_2","CATL_3","CATL_4","locataire","autre","locataireHlm","autre","appartement","autre", #Logement
                             "hlm","surf100etplus","surf100moins","bain_douche","Eau_chaude","tout_egout","n_hlm","N_bain_douche","N_eau_chaude","N_tout_egout") # Residence principal
    TablePassage$Domaine <-   c(1,1,1,1,1,1,1,1,1,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,5,5,5,5,6,6,6,6,6,6,6,6,6,6,7,7,7,7,7,7,7,7,7,7)
    TablePassage$Categorie <- c(1,2,2,3,3,3,3,3,3,1,1,1,1,2,2,2,2,3,3,1,1,2,3,4,1,1,2,3,4,1,1,2,2,1,1,1,1,2,2,3,3,4,4,1,2,2,3,4,5,1,3,4,5)
    
    # VI.3. Estimation de la qualité
    rv$qualityZone <- Qlfinal(rpa,group_var.qualite,ril = rilqualite)
    
    # # VI.1. Chargement de la base adresses
    # rpa <- read_fst("Data/Rp/rpa13.fst") %>% 
    #   filter(idx %in% ril@data$idx) %>% 
    #   left_join(pts.sp@data[,c("idx","idZonage")], "idx") %>% 
    #   mutate(idZonage = ifelse(is.na(idZonage) | idZonage == "Hors zonage","horsZon", idZonage))
    # 
    # # VI.2. Base sondage (special calage)
    # sondage <- sondageZon(rpa = rpa)
    # 
    # # VI.3. Calcul de la precision analytique sans calage
    # rv$qualityZone <- precision_analytique_nc(rpa = rpa,Y = INPER,zonage = zonage,idZonage = "idZonage",sondage = sondage) # Nombre de personne
      
    }) # Fermeture du withprogress

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
  observeEvent(input$si_domaine,{
    cat <- lstCategorie$idCategorie[lstCategorie$idDomaine == input$si_domaine]
    names(cat) <- lstCategorie$labelCategorie[lstCategorie$idDomaine == input$si_domaine]
    updateSelectInput(session, "si_categorie",
                      choices = cat)
  })
  
  # V.3. Reactive data table
  #-------------------------
  observeEvent(c(input$si_categorie,input$si_domaine,input$si_zoneSelect),{
    
    if(input$si_categorie != ""){
      # V.3.1. Selection de la base de données
      if(input$si_zoneSelect == 4){
        df <- rv$df.zone %>% 
          select(source,domaine,categorie,com,idZonage,idZonage.name,indicateur,type.indicateur,value) %>%
          filter(type.indicateur != "part_np") %>% 
          filter(domaine == input$si_domaine & categorie == input$si_categorie)
      }else{
        df <- read_fst("Data/Stats/Prefine aera/Real/fst/indicateur_stat.fst") %>% 
          filter(zone.predefine == input$si_zoneSelect & domaine == input$si_domaine & 
                   categorie == input$si_categorie & source %in% rv$source) %>%
          filter(type.indicateur != "part_np") %>% 
          select(source,domaine,categorie,idZonage,idZonage.name,indicateur,type.indicateur,value)
      }
      
      # V.3.2. Libelles de colonnes du tableau
      type.ind <- typInd[typInd %in% c("idZonage","idZonage.name",unique(df$type.indicateur))]
      
      # V.3.3. Construction tableau
      df <- df %>%
        spread(key = type.indicateur, value = value) %>%
        left_join(lstIndicateur %>% select(nomIndicateur,labelIndicateur),c("indicateur" = "nomIndicateur")) %>% 
        mutate(indicateur = labelIndicateur) %>% 
        select(-domaine,-categorie,-labelIndicateur)
      
      # V.3.4. Titre du tableau
      output$TO_titleTab <- renderText({lstCategorie$titreTab[lstCategorie$idDomaine == input$si_domaine &
                                                                lstCategorie$idCategorie == input$si_categorie]})
      # V.3.5. Affichage du tableau
      output$table = renderDT(
        datatable(df,
                  colnames = type.ind,
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
    } # end if
    
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
