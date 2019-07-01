#---------------------------------------------------------------------------------------------------------------------------#
#                                             Agate - UI refonte                                                            #
#---------------------------------------------------------------------------------------------------------------------------#

# 01.07.2019

library(rgdal)
library(rgeos)
library(leaflet)
library(leaflet.extras) # 
library(rlang) #
library(tidyverse)
library(fst)
library(fstplyr)
library(shiny) # Graphic web interface
library(shinyBS) # Pop-up windows
library(shinyjs) # Mask buttons/elements on graphic interface
library(shinyWidgets) # Widgets supplementaires
library(shinydashboard) # Tools like infoBox
library(shinycssloaders) # spinner loader
library(plotly)
library(DT)
library(easySdcTable)
library(rintrojs) # Tutoriel intéractif

# Fonctions  particulières
source("Other programs/2 Fonctions Agate/4 Dashboard/Agate - Dashboard fct.R",encoding = "UTF-8")
source("Other programs/2 Fonctions Agate/3 Indicateurs statistiques/Agate - Statistics Zonage_v10.R",encoding = "UTF-8")

# Label et choix
load("data/Liste indicateurs statistiques/lstIndicateur.RData")
dmn <- lstDomaine$domaine
names(dmn) <- lstDomaine$labelDomaine
typInd <- lstTypeIndicateur$typeIndicateur
names(typInd) <- lstTypeIndicateur$labelTypeIndicateur

# heatPoints
load("Data/Maps/HeatPoint/heatpoints.Rdata") 

# Communes
load("Data/Maps/Zones predefinies/zp_communes.RData")

# Markdown files
qualiteRpFile <- "Other programs/3 Documentation/1 Agate documentation/2 Qualite RP/NoteQualite.Rmd"
rmdfiles <- c(qualiteRpFile)
sapply(rmdfiles, knit, quiet = T)


# NavBar Spéciale :)
navbarPageWithInputs <- function(..., inputs) {
  navbar <- navbarPage(...)
  form <- tags$form(class = "navbar-form", inputs)
  navbar[[3]][[1]]$children[[1]] <- htmltools::tagAppendChild(
    navbar[[3]][[1]]$children[[1]], form)
  navbar
}


ui <- tagList(
  introjsUI(),
  useShinyjs(),
  navbarPageWithInputs(
    "Agate",
    
    # I. Carte
    #-------------------------------------------------------------------------------------------------------------------------------------------------
    tabPanel(
      "Carte",
      div(class="outer",
          tags$head(includeCSS("www/agate.css")),
          # I.1.1. Leaflet map
          #-------------------
          leafletOutput("llo_agateMap", width = "100%", height = "100%"),
          
          # I.1.2. Statistical controls
          #----------------------------
          absolutePanel(id = "ap_controls",
                        draggable = TRUE,top = 20, right = 10,width = 300,#height = 500,
                        useSweetAlert(),
                        # h3("Navigation"),
                        fluidRow(id="fr_typeZone",
                                 radioGroupButtons(
                                   inputId = "rg_typeZone",
                                   choices = c("Utilisateur" = 1,
                                               "Prédéfini" = 2),
                                   justified = TRUE
                                 )
                        ),
                        # I.1.2.1 UTILISATEUR
                        fluidRow(id = "fr_utilimportMap",
                                 column(12,
                                        fileInput('fi_userMap', 'Importer un shapeFile',multiple = T),
                                        selectInput(inputId = "si_userMap_id", label = "Identifiant", 
                                                    choices = c("Choice" =""),selected = c("")),
                                        selectInput(inputId = "si_userMap_name", label = "Libellé", 
                                                    choices = c("Choice" =""),
                                                    selected = c("")),
                                        fluidRow(
                                          column(6,
                                                 switchInput(
                                                   inputId = "swi_userMapEdit",
                                                   value = FALSE,label = "Editer",onStatus = "success"
                                                 )
                                          ),
                                          column(6,
                                                 
                                                 switchInput(
                                                   inputId = "swi_heatPoint",
                                                   value = FALSE,label = "Chaleur",onStatus = "warning"
                                                 )
                                          )
                                        )     
                                 )
                        ),
                        fluidRow(id = "fr_utilStat",
                                 column(12,
                                        
                                        selectInput(inputId = "si_rp", label = "Recensement de la population", 
                                                    choices = c("2015" = "15","2014" = "14","2013" = "13"),
                                                    selected = "15" ),
                                        pickerInput(inputId = "pi_userMapSelect",
                                                    label = "Selection des zones à calculer", 
                                                    choices = c("Choice" =""),
                                                    options = list(
                                                      `actions-box` = TRUE), 
                                                    multiple = TRUE),
                                        actionButton("ab_userStat","Indicateurs statistiques")
                                 ))
                        ,
                        # I.1.2.2 Zones prédéfinies
                        hidden(
                          fluidRow(id = "fr_predOption",
                                   column(12,
                                          selectInput("si_zonePred", "Selectionner une maille",
                                                      choices = c("",pred.choice),
                                                      selected =c(4)
                                          ),
                                          
                                          selectInput("si_zonePred_dep", "Selectionner une zone géographique",
                                                      choices = "",
                                                      selected = ""
                                          ) 
                                   )
                          )
                        ),
                        # Affichage / réduction du menu
                        prettyToggle(
                          inputId = "pt_hideMenu",
                          label_on = "Réduire", 
                          label_off = "Afficher",
                          outline = TRUE,
                          plain = TRUE,value = TRUE,status_on = "success",status_off = "success",
                          icon_on = icon("arrow-up"), 
                          icon_off = icon("arrow-down")
                        )
          ) # end absolutepanel 
      ) %>% withSpinner(type = 6,size = 2,proxy.height = 100) # end div
      
      
      
      # IV. Dashboard
      #-------------------------------------------------------------------------------------------------------------------------------------------------
      ,
      tags$head(tags$style("#bs_dashboard .modal-footer{ display:none}
                         #bs_dashboard .modal-header{ display:none}
                         #bs_dashboard { opacity:0.95}")), # Remove BS modal footer
      tags$head(tags$style(HTML('.modal-lg {width: 90%;}'))), # Increase modal size
      bsModal('bs_dashboard', title = "",'test',size = "large",
              # Add CSS files : use infobox from shinydashboard package into a shinyApp
              includeCSS(path = "www/AdminLTE.css"),
              includeCSS(path = "www/shinydashboard.css"),
              includeCSS(path = "www/agateDashboard.css"),
              
              navbarPageWithInputs(id="nbwi_agate_dashboard",textOutput("to_titleDash"),
                                   
                                   # II. Thème Démographie
                                   #-----------------------------------------------------------------------------------------------------------------
                                   tabPanel("Démographie",
                                            includeCSS(path = "www/AdminLTE.css"),
                                            includeCSS(path = "www/shinydashboard.css"),
                                            
                                            fluidRow(
                                              infoBoxOutput(outputId = "ib_dem_feminite"),
                                              infoBoxOutput(outputId = "ib_dem_population"),
                                              infoBoxOutput(outputId = "ib_dem_superficie")
                                            ) ,
                                            fluidRow(
                                              box(id="b_test",title = "Individus", solidHeader = TRUE,
                                                  collapsible = TRUE,
                                                  DT::dataTableOutput("dt_dem_hg") %>% withSpinner(type = 6) 
                                              ),
                                              box(title = "Ménages", solidHeader = TRUE,
                                                  collapsible = TRUE,
                                                  DT::dataTableOutput("dt_dem_hd") %>% withSpinner(type = 6) 
                                              )
                                            ),
                                            fluidRow(
                                              box(title = "Structure de la population", solidHeader = TRUE,
                                                  collapsible = TRUE,
                                                  plotlyOutput("g_dem_bg") %>% withSpinner(type = 6) 
                                              ),
                                              box(title = "Étrangers - immigrés", solidHeader = TRUE,
                                                  collapsible = TRUE,
                                                  DT::dataTableOutput("dt_dem_bd") %>% withSpinner(type = 6) 
                                              )
                                            ),
                                            fluidRow(
                                              textOutput("to_source")
                                            )
                               
                                   ),
                                   # III. Thème  Emploi
                                   #-----------------------------------------------------------------------------------------------------
                                   tabPanel("Emploi",
                                            
                                            fluidRow(
                                              infoBoxOutput(outputId = "ib_emp_pop_trav"),
                                              infoBoxOutput(outputId = "ib_emp_chomeur"),
                                              infoBoxOutput(outputId = "ib_emp_inactif")
                                            ),
                                            fluidRow(
                                              box(title = "Marché de l'emploi", solidHeader = TRUE,
                                                  collapsible = TRUE,
                                                  DT::dataTableOutput("dt_emp_hg") %>% withSpinner(type = 6) 
                                              ),
                                              box(title = "Chômage", solidHeader = TRUE,
                                                  collapsible = TRUE,
                                                  DT::dataTableOutput("dt_emp_hd") %>% withSpinner(type = 6) 
                                              )
                                            ),
                                            fluidRow(
                                              box(title = "Type d'activité", solidHeader = TRUE,
                                                  collapsible = TRUE,
                                                  plotlyOutput("g_emp_bg") %>% withSpinner(type = 6) 
                                              ),
                                              box(title = "Travail", solidHeader = TRUE,
                                                  collapsible = TRUE,
                                                  DT::dataTableOutput("dt_emp_bd") %>% withSpinner(type = 6) 
                                              )
                                            )
                                   ),
                                   
                                   # IV. Thème Scolarisation
                                   #-----------------------------------------------------------------------------------------------------
                                   tabPanel("Scolarisation",
                                            
                                            fluidRow(
                                              infoBoxOutput(outputId = "ib_sco_pop_sco"),
                                              infoBoxOutput(outputId = "ib_sco_etud"),
                                              infoBoxOutput(outputId = "ib_sco_decrocheur")
                                            ),
                                            fluidRow(
                                              box(title = "Jeunes scolarisés", solidHeader = TRUE,
                                                  collapsible = TRUE,
                                                  DT::dataTableOutput("dt_sco_hg") %>% withSpinner(type = 6) 
                                              ),
                                              box(title = "Jeunes non scolarisés", solidHeader = TRUE,
                                                  collapsible = TRUE,
                                                  DT::dataTableOutput("dt_sco_bd") %>% withSpinner(type = 6) 
                                              )
                                            ),
                                            fluidRow(
                                              box(title = "Niveau de diplôme", solidHeader = TRUE,
                                                  collapsible = TRUE,
                                                  plotlyOutput("g_sco_bg") %>% withSpinner(type = 6) 
                                              ),
                                              
                                              box(title = "Jeunes", solidHeader = TRUE,
                                                  collapsible = TRUE,
                                                  plotlyOutput("g_sco_hd") %>% withSpinner(type = 6) 
                                              )
                                            )
                                   ),
                                   
                                   # V. Thème Logement
                                   #-----------------------------------------------------------------------------------------------------
                                   tabPanel("Logement",
                                            
                                            fluidRow(
                                              infoBoxOutput(outputId = "ib_log_pop"),
                                              infoBoxOutput(outputId = "ib_log_hlm"),
                                              infoBoxOutput(outputId = "ib_log_maison")
                                            ),
                                            fluidRow(
                                              box(title = "Caractéristiques des logements", solidHeader = TRUE,
                                                  collapsible = TRUE,
                                                  DT::dataTableOutput("dt_log_hg") %>% withSpinner(type = 6) 
                                              ),
                                              box(title = "Catégorie de logement", solidHeader = TRUE,
                                                  collapsible = TRUE,
                                                  plotlyOutput("g_log_hd") %>% withSpinner(type = 6) 
                                              )
                                            ),
                                            fluidRow(
                                              box(title = "Année d'achevement", solidHeader = TRUE,
                                                  collapsible = TRUE,
                                                  plotlyOutput("g_log_bg") %>% withSpinner(type = 6) 
                                              ),
                                              box(title = "Aspect du bâti", solidHeader = TRUE,
                                                  collapsible = TRUE,
                                                  plotlyOutput("g_log_bd") %>% withSpinner(type = 6) 
                                              )
                                            )
                                   ),
                                   # V. Thème Résidences principales
                                   #-----------------------------------------------------------------------------------------------------
                                   tabPanel("Résidences principales",
                                            fluidRow(
                                              infoBoxOutput(outputId = "ib_res_pop"),
                                              infoBoxOutput(outputId = "ib_res_collectif"),
                                              infoBoxOutput(outputId = "ib_res_todo")
                                            ),
                                            fluidRow(
                                              box(title = "Caractéristiques des résidences principales", solidHeader = TRUE,
                                                  collapsible = TRUE,
                                                  DT::dataTableOutput("dt_res_hg") %>% withSpinner(type = 6) 
                                              ),
                                              box(title = "Équipements", solidHeader = TRUE,
                                                  collapsible = TRUE,
                                                  DT::dataTableOutput("dt_res_bd") %>% withSpinner(type = 6) 
                                              )
                                            ),
                                            fluidRow(
                                              box(title = "Surface", solidHeader = TRUE,
                                                  collapsible = TRUE,
                                                  plotlyOutput("g_res_bg") %>% withSpinner(type = 6) 
                                              ),
                                              box(title = "Nombre de pièces", solidHeader = TRUE,
                                                  collapsible = TRUE,
                                                  plotlyOutput("g_res_hd") %>% withSpinner(type = 6) 
                                              )
                                            )
                                   ),
                                   
                                   # VI. Zones de comparaisons
                                   #-----------------------------------------------------------------------------------------------------
                                   inputs = dropdownButton(inputId = "ddb_compare" ,
                                                           selectInput("si_typeZone", "Type de zone",
                                                                       choices = pred.choice,
                                                                       selected = c(4)),
                                                           selectInput("si_zone", "Zone de comparaison",
                                                                       choices = c("Choice" =""),
                                                                       selected = c("")),
                                                           circle = TRUE, status = "danger",
                                                           icon = icon("gear"),size = "sm",right = TRUE,
                                                           tooltip = tooltipOptions(title = "Zones de comparaison")
                                   )
              ) 
      ) # end Dashboard
      
    ),# Tabset Carte
    
    # II. Statistiques et téléchargement
    #-------------------------------------------------------------------------------------------------------------------------------------------------
    tabPanel("Statistiques",
             # value="vis", # MAJ 24 juin 2019
             sidebarPanel(width=2,
                          # II.1. Maille géographique
                          #--------------------------
                          selectInput("si_stat_zoneSelect", "Maille géographique",
                                      choices = pred.choice,
                                      selected = 1),
                          
                          # II.2. Zones étudiéds
                          #---------------------
                          pickerInput(inputId = "pi_stat_zone_etude",
                                      label = "Zones étudiées",
                                      choices = c("Choice" =""),
                                      options = list(
                                        `live-search` = TRUE,
                                        `actions-box` = TRUE), 
                                      multiple = TRUE
                          ),
                          
                          # II.3. Domaine selection
                          #------------------------
                          selectInput("si_stat_domaine", "Thématique",
                                      choices = c("Choice" ="",dmn),
                                      selected = dmn[2]),
                          
                          # II.4. Categorie selection
                          #--------------------------
                          selectInput("si_stat_categorie", "Indicateurs",
                                      choices = c("Choice" =""),
                                      selected = c(""))
             ),
             
             mainPanel(
               
               # II.4. Table visualisation
               #--------------------------
               textOutput("to_stat_title"),
               tags$head(tags$style("#to_stat_title{font-size: 30px;font-style: bold;}")),
               DT::dataTableOutput("dt_stat_explore")
             )
             
             
    ),
    # III. Documentation
    #-------------------------------------------------------------------------------------------------------------------------------------------------
    navbarMenu("Documentation",
               # III.1. indicateurs
               #-------------------
               tabPanel("Indicateurs",
                        
                        sidebarPanel(width=2,
                                     selectInput("si_ind_domaine", "Thématique",
                                                 choices = c("Choice" ="",dmn),
                                                 selected = dmn[2]),

                                     selectInput("si_ind_categorie", "Indicateurs",
                                                 choices = c("Choice" =""),
                                                 selected = c(""))
                        ),
                        mainPanel(
                          DT::dataTableOutput("dt_indicateurs") 
                        )
               ),
               # III.2. Qualité du RP
               #---------------------
               tabPanel("Qualité du RP",
                        withMathJax(includeMarkdown(qualiteRpFile))
                        )
               
               
    ),# End documentation
    # IV. Aide Agate
    #-------------------------------------------------------------------------------------------------------------------------------------------------
    # Aide bouton
    inputs = actionBttn(
      inputId = "ab_aide",
      style = "stretch",
      color = "warning",
      icon = icon("question")
    )
    
  )
)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

server = function(input, output, session) {
  
  # Reactive Values
  #----------------------------------------------------------------------------------------------------------------------------
  rv <- reactiveValues(AgateMap = NULL,
                       userMap = NULL,
                       importUserMap = NULL,
                       zone.active = NULL,
                       zone.etude = NULL,
                       zone.comparaison = NULL,
                       df.zone.etude=NULL,
                       pyramide.etude = NULL,
                       dash.indicateur = NULL,
                       zone.dep = NULL,
                       df.zone.user = NULL,
                       zonage.com.user = NULL,
                       pyramide.user = NULL,
                       source.an = NULL)
  
  # I. Interactive map
  #----------------------------------------------------------------------------------------------------------------------------
  output$llo_agateMap <- renderLeaflet({
    leaflet("agateMap",data = heat.pts) %>% addTiles()%>% 
      fitBounds(lng1 = -65,lat1 = 18,lng2 = -45,lat2 = 3) %>% 
      addHeatmap(group = "heatpts",lng = ~x, lat = ~y,
                 # intensity = ~nivviem,
                 blur = 60, radius = 30) %>% 
      hideGroup("heatpts")
  })
  
  # Zoom sur click
  observeEvent(input$llo_agateMap_shape_click, { 
    if(!is.null(rv$AgateMap)){
      mapSelect <- rv$AgateMap[rv$AgateMap@data$idZonage == input$llo_agateMap_shape_click$id,]
      
      mapSelect.bbox <- as.data.frame(bbox(mapSelect))
      zoom_lng <- (mapSelect.bbox$max[1] - mapSelect.bbox$min[1])/2
      zoom_lat <- (mapSelect.bbox$max[2] - mapSelect.bbox$min[2])/2
      
      leafletProxy("llo_agateMap") %>%
        fitBounds(lng1 = mapSelect.bbox$min[1] - zoom_lng,
                  lat1 = mapSelect.bbox$max[2] + zoom_lat,
                  lng2 = mapSelect.bbox$max[1] + zoom_lng,
                  lat2 = mapSelect.bbox$min[2] - zoom_lat)
    }
  })
  
  # Heatpoints
  observeEvent(input$swi_heatPoint, {
    if(input$swi_heatPoint){
      leafletProxy("llo_agateMap") %>% 
        showGroup("heatpts")
    }else{
      leafletProxy("llo_agateMap") %>% 
        hideGroup("heatpts")
    }
  })
  
  # Affichage d'un zonage
  observeEvent(rv$AgateMap,{
    
    if(!is.null(rv$AgateMap)){
      
      if(input$rg_typeZone == 1){
        zone <- rv$AgateMap@data$idZonage
        names(zone) <- rv$AgateMap@data$idZonage.name
        updatePickerInput(session = session,inputId = "pi_userMapSelect",choices = zone,selected = zone)
      }
      
      if(!is.null(rv$AgateMap@data$idZonage) & !is.null(rv$AgateMap@data$idZonage.name)){
        rv$AgateMap <- spTransform(rv$AgateMap, "+init=epsg:4326")
        AgateMap.bbox <- as.data.frame(bbox(rv$AgateMap))
        leafletProxy("llo_agateMap") %>%
          clearShapes() %>% 
          fitBounds(lng1 = AgateMap.bbox$min[1],lat1 = AgateMap.bbox$max[2],lng2 = AgateMap.bbox$max[1],lat2 = AgateMap.bbox$min[2]) %>%
          addPolygons(data=rv$AgateMap,opacity = 3,
                      color = "green", stroke = TRUE, weight = 2,
                      fill = TRUE, fillOpacity = 0.2,popup = ~paste(idZonage.name),layerId = ~paste(idZonage))
      }
    }
  })
  
  # Import d'une carte utilisateur
  observeEvent(input$fi_userMap,{
    
    rv$importUserMap <- NULL
    
    if(input$rg_typeZone == 1){
      
      user.files <- input$fi_userMap
      
      dir <- dirname(user.files[1,4])
      for ( i in 1:nrow(user.files)) {
        file.rename(user.files[i,4], paste0(dir,"/",user.files[i,1]))
      }
      getshp <- list.files(dir, pattern="*.shp", full.names=TRUE)
      userMap <- readOGR(dsn = getshp,encoding = "UTF-8",stringsAsFactors = FALSE)
      rv$importUserMap <- spTransform(userMap, "+init=epsg:4326")
      
      if(!is.null(rv$importUserMap)){
        lst_var <- colnames(rv$importUserMap@data)
        updateSelectInput(session = session,inputId = "si_userMap_id",choices = lst_var,selected = lst_var[1])
        updateSelectInput(session = session,inputId = "si_userMap_name",choices = lst_var,selected = lst_var[2])
      }
    }
  })
  
  # Mise a jour des variables identifiant et libellé
  observeEvent(c(input$si_userMap_id,input$si_userMap_name), {
    
    if(input$si_userMap_id != ""){
      mapTmp <- rv$importUserMap

      # Suppression des caractères spéciaux et des valeurs manquantes
      idtmp <- mapTmp@data[,input$si_userMap_id] %>% 
        as.character() %>% 
        str_replace_all("[[:punct:]]", "") %>% 
        str_replace_all("\032", "") %>% 
        str_trim() %>% 
        iconv(from = 'UTF-8', to = 'ASCII//TRANSLIT')
      idtmp[is.na(idtmp)] <- "valManquante"
      
      nametmp <- mapTmp@data[,input$si_userMap_name] %>% 
        as.character() %>% 
        str_replace_all("[[:punct:]]", "") %>% 
        str_replace_all("\032", "") %>% 
        str_trim() %>% 
        iconv(from = 'UTF-8', to = 'ASCII//TRANSLIT')
      nametmp[is.na(nametmp)] <- "valManquante"

      # Gestion de l'unicité des identifiants 
      if(sum(idtmp %in% idtmp[duplicated(idtmp)])>0){
        idtmp <- paste0(idtmp,"_",1:nrow(mapTmp@data))
      }
      
      # Création des variables idZonage et idZonage.name
      mapTmp@data$idZonage <- idtmp
      mapTmp@data$idZonage.name <- nametmp
      
      # Selection des attributs
      mapTmp@data <- mapTmp@data %>% select(idZonage,idZonage.name)
      
      # MAJ des cartes
      rv$userMap <- mapTmp
      print(rv$userMap@proj4string)
      
      rv$AgateMap <- rv$userMap
      print(rv$AgateMap@proj4string)
    }
  })
  
  # II. Navigation
  #----------------------------------------------------------------------------------------------------------------------------
  
  # Année
  observeEvent(input$si_rp,{
    rv$source.an <- input$si_rp
  })
  
  # Affichage / réduction du menu de navigation de la carte
  observeEvent(c(input$pt_hideMenu,input$rg_typeZone), {
    
    if (input$rg_typeZone == 1) {
      shinyjs::hide("fr_predOption")
      
      if(input$pt_hideMenu){
        shinyjs::show("fr_utilimportMap")
        shinyjs::show("fr_utilStat")
      }else{
        shinyjs::hide("fr_utilimportMap")
        shinyjs::hide("fr_utilStat")
      }
    }
    else{
      shinyjs::hide("fr_utilimportMap")
      shinyjs::hide("fr_utilStat")
      
      if(input$pt_hideMenu){
        shinyjs::show("fr_predOption")
      }else{
        shinyjs::hide("fr_predOption")
      }
    }
  })
  
  
  
  # Zone utilisateur ou predefinie
  observeEvent(input$rg_typeZone,{
    # Init
    rv$zone.etude <- NULL
    rv$pyramide.etude <- NULL
    rv$zone.comparaison <- NULL
    rv$dash.indicateur <- NULL
    rv$zone.etude.previous <- NULL
    
    if(input$rg_typeZone == 1){
      updateSelectInput(session,"si_zonePred",selected = c(4))
      
      # shinyjs::hide("si_zonePred")
      # shinyjs::hide("ab_modal")
      # shinyjs::show("ddb_import")
      # shinyjs::show("ddb_userMapStat")
      # updateSelectInput(session,"si_zonePred",selected = 4)
      # rv$AgateMap <- rv$userMap

    }else{
      # shinyjs::show("si_zonePred")
      # shinyjs::show("ab_modal")
      # shinyjs::hide("ddb_import")
      # shinyjs::hide("ddb_userMapStat")
    }
  })
  
  
  observeEvent(input$si_zonePred,{
    if(input$si_zonePred != ""){
      if(as.numeric(input$si_zonePred) == 1) {
        updateSelectInput(session,"si_zonePred_dep",choices = c("Tous les départements"="all",
                                                        "Guadeloupe" = "971","Martinique" = "972","Guyane" = "973",
                                                        "Saint-Barthélemy" = "977",
                                                        "Saint-Martin" = "978"))
      }else{
        updateSelectInput(session,"si_zonePred_dep",choices = c("Tous les départements"="all",
                                                        "Guadeloupe" = "971","Martinique" = "972","Guyane" = "973"))
      }
    }
  })
  
  
  # PREDEFINIE : Selection de la zone a afficher
  observeEvent(c(input$si_zonePred_dep,input$si_zonePred),{
    
    # Init parameters
    rv$zone.etude <- NULL
    rv$pyramide.etude <- NULL
    rv$zone.comparaison <- NULL
    rv$dash.indicateur <- NULL

  if(input$si_zonePred_dep != "") {
    switch(as.numeric(input$si_zonePred),
           { # Département
             print(!exists("dep.dom"))

             if(!exists("dep.dom")){
               print("Chargement de la carte departement")
               load("Data/Maps/Zones predefinies/zp_departements.RData")
             }

             if(input$si_zonePred_dep != "all"){
               rv$AgateMap <- dep.dom[dep.dom@data$dep == input$si_zonePred_dep,]
             }else{
               rv$AgateMap <- dep.dom
             }

             },
           { # Commune
             print("Chargement de la carte commune")

             if(input$si_zonePred_dep != "all"){
               rv$AgateMap <- com.dom[com.dom@data$dep == input$si_zonePred_dep,]
             }else{
               rv$AgateMap <- com.dom
             }

           },
           {
             if(!exists("qpv")){
               print("Chargement de la carte QPV")
               load("Data/Maps/Zones predefinies/zp_qpv.RData")
             }

             if(input$si_zonePred_dep != "all"){
               rv$AgateMap <- qpv[qpv@data$dep == input$si_zonePred_dep,]
             }else{
               rv$AgateMap <- qpv
             }
             },
           {
             if(!is.null(rv$userMap)){
               rv$AgateMap <- rv$userMap
             }else{
               print("En attente d'une carte utilisateur...")
             }
           })
  }
  })
  
  # Ouverture du dashboard
  observeEvent(input$llo_agateMap_shape_click,{
    
    if(input$rg_typeZone == 1){
      if(!is.null(rv$zonage.com.user)){
        
        if(input$llo_agateMap_shape_click$id %in% rv$zonage.com.user$idZonage){
          
          if(rv$zonage.com.user$appariement_diff[rv$zonage.com.user$idZonage == input$llo_agateMap_shape_click$id]){
            toggleModal(session, "bs_dashboard", toggle = "toggle")
            rv$zone.etude <- input$llo_agateMap_shape_click$id
            
          }else{
            rv$zone.etude <- NULL
            
            shinyWidgets::sendSweetAlert(
              session = session,
              title = "Données non diffusable", text = "La qualité de l'appariement entre le recensement de la population et l'enquête cartographique est insuffisante",
              type = "warning"
            )
          }
        }else{
          rv$zone.etude <- NULL
          shinyWidgets::sendSweetAlert(
            session = session,
            title = "Absence de données", text = "Les indicateurs statistiques n'ont pas été calculés sur cette zone. Merci de la selectionner et de relancer le calcul.",
            type = "warning"
          )
        }
      }
      # else{
      #   rv$zone.etude <- NULL
      #   shinyWidgets::sendSweetAlert(
      #     session = session,
      #     title = "Absence de données", text = "Merci d'effectuer le calcul des indicateurs statistiques avant de pouvoir afficher le tableau de bord.",
      #     type = "warning"
      #   )
      # }
    }else{
      if(input$si_zonePred == 4){
        shinyWidgets::sendSweetAlert(
          session = session,
          title = "Tableau de bord non disponible", text = "Basculer côté utilisateur pour ouvrir le tableau de bord.",
          type = "warning"
        )
      }else{
        print("Zonage prédéfini")
        rv$zone.etude <- input$llo_agateMap_shape_click$id
        print(rv$zone.etude)
        toggleModal(session, "bs_dashboard", toggle = "toggle")
      }
      
    }
  })
  
  # Mise a jour des listes de comparaison dashboard
  observeEvent(c(input$si_typeZone,rv$zone.etude,input$rg_typeZone),{
    
    # init
    rv$zone.comparaison <- NULL
    rv$dash.indicateur <- NULL
    updateSelectInput(session, "si_zone",
                      choices = ""
    )
    
    
    if(!is.null(rv$zone.etude)){
      
      if(input$rg_typeZone == 1){
        
        if(!is.null(rv$df.zone.user)){
          
          if(input$si_typeZone == 4){
            test <- !rv$df.zone.user$idZonage %in% rv$zone.etude
            cat <- unique(rv$df.zone.user$idZonage[test])
            names(cat) <- unique(rv$df.zone.user$idZonage.name[test])
          }else{
            test <- lstZonePreType$zone.pred == input$si_typeZone & 
              lstZonePreType$dep %in% c(0,rv$zonage.com.user$dep[rv$zonage.com.user$idZonage == rv$zone.etude]) & 
              lstZonePreType$idZonage != rv$zone.etude
            cat <- lstZonePreType$idZonage[test]
            names(cat) <- lstZonePreType$idZonage.name[test]
          }
        }
      }else{
        test <- lstZonePreType$zone.pred == input$si_typeZone & 
          lstZonePreType$dep %in% c(0,rv$AgateMap@data$dep[rv$AgateMap@data$idZonage == rv$zone.etude]) & 
          lstZonePreType$idZonage != rv$zone.etude
        cat <- lstZonePreType$idZonage[test]
        names(cat) <- lstZonePreType$idZonage.name[test]
      }
      updateSelectInput(session, "si_zone",
                        choices = cat
      )
    }
  })
  
  # III. Indicateurs statistiques sur userMap
  #--------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  observeEvent(input$ab_userStat,{
    
    
    
    if(!is.null(rv$userMap) & input$rg_typeZone == 1){
      
      withProgress(message = "Creation de l'identifiant",style = "notification", value = 0, {
        
        t1 <- Sys.time()
        zonage <- rv$userMap[rv$userMap$idZonage %in% input$pi_userMapSelect,]
        print(zonage@data)
        agate.stat <- agate_statRp.shiny(rp.an = rv$source.an,
                                         zone.pred = 4,
                                         zoneType = "ZU",
                                         zonage = zonage,
                                         group_var = c("idZonage","idZonage.name"),
                                         com.dom = com.dom,
                                         rpi.weight = "IPONDI.cal",
                                         rpl.weight = "IPONDL.cal",secret_stat = FALSE)
        
        rv$df.zone.user <- agate.stat$df.zone
        rv$zonage.com.user <- agate.stat$zonage.com
        rv$pyramide.user <- agate.stat$pyramide
        
      }) #end withprogress
      
      temps <- as.character(round(abs(difftime(t1,Sys.time(), units="secs")),2))
      
      # Update data explore
      updateSelectInput(session,"si_stat_zoneSelect",selected = 4)
      
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Terminé !", text = paste("Le calcul a été effectué en ",temps," secondes"), type = "success"
      )
      
      
    }# end if
  })
  
  
  # IV. Dashboard
  #--------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  # Selection des données d'études
  observeEvent(rv$zone.etude,{
    
    if(!is.null(rv$zone.etude)){
      
      if(input$rg_typeZone == 1){
        if(!is.null(rv$df.zone.user)){
          
          print(paste0("Zone étude : ",rv$zone.etude))
          
          rv$df.zone.etude <- rv$df.zone.user %>%
            filter(idZonage == rv$zone.etude)
          rv$pyramide.etude <- rv$pyramide.user %>%
            filter(idZonage == rv$zone.etude)
          
          updateSelectInput(session, "si_typeZone",
                            choices = pred.choice,
                            selected = 1
          )
        }
      }else{
        df <- read_fst("Data/Stats/Prefine aera/Real/fst/indicateur_stat.fst") %>%
          filter(zone.pred == input$si_zonePred & idZonage == rv$zone.etude & substr(source,4,5) == rv$source.an)
        df2 <- df %>%
          filter(nomVariable %in% c("emp_typeActivite","sco_popSco2","sco_diplome","log_cat","log_ach_constru",
                                    "log_bati","res_nbPiece","res_surface")) %>%
          mutate(type.indicateur = "part_p")
        
        rv$df.zone.etude <- bind_rows(df,df2)
        
        rv$pyramide.etude <- read_fst("Data/Stats/Prefine aera/Real/fst/pyramide.fst") %>%
          filter(zone.pred == input$si_zonePred & idZonage == rv$zone.etude & substr(source,4,5) == rv$source.an)
        updateSelectInput(session, "si_typeZone",
                          choices = pred.choice[pred.choice != 4],
                          selected = 1
        )
      }
    }
    
  })
  
  # Données de comparaison et indicateurs du dashboard
  observeEvent(c(input$si_typeZone,input$si_zone,rv$zone.etude),{
    
    rv$zone.comparaison <- input$si_zone

    if(!is.null(rv$zone.etude) & !is.null(rv$zone.comparaison) & input$si_zone != ""){
      
      print(paste0("zone comparaison : ",rv$zone.comparaison))
      
      if(input$rg_typeZone == 1){
        
        if(input$si_typeZone == 4){
          
          if(!is.null(rv$df.zone.user)){
            df.dashboard <- rv$df.zone.user %>% filter(idZonage %in% c(rv$zone.etude,rv$zone.comparaison))
            pyr.dashboard <- rv$pyramide.user %>% filter(idZonage %in% c(rv$zone.etude,rv$zone.comparaison))
            
            rv$dash.indicateur <- stat.dashboard_agate(df = df.dashboard,zone.etude = rv$zone.etude,
                                                       zone.compare = rv$zone.comparaison, lstIndicateur = lstIndicateur,
                                                       pyramide_tr = pyr.dashboard)
          }
        }else{
          df <- read_fst("Data/Stats/Prefine aera/Real/fst/indicateur_stat.fst") %>%
            filter(zone.pred == input$si_typeZone & idZonage == rv$zone.comparaison & substr(source,4,5) == rv$source.an)
          df2 <- df %>%
            filter(nomVariable %in% c("emp_typeActivite","sco_popSco2","sco_diplome","log_cat","log_ach_constru",
                                      "log_bati","res_nbPiece","res_surface")) %>%
            mutate(type.indicateur = "part_p")
          df <- bind_rows(df,df2)
          
          pyr <- read_fst("Data/Stats/Prefine aera/Real/fst/pyramide.fst") %>%
            filter(zone.pred == input$si_typeZone & idZonage == rv$zone.comparaison & substr(source,4,5) == rv$source.an)
          
          # Temporaire
          df.dashboard <- bind_rows(rv$df.zone.etude,df)
          pyr.dashboard <- bind_rows(rv$pyramide.etude,pyr)
          
          rv$dash.indicateur <- stat.dashboard_agate(df = df.dashboard,zone.etude = rv$zone.etude,
                                                     zone.compare = rv$zone.comparaison, lstIndicateur = lstIndicateur,
                                                     pyramide_tr = pyr.dashboard)
        }
        
      }else{
        if(input$si_typeZone == 4){
          if(!is.null(rv$df.zone.user)){
            df.dashboard <- rv$df.zone.user %>% filter(idZonage %in% c(rv$zone.etude,rv$zone.comparaison))
            pyr.dashboard <- rv$pyramide.user %>% filter(idZonage %in% c(rv$zone.etude,rv$zone.comparaison))
            
            rv$dash.indicateur <- stat.dashboard_agate(df = df.dashboard,zone.etude = rv$zone.etude,
                                                       zone.compare = rv$zone.comparaison, lstIndicateur = lstIndicateur,
                                                       pyramide_tr = pyr.dashboard)
          }
          
        }else{
          df <- read_fst("Data/Stats/Prefine aera/Real/fst/indicateur_stat.fst") %>%
            filter(zone.pred == input$si_typeZone & idZonage == rv$zone.comparaison & substr(source,4,5) == rv$source.an)
          df2 <- df %>%
            filter(nomVariable %in% c("emp_typeActivite","sco_popSco2","sco_diplome","log_cat","log_ach_constru",
                                      "log_bati","res_nbPiece","res_surface")) %>%
            mutate(type.indicateur = "part_p")
          df <- bind_rows(df,df2)
          
          pyr <- read_fst("Data/Stats/Prefine aera/Real/fst/pyramide.fst") %>%
            filter(zone.pred == input$si_typeZone & idZonage == rv$zone.comparaison & substr(source,4,5) == rv$source.an)
          
          # Temporaire
          df.dashboard <- bind_rows(rv$df.zone.etude,df)
          pyr.dashboard <- bind_rows(rv$pyramide.etude,pyr)
          
          rv$dash.indicateur <- stat.dashboard_agate(df = df.dashboard,zone.etude = rv$zone.etude,
                                                     zone.compare = rv$zone.comparaison, lstIndicateur = lstIndicateur,
                                                     pyramide_tr = pyr.dashboard)
        }
      }
    } # end Test1
  })
  
  # Mise a jour du dashboard
  observeEvent(rv$dash.indicateur,{
    
    if(!is.null(rv$dash.indicateur) & !is.null(rv$zone.comparaison)){
      
      # Titre dashboard
      output$to_titleDash = renderText({
        rv$dash.indicateur$titreDash
      })
      
      output$to_source = renderText({
        paste0("Source : recensement de la population 20",rv$dash.indicateur$source.an,
               " niveau individu et logement - exploitation principale. \n Note : c = données confidentielles, intervalles de confiance 
             à 95 % calculés à partir d'une estimation de la variance du plan de sondage.")
      })
      
      # Theme Démographie
      #------------------
      output$ib_dem_feminite <- renderInfoBox({
        infoBox(title = "Part des femmes", value = rv$dash.indicateur$vb.dem.fem,
                icon = icon("female"),fill=TRUE)
      })
      
      output$ib_dem_population <- renderInfoBox({
        infoBox(title = "Population", value = rv$dash.indicateur$vb.dem.pop,
                icon = icon("users"),fill=TRUE)
      })
      
      output$ib_dem_superficie <- renderInfoBox({
        infoBox(title = "Superficie", value = rv$dash.indicateur$vb.dem.super,
                icon = icon("tree"),fill=TRUE)
      })
      
      output$dt_dem_hg = renderDT(
        datatable(rv$dash.indicateur$df.dem.tab.hg, colnames = rv$dash.indicateur$dash.label,
                  rownames = FALSE, options = list(dom = 't'))
      )
      
      output$dt_dem_hd = renderDT(
        datatable(rv$dash.indicateur$df.dem.tab.hd, colnames = rv$dash.indicateur$dash.label,
                  rownames = FALSE, options = list(dom = 't'))
      )
      
      output$g_dem_bg = renderPlotly({
        hide_legend(ggplotly(rv$dash.indicateur$g.dem.pyramide,tooltip = c("label","label2","label3","label4")))
      })
      
      output$dt_dem_bd = renderDT(
        datatable(rv$dash.indicateur$df.dem.tab.bd, colnames = rv$dash.indicateur$dash.label,
                  rownames = FALSE, options = list(dom = 't'))
      )
      
      # Theme Emploi
      #-------------
      output$ib_emp_pop_trav <- renderInfoBox({
        infoBox(title = "Population en âge de travailler", value = rv$dash.indicateur$vb.emp.popTrav,
                icon = icon("fa-user-cog"),fill=TRUE)
      })
      
      output$ib_emp_chomeur <- renderInfoBox({
        infoBox(title = "Taux de chômage", value = rv$dash.indicateur$vb.emp.chom,
                icon = icon("fa-people-carry"),fill=TRUE,subtitle = "selon le recensement de la population")
      })
      
      output$ib_emp_inactif <- renderInfoBox({
        infoBox(title = "Inactifs", value = rv$dash.indicateur$vb.emp.inact,
                icon = icon("user"),fill=TRUE)
      })
      
      output$dt_emp_hg = renderDT(
        datatable(rv$dash.indicateur$df.emp.tab.hg, colnames = rv$dash.indicateur$dash.label,
                  rownames = FALSE, options = list(dom = 't'))
      )
      
      output$dt_emp_hd = renderDT(
        datatable(rv$dash.indicateur$df.emp.tab.hd, colnames = rv$dash.indicateur$dash.label,
                  rownames = FALSE, options = list(dom = 't'))
      )
      
      output$g_emp_bg = renderPlotly({
        hide_legend(ggplotly(rv$dash.indicateur$g.emp.typeAct,tooltip = c("label","label2")))
      })
      
      output$dt_emp_bd = renderDT(
        datatable(rv$dash.indicateur$df.emp.tab.bd, colnames = rv$dash.indicateur$dash.label,
                  rownames = FALSE, options = list(dom = 't'))
      )
      
      # Theme Scolarisation
      #--------------------
      output$ib_sco_pop_sco <- renderInfoBox({
        infoBox(title = "Population en âge d'être scolarisée", value = rv$dash.indicateur$vb.sco.popSco,
                icon = icon("child"),fill=TRUE)
      })
      
      output$ib_sco_etud <- renderInfoBox({
        infoBox(title = "Jeunes inscrit dans un établissement scolaire", value = rv$dash.indicateur$vb.sco.etud,
                icon = icon("user-graduate"),fill=TRUE)
      })
      
      output$ib_sco_decrocheur <- renderInfoBox({
        infoBox(title = "Taux de décrocheur", value = rv$dash.indicateur$vb.sco.decrocheur,
                icon = icon("user-slash"),fill=TRUE)
      })
      
      output$dt_sco_hg = renderDT(
        datatable(rv$dash.indicateur$df.sco.tab.hg, colnames = rv$dash.indicateur$dash.label,
                  rownames = FALSE, options = list(dom = 't'))
      )
      
      output$g_sco_hd = renderPlotly({
        hide_legend(ggplotly(rv$dash.indicateur$g.sco.pop,tooltip = c("label","label2")))
      })
      
      output$g_sco_bg = renderPlotly({
        hide_legend(ggplotly(rv$dash.indicateur$g.sco.diplome,tooltip = c("label","label2")))
      })
      
      output$dt_sco_bd = renderDT(
        datatable(rv$dash.indicateur$df.sco.tab.bd, colnames = rv$dash.indicateur$dash.label,
                  rownames = FALSE, options = list(dom = 't'))
      )
      
      # Theme Logement
      #---------------
      output$ib_log_pop <- renderInfoBox({
        infoBox(title = "Nombre de logements", value = rv$dash.indicateur$vb.log.tot,
                icon = icon("child"),fill=TRUE)
      })
      
      output$ib_log_hlm <- renderInfoBox({
        infoBox(title = "HLM", value = rv$dash.indicateur$vb.log.hlm,
                icon = icon("user-graduate"),fill=TRUE)
      })
      
      output$ib_log_maison <- renderInfoBox({
        infoBox(title = "Maisons", value = rv$dash.indicateur$vb.log.maison,
                icon = icon("user-slash"),fill=TRUE)
      })
      
      output$dt_log_hg = renderDT(
        datatable(rv$dash.indicateur$df.log.tab.hg, colnames = rv$dash.indicateur$dash.label,
                  rownames = FALSE, options = list(dom = 't'))
      )
      
      output$g_log_hd = renderPlotly({
        hide_legend(ggplotly(rv$dash.indicateur$g.log.cat,tooltip = c("label","label2")))
      })
      
      output$g_log_bg = renderPlotly({
        hide_legend(ggplotly(rv$dash.indicateur$g.log.ach,tooltip = c("label","label2")))
      })
      
      output$g_log_bd = renderPlotly({
        hide_legend(ggplotly(rv$dash.indicateur$g.log.bati,tooltip = c("label","label2")))
      })
      
      # Theme Résidences principales
      #-----------------------------
      output$ib_res_pop <- renderInfoBox({
        infoBox(title = "Résidences principales", value = rv$dash.indicateur$vb.res.part,
                icon = icon("child"),fill=TRUE)
      })
      
      output$ib_res_collectif <- renderInfoBox({
        infoBox(title = "Logements collectifs", value = rv$dash.indicateur$vb.res.collectif,
                icon = icon("user-graduate"),fill=TRUE)
      })
      
      output$ib_res_todo <- renderInfoBox({
        infoBox(title = "TODO", value = "TODO",
                icon = icon("user-slash"),fill=TRUE)
      })
      
      output$dt_res_hg = renderDT(
        datatable(rv$dash.indicateur$df.res.tab.hg, colnames = rv$dash.indicateur$dash.label,
                  rownames = FALSE, options = list(dom = 't'))
      )
      
      output$g_res_hd = renderPlotly({
        hide_legend(ggplotly(rv$dash.indicateur$g.res.nbp,tooltip = c("label","label2")))
      })
      
      output$g_res_bg = renderPlotly({
        hide_legend(ggplotly(rv$dash.indicateur$g.res.surf,tooltip = c("label","label2")))
      })
      
      output$dt_res_bd = renderDT(
        datatable(rv$dash.indicateur$df.res.tab.bd, colnames = rv$dash.indicateur$dash.label,
                  rownames = FALSE, options = list(dom = 't'))
      )
    }
  })
  
  # V. Data explore
  #------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  # Mise à jour des catégories
  observeEvent(input$si_stat_domaine,{
    
    cat <- lstCategorie$categorie[lstCategorie$domaine == input$si_stat_domaine]
    names(cat) <- lstCategorie$labelCategorie[lstCategorie$domaine == input$si_stat_domaine]
    
    updateSelectInput(session, "si_stat_categorie",
                      choices = cat
    )
  })
  
  # MAJ des zones étudiées
  observeEvent(input$si_stat_zoneSelect,{
    
    if(input$si_stat_zoneSelect == 4){
      
      print(rv$df.zone.user)
      
      if(!is.null(rv$df.zone.user)){
        # Selection des données
        df <- rv$df.zone.user %>%
          select(idZonage,idZonage.name) %>%
          filter(!duplicated(idZonage))
        
        choice_zone <- df$idZonage
        names(choice_zone) <- df$idZonage.name

        updatePickerInput(session,"pi_stat_zone_etude",choices = choice_zone,selected = choice_zone)

      }else{
        df <- NULL
        updatePickerInput(session,"pi_stat_zone_etude",choices = "")
      }

      
    }else{
      df <- read_fst("Data/Stats/Prefine aera/Real/fst/indicateur_stat.fst") %>% 
        filter(zone.pred == input$si_stat_zoneSelect & substr(source,4,5) %in% rv$source.an) %>% 
        select(idZonage,idZonage.name) %>% 
        filter(!duplicated(idZonage)) %>% 
        as.data.frame()

      choice_zone <- df$idZonage
      names(choice_zone) <- df$idZonage.name

      updatePickerInput(session,"pi_stat_zone_etude",choices = choice_zone,selected = choice_zone)
    }
    
  })
  
  
  
  
  # Tableau de données dynamique
  observeEvent(c(input$si_stat_categorie,input$si_stat_domaine,input$pi_stat_zone_etude),{
    
    if(input$si_stat_categorie != ""){
      df <- NULL
      type.indicateur.filtre <- c("freq","part_np","CoefVariation","IntervalConf.","valeur.diffusable")

      if(input$si_stat_zoneSelect == 4){

        if(!is.null(rv$df.zone.user)){
          # Selection des données
          df <- rv$df.zone.user %>%
            filter(idZonage %in% input$pi_stat_zone_etude) %>%
            select(source,domaine,categorie,idZonage,idZonage.name,nomIndicateur,type.indicateur,value) %>%
            filter(type.indicateur %in% type.indicateur.filtre) %>%
            filter(domaine == input$si_stat_domaine & categorie == input$si_stat_categorie)
        }else{
          df <- NULL
        }

      }else{
        df <- read_fst("Data/Stats/Prefine aera/Real/fst/indicateur_stat.fst") %>%
          filter(zone.pred == input$si_stat_zoneSelect & domaine == input$si_stat_domaine &
                   categorie == input$si_stat_categorie & substr(source,4,5) %in% rv$source.an) %>%
          filter(type.indicateur != "part_np") %>%
          select(source,domaine,categorie,idZonage,idZonage.name,nomIndicateur,type.indicateur,value)
      }

      if(!is.null(df)){
        # selection des libelles de colonnes
        typInd <- lstTypeIndicateur$typeIndicateur
        names(typInd) <- lstTypeIndicateur$labelTypeIndicateur
        type.ind <- typInd[typInd %in% c("idZonage","idZonage.name",unique(df$type.indicateur))]
        print(type.ind)

        df <- df %>%
          filter(idZonage %in% input$pi_stat_zone_etude) %>%
          spread(key = type.indicateur, value = value) %>%
          left_join(lstIndicateur %>% select(nomIndicateur,labelIndicateur),"nomIndicateur") %>%
          mutate(nomIndicateur = labelIndicateur) %>%
          select(-labelIndicateur,-domaine,-categorie)

        # Affichage du titre de la
        output$to_stat_title <- renderText({lstCategorie$titreTab[lstCategorie$domaine == input$si_domaine &
                                                                    lstCategorie$categorie == input$si_categorie]})
        # Affichage
        output$dt_stat_explore = renderDT(
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

      }

    } # end if
  })
  
  
  # VI. Documentation
  #-----------------------------------------------------------------------------------------------------------------------------------
  
  # Mise à jour des catégories
  observeEvent(input$si_ind_domaine,{
    cat <- lstCategorie$categorie[lstCategorie$domaine == input$si_ind_domaine]
    names(cat) <- lstCategorie$labelCategorie[lstCategorie$domaine == input$si_ind_domaine]
    
    updateSelectInput(session, "si_ind_categorie",
                      choices = c("Tous",cat),selected = "Tous"
    )
  })
  
  observeEvent(c(input$si_ind_categorie,input$si_ind_domaine),{
    
    indicateur_description <- lstCategorie %>% 
      filter(substr(source,1,2) %in% c("rp","ig")) %>% 
      left_join(lstDomaine,by = c("domaine")) %>% 
      left_join(lstIndicateur %>% select(nomVariable,labelIndicateur),by = c("nomVariable"))
    

    if(input$si_ind_categorie == "Tous"){
      indicateur_description <- indicateur_description %>% 
        filter(domaine %in% input$si_ind_domaine) %>% 
        select(labelDomaine,labelCategorie,labelIndicateur,champLabel,source) 
      
    }else{
      indicateur_description <- indicateur_description %>% 
        filter(domaine %in% input$si_ind_domaine & categorie %in% input$si_ind_categorie) %>% 
        select(labelDomaine,labelCategorie,labelIndicateur,champLabel,source) 
    }

    output$dt_indicateurs <- DT::renderDataTable({
      
      DT::datatable(indicateur_description,colnames = c("Thématique","Indicateur","Modalité","Champ","Source"),
                    extensions = 'Buttons',
                    options = list(
                      scrollX = TRUE,
                      # fixedColumns = TRUE,
                      # autoWidth = TRUE,
                      ordering = FALSE,
                      dom = 'lBfrtip',
                      buttons = c(I('colvis'),'excel')),
                    rownames= FALSE)
    })
  })
  
  # VI. Aide Agate
  #-----------------------------------------------------------------------------------------------------------------------------------
  observeEvent(input$ab_aide,{
    
    introjs(session, options = 
              list(
                steps = data.frame(
                  data.frame(
                    element = c(NA,
                                "#llo_agateMap",
                                "#ap_controls",
                                "#rg_typeZone",
                                "#fi_userMap",
                                "#si_userMap_id",
                                "#si_userMap_name",
                                "#swi_heatPoint",
                                "#si_rp",
                                "#ab_userStat",
                                "#pi_userMapSelect",
                                "#pt_hideMenu",
                                "a[data-value=\"Carte\"]",
                                "a[data-value=\"Statistiques\"]",
                                "#dt_stat_explore",
                                "#si_stat_zoneSelect"
                    ),
                    intro = c(
                      # 0
                      "Bienvenue dans Agate : application en Guyane et aux Antilles de statistiques infracommunales. Elle permet de calculer des indicateurs statistiques issus du recensement de la population 
                    dans n'importe quelle zone. Elle affiche également ces données sous forme d'un tableau bord interactif.",
                      # 1
                      "Fond de carte Agate. Pour zoomer, utiliser les boutons '+' et '-' ou la roulette de la souris. Les cartes chargées par l'utilisateur apparaîssent en vert.
                    Un click sur une carte chargée permet d'afficher un tableau de bord.",
                      # 2
                      "Menu de gestion des cartes et des indicateurs statistiques",
                      # 3
                      "Selectionner 'Utilisateur' permet d'importer manuellement une carte. Selectionner 'Prédéfini' permet de visualiser les données associées à 
                    des zonages administratifs comme les départements, les communes et les quartiers prioritaires de la politique de la ville",
                      # 4
                      "Les cartes à importer doivent être au format Shapefile. Il faut selectionner les quatre fichiers associés (*.shp, *.shx, *.dbf, *.prj).",
                      # 5
                      "Pour fonctionner, Agate doit identifier l'identifiant et le nom de chaque zone. Selectionner cette variable dans la liste.",
                      # 6
                      "Selectionner la variable correspondant au nom de la zone.",
                      # 7
                      "Le bouton 'Chaleur' affiche dynamiquement la quantité de données disponibles dans les zones. Plus la chaleur tend vers le rouge plus les données 
                    sont nombreuses. A l'inverse, plus la couleur tend vers le bleu ou le transparent, moins il y a de données.",
                      # 8
                      "Selection du millesime du recensement de la population utilisé pour le calcul des indicateurs statistiques",
                      # 9
                      "Selection des zones où seront calculés les indicateurs statistiques pour éviter des temps de calcul trop long",
                      # 10
                      "Calcul des indicateurs statistiques issus du RP dans les zones sélectionnées par l'utilisateur.",
                      # 11
                      "Affiche ou réduire le menu.",
                      # 12
                      "Page carte",
                      # 13
                      "Page Statistiques. Permet d'explorer les statistiques issus d'Agate. Il est possible d'exporter les tableaux au format tableur 
                    en cliquant sur le bouton 'Excel'.",
                      # 14
                      "Tableau dynamique pour explorer les indicateurs statistiques.",
                      # 15
                      "Selection du type de zone : département, commune, QPV et zonage importé par l'utilisateur."
                    )
                  )
                )),
            events = list(
              "onchange" = I(
                "
                if (this._currentStep == 0) {
                $('a[data-value=\"Statistiques\"]').removeClass('active');
                $('a[data-value=\"Carte\"]').addClass('active');
                $('a[data-value=\"Carte\"]').trigger('click');
                }
                
                if (this._currentStep == 12) {
                $('a[data-value=\"Statistiques\"]').removeClass('active');
                $('a[data-value=\"Carte\"]').addClass('active');
                $('a[data-value=\"Carte\"]').trigger('click');
                }

                  if (this._currentStep == 13) { 
                  $('a[data-value=\"Carte\"]').removeClass('active');
                  $('a[data-value=\"Statistiques\"]').addClass('active');
                  $('a[data-value=\"Statistiques\"]').trigger('click');
                  }
                  ")
            )
            
    )
    
  })
  
  
  
  
} # End server



# Application
shinyApp(ui = ui, server = server)
