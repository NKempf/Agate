#---------------------------------------------------------------------------------------------------------------------------#
#                                             Agate - UI refonte                                                            #
#---------------------------------------------------------------------------------------------------------------------------#

# 03.04.2019

library(sp)
library(leaflet)
library(leaflet.extras)
library(tidyverse)
library(fst)
library(fstplyr)
library(shiny) # Graphic web interface
library(shinyBS) # Pop-up windows
library(shinyjs) # Mask buttons/elements on graphic interface
library(shinyWidgets) # Widgets supplementaires
library(shinydashboard) # Tools like infoBox
library(plotly)



# Label et choix
load("data/Liste indicateurs statistiques/lstIndicateur.RData")
# heatPoints
load("Data/Maps/HeatPoint/heatpoints.Rdata") 
# load("Data/Maps/Zones predefinies/zp_departements.RData")
# load("Data/Maps/Zones predefinies/zp_communes.RData")
# load("Data/Maps/Zones predefinies/zp_qpv.RData")

# NavBar Spéciale :)
navbarPageWithInputs <- function(..., inputs) {
  navbar <- navbarPage(...)
  form <- tags$form(class = "navbar-form", inputs)
  navbar[[3]][[1]]$children[[1]] <- htmltools::tagAppendChild(
    navbar[[3]][[1]]$children[[1]], form)
  navbar
}


ui <- tagList(
  useShinyjs(),
  navbarPage(
    "Agate",

    # I. Carte
    #-------------------------------------------------------------------------------------------------------------------------------------------------
    tabPanel(
      "Carte",
      div(class="outer",
          tags$head(includeCSS("www/agate.css")),
          # I.1.1. Leaflet map
          #-----------------
          leafletOutput("llo_agateMap", width = "100%", height = "100%"),
          
          # I.1.2. Statistical controls
          #----------------------------
          absolutePanel(
            draggable = TRUE,top = 20, right = 10,width = 200,#height = 500,
            useSweetAlert(),
            radioGroupButtons(
              inputId = "rg_typeZone",
              choices = c("Utilisateur",
                          "Prédéfini"),
              justified = TRUE
            ),
            # Utilisateur
            fluidRow(
              column(5,
                     dropdownButton(
                       inputId = "ddb_import",label = "Importer",size = "sm",circle = FALSE,right = TRUE,
                       tooltip = tooltipOptions(title = "Importer une carte"),
                       fileInput('fi_userMap', 'Importer un shapeFile',multiple = T),
                       selectInput(inputId = "si_userMap_id", label = "Identifiant", 
                                   choices = c("Choice" =""),selected = c("")),
                       selectInput(inputId = "si_userMap_idName", label = "Libellé", 
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
              column(7,
                     dropdownButton(inputId = "ddb_userMapStat",label = "Statistiques",size = "sm",circle = FALSE,right = TRUE,
                                    tooltip = tooltipOptions(title = "Calcul des indicateurs statistiques"),
                                    selectInput(inputId = "si_rp", label = "Recensement de la population", 
                                                choices = c("2015" = "15","2014" = "14","2013" = "13"),
                                                selected = "15" ),
                                    pickerInput(inputId = "pi_userMapSelect",
                                                label = "Selection des zones à calculer", 
                                                choices = c("Choice" =""),
                                                options = list(
                                                  `actions-box` = TRUE), 
                                                multiple = TRUE)
                     )
              )
            ),
            # Zones prédéfinies
            shinyjs::hidden(
              selectInput("si_zonePred", "Selectionner une zone",
                          choices = pred.choice,
                          selected = c(4))
            ),
            style = "opacity: 0.75; z-index: 1000;" # IMPORTANT : Absolute panel not hidden by tiles
          )
          
      )
    
    
    # II. Statistiques et téléchargement
    #-------------------------------------------------------------------------------------------------------------------------------------------------
    
    # III. Documentation
    #-------------------------------------------------------------------------------------------------------------------------------------------------
    
    # IV. Dashboard
    #-------------------------------------------------------------------------------------------------------------------------------------------------
    ,
    tags$head(tags$style("#bs_dashboard .modal-footer{ display:none}
                         #bs_dashboard .modal-header{ display:none}")), # Remove BS modal footer
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
                                          ),
                                          fluidRow(
                                            box(id="b_test",title = "Individus", solidHeader = TRUE,
                                                collapsible = TRUE,
                                                DT::dataTableOutput("dt_dem_hg")
                                            ),
                                            box(title = "Ménages", solidHeader = TRUE,
                                                collapsible = TRUE,
                                                DT::dataTableOutput("dt_dem_hd")
                                            )
                                          ),
                                          fluidRow(
                                            box(title = "Pyramide des âges", solidHeader = TRUE,
                                                collapsible = TRUE,
                                                plotlyOutput("g_dem_bg")
                                            ),
                                            box(title = "Immigration", solidHeader = TRUE,
                                                collapsible = TRUE,
                                                DT::dataTableOutput("dt_dem_bd")
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
                                                DT::dataTableOutput("dt_emp_hg")
                                            ),
                                            box(title = "Chômage", solidHeader = TRUE,
                                                collapsible = TRUE,
                                                DT::dataTableOutput("dt_emp_hd")
                                            )
                                          ),
                                          fluidRow(
                                            box(title = "Type d'activité", solidHeader = TRUE,
                                                collapsible = TRUE,
                                                plotlyOutput("g_emp_bg")
                                            ),
                                            box(title = "Travail", solidHeader = TRUE,
                                                collapsible = TRUE,
                                                DT::dataTableOutput("dt_emp_bd")
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
                                                DT::dataTableOutput("dt_sco_hg")
                                            ),
                                            box(title = "Jeunes non scolarisés", solidHeader = TRUE,
                                                collapsible = TRUE,
                                                DT::dataTableOutput("dt_sco_bd")
                                            )
                                          ),
                                          fluidRow(
                                            box(title = "Niveau de diplôme", solidHeader = TRUE,
                                                collapsible = TRUE,
                                                plotlyOutput("g_sco_bg")
                                            ),
                                            
                                            box(title = "Jeunes", solidHeader = TRUE,
                                                collapsible = TRUE,
                                                plotlyOutput("g_sco_hd")
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
                                                DT::dataTableOutput("dt_log_hg")
                                            ),
                                            box(title = "Catégorie de logement", solidHeader = TRUE,
                                                collapsible = TRUE,
                                                plotlyOutput("g_log_hd")
                                            )
                                          ),
                                          fluidRow(
                                            box(title = "Année d'achevement", solidHeader = TRUE,
                                                collapsible = TRUE,
                                                plotlyOutput("g_log_bg")
                                            ),
                                            box(title = "Aspect du bâti", solidHeader = TRUE,
                                                collapsible = TRUE,
                                                plotlyOutput("g_log_bd")
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
                                                DT::dataTableOutput("dt_res_hg")
                                            ),
                                            box(title = "Équipements", solidHeader = TRUE,
                                                collapsible = TRUE,
                                                DT::dataTableOutput("dt_res_bd")
                                            )
                                          ),
                                          fluidRow(
                                            box(title = "Surface", solidHeader = TRUE,
                                                collapsible = TRUE,
                                                plotlyOutput("g_res_bg")
                                            ),
                                            box(title = "Nombre de pièces", solidHeader = TRUE,
                                                collapsible = TRUE,
                                                plotlyOutput("g_res_hd")
                                            )
                                          )
                                 ),
                                 
                                 # VI. Zones de comparaisons
                                 #-----------------------------------------------------------------------------------------------------
                                 inputs = dropdownButton(inputId = "ddb_compare" ,
                                                         selectInput("si_typeZone", "Type de zone",
                                                                     choices = pred.choice,
                                                                     selected = c(4)),
                                                         # II.2.2. Zone
                                                         selectInput("si_zone", "Zone",
                                                                     choices = c("Choice" =""),
                                                                     selected = c("")),
                                                         circle = TRUE, status = "danger",
                                                         icon = icon("gear"),size = "sm",right = TRUE,
                                                         tooltip = tooltipOptions(title = "Zones de comparaison")
                                 )
            )
    ) # end Dashboard
    
    )# Tabset Carte
    
    
  )
)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

server = function(input, output, session) {
  
  load("Data/Tmp/qpv_stat_tmp.RData")
  
  # Reactive Values
  #----------------------------------------------------------------------------------------------------------------------------
  rv <- reactiveValues(AgateMap = NULL,
                       userMap = NULL,
                       zone.active = NULL,
                       zone.etude = NULL,
                       zone.compare = NULL,
                       df.zone.etude=NULL,
                       pyramide.etude = NULL,
                       dash.indicateur = NULL,
                       source.an = "13",
                       zonage.com = zonage.com)
  
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
      if(!is.null(rv$AgateMap@data$idZonage) & !is.null(rv$AgateMap@data$idZonage.name)){

        rv$AgateMap <- spTransform(rv$AgateMap, "+init=epsg:4326")
        # Boundary box
        AgateMap.bbox <- as.data.frame(bbox(rv$AgateMap))
        
        # Update leaflet
        leafletProxy("llo_agateMap") %>%
          clearShapes() %>% 
          fitBounds(lng1 = AgateMap.bbox$min[1],lat1 = AgateMap.bbox$max[2],lng2 = AgateMap.bbox$max[1],lat2 = AgateMap.bbox$min[2]) %>%
          addPolygons(data=rv$AgateMap,opacity = 3,
                      color = "green", stroke = TRUE, weight = 2,
                      fill = TRUE, fillOpacity = 0.2,popup = ~paste(idZonage.name),layerId = ~paste(idZonage))
      }
    }
  })
  
  # II. Navigation
  #----------------------------------------------------------------------------------------------------------------------------
  
  # Zone utilisateur ou predefinie
  observeEvent(input$rg_typeZone,{
    if(input$rg_typeZone == "Utilisateur"){
      shinyjs::hide("si_zonePred")
      shinyjs::hide("ab_modal")
      shinyjs::show("ddb_import")
      shinyjs::show("ddb_userMapStat")
    }else{
      shinyjs::show("si_zonePred")
      shinyjs::show("ab_modal")
      shinyjs::hide("ddb_import")
      shinyjs::hide("ddb_userMapStat")
    }
  })
  
  # Selection de la zone a afficher
  observeEvent(input$si_zonePred,{
    
    switch(as.numeric(input$si_zonePred),
           {
             if(!exists("dep.dom")){
               print("argh !")
               load("Data/Maps/Zones predefinies/zp_departements.RData")
             }
             rv$AgateMap <- dep.dom},
           {             
             if(!exists("com.dom")){
               print("orf !")
               load("Data/Maps/Zones predefinies/zp_communes.RData")
             }
             rv$AgateMap <- com.dom
           },
           {
             if(!exists("qpv")){
               print("Humpf !")
               load("Data/Maps/Zones predefinies/zp_qpv.RData")
             }
             rv$AgateMap <- qpv},
           {print("TODO")})
  })
  
  
  # Ouverture du dashboard
  observeEvent(input$llo_agateMap_shape_click,{
    rv$zone.etude <- input$llo_agateMap_shape_click$id

    toggleModal(session, "bs_dashboard", toggle = "toggle")
  })
  
  # Mise a jour des listes de comparaison dashboard
  observeEvent(c(input$si_typeZone),{
    if(input$si_typeZone != 4){
      test <- lstZonePreType$zone.pred == input$si_typeZone & lstZonePreType$dep == unique(rv$zonage.com$dep) & 
        lstZonePreType$idZonage != rv$zone.etude
      cat <- lstZonePreType$idZonage[test]
      names(cat) <- lstZonePreType$idZonage.name[test] 
      print(cat)
    }else{
      test <- !rv$df.zone$idZonage %in% rv$zone.etude
      cat <- unique(rv$df.zone$idZonage[test])
      names(cat) <- unique(rv$df.zone$idZonage.name[test]) 
    }
    updateSelectInput(session, "si_zone",
                      choices = cat
    )
  })
  
  
  

  # III. Dashboard
  #--------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  # Selection des données d'études
  observeEvent(c(input$rg_typeZone,input$si_zonePred),{
    
    # if(input$rg_typeZone == "Utilisateur"){
    #   #TODO
    #   print("TODO")
    # }else{
    #   df <- read_fst("Data/Stats/Prefine aera/Real/fst/indicateur_stat.fst") %>% 
    #     filter(zone.pred == input$si_zonePred & idZonage == rv$zone.etude & substr(source,4,5) == rv$source.an) 
    #   df2 <- df %>% 
    #     filter(nomVariable %in% c("emp_typeActivite","sco_popSco2","sco_diplome","log_cat","log_ach_constru",
    #                               "log_bati","res_nbPiece","res_surface")) %>% 
    #     mutate(type.indicateur = "part_p")
    #   rv$df.zone.etude <- bind_rows(df,df2)
    #   
    #   print(rv$df.zone.etude)
    # }
  })
  
  # Selection des données de comparaison
  
  
  
  observeEvent(c(input$si_typeZone,input$si_zone),{
    
    # if(input$si_typeZone != 4){
    #   df <- read_fst("Data/Stats/Prefine aera/Real/fst/indicateur_stat.fst") %>% 
    #     filter(zone.pred == input$si_typeZone & idZonage == input$si_zone & substr(source,4,5) == rv$source.an) 
    #   
    #   df2 <- df %>% 
    #     filter(nomVariable %in% c("emp_typeActivite","sco_popSco2","sco_diplome","log_cat","log_ach_constru",
    #                               "log_bati","res_nbPiece","res_surface")) %>% 
    #     mutate(type.indicateur = "part_p")
    #   df <- bind_rows(df,df2,rv$df.zone %>% filter(idZonage == rv$zone.etude))
    #   
    #   pyr <- read_fst("Data/Stats/Prefine aera/Real/fst/pyramide.fst") %>% 
    #     filter(zone.pred == input$si_typeZone & idZonage == input$si_zone & substr(source,4,5) == rv$source.an) %>% 
    #     bind_rows(rv$pyramide %>% filter(idZonage == rv$zone.etude))
    # }else{
    #   df <- rv$df.zone %>% filter(idZonage %in% c(rv$zone.etude,input$si_zone))
    #   pyr <- rv$pyramide %>% filter(idZonage %in% c(rv$zone.etude,input$si_zone))
    # }
    # 
    # rv$dash.indicateur <- stat.dashboard_agate(df = df,zone.etude = rv$zone.etude,
    #                                            zone.compare = input$si_zone, lstIndicateur = lstIndicateur,
    #                                            pyramide_tr = pyr)
  })
  
  # Mise a jour du dashboard
  

  
  
  
  
  
  
  
  
  
  
} # End server



# Application
shinyApp(ui = ui, server = server)
