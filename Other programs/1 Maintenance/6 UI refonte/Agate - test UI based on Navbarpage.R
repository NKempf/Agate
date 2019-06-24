# Agate - test de la librairie Rintrojs

# https://stackoverflow.com/questions/39545002/multi-page-intro-js-with-shiny

# Tests avec la syntaxe basée sur les balises c'est-a-dire les identifiants des objets

library(shiny)
library(shinyjs)
library(shinyWidgets)
library(leaflet)
library(rintrojs)

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
    
    tabPanel(title = "Carte",id="carte",
             div(class="outer",id="testNk",
                 tags$head(includeCSS("www/agate.css")),
                 
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
                               
                               fluidRow(id = "fr_predOption",
                                        column(12,
                                               selectInput("si_zonePred_dep", "Selectionner une zone",
                                                           choices = c("Guadeloupe" = "971","Martinique" = "972","Guyane" = "973","Saint-Martin et Saint-Barthélémy" = "978"),
                                                           selected = "Guadeloupe"
                                               ),  
                                               selectInput("si_zonePred", "Selectionner une zone",
                                                           choices = c(""),
                                                           selected = "")
                                        )
                               ),
                               
                               prettyToggle(
                                 inputId = "pt_hideMenu",
                                 label_on = "Réduire", 
                                 label_off = "Afficher",
                                 outline = TRUE,
                                 plain = TRUE,value = TRUE,status_on = "success",status_off = "success",
                                 icon_on = icon("arrow-up"), 
                                 icon_off = icon("arrow-down")
                               )
                               
                               
                               # switchInput(
                               #   inputId = "si_hideMenu",
                               #   label = "<i class=\"fas fa-arrows-alt-v\"></i>",onLabel = "Réduire",offLabel = "Afficher",value = TRUE,
                               #   onStatus = "success",offStatus = "success"
                               # )
                 ),
                 # 
                 # I.1.1. Leaflet map
                 #-----------------
                 leafletOutput("llo_agateMap", width = "100%", height = "100%")
                 
                 
             )# end div
    ),# Tabset Carte
    # II. Statistiques et téléchargement
    #-------------------------------------------------------------------------------------------------------------------------------------------------
    tabPanel(title = "Statistiques",id="statistiques",
             
             sidebarPanel(width=4,
                          selectInput("si_stat_zoneSelect", "Zone",
                                      choices = "",
                                      selected = ""),
                          selectInput("si_stat_domaine", "Domaine",
                                      choices = c("Choice" =""),
                                      selected = ""),
                          selectInput("si_stat_categorie", "Catégorie",
                                      choices = c("Choice" =""),
                                      selected = c("")),
                          pickerInput(inputId = "pi_zoneSelect",
                                      label = "Selection des zones à calculer", 
                                      choices = c("Choice" =""),
                                      options = list(
                                        `actions-box` = TRUE), 
                                      multiple = TRUE)
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



server = function(input, output, session) {
  
  output$llo_agateMap <- renderLeaflet({
    leaflet("agateMap") %>% addTiles()%>% 
      fitBounds(lng1 = -65,lat1 = 18,lng2 = -45,lat2 = 3) 
  })
  
  
  # Menu - Navigation Agate
  #----------------------------------------------------------------------------------------------------------------------------------
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
  
  
}# end server



# Application
shinyApp(ui = ui, server = server)