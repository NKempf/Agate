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
                        draggable = TRUE,top = 20, right = 10,width = 200,#height = 500,
                        
                        useSweetAlert(),
                        radioGroupButtons(
                          inputId = "rg_typeZone",
                          choices = c("Utilisateur" = 1,
                                      "Prédéfini" = 2),
                          justified = TRUE
                        ),
                        # Utilisateur
                        fluidRow(
                          column(5,
                                 dropdown(
                                   inputId = "ddb_import",label = "Importer",size = "sm",circle = FALSE,right = TRUE,
                                   tooltip = tooltipOptions(title = "Importer une carte",placement = "left"),
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
                                 ) # end dropdown
                          ),
                          column(7,
                                 dropdown(inputId = "ddb_userMapStat",label = "Statistiques",size = "sm",circle = FALSE,right = TRUE,
                                          tooltip = tooltipOptions(title = "Calcul des indicateurs statistiques",placement = "left"),
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
                                 )
                          )
                        ),
                        # Zones prédéfinies
                        shinyjs::hidden(
                          selectInput("si_zonePred", "Selectionner une zone",
                                      choices = pred.choice,
                                      selected = c(4))
                        )

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
             # value="vis",
             
             # fluidRow(
             #   column(4,
             #          # II.1. Zone selection
             #          #---------------------
             #          selectInput("si_stat_zoneSelect", "Zone",
             #                      choices = pred.choice,
             #                      selected = 4)
             #   ),
             #   column(4,
             #          
             #          # II.2. Domaine selection
             #          #------------------------
             #          selectInput("si_stat_domaine", "Domaine",
             #                      choices = c("Choice" ="",dmn),
             #                      selected = dmn[2])
             #   ),
             #   column(4,
             #          # II.3. Categorie selection
             #          #--------------------------
             #          selectInput("si_stat_categorie", "Catégorie",
             #                      choices = c("Choice" =""),
             #                      selected = c(""))
             #   )
             # ),
             # 
             # hr(), # Line between buttons and plot
             
             # II.4. Table visualisation
             #--------------------------
             textOutput("to_stat_title"),
             tags$head(tags$style("#to_stat_title{font-size: 30px;font-style: bold;}")),
               DT::dataTableOutput("dt_stat_explore")
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
  
  # Aide Agate
  #-----------------------------------------------------------------------------------------------------------------------------------
  # start introjs when button is pressed with custom options and events
  
  # steps <- reactive(
  #   data.frame(element = c("#ap_controls",
  #                          "#rg_typeZone",
  #                          "#ddb_import",
  #                          "#fi_userMap"),
  #              intro = c("Menu de gestion des cartes et des statistiques",
  #                        "Selectionner 'Utilisateur' permet d'importer manuellement une carte. 
  #                                        Selectionner 'Prédéfini' permet de visualiser les données associées à des zonages administratifs comme les départements, 
  #                                        les communes et les quartiers prioritaires de la politique de la ville",
  #                        "Menu spécifique à l'import de carte. Ne fonctionne que le bouton 'Utilisateur' est selectionné.",
  #                        "Gère l'import de cartes au format Shapefile. Il faut selectionner les quatre fichiers associés au format (*.shp, *.shx, *.dbf, *.prj)"
  #              )
  #   )
  # )

  
  observeEvent(input$ab_aide,{
    
    introjs(session, options = 
              list(
      steps = data.frame(
        data.frame(
          element = c("#llo_agateMap",
                      "#ap_controls",
                      "#rg_typeZone",
                      "#ddb_import",
                      "#ddb_userMapStat",
                      "a[data-value=\"Carte\"]",
                      "a[data-value=\"Statistiques\"]",
                      "#dt_stat_explore"
          ),
          intro = c("Fond de carte Agate. Pour zoomer, utiliser les boutons '+' et '-' ou la roulette de la souris. Les cartes chargées par l'utilisateur apparaîssent en vert.
                    Un click sur une carte chargée permet d'afficher un tableau de bord contenant une série d'indicateurs issus du recensement de la population.",
                    "Menu de gestion des cartes et des indicateurs statistiques",
                    "Selectionner 'Utilisateur' permet d'importer manuellement une carte. 
                    Selectionner 'Prédéfini' permet de visualiser les données associées à des zonages administratifs comme les départements, 
                                         les communes et les quartiers prioritaires de la politique de la ville",
                    "Menu spécifique à l'import de carte (Ne fonctionne que le bouton 'Utilisateur' est selectionné). Les cartes à importer doivent être au format Shapefile. 
                                              Il faut selectionner les quatre fichiers associés (*.shp, *.shx, *.dbf, *.prj). Une fois la carte chargée, Agate doit identifier les variables 
                                              identifiant et nom de chaque zone. Pour cela, il faut utiliser les listes 'Identifiant' et 'Libellé'. Enfin, le bouton 'Chaleur' permet de visualiser 
                                              dynamiquement la quantité de données disponibles dans les zones. Plus la chaleur tend vers le rouge plus les données sont nombreuses. A l'inverse, plus 
                                              la couleur tend vers le bleu ou le transparent, moins il y a de données.",
                    "Paramètre de calcul des indicateurs statistiques (Ne fonctionne que le bouton 'Utilisateur' est selectionné). Une fois la carte importée, il est possible de
                                              calculer une série d'indicateurs statistiques. La liste 'Recensement de la population' permet de sélectionner le millesime.
                                              L'utilisateur peut choisir de ne calculer les indicateurs que sur certaines zones en utilisant la liste 'Selection des zones à calculer'
                                              afin de réduire les temps de calcul. Enfin, le bouton 'Indicateurs statistiques' lance le calcul (durée minimal environ 45 sec). Plus
                                              le nombre de zones est important, plus les temps de calculs s'allongent. ",
                    "Page carte",
                    "Page Statistiques. Permet d'explorer les statistiques issus d'Agate. Il est possible d'exporter les tableaux au format
                                              tableur en cliquant sur le bouton 'Excel'.",
                    "Tableau dynamique pour explorer les indicateurs statistiques."
          )
        )
      )),
      events = list(
                  "onchange" = I(
                  "
                if (this._currentStep == 5) {
                $('a[data-value=\"Statistiques\"]').removeClass('active');
                $('a[data-value=\"Carte\"]').addClass('active');
                $('a[data-value=\"Carte\"]').trigger('click');
                }

                  if (this._currentStep == 6) { 
                  $('a[data-value=\"Carte\"]').removeClass('active');
                  $('a[data-value=\"Statistiques\"]').addClass('active');
                  $('a[data-value=\"Statistiques\"]').trigger('click');
                  }
                  ")
                  )
    
    )
    
    
    
    #                   var steps = {0,1,2,3,4,5}
    # if (this._currentStep in 6) {
    #   $('a[data-value=\"Statistiques\"]').removeClass('active');
    #   $('a[data-value=\"Carte\"]').addClass('active');
    #   $('a[data-value=\"Carte\"]').trigger('click');
    # }
    # 
    # stepEquals <- function(i) paste0("this._currentStep==", i-1, collapse=" || ") # helper
    # introjs(session,
    #         ### separate issue: turning off bullets as a way to cheat this problem
    #         ### appears to have no effect anyway. BUllets always display.
    #         options = list(steps = steps(), "showBullets"="false"),
    #         ### Make sure correct tab box tab panel active
    #         ### Normally only need for step 0 (if tour launched while on tab panel 2)
    #         ### But now need for step 0 and 2 in case user changes to tab 2 on tour...
    #         ### Actually need for all steps because user can go to tab 2 on step 0 or 2,
    #         ### and from there change to any step using by clicking bullets.
    #         ### But accessing the bullets is where the below callback breaks down:
    #         ### E.g., on step 0 or 2, click tab panel 2 to change tabs,
    #         ### then use bullets in tooltip to jump to later step.
    #         events=list(
    #           "onchange" = I(("if (this._currentStep== 3) {
    #           
    #           alert('test');
    #           //$('#testNk').removeClass('active');
    #           $('#ddb_import').addClass('active');
    #           $('#ddb_import').trigger('click');
    #           alert('test');
    #           //$('a[data-value=\"Tab box tab 2\"]').removeClass('active');
    #           //$('a[data-value=\"Tab box tab 1\"]').addClass('active');
    #           //$('a[data-value=\"Tab box tab 1\"]').trigger('click');
    #           }")))
    # )
  })
    

}# end server



# Application
shinyApp(ui = ui, server = server)

