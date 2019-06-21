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
    
      # tabPanel(
      #   "Carte",
      #   div(class="outer",
      #       tags$head(includeCSS("www/agate.css")),
      #       # I.1.1. Leaflet map
      #       #-----------------
      #       leafletOutput("llo_agateMap", width = "100%", height = "100%"),
      #       
      #       # I.1.2. Statistical controls
      #       #----------------------------
      #       
      #         absolutePanel(id = "ap_controls",
      #           draggable = TRUE,top = 20, right = 10,width = 200,#height = 500,
      #           useSweetAlert(),
      #           
      #             radioGroupButtons(
      #               inputId = "rg_typeZone",
      #               choices = c("Utilisateur" = 1,
      #                           "Prédéfini" = 2),
      #               justified = TRUE
      #             )
      #         )
      #   )# end div
      # ),# Tabset Carte
    
    tabPanel(
      "Carte",
      div(class="outer",id="testNk",
          tags$head(includeCSS("www/agate.css")),
          
          # I.1.2. Statistical controls
          #----------------------------

          absolutePanel(id = "ap_controls",
                        draggable = FALSE,top = 20, right = 10,width = 200,#height = 500,
                        
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
    tabPanel("Statistiques",value="vis",
             
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
             introBox(
               DT::dataTableOutput("dt_stat_explore"),
               data.step = 2,
               data.intro = "Test"
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
  
  # Aide Agate
  #-----------------------------------------------------------------------------------------------------------------------------------
  # start introjs when button is pressed with custom options and events
  
  steps <- reactive(
    data.frame(element = c("#ap_controls",
                           "#rg_typeZone",
                           "#ddb_import",
                           "#fi_userMap"),
               intro = c("Menu de gestion des cartes et des statistiques",
                         "Selectionner 'Utilisateur' permet d'importer manuellement une carte. 
                                         Selectionner 'Prédéfini' permet de visualiser les données associées à des zonages administratifs comme les départements, 
                                         les communes et les quartiers prioritaires de la politique de la ville",
                         "Menu spécifique à l'import de carte. Ne fonctionne que le bouton 'Utilisateur' est selectionné.",
                         "Gère l'import de cartes au format Shapefile. Il faut selectionner les quatre fichiers associés au format (*.shp, *.shx, *.dbf, *.prj)"
               )
    )
  )
  
#   observeEvent(input$ab_aide,
#                introjs(
#                  session,
#                  events = list(
#                    "onchange" = I(
#                    "if (this._currentStep==0) {
#                                   $('a[data-value=\"Statistiques\"]').removeClass('active');
#                                   $('a[data-value=\"Carte\"]').addClass('active');
#                                   $('a[data-value=\"Carte\"]').trigger('click');
# }
# if (this._currentStep==1) {
# $('a[data-value=\"Carte\"]').removeClass('active');
# $('a[data-value=\"Statistiques\"]').addClass('active');
# $('a[data-value=\"Statistiques\"]').trigger('click');
# }"
#                      "if (this._currentStep==3) {
#                      $('#ddb_import').addClass('active');
#                      $('#ddb_import').trigger('click');
#                      }
#                      "
#                      
#                      
#                    ))
#                )
#   )
  
  observeEvent(input$ab_aide,{
    
    stepEquals <- function(i) paste0("this._currentStep==", i-1, collapse=" || ") # helper
    introjs(session,
            ### separate issue: turning off bullets as a way to cheat this problem
            ### appears to have no effect anyway. BUllets always display.
            options = list(steps = steps(), "showBullets"="false"),
            ### Make sure correct tab box tab panel active
            ### Normally only need for step 0 (if tour launched while on tab panel 2)
            ### But now need for step 0 and 2 in case user changes to tab 2 on tour...
            ### Actually need for all steps because user can go to tab 2 on step 0 or 2,
            ### and from there change to any step using by clicking bullets.
            ### But accessing the bullets is where the below callback breaks down:
            ### E.g., on step 0 or 2, click tab panel 2 to change tabs,
            ### then use bullets in tooltip to jump to later step.
            events=list(
              "onchange" = I(("if (this._currentStep== 3) {
              
              alert('test');
              //$('#testNk').removeClass('active');
              $('#ddb_import').addClass('active');
              $('#ddb_import').trigger('click');
              alert('test');
              //$('a[data-value=\"Tab box tab 2\"]').removeClass('active');
              //$('a[data-value=\"Tab box tab 1\"]').addClass('active');
              //$('a[data-value=\"Tab box tab 1\"]').trigger('click');
              }")))
    )
  })
    
    
    
    
    
    # introjs(session,
    #         options = list(steps=steps(),
    #                        events = list(
    #                          onbeforechange=
    #                                        I("
    #                                        alert('test');
    #                                        
    #                                        function eventFire (el, etype){
    #                                        
    #                                        if(el.fireEvent){
    #                                        el.fireEvent('on' + etype);
    #                                        
    #                                        }else{
    #                                        var evObj = document.createEvent('Events');
    #                                        evObj.initEvent(etype, true, false);
    #                                        el.dispatchEvent(evObj);
    #                                        
    #                                        }
    #                                        
    #                                        }
    #                                        
    #                                        
    #                                        
    #                                        // this is javascript
    #                                        if (this._currentStep == 1) {
    #                                        $('.outer').css('opacity', 0.5);
    #                                        } else {
    #                                        // change opacity back
    #                                        $('.outer').css('opacity', 1);
    #                                        }
    #                                       
    #                                        // Open a dropdown
    #                                        if (this._currentStep==3) {
    #                                        swDrop('ddb_import', 'sw-content-ddb_import', 'sw-drop-ddb_import', 'sw-none', 'sw-none', '1');
    #                                        alert('test');
    #                                        // eventFire(document.getElementById('ddb_import'), 'click');
    # 
    # 
    #                                        }
    #                                        
    #                                       ")
    #                          ,
    #                          "onchange" = I(
    #                            "
    #                              // Open a dropdown
    #                                        if (this._currentStep==3) {
    #                                        //swDrop('ddb_import', 'sw-content-ddb_import', 'sw-drop-ddb_import', 'sw-none', 'sw-none', '1');
    #                                        alert('test');
    #                  }
    #                  "
    #                  # $('#ddb_import').addClass('active');
    #                  # $('#ddb_import').trigger('click');
    # 
    # 
    #                          )
    #                          
    #                        )
    #         )
    # )

  # })
  
}# end server



# Application
shinyApp(ui = ui, server = server)

