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
#   observeEvent(input$ab_aide,
#                introjs(
#                  session,
#                  events = list(
#                    "onchange" = I("if (this._currentStep==0) {
#                                   $('a[data-value=\"Statistiques\"]').removeClass('active');
#                                   $('a[data-value=\"Carte\"]').addClass('active');
#                                   $('a[data-value=\"Carte\"]').trigger('click');
# }
# if (this._currentStep==1) {
# $('a[data-value=\"Carte\"]').removeClass('active');
# $('a[data-value=\"Statistiques\"]').addClass('active');
# $('a[data-value=\"Statistiques\"]').trigger('click');
# }"
#                    ))
#                )
#   )
  
  steps <- reactive(data.frame(element = c("#llo_agateMap","#ap_controls"),
                               intro = c("Affichage de la carte","Menu de navigation")))
  
  observeEvent(input$ab_aide,{
    introjs(session,
            options = list(steps=steps(),
                           events = list(onbeforechange=I("
// this is javascript
if (this._currentStep == 1) {
 $('.outer').css('opacity', 0.5);
} else {
// change opacity back
$('.outer').css('opacity', 1);
}
")
                                         )
                           )
    )
    
  })
  
}# end server



# Application
shinyApp(ui = ui, server = server)

