# Agate - test de la librairie Rintrojs

library(shiny)
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
    tabPanel(
      "Carte",
      div(class="outer",
          tags$head(includeCSS("www/agate.css")),
          # I.1.1. Leaflet map
          #-----------------
          leafletOutput("llo_agateMap", width = "100%", height = "100%")
      
      )
      # ,# Tabset Carte
    ),
    # II. Statistiques et téléchargement
    #-------------------------------------------------------------------------------------------------------------------------------------------------
    tabPanel("Statistiques",value="vis",
             
             fluidRow(
               column(4,
                      # II.1. Zone selection
                      #---------------------
                      selectInput("si_stat_zoneSelect", "Zone",
                                  choices = pred.choice,
                                  selected = 4)
               ),
               column(4,
                      
                      # II.2. Domaine selection
                      #------------------------
                      selectInput("si_stat_domaine", "Domaine",
                                  choices = c("Choice" ="",dmn),
                                  selected = dmn[2])
               ),
               column(4,
                      # II.3. Categorie selection
                      #--------------------------
                      selectInput("si_stat_categorie", "Catégorie",
                                  choices = c("Choice" =""),
                                  selected = c(""))
               )
             ),
             
             hr(), # Line between buttons and plot
             
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
  observeEvent(input$ab_aide,
               introjs(
                 session,
                 events = list(
                   "onchange" = I("if (this._currentStep==0) {
                                  $('a[data-value=\"Second tab\"]').removeClass('active');
                                  $('a[data-value=\"First tab\"]').addClass('active');
                                  $('a[data-value=\"First tab\"]').trigger('click');
}
if (this._currentStep==1) {
$('a[data-value=\"First tab\"]').removeClass('active');
$('a[data-value=\"Second tab\"]').addClass('active');
$('a[data-value=\"Second tab\"]').trigger('click');
}"
))
                   )
                   )
}# end server



# Application
shinyApp(ui = ui, server = server)

