#--------------------------------------------------------------------------------------------------------------------------------#
#                      Agate - Gantt chart app                                                                                   #
#--------------------------------------------------------------------------------------------------------------------------------#

# MAJ : 30.08.2018

# Nicolas Kempf

# Application RShiny de creation, modification et affichage du diagramme de gantt du projet Agate

# https://github.com/daattali/timevis
# https://www.w3schools.com/css/css_border.asp

# Packages nécessaires
#---------------------
library(timevis) # Interactive Gantt chart
library(tidyverse) # transformation des données et calcul de statistiques descriptives simples.
library(shiny)

# Data
load("Data/Gantt/gantt.RData")


# I. Interface graphique
#---------------------------------------------------------------------------------------------------------------------------------------
ui <- fluidPage(
  title = "Fully interactive",

  fluidRow(
    column(
      8,
      fluidRow(column(12,
                      timevisOutput("timelineInteractive")
      )),
      # fluidRow(
      #   column(
      #     12,
      #     div(id = "interactiveActions",
      #         class = "optionsSection",
      #         tags$h4("Actions:"),
      #         actionButton("fit", "Fit all items"),
      #         actionButton("setWindowAnim", "Set window 2016-01-07 to 2016-01-25"),
      #         actionButton("setWindowNoAnim", "Set window without animation"),
      #         actionButton("center", "Center around 2016-01-23"),
      #         actionButton("focus2", "Focus item 4"),
      #         actionButton("focusSelection", "Focus current selection"),
      #         actionButton("addTime", "Add a draggable vertical bar 2016-01-17")
      #     )
      #   )
      # ),
      fluidRow(
        column(
          4,
          div(class = "optionsSection",
              uiOutput("selectIdsOutput", inline = TRUE),
              actionButton("selectItems", "Select"),
              checkboxInput("selectFocus", "Focus on selection", FALSE)
          )
        ),
        column(
          4,
          div(class = "optionsSection",
              textInput("addTache", tags$h4("Ajouter une tache"), "Nouvelle tache"),
              textInput("addEquipe", tags$h4("Ajouter participant"), "Hypolite"),
              textInput("addPourcent", tags$h4("Pourcentage"), "%"),
              dateInput("startDate", label = "Date de début", Sys.Date()),
              dateInput("endDate", label = "Date de fin", Sys.Date()),
              actionButton("addBtn", "Ajouter")
          )
        ),
        column(
          4,
          div(class = "optionsSection",
              uiOutput("removeIdsOutput", inline = TRUE),
              actionButton("removeItem", "Remove")
          )
        )
      )
    ),
    column(4,
           div(
             id = "timelinedata",
             class = "optionsSection",
             tags$h4("Data:"),
             tableOutput("table"),
             hr(),
             div(tags$strong("Visible window:"),
                 textOutput("window", inline = TRUE)),
             div(tags$strong("Selected items:"),
                 textOutput("selected", inline = TRUE))
           )
    )
  )
)


# II. Server
#--------------------------------------------------------------------------------------------------------------------------------------------------
server <- function(input, output, session) {

  output$timelineInteractive <- renderTimevis({
    config <- list(
      editable = TRUE,
      multiselect = TRUE
    )
    timevis(df,
            groups = groups,
            options = config
            )
  })
  
  output$table <- renderTable({
    data <- input$timelineInteractive_data
    data$start <- prettyDate(data$start)
    if(!is.null(data$end)) {
      data$end <- prettyDate(data$end)
    }
    data
  })
  
}



# generate a random string of 16 characters
randomID <- function() {
  paste(sample(c(letters, LETTERS, 0:9), 16, replace = TRUE), collapse = "")
}

prettyDate <- function(d) {
  suppressWarnings(format(as.POSIXct(gsub("T", " ", d), "%Y-%m-%d %H:%M")))
}



# III. Application
#---------------------------------------------------------------------------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)

