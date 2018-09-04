#--------------------------------------------------------------------------------------------------------------------------------#
#                      Agate - Gantt chart app                                                                                   #
#--------------------------------------------------------------------------------------------------------------------------------#

# MAJ : 30.08.2018

# Nicolas Kempf

# Application RShiny de creation, modification et affichage du diagramme de gantt du projet Agate

# https://github.com/daattali/timevis
# https://www.w3schools.com/css/css_border.asp
# https://clrs.cc/

# Packages nécessaires
#---------------------
library(timevis) # Interactive Gantt chart
library(tidyverse) # transformation des données et calcul de statistiques descriptives simples.
library(shiny) # Interface graphique
library(shinyWidgets) # Widgets supplementaires

# Data
load("Data/Gantt/gantt.RData")

templateNK <- function(tache, equipe, pourcent) {
  sprintf(    
    '<table><tbody>
    <tr><td colspan="3"><b>%s</b></td></tr>
    <tr><td><i>%s</i></td></tr>
    <tr><td>%s</td></tr>
    </tbody></table>',
    tache, equipe,pourcent 
  )
}



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
              selectInput("AddGroup",label = "Selectionner le groupe",choices = groups$content),
              textInput("addTache", tags$h4("Ajouter une tache"), "Nouvelle tache"),
              textInput("addEquipe", tags$h4("Ajouter participant"), "Hypolite")
              
              # dateInput("startDate", label = "Date de début", Sys.Date()),
              # dateInput("endDate", label = "Date de fin", Sys.Date()),
              # actionButton("addBtn", "Ajouter")
          )
        ),
        column(
          4,
          div(class = "optionsSection",
              textInput("addPourcent", tags$h4("Pourcentage"), "%"),
              dateInput("startDate", label = "Date de début", Sys.Date()),
              dateInput("endDate", label = "Date de fin", Sys.Date()),
              fluidRow(
                actionButton("addBtn", "Ajouter"),
                actionButton("addSave", "Sauvegarder")
                
              )
              
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
    ),
    
    useSweetAlert()
  )
)


# II. Server
#--------------------------------------------------------------------------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # Affichage de la timeline
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
  
  # Affichage du tableau
  output$table <- renderTable({
    data <- input$timelineInteractive_data
    data$start <- prettyDate(data$start)
    if(!is.null(data$end)) {
      data$end <- prettyDate(data$end)
    }
    data[,c("id","tache","start","end","group")]
  })
  
  # Ajout d'une tache
  observeEvent(input$addBtn, {
    addItem("timelineInteractive",
            data = list(id = randomID(),
                        tache = input$addTache,
                        team = input$addEquipe,
                        pct = input$addPourcent,
                        content = templateNK(input$addTache,input$addEquipe,paste0(input$addPourcent," %")),
                        start = input$startDate,
                        end = input$endDate,
                        group = groups$id[groups$content == input$AddGroup],
                        style = case_when(input$addPourcent >=75 ~ "color:#001f3f;border-style:solid;background-color:#2ECC40;",
                                          input$addPourcent < 25 ~ "color:#001f3f;border-style:solid;background-color:#FF4136;",
                                          TRUE ~ "color:#001f3f;border-style:solid;background-color:#FFDC00;")))
  })
  
  # Sauvegarde de la table
  observeEvent(input$addSave, {
    
    
    df <- input$timelineInteractive_data
    save(df,groups,file="Data/Gantt/gantt.RData")
    
    
    shinyWidgets::sendSweetAlert(
      session = session, 
      title = "Succes", text = "Table sauvegardée !", type = "success"
    )
    
    
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

