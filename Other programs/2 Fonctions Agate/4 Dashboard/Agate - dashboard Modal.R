library(shiny)
library("shinyWidgets")


navbarPageWithInputs <- function(..., inputs) {
  navbar <- navbarPage(...)
  form <- tags$form(class = "navbar-form", inputs)
  navbar[[3]][[1]]$children[[1]] <- htmltools::tagAppendChild(
    navbar[[3]][[1]]$children[[1]], form)
  navbar
}

ui <- shinyUI(fluidPage(
  # tags$head(tags$script(HTML(jsStr))),
  navbarPage("title",
             tabPanel("OpenModal",
                      actionButton("ab_modal","Dashboard"))
  ),
  tags$head(tags$style("#bs_dashboard .modal-footer{ display:none}
                        #bs_dashboard .modal-header{ display:none}")), # Remove BS modal footer
  tags$head(tags$style(HTML('

                                                    .modal-lg {
                                                    width: 90%;
                                                    
                                                    }
                                                    '))), # Increase modal size
  bsModal('bs_dashboard', title = "",'test',size = "large",
          # Add CSS files : use infobox from shinydashboard package into a shinyApp
          includeCSS(path = "www/AdminLTE.css"),
          includeCSS(path = "www/shinydashboard.css"),
          includeCSS(path = "www/agateDashboard.css"),
          
          navbarPageWithInputs(id="nbwi_agate_dashboard","",
                               tabPanel("Emploi","Test"),
                               tabPanel("Démographie","Test"),
                               tabPanel("Scolarisation","Nardine"),
                               inputs = dropdownButton(inputId = "ddb_compare" ,
                                                       selectInput("si_typeZone", "Type de zone",
                                                                   choices = c("Département","Commune","QPV","Zone utilisateur"),
                                                                   selected = c(4)),
                                                       selectInput("si_zone", "Zone",
                                                                   choices = c("Choice" =""),
                                                                   selected = c("")),
                                                       circle = TRUE, status = "danger",
                                                       icon = icon("gear"),size = "sm",right = TRUE,
                                                       tooltip = tooltipOptions(title = "Zones de comparaison")
                               )
                               
          )
  )
))

server <- function(input, output, session) {
  observeEvent(input$ab_modal,{
    toggleModal(session, "bs_dashboard", toggle = "toggle")
  })
}

shinyApp(ui = ui, server = server)
