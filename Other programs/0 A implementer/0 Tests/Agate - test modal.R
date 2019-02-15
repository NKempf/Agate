library(shiny)
library(shinyBS)
shinyApp(
  ui = basicPage(
    actionButton("show1", "Show modal dialog"),
    actionButton("show2", "Show modal dialog"),
    # tags$head(tags$style("#boxPopUp1 .modal-dialog{ width:1000px}")),
    # tags$head(tags$style("#boxPopUp1 .modal-body{ min-height:700px}")),
    tags$head(tags$style("#boxPopUp1 .modal-footer{ display:none}")),
    tags$head(tags$style("#boxPopUp2 .modal-dialog{ width:100px}")),
    tags$head(tags$style("#boxPopUp2 .modal-body{ min-height:100px}")),
    bsModal('boxPopUp1', 'Big','test',size = "large"),
    bsModal('boxPopUp2', 'Small','test')
    
    
  ),
  server = function(input, output,session) {
    observeEvent(input$show1, {
      toggleModal(session, "boxPopUp1", toggle = "toggle")
    })
    observeEvent(input$show2, {
      toggleModal(session, "boxPopUp2", toggle = "toggle")
    })
  }
)
#exists("qpv_stat2")

# table(qpv_stat@data$NOM_QP)
