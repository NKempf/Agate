ui <- shinyUI(
  fluidPage(
    
    dropdown(inputId = "ddb_import",label = "Nardine",right = TRUE,size = "sm",tooltip = tooltipOptions(title = "Importer une carte",placement = "left"),
             
             # label = "Importer",size = "sm",circle = FALSE,right = TRUE,
             # tooltip = tooltipOptions(title = "Importer une carte"),
      
      pickerInput(inputId = "pi_zone_compare",
                              label = "Selection des zones à calculer", 
                              choices = c("Choice" =""),
                              options = list(
                                `actions-box` = TRUE), 
                              multiple = TRUE
      )
      
      # ,
      # animate = animateOptions(enter = animations$fading_entrances$fadeInLeftBig,
      #                          exit = animations$fading_exits$fadeOutRightBig)
    )
    
,
    actionButton("ab_modal","Dashboard")
  )
)

server = function(input, output, session) {
  
  cat <- c("Nardine" = 1,"o" = 2,"mouk" = 3)
  
  observeEvent(input$ab_modal,{
    
    updatePickerInput(session, "pi_zone_compare",
                      choices = cat,
                      selected = cat
                      # choices = cat
    )
    print(input$pi_zone_compare)
  })
  
  observeEvent(input$pi_zone_compare,{
    print(input$pi_zone_compare)
  })
  
}

# Application
shinyApp(ui = ui, server = server)