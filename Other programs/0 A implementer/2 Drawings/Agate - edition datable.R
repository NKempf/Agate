#---------------------------------------------------------------------------------------------------------------------------#
#                         Agate - Edition d'une datatable  dans une Shiny App                                               #
#---------------------------------------------------------------------------------------------------------------------------#

# 14.11.2018

# Nicolas Kempf

# Tester l'edition d'une datatable

library(shiny)
library(DT)

# 1) Table d'entrainement
# df <- data.frame(id=seq(1:10),val = round(rnorm(10,0,1),2))
# d3 <- df

# 2) UI
ui <-  DT::dataTableOutput('x3')

# 3) Server 

server <- function(input, output, session) {
  # Reactive data.frame
  rv <- reactiveValues(df=data.frame(id=seq(1:10),val = round(rnorm(10,0,1),2)))
  
  
  # Affichage de la table attributaire dans un DT
  # output$x1 = renderDT(df, selection = 'none', server = F, editable = T,rownames = FALSE)
  output$x3 = renderDT(rv$df, selection = 'single', rownames = FALSE, editable = TRUE)
  
  proxy3 = dataTableProxy('x3')
  
  observeEvent(input$x3_cell_edit, {
    info = input$x3_cell_edit
    str(info)
    i = info$row
    j = info$col + 1  # column index offset by 1
    v = info$value
    rv$df[i, j] <- DT::coerceValue(v, rv$df[i, j])
    replaceData(proxy3, rv$df, resetPaging = FALSE, rownames = FALSE)
    
    print(rv$df)
  })
  
  # Ligne selectionnée
  observeEvent(input$x1_rows_selected,{
    print(input$x1_rows_selected)
  })
  
  
  
}



# 4) Launch app
shinyApp(ui, server)
