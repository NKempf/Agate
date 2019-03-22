library(shiny)
library(DT)
library(tidyverse)
library(readxl)


QualiteVar<- qualite %>% filter(!duplicated(Variable)) %>% select(Variable)
QualiteZone<- qualite %>% filter(!duplicated(zonage)) %>% select(zonage)

shinyApp(
  
  ui = fluidPage( fluidRow(
    column(4,
           # II.1. Zone selection
           #---------------------
           selectInput("si_select_qual", "Zone selection",
                       choices = c("Par Zone","Par Variable","Tout Selectionner"),
                       selected = c("Par Zone"))
    ),
    column(4,
           # II.3. Categorie selection
           #--------------------------
           selectInput("si_variable_qual", "Variable",
                       choices = as.character(QualiteVar$Variable),
                       selected = as.character(QualiteVar$Variable[1]))
    ),   
    column(4,
                # II.3. Categorie selection
                #--------------------------
                selectInput("si_zone_qual", "Zone",
                            choices = as.character(QualiteZone$zonage),
                            selected = as.character(QualiteZone$zonage[1]))
    )
  ),
  
  
  hr(), # Line between buttons and plot
  
  # II.4. Table visualisation
  #--------------------------
  textOutput("TO_titleTab_qual"),
  tags$head(tags$style("#TO_titleTab_qual{font-size: 30px;font-style: bold;}")),
  DT::dataTableOutput("dt_qualite")
  
  ), #end ui
  
  
  
  server = function(session,input, output) {
    
    
    # 0. Reactive Values
    #----------------------------------------------------------------------------------------------------------------------------
    rv <- reactiveValues(df.explore=NULL)
  
    observeEvent(c(input$si_zone_qual,input$si_variable_qual,input$si_select_qual),{
      if(input$si_select_qual=="Par Zone"){
        output$TO_titleTab_qual <- renderText(paste0("Toutes les variables sur ",input$si_zone_qual))
        
        df<-qualite %>% filter(zonage==input$si_zone_qual)
        output$dt_qualite = renderDT(
          datatable(df,
                    #colnames = type.ind,
                    extensions = 'Buttons',
                    options = list(
                      scrollX = TRUE,
                      # fixedColumns = TRUE,
                      # autoWidth = TRUE,
                      ordering = FALSE,
                      dom = 'lBfrtip',
                      buttons = c(I('colvis'),'excel')),
                    rownames= FALSE)
        )
      } else if(input$si_select_qual=="Par Variable"){
        output$TO_titleTab_qual <- renderText(paste0(input$si_variable_qual," sur toutes les zones"))
        
        df<-qualite %>% filter(Variable==input$si_variable_qual)
        output$dt_qualite = renderDT(
          datatable(df,
                    #colnames = type.ind,
                    extensions = 'Buttons',
                    options = list(
                      scrollX = TRUE,
                      # fixedColumns = TRUE,
                      # autoWidth = TRUE,
                      ordering = FALSE,
                      dom = 'lBfrtip',
                      buttons = c(I('colvis'),'excel')),
                    rownames= FALSE)
        )
      }
      else{
        output$TO_titleTab_qual <- renderText("Toutes les Variables sur toutes les zones")
        
        df<-qualite
        output$dt_qualite = renderDT(
          datatable(df,
                    #colnames = type.ind,
                    extensions = 'Buttons',
                    options = list(
                      scrollX = TRUE,
                      # fixedColumns = TRUE,
                      # autoWidth = TRUE,
                      ordering = FALSE,
                      dom = 'lBfrtip',
                      buttons = c(I('colvis'),'excel')),
                    rownames= FALSE)
        )
      }
    })
  }
)
