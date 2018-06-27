library(writexl)
library(xlsx)
library(XLConnect)

library(openxlsx)


# wb <- loadWorkbook("test.xlsx",create=T)
# 
# 
# l <- list(rock=rock,mtcars = mtcars
#      )
# 
# lapply(names(l), function(x){
#   createSheet(wb,x)
#   writeWorksheet(wb,l[[x]],x)
# })
# 
# saveWorkbook(wb)


load("Data/Stats/Region and cities/region_stat.RData") # Fake region and cities stat
source("Other programs/Export Report/Agate - Export Excel fct.R")
# write_xlsx(dep.stat[2:length(dep.stat)],path = "Output/test.xlsx")
# 
# class(dep.stat$tRp.I.2)

library(shiny)

# Define UI for data download app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Downloading Data"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Choose dataset ----
      selectInput("dataset", "Choose a dataset:",
                  choices = c("Commune", "Departement", "cars")),
      
      # Button
      downloadButton("downloadData", "Download")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      tableOutput("table")
      
    )
    
  )
)

# Define server logic to display and download selected file ----
server <- function(input, output) {
  
  # Reactive value for selected dataset ----
  datasetInput <- reactive({
    switch(input$dataset,
           "Commune" = com.stat[2:length(com.stat)],
           "Departement" = dep.stat[2:length(dep.stat)],
           "cars" = cars)
  })
  
  # Table of selected dataset ----
  output$table <- renderTable({
    datasetInput()
  })
  
  
  wbInput <- reactive({
    wb<<-createWorkbook(type="xlsx")
    sheet <<- createSheet(wb, sheetName = "Synthese")

    # Ajout du tableau1
    excel.ajoutTabNk(sheet,tableau = mtcars,
                     titre1 = "Synthese : Indicateurs statistiques du RP selon le zonage",
                     sources = "Nardine",
                     champ = "Depend de la statistique consideree. Regarder les resultats en details pour connaitre le champ",
                     ligne = 1)
    
  })
  
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".xlsx", sep = "")
    },
    content = function(file) {
      
      df <- data.frame("Date" = Sys.Date()-0:4,
                       "Logical" = c(TRUE, FALSE, TRUE, TRUE, FALSE),
                       "Currency" = paste("$",-2:2),
                       "Accounting" = -2:2,
                       "hLink" = "https://CRAN.R-project.org/",
                       "Percentage" = seq(-1, 1, length.out=5),
                       "TinyNumber" = runif(5) / 1E9, stringsAsFactors = FALSE)
      
      # write.csv(mtcars, file, row.names = FALSE) # Fonctionne
      # write.xlsx(x = mtcars,file = file)
      # write_xlsx(x = mtcars,path = file)
      # write_xlsx(x = list(mtcars,rock),path = file)
      
      # print("Je marche bordel")
      # write.xlsx(mtcars,file = file,row.names = FALSE) # Fonctionne
      openxlsx::write.xlsx(df, file = file)
      
      # saveWorkbook(wbInput(), file)
    }
  )
  
}

# Create Shiny app ----
runApp(shinyApp(ui, server),launch.browser = T)




