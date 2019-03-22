library(shiny)
library(shinyBS)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(leaflet)
library(rgdal)
library(tidyverse)


# Chargement de la carte des qpv
zonage <- readOGR("Data/QPV/qpv.shp",encoding = "utf-8",stringsAsFactors = FALSE)[1:3,] 
zonage <- spTransform(zonage,"+init=epsg:4326")

n <- nrow(zonage@data)
sd <- 10

# Transformation des variables
zonage@data <- zonage@data %>% 
  rename(idZonage = CODE_QP,
         labelZonage = NOM_QP) %>% 
  select(idZonage,labelZonage) %>% 
  bind_cols(data.frame(pop = rnorm(n,50,sd),diffusable = c(TRUE,FALSE,TRUE))) %>% 
  mutate(pop_icMin = pop - sd,
         pop_icMax = pop + sd)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

ui <- fluidPage(
  # NB : Leaflet map need agate.css to fit windows
  tags$head(includeCSS("www/agate.css")),
  leafletOutput("mymap"),

  # I.2.3. Dashboard
  #-----------------
  tags$head(tags$style("#boxPopUp1 .modal-footer{ display:none}")), # Remove BS modal footer
  tags$head(tags$style(HTML('
                            
                            .modal-lg {
                            width: 80%;
                            
                            }
                            '))), # Increase modal size
  bsModal('boxPopUp1', textOutput("modalTitle"),'test',size = "large",
          # Add CSS files : use infobox from shinydashboard package into a shinyApp
          includeCSS(path = "www/AdminLTE.css"),
          includeCSS(path = "www/shinydashboard.css"),
          
          # I.2.2.1 Infobox
          #----------------
          fluidRow(
            infoBoxOutput(outputId = "IB_pop"),
            infoBoxOutput(outputId = "IB_rev"),
            infoBoxOutput(outputId = "IB_chom")
          )
  ),
  # I.1.2.4. Sweet pop-up
  #----------------------
  useSweetAlert()
  
)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
server <- function(input, output, session) {

  # I. Carte
  #-----------------------------------------------------------------------------------------------------------------------
  output$mymap <- renderLeaflet({
    
    # Boundary box
    AgateMap.bbox <- as.data.frame(bbox(zonage))
    
    leaflet(zonage) %>% addTiles()%>% 
      fitBounds(lng1 = AgateMap.bbox$min[1],lat1 = AgateMap.bbox$max[2],lng2 = AgateMap.bbox$max[1],lat2 = AgateMap.bbox$min[2]) %>%
        addPolygons(opacity = 3,
                    color = "green", stroke = TRUE, weight = 2,
                    fill = TRUE, fillOpacity = 0.2
                    ,popup = ~paste(labelZonage),layerId = ~paste(idZonage)
                    )
  })
  
  # II. Dashboard
  #------------------------------------------------------------------------------------------------------------------------
  observeEvent(input$mymap_shape_click,{
    print(input$mymap_shape_click$id)
    
    if(input$mymap_shape_click$id == "QP971001"){
      sendSweetAlert(
        session = session,
        title = "Données non diffusables",
        text = "Qualité insuffisante",
        type = "error"
      )
      
    }else{
      toggleModal(session, "boxPopUp1", toggle = "toggle")
      
      
      
      
      
      
      
    }
  })
  
}

shinyApp(ui, server)
