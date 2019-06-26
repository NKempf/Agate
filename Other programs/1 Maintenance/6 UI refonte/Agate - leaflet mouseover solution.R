# https://stackoverflow.com/questions/37449747/shiny-leaflet-mouseover-popup

library(shiny)
library(leaflet)
library(data.table)

uu <-  data.table(
  row_num=seq(100),
  Latitude=c(52+cumsum(runif(100,-0.001,0.001))),
  Longitude=c(1+cumsum(runif(100,-0.001,0.001)))
)

ui <- fluidPage(
  leafletOutput("mymap")
)

server <- function(input, output, session) {
  
  radius = 3
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addCircles(lng = uu$Longitude, lat = uu$Latitude, radius = radius, layerId = uu$row_num)
  })
  
  observeEvent(input$mymap_shape_mouseout$id, {
    leafletProxy("mymap") %>% clearPopups()
  })
  
  # When circle is hovered over...show a popup
  observeEvent(input$mymap_shape_mouseover$id, {
    pointId <- input$mymap_shape_mouseover$id
    lat = uu[uu$row_num == pointId, Latitude]
    lng = uu[uu$row_num == pointId, Longitude]
    offset = isolate((input$mymap_bounds$north - input$mymap_bounds$south) / (23 + radius + (18 - input$mymap_zoom)^2 ))
    
    leafletProxy("mymap") %>% addPopups(lat = lat + offset, lng = lng, as.character(pointId))
  })
}

shinyApp(ui, server) 