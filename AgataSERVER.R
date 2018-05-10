#---------------------------------------------------------------------------------------------------------------------------#
#                                             Agate - Server                                                                #
#---------------------------------------------------------------------------------------------------------------------------#

server <- function(input, output,session) {
  
  # I. Interactive web map
  #----------------------------------------------------------------------------------------------------------------------------
  
  # I.1. Initiate interactive web map
  output$mymap <- renderLeaflet({
    leaflet("map") %>% addTiles()%>% 
      fitBounds(lng1 = -65,lat1 = 18,lng2 = -45,lat2 = 3) %>%
      addPolygons(data=qpv_stat,opacity = 3,
                  color = "green", stroke = TRUE, weight = 2,
                  fill = TRUE, fillOpacity = 0.2,popup = ~paste(NOM_QP),layerId = ~paste(CODE_QP))

  })
   
  
  
  
  
  
  
  
  
  
  # points <- eventReactive(input$recalc, {
  #   cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
  # }, ignoreNULL = FALSE)
  # 
  
  
  
  
  # output$mymap <- renderLeaflet({
  #   leaflet() %>%
  #     addProviderTiles(providers$Stamen.TonerLite,
  #                      options = providerTileOptions(noWrap = TRUE)
  #     ) %>%
  #     addMarkers(data = points())
  # })
  # 
  # 
  
}
