#---------------------------------------------------------------------------------------------------------------------------#
#                                             Agate - Server Dashboard                                                      #
#---------------------------------------------------------------------------------------------------------------------------#

# Include server code for plotting dashboard graphs


# I. Modal dashboard
#---------------------------------------------------------------------------------------------------------------------------------------------------

# I.1. BoxPlot Income
#--------------------
plotlyBoxplotIncome <- function(input, output, session){
  
  boxplotIncome <-  renderPlotly({
  
    plot_ly(y = ~rnorm(50), type = "box") %>%
      add_trace(y = ~rnorm(50, 1))

 # boxplotIncome <-  renderPlotly({
 #    # Selection de la commune
 #    comZone <- statZona$filo$depcom[statZona$filo$idZonage %in% qpv_filtre()$idZonage][1]
 #    
 #    plot_ly(type = 'box') %>%
 #      # Boxplot d'un equipement
 #      add_boxplot(y = statZona$filo$nivviem[statZona$filo$idZonage %in% qpv_filtre()$idZonage],
 #                  boxpoints = FALSE,
 #                  marker = list(color = 'rgb(255,69,0)'),
 #                  line = list(color = 'rgb(255,69,0)'),
 #                  name = "Zonage") %>%
 #      # Boxplot des individus de la commune qui ne vivent pas le zonage
 #      add_boxplot(y = statZona$filo$nivviem[!(statZona$filo$idZonage %in% qpv_filtre()$idZonage) &
 #                                              statZona$filo$depcom %in% comZone],
 #                  name = "Hors zonage", boxpoints = FALSE,
 #                  marker = list(color = 'rgb(113,113,198)'),
 #                  line = list(color = 'rgb(113,113,198)')) %>%
 #      # Boxplot des individus de la commune
 #      add_boxplot(y = statZona$filo$nivviem[statZona$filo$depcom %in% comZone],
 #                  name = "Communal", boxpoints = FALSE,
 #                  marker = list(color = 'rgb(113,198,113)'),
 #                  line = list(color = 'rgb(113,198,113)')) %>%
 #      
 #      # Boxplot des individus des autres zonages du departement
 #      # add_boxplot(y = statZona$filo$nivviem[!(statZona$filo$idZonage %in% qpv_filtre()$idZonage) & !is.na(statZona$filo$idZonage)
 #      #                                       & substr(statZona$filo$depcom,1,3) %in% substr(comZone,1,3)],
 #      #             name = "Autres zonages", boxpoints = FALSE,
 #      #             marker = list(color = 'rgb(252,141,98)'),
 #      #             line = list(color = 'rgb(252,141,98)')) %>%
 #      
 #      # Reglage des axes
 #      layout(
 #        yaxis = list(range = c(0, 50000)))
  })
  
  return(boxplotIncome)
}











