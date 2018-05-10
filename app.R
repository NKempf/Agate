#---------------------------------------------------------------------------------------------------------------------------#
#                                             Agate - Main program                                                          #
#---------------------------------------------------------------------------------------------------------------------------#
# 
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# I. Packages
#----------------------------------------------------------------------------------------------------------------------------------
library(shiny) # Graphic web interface
library(shinyBS) # Pop-up windows
library(shinyjs) # Mask buttons/elements on graphic interface
library(leaflet) # Interactive web map


# II. Data
#----------------------------------------------------------------------------------------------------------------------------------

  # II.1. QPV (fake data)
  #----------------------
  load("Data/QPV/qpvFake.RData")
  qpv_stat <- qpv_stat.fake
  qpv_stat@data$idZonage <- qpv_stat@data$CODE_QP
  rm(qpv_stat.fake)

# III. Graphic web interface
#----------------------------------------------------------------------------------------------------------------------------------
source("AgataUI.R")

# IV. Server
#----------------------------------------------------------------------------------------------------------------------------------
source("AgataSERVER.R")


# V. Run the application 
#----------------------------------------------------------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)

