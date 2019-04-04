#---------------------------------------------------------------------------------------------------------------------------#
#                                             Agate - Main program                                                          #
#---------------------------------------------------------------------------------------------------------------------------#
# 
# Nicolas Kempf
#
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# I. Packages
#-----------------------------------------------------------------------------------------------------------------------------------
# .libPaths("../Server RShiny/Agate packages R3_3_3")
library(shiny) # Graphic web interface
library(shinyBS) # Pop-up windows
library(shinyjs) # Mask buttons/elements on graphic interface
library(shinyWidgets) # Widgets supplementaires
library(shinydashboard) # Tools like infoBox
library(leaflet) # Interactive web map
library(leaflet.extras) # Extras functions for leaflet
library(rgdal) # Input shapefile
library(rgeos) # Map tools
library(sampling) # Sampling tools
library(tidyverse) # Tidy data
library(plotly) # Interactive graphics
library(openxlsx) # Export data in Excel
library(fst) # Read partial data
library(fstplyr) # dplyr for fst object
library(DT) # Interactive datatable
library(rlang) # Non Standard evaluation

# II. Data
#----------------------------------------------------------------------------------------------------------------------------------

# II. 1 Functions
#----------------
source ("Other programs/2 Fonctions Agate/1 Fake data/Agate - Fake data fct.R",encoding = "UTF-8")
source("Other programs/2 Fonctions Agate/2 Cartographie/Agate - Cartographie fct.R",encoding = "UTF-8")
source("Other programs/2 Fonctions Agate/3 Indicateurs statistiques/Agate - Statistics Zonage_v9.R",encoding = "UTF-8")
source("Other programs/2 Fonctions Agate/6 Census Quality/Agate - Qualite du RP.R",encoding = "UTF-8")
source("Other programs/2 Fonctions Agate/7 Statistical disclosure/Agate - statistiscal disclosure.R",encoding = "UTF-8")
source("Other programs/2 Fonctions Agate/4 Dashboard/Agate - Dashboard fct.R",encoding = "UTF-8")

# II.2. Maps
#-----------
load("Data/Maps/Cities/cities.RData") 
load("Data/Maps/HeatPoint/heatpoints.Rdata") # heat points

# III. Graphic web interface
#----------------------------------------------------------------------------------------------------------------------------------
source("AgataUI.R",encoding = "UTF-8")

# IV. Server
#----------------------------------------------------------------------------------------------------------------------------------
source("AgataSERVER.R",encoding = "UTF-8")

# V. Run the application 
#----------------------------------------------------------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)


