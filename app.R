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

# II. Data
#----------------------------------------------------------------------------------------------------------------------------------

# II. 1 Functions
#----------------
source ("Other programs/Fake data/Agate - Fake data fct.R",encoding = "UTF-8")
source("Other programs/Zonage/Agate - Cartographie fct.R",encoding = "UTF-8")
source("Other programs/Quality/Agate - Census infra quality.R",encoding = "UTF-8")
source("Other programs/Export Report/Agate - Export Excel fct.R",encoding = "UTF-8")
source("Other programs/StatZonage/Agate - Statistics Zonage_v4.R",encoding = "UTF-8")
source("Other programs/Plotly/Agate - plotly graphes fonctions_v3.R",encoding = "UTF-8")
# source("Other programs/Export Report/Agate - excel report.R")

# II.2. Fake data
#----------------
load("Data/Fake/fakeHeatPts.RData") # Fake spatialpoints for heatmap
FakeHeatpoint <-  spTransform(FakeHeatpoint,"+init=epsg:4326")
FakeHeatpoint@data %>% 
  mutate(x = FakeHeatpoint@coords[,1],
         y = FakeHeatpoint@coords[,2]) -> FakeHeatpoint

# Statistical summary about cities and territory
if(file.exists("Data/Statistiques Zonage/StatRegCom_rp14_filo14.RData")){
  load("Data/Statistiques Zonage/StatRegCom_rp14_filo14.RData")  # Real data
  com.stat <- statCom_rp14_filo14
  dep.stat <- statReg_rp14_filo14
}else{
  load("Data/Stats/Region and cities/region_stat.RData") # Fake region and cities stat
}

# II.3. Maps
#----------------
load("Data/Maps/Cities/cities.RData") # Cities map

# III. Graphic web interface
#----------------------------------------------------------------------------------------------------------------------------------
source("AgataUI.R",encoding = "UTF-8")

# IV. Server
#----------------------------------------------------------------------------------------------------------------------------------
source("AgataSERVER.R",encoding = "UTF-8")

# V. Run the application 
#----------------------------------------------------------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)


