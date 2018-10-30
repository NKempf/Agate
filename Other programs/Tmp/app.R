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
library(rgdal) # Input shapefile
library(rgeos) # Map tools
library(tidyverse) # Tidy data
library(plotly) # Interactive graphics
library(openxlsx) # Export data in Excel

# II. Data
#----------------------------------------------------------------------------------------------------------------------------------

# II. 1 Functions
#----------------
source ("Other programs/Fake data/Agate - Fake data fct.R",encoding = "UTF-8")
source("Other programs/Zonage/Agate - Cartographie fct.R",encoding = "UTF-8")
source("Other programs/Export Report/Agate - Export Excel fct.R",encoding = "UTF-8")
source("Other programs/StatZonage/Agate - Statistics Zonage_v4.R",encoding = "UTF-8")
source("Other programs/Plotly/Agate - plotly graphes fonctions_v3.R",encoding = "UTF-8")
# source("Other programs/Export Report/Agate - excel report.R")

# II.2. Fake data
#----------------
# load("Data/fakePts.RData") # fake position
# load("Data/fakeData.Rdata") # fake data base
# load("Data/Stats/Region and cities/region_stat.RData") # Fake region and cities stat
# 
# # Penser à modifier SERVER si on utilise les fakedata
# ril <- pts.fake

# II.3. Real Data
#----------------
load("Data/Ril/ril15.RData")
load("Data/Rp/rp14.RData")
load("Data/Filosofi/filo14.Rdata")
load("Data/Statistiques Zonage/StatRegCom_rp14_filo14.RData")

# Parameters
ril <- rilhab15
rpi <- rp14i
rpl <- rp14l
filo <- filo14.disp
com.stat <- statCom_rp14_filo14
dep.stat <- statReg_rp14_filo14

rm(rilhab15,rp14i,rp14l,Rp14dico,filo14.disp,filo14.disp.dico,statCom_rp14_filo14,statReg_rp14_filo14)

# III. Graphic web interface
#----------------------------------------------------------------------------------------------------------------------------------
source("AgataUI.R",encoding = "UTF-8")

# IV. Server
#----------------------------------------------------------------------------------------------------------------------------------
source("AgataSERVER.R",encoding = "UTF-8")

# V. Run the application 
#----------------------------------------------------------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)


