#---------------------------------------------------------------------------------------------------------------------------#
#                                             Agate - packages install                                                      #
#---------------------------------------------------------------------------------------------------------------------------#

# 28.08.2018

# Nicolas Kempf

# Répertoire d'installation des packages
# .libPaths("../Server RShiny/Agate packages R3_3_3")

# debug(utils:::unpackPkgZip)

# Shiny : Graphic web interface
#------------------------------
install.packages("shiny",type="binary")

# shinyBS : Pop-up windows
#-------------------------
install.packages("shinyBS")

# shinyjs : Mask buttons/elements on graphic interface
#-----------------------------------------------------
install.packages("shinyjs")

# shinyWidgets : Widgets supplementaires
#---------------------------------------
install.packages("shinyWidgets")

# shinydashboard : Tools like infoBox
#------------------------------------
install.packages("shinydashboard")

# leaflet : Interactive web map
#------------------------------
install.packages("leaflet")

# rgdal : Input shapefile
#------------------------
install.packages("rgdal")

# rgeos : Map tools
#------------------
install.packages("rgeos")

# tidyverse : tidy tools data
#----------------------------
install.packages("tidyverse")

# Plotly : Interactive graphics 
#------------------------------
# NB : radar chart available only on Github
options(download.file.method = "wininet")
install.packages("httr")
library(httr)
set_config(use_proxy(url = "proxy-rie.http.insee.fr", port = 8080))
install.packages("devtools")
library(devtools)
devtools::install_github("ropensci/plotly")

# openxlsx : export report excel
#-------------------------------
install.packages("openxlsx")

# DT : Shiny table
#-----------------
install.packages("DT")






