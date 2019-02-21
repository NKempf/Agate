#---------------------------------------------------------------------------------------------------------------------------#
#                                             Agate - packages install                                                      #
#---------------------------------------------------------------------------------------------------------------------------#

# 28.08.2018

# Nicolas Kempf

# Répertoire d'installation des packages
.libPaths("Packages")

# Shiny : Graphic web interface
#------------------------------
install.packages("later")
install.packages("shiny")

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

# leaflet.extras
#---------------
install.packages("leaflet.extras")

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
# options(download.file.method = "wininet")
# install.packages("httr")
# library(httr)
# set_config(use_proxy(url = "proxy-rie.http.insee.fr", port = 8080))
# install.packages("devtools")
# library(devtools)
# devtools::install_github("ropensci/plotly")
install.packages("plotly")

# openxlsx : export report excel
#-------------------------------
install.packages("openxlsx")

# DT : Shiny table
#-----------------
install.packages("DT")

# rlang : Non standard evaluation
#--------------------------------
install.packages("rlang")

# fst : Efficient table format
#-----------------------------
install.packages("fst")

# fstplyr : dplyr for fst table
#------------------------------
# Le plus simple est de télécharger le zip depuis github 
library(devtools)
install_local("chemin vers le zip")







