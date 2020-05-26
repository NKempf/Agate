#-------------------------------------------------------------------------------------------------------------------#
#                                 Agate - global programm                                                           #
#-------------------------------------------------------------------------------------------------------------------#

library(rgdal)
library(rgeos)
library(leaflet)
library(leaflet.extras) # 
library(rlang) #
library(Hmisc)
library(tidyverse)
library(fst)
# library(fstplyr)
library(shiny) # Graphic web interface
library(shinyBS) # Pop-up windows
library(shinyjs) # Mask buttons/elements on graphic interface
library(shinyWidgets) # Widgets supplementaires
library(shinydashboard) # Tools like infoBox
library(shinycssloaders) # spinner loader
library(plotly)
library(DT)
# library(easySdcTable)
library(rintrojs) # Tutoriel intéractif

# Fonctions  particulières
source("fonctions/Agate-Dashboard.R",encoding = "UTF-8")
source("fonctions/Agate_-_Cartographie_fct.R",encoding = "UTF-8")
source("fonctions/Agate_-_Qualite_du_RP.R",encoding = "UTF-8")
source("fonctions/Agate_-_statistiscal_disclosure.R",encoding = "UTF-8")
source("fonctions/Agate_-_Statistics_Zonage_v10.R",encoding = "UTF-8")

# Label et choix
load("data/ListeIndicateurs/lstIndicateur.RData")
dmn <- lstDomaine$domaine
names(dmn) <- lstDomaine$labelDomaine
typInd <- lstTypeIndicateur$typeIndicateur
names(typInd) <- lstTypeIndicateur$labelTypeIndicateur

# heatPoints
load("data/heatpoints/heatpoints.Rdata") 

# Départements
load("data/zonesPred/zp_departements.RData")
dep.dom <- spTransform(dep.dom, "+init=epsg:4326")

# Communes
load("data/zonesPred/zp_communes.RData")
com.dom <- spTransform(com.dom, "+init=epsg:4326")

# QPV
load("data/zonesPred/zp_qpv.RData")
qpv <- spTransform(qpv, "+init=epsg:4326")

# Markdown files
# qualiteRpFile <- "Other programs/3 Documentation/1 Agate documentation/2 Qualite RP/NoteQualite.Rmd"
# rmdfiles <- c(qualiteRpFile)
# sapply(rmdfiles, knit, quiet = T)

# NavBar Spéciale :)
navbarPageWithInputs <- function(..., inputs) {
  navbar <- navbarPage(...)
  form <- tags$form(class = "navbar-form", inputs)
  navbar[[3]][[1]]$children[[1]] <- htmltools::tagAppendChild(
    navbar[[3]][[1]]$children[[1]], form)
  navbar
}


# # Correctif de bases
# lstZonePreType <- lstZonePreType %>% 
#   mutate(dep2 = dep,
#          dep = ifelse(dep == 0, idZonage, dep),
#          dep = ifelse(idZonage %in% c("977","978"),"971",dep))





