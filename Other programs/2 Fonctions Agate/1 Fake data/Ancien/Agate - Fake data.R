#--------------------------------------------------------------------------------------------------------------------------------#
#                     Agate - RP and Filo Fake data                                                                              #
#--------------------------------------------------------------------------------------------------------------------------------#

# MAJ : 3.06.2018

# Nicolas Kempf

# Statistiques descriptives par îlots

### Working directory 
# Paramètre automatiquement le dossier dans lequel se trouve le programme comme espace de travail
# Ne fonction que pour une version de R supérieure à 3.3
setwd(dir=dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Packages nécessaires
#---------------------
library(rgdal) # Chargement des objets spatiaux
library(tidyverse) # transform data
library(writexl) # Export des données en excel

# BBOX pour chaque département
# dep971 <- readOGR("../Cartes/Departements/departement-971/admin-departement.shp") %>% 
#   spTransform(CRSobj = CRS("+init=epsg:3857"))
# dep971@proj4string
# bbox971 <- as.data.frame(bbox(dep971))
# 
# dep972 <- readOGR("../Cartes/Departements/departement-972/admin-departement.shp")%>% 
#   spTransform(CRSobj = CRS("+init=epsg:3857"))
# dep972@proj4string
# bbox972 <- as.data.frame(bbox(dep972))
# 
# dep973 <- readOGR("../Cartes/Departements/departement-973/admin-departement.shp") %>% 
#   spTransform(CRSobj = CRS("+init=epsg:3857"))
# dep973@proj4string
# bbox973 <- as.data.frame(bbox(dep973))
# 
# dep974 <- readOGR("../Cartes/Departements/departement-974/admin-departement.shp") %>%
# spTransform(CRSobj = CRS("+init=epsg:3857"))
# dep974@proj4string
# bbox974 <- as.data.frame(bbox(dep974))
# 
# bboxDom <- list(bbox971,bbox972,bbox973,bbox974)
# 
# # Enregistrement en RData
# save(bboxDom,file="../Bases/R/bboxDom.RData")

load("../Bases/R/bboxDom.RData")

# Tirage d'un échantillon de point dans chaque DOM
sample_spPoints <- function(bbox){
  pts <- data.frame(x = c(bbox$min[1],bbox$max[1]),y = c(bbox$min[2],bbox$max[2]))
  coordinates(pts) <- ~ x + y
  pts <- data.frame( spsample( pts , n = 200000,type = "random"))
  return(pts)
}

pts.sample <- lapply(bboxDom,FUN = sample_spPoints)



# Simule des fausses données ayant la structure de vrai données
faking_data <- function(df){
  L <- lapply(colnames(df), function(x){
    return(sample(df[,x],replace = TRUE))
  })
  df2 <-  data.frame(do.call(data.frame, L))
  colnames(df2) <-  colnames(df)
  return(df2)
}

# Simulation du RP
load("../Bases/R/rp14.RData")

df <- rp14i[1:2000,]
df2 <- faking_data(df)

identical(rpi,rpi)



L <- list(a = 1:4, b = 4:1) # test input

n <- length(L[[1]])
DF <- structure(L, row.names = c(NA, -n), class = "data.frame")



df <- mtcars
# change one of the variable types to an unordered factor
df$carb <- as.factor(df$carb)
# change another variable type to an ordered factor
df$gear <- as.ordered(as.factor(df$gear))
df[2,] <- NA
sim_df <- simulate_dataset(df,n = 10000)

identical(sim_df,df)

