#----------------------------------------------------------------------------------------------------------------------#
#                                   Agata - Statistical calculation improvement                                        #
#----------------------------------------------------------------------------------------------------------------------#

# 23.01.2019

# Improve statistical calculation of indicators

# Packages nécessaires
#---------------------
library(rgdal) # Input shapefile
library(rgeos) # Map tools
library(tidyverse) # Tidy data
library(fst) # Read partial data
library(fstplyr) # dplyr for fst object


source("Other programs/StatZonage/Agate - Statistics Zonage_v5.R",encoding = "UTF-8")

load("Data/Tmp/FakeDataImproveCalcul.RData")


zonage@data$superficie <- gArea(zonage,byid = T)/1000000



# calcul des statistiques sur la zone de deux qpv
qpv_stat <- statistics_zone(group_var = c("com","idZonage"),rpi = rpi,rpl = rpl, filo = filo,sourceRpi = "rpi14",sourceRpl = "rpl14",
                        sourceFilo = "filo14",rpi.weight = "IPONDI",rpl.weight = "IPONDL",filo.weight = "nbpersm")

# Enregistrement
save(qpv_stat,file = "Data/Tmp/qpv_stat_tmp.RData")



# POur ne pas avoir de problème, il vaut mieux fonctionner à l'idZonage et non a la combinaison idZonage + com


group_var <- c("idZonage")
rpi.weight = "IPONDI"
domaine <- "Territoire"
zone <- zonage@data
sourceRpi = "rpi14"

# I.1. Population et densité de population par zonage
#----------------------------------------------------
indicateur_stat <- rpi %>%
  group_by(!!! syms(group_var)) %>% 
  summarise(freq = n(),
            freq_p = round(sum(!!! syms(rpi.weight),na.rm = TRUE),0)) %>% 
  left_join(zone %>% select(idZonage,superficie),"idZonage") %>% 
  mutate(densitepop = round(freq_p / superficie,0)) %>%
  gather("type.indicateur","value",-group_var) %>% 
  mutate(indicateur = "population",
         source = sourceRpi,
         # unite = "nb",
         domaine = "territoire",
         categorie = "population") 


















# Couper le zonage par commune
#------------------------------

# Chargement des QPV
load("Data/QPV/qpvFake.RData")
zonage <- qpv_stat.fake[1:2,1:3]
zonage@data <- zonage@data %>% 
  rename(idZonage = CODE_QP)
zonage <- spTransform(zonage, "+init=epsg:3857")

# Chargement des villes
load("Data/Maps/Cities/cities.RData") # Cities map

# II.1 Communes dans lesquelles se trouvent une ou plusieurs zones
zoneInter <- gIntersects(zonage,com.dom,byid = TRUE)
test <- apply(zoneInter, 1, function(x){
  test <- sum(x)
  return(ifelse(test>0,TRUE,FALSE))}) 
com.dom.select <- com.dom@data$Codgeo[test]



communeTest <- com.dom[test,]
test <- as.data.frame(gArea(gIntersection(communeTest,zonage,byid = T),byid = T))
test$id <- gsub(" ", "_", rownames(test), fixed = TRUE)

test <- test %>% 
  mutate(idCom = word(test$id,1,sep = "\\_"),
         idZone = word(test$id,2,sep = "\\_")) %>% 
  left_join(communeTest@data %>% 
              mutate(idCom = rownames(communeTest@data),
                     com = Codgeo) %>% 
              select(idCom, com),
            'idCom')

communeTest@data %>% 
  mutate(idCom = rownames(communeTest@data))

# test$idCom = substr(rownames(test),1,1)

str_extract(rownames(test), "(.*)(?=\\s)")





gsub(" ", "\\_", "a b", fixed = TRUE)


as.numeric(str_extract(rownames(test), "(?=\\s)(.*)"))

strsplit(rownames(test)," ")
lst <- strsplit(rownames(test)," ")
sapply(lst, '[[', 2) 


test %>% 
  mutate(idCom = rownames(test))

rownames(communeTest@data)
rownames(zonage@data)

names(test)

df <- gIntersects(zonage,test,byid = T)


plot(zonage,border = "red")
plot(test,add = TRUE)
plot(communeTest,border = "green",add=TRUE)

colnames(zoneInter) <- zonage@data$idZonage
commu





group_var <- c("com","idZonage")
rpi.weight = "IPONDI"
domaine <- "Territoire"
zone <- zonage@data


# I.1. Population et densité de population par zonage
#----------------------------------------------------
indicateur_stat <- rpi %>%
  group_by(!!! syms(group_var)) %>% 
  summarise(freq = n(),
            freq_p = round(sum(!!! syms(rpi.weight),na.rm = TRUE),0)) %>% 
  left_join(zone %>% select(idZonage,superficie),group_var)



%>%
  gather("type.indicateur","value",-group_var) %>% 
  mutate(indicateur = "population",
         source = sourceRpi,
         # unite = "nb",
         domaine = "territoire",
         categorie = "population") 




