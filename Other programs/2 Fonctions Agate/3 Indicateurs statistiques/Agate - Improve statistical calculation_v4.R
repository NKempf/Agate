#----------------------------------------------------------------------------------------------------------------------#
#                                   Agata - Statistical calculation improvement                                        #
#----------------------------------------------------------------------------------------------------------------------#

# 15.02.2019

# Improve statistical calculation of indicators and display them into cool datatable (package DT)

# Packages nécessaires
#---------------------
library(rgdal) # Input shapefile
library(rgeos) # Map tools
library(tidyverse) # Tidy data
library(fst) # Read partial data
library(fstplyr) # dplyr for fst object
library(DT) # Interactive datatable
library(openxlsx)

# Fonction necessaire
source("Other programs/2 Fonctions Agate/3 Indicateurs statistiques/Agate - Statistics Zonage_v6.R",encoding = "UTF-8")

# I. Import des bases
#----------------------------------------------------------------------------------------------------------------------------------------------
load("Data/Tmp/FakeDataImproveCalcul.RData")

# Chargement des QPV
# load("Data/QPV/qpvFake.RData")
# zonage <- qpv_stat.fake[1:2,1:3]
# zonage@data <- zonage@data %>% 
#   rename(idZonage = CODE_QP)
# zonage <- spTransform(zonage, "+init=epsg:3857")

# II. Indicateurs statistiques par zones
#---------------------------------------------------------------------------------------------------------------------------------------------

indStat <- statistics_zone(group_var = c("com","idZonage"),zone = zonage,rpi = rpi,rpl = rpl, filo = filo,sourceRpi = "rpi14",sourceRpl = "rpl14",
                        sourceFilo = "filo14",rpi.weight = "IPONDI",rpl.weight = "IPONDL",filo.weight = "nbpersm")

save(indStat,file = "Data/Tmp/qpv_stat_tmp.RData")


# III. Affichage en tableau
#--------------------------------------------------------------------------------------------------------------------------------------------

test <- indStat$indicateur_stat %>% 
  select(source,domaine,categorie,com,idZonage,indicateur,type.indicateur,value) %>% 
  filter(domaine == "Territoire" & categorie == "age") %>% 
  spread(key = type.indicateur, value = value)



datatable(test)

table(indStat$indicateur_stat$domaine,indStat$indicateur_stat$categorie)



# Enregistrement des domaines, des categories et des indicateurs
df <- indStat$indicateur_stat[duplicated(indStat$indicateur_stat$domaine)==F,c("domaine","categorie")]
df2 <- indStat$indicateur_stat[duplicated(indStat$indicateur_stat$categorie)==F,c("domaine","categorie")]
test2 <- indStat$indicateur_stat[duplicated(indStat$indicateur_stat$indicateur)==F,c("domaine","categorie","indicateur")]
write.xlsx(test,file = "Data/Tmp/Agate - indicateurs statistiques.xlsx")

test2 <- indStat$indicateur_stat[duplicated(indStat$indicateur_stat$indicateur)==F,c("domaine","categorie","indicateur")]
write.xlsx(test2,file = "Data/Tmp/Agate - indicateurs statistiques2.xlsx")

unique(test2$domaine)



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




