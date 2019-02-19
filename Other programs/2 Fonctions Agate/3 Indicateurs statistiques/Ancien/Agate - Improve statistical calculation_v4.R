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
source("Other programs/2 Fonctions Agate/2 Cartographie/Agate - Cartographie fct.R",encoding = "UTF-8")
source("Other programs/2 Fonctions Agate/3 Indicateurs statistiques/Agate - Statistics Zonage_v6.R",encoding = "UTF-8")

# I. Import des bases
#----------------------------------------------------------------------------------------------------------------------------------------------

# Statistical summary about cities and territory
if(file.exists("Data/Statistiques Zonage/StatRegCom_rp14_filo14.RData")){
  load("Data/Statistiques Zonage/StatRegCom_rp14_filo14.RData")  # Real data
  com.stat <- statCom_rp14_filo14
  dep.stat <- statReg_rp14_filo14
}else{
  load("Data/Stats/Region and cities/region_stat.RData") # Fake region and cities stat
}

# O. Selection des bases de travail (donnees reelles ou fausses)
#---------------------------------------------------------------
# Ril
rilPath <- ifelse(file.exists("Data/Ril/ril15.fst"),"Data/Ril/ril15.fst","Data/Ril/FakeRil.fst")
# RP
rpiPath <- ifelse(file.exists("Data/Rp/rp13i.fst"),"Data/Rp/rp13i.fst","Data/Rp/FakeRpi.fst")
rplPath <- ifelse(file.exists("Data/Rp/rp13l.fst"),"Data/Rp/rp13l.fst","Data/Rp/FakeRpl.fst")
# Filosofi
filoPath <- ifelse(file.exists("Data/Filosofi/filo14.fst"),"Data/Filosofi/filo14.fst","Data/Filosofi/FakeFilo.fst")

# I. Preparation du zonage
#-------------------------
# I.1. Creation de la variable zonage
zonage <- readOGR(dsn = "Data/QPV/qpv.shp")[1:2,]
zonage@data <- zonage@data %>% 
  rename(idZonage = CODE_QP,
         idZonage.label = NOM_QP) %>% 
  select(idZonage,idZonage.label)
zonage <- spTransform(zonage, "+init=epsg:3857")


# II. Logements géolocalisés du RIL 
#----------------------------------
load("Data/Maps/Cities/cities.RData") # Cities map
# II.1 Communes dans lesquelles se trouvent une ou plusieurs zones
zoneInter <- gIntersects(zonage,com.dom,byid = TRUE)
test <- apply(zoneInter, 1, function(x){
  test <- sum(x)
  return(ifelse(test>0,TRUE,FALSE))}) 
com.dom.select <- com.dom@data$Codgeo[test]

# II.2. Chargement des logements du RIL dans les communes d'interets
ril <- read_fst(rilPath) %>% 
  select(idx,x,y) %>% 
  mutate(com = substr(idx,1,5)) %>%
  filter(com %in% com.dom.select)

# II.3. Transformation du ril en objet spatial
coordinates(ril) <- ~x+y
ril@proj4string <- CRS("+init=epsg:3857")

# III. Ajout de la zone aux données du recensement
#-------------------------------------------------

# III.1. Zone dans laquelle chaque logement se situe
incProgress(amount = 0.1,message = "Zone dans laquelle chaque logement se situe")
pts.sp <- zonaPts(pts.sp = ril,zonage = zonage)

# III.2. Ajout de la zone aux données du rp individu
# Note : pour des raisons de performances, les données du RP sont préalablement filtrées selon les communes étudiées
incProgress(amount = 0.2,message = "Ajout de la zone aux données du RP")

rpi <- read_fst(rpiPath) %>% 
  filter(idx %in% ril@data$idx) %>% 
  left_join(pts.sp@data[,c("idx","idZonage")], "idx") %>% 
  mutate(idZonage = ifelse(is.na(idZonage),"Hors zonage", idZonage)) %>% 
  left_join(zonage@data,"idZonage")

# III.3. Ajout de la zone aux données du rp logement
rpl <- read_fst(rplPath) %>% 
  filter(idx %in% ril@data$idx) %>% 
  left_join(pts.sp@data[,c("idx","idZonage")], "idx") %>% 
  mutate(idZonage = ifelse(is.na(idZonage),"Hors zonage", idZonage)) %>% 
  left_join(zonage@data,"idZonage")

# IV. Ajout de la zone aux données fiscales
#------------------------------------------

# IV.1. Chargement des données fiscales filtrées selon les communes d'intérêts
incProgress(amount = 0.3,message = "Ajout des données fiscales")
filo <- read_fst(filoPath) %>% 
  filter(com %in% com.dom.select)

# IV.2. Transformation des données en objet spatial
filo.sp <- SpatialPointsDataFrame(coords = filo[,c("x","y")],data = filo,proj4string = CRS("+init=epsg:3857"))

# IV.3. Zone dans laquelle chaque foyer fiscal se situe
filo.sp <- zonaPts(pts.sp = filo.sp,zonage = zonage)

# IV.4. Ajout de la zone aux données fiscales
typmen.label <- c("famille monoparentale","couple sans enfant","couple avec enfant(s)","menage complexe",
                  "femme seule","homme seul")

filo <- filo.sp@data %>% 
  left_join(data.frame(unique(rpl[,c("com","com.lib")])),"com") %>% 
  mutate(dep = substr(com,1,3),
         idZonage = ifelse(is.na(idZonage),"Hors zonage", idZonage),
         typmenR.lib = factor(typmenR,labels = typmen.label)) %>% 
  left_join(zonage@data,"idZonage")
rm(filo.sp)

# Chargement des QPV
# load("Data/QPV/qpvFake.RData")
# zonage <- qpv_stat.fake[1:2,1:3]
# zonage@data <- zonage@data %>% 
#   rename(idZonage = CODE_QP)
# zonage <- spTransform(zonage, "+init=epsg:3857")

# II. Indicateurs statistiques par zones
#---------------------------------------------------------------------------------------------------------------------------------------------

indStat <- statistics_zone(group_var = c("com","idZonage","idZonage.label"),zone = zonage,rpi = rpi,rpl = rpl, filo = filo,sourceRpi = "rpi14",sourceRpl = "rpl14",
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




