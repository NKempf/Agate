#----------------------------------------------------------------------------------------------------------------------#
#                                   Agata - Statistical calculation improvement                                        #
#----------------------------------------------------------------------------------------------------------------------#

# 18.02.2019

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
ril.an <- "15"
ril.path.string <- paste0("Data/Ril/ril",ril.an,".fst")
# ril.path.string <- "Data/Ril/ril_leger.fst"
rilPath <- ifelse(file.exists(ril.path.string),ril.path.string,"Data/Ril/FakeRil.fst")
# RP
rp.an <- "14"
rpi.path.string <- paste0("Data/Rp/rp",rp.an,"i.fst")
rpl.path.string <- paste0("Data/Rp/rp",rp.an,"l.fst")
rpiPath <- ifelse(file.exists(rpi.path.string),rpi.path.string,"Data/Rp/FakeRpi.fst")
rplPath <- ifelse(file.exists(rpl.path.string),rpl.path.string,"Data/Rp/FakeRpl.fst")
# Filosofi
filo.an <- "14"
filo.path.string <- paste0("Data/Filosofi/filo",filo.an,".fst")
filoPath <- ifelse(file.exists(filo.path.string),filo.path.string,"Data/Filosofi/FakeFilo.fst")

# I. Preparation du zonage
#-------------------------
# I.1. Creation de la variable zonage
zonage <- readOGR(dsn = "Data/QPV/qpv.shp",encoding = "UTF-8",stringsAsFactors = FALSE)[1:2,]
zonage@data <- zonage@data %>% 
  rename(idZonage = CODE_QP,
         idZonage.name = NOM_QP) %>% 
  select(idZonage,idZonage.name)
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

# III.4. Table multicommunes
#---------------------------
zonage.com <- rpl %>% 
  group_by(dep,com,com.lib,idZonage,idZonage.name) %>% 
  summarise(freq=n()) %>% 
  ungroup() %>% 
  mutate(dep = substr(com,1,3))

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
         typmenR.lib = factor(typmenR,labels = typmen.label)) 
rm(filo.sp)

# II. Indicateurs statistiques par zones
#---------------------------------------------------------------------------------------------------------------------------------------------
indStat <- statistics_zone(group_var = c("idZonage","idZonage.name"),zone = zonage,rpi = rpi,rpl = rpl, filo = filo,
                           sourceRpi = "rpi14",sourceRpl = "rpl14",
                           sourceFilo = "filo14",rpi.weight = "IPONDI",rpl.weight = "IPONDL",filo.weight = "nbpersm")

save(zonage,indStat,zonage.com,file = "Data/Tmp/qpv_stat_tmp.RData")

