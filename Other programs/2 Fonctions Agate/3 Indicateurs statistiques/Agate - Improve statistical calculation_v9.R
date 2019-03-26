#----------------------------------------------------------------------------------------------------------------------#
#                                   Agata - Statistical calculation improvement                                        #
#----------------------------------------------------------------------------------------------------------------------#

# 26.03.2019

# Improve statistical calculation of indicators and display them into cool datatable (package DT)
# Ajout des travaux de Baptiste Raimbaud
# Suppression du RIL au profil du rp niveau adresse géolocalisé

# Packages nécessaires
#---------------------
library(rgdal) # Input shapefile
library(rgeos) # Map tools
library(tidyverse) # Tidy data
library(fst) # Read partial data
library(fstplyr) # dplyr for fst object
library(DT) # Interactive datatable
library(openxlsx) # Export en excel
library(rlang) # non standard evaluation
library(easySdcTable) # Statistical disclosure

# Fonction necessaire
source("Other programs/2 Fonctions Agate/2 Cartographie/Agate - Cartographie fct.R",encoding = "UTF-8")
source("Other programs/2 Fonctions Agate/3 Indicateurs statistiques/Agate - Statistics Zonage_v8.R",encoding = "UTF-8")
# source("Other programs/2 Fonctions Agate/6 Census Quality/Agate - Census infra quality.R",encoding = "UTF-8")
source("Other programs/2 Fonctions Agate/6 Census Quality/Agate - Qualite du RP.R",encoding = "UTF-8")
source("Other programs/2 Fonctions Agate/7 Statistical disclosure/Agate - statistiscal disclosure.R",encoding = "UTF-8")

# Listes des indicateurs statistiques
load("Data/Liste indicateurs statistiques/lstIndicateur.RData")

# O. Selection des bases de travail (donnees reelles ou fausses)
#---------------------------------------------------------------
t1 <- Sys.time()
# RP
rp.an <- "13"
rpi.path.string <- paste0("Data/Rp/rpi",rp.an,".fst")
rpl.path.string <- paste0("Data/Rp/rpl",rp.an,".fst")
rpa.path.string <- paste0("Data/Rp/rpa",rp.an,".fst")
rpiPath <- ifelse(file.exists(rpi.path.string),rpi.path.string,"Data/Rp/FakeRpi.fst")
rplPath <- ifelse(file.exists(rpl.path.string),rpl.path.string,"Data/Rp/FakeRpl.fst")
rpaPath <- ifelse(file.exists(rpa.path.string),rpa.path.string,NA)
# Filosofi
# filo.an <- "14"
# filo.path.string <- paste0("Data/Filosofi/filo",filo.an,".fst")
# filoPath <- ifelse(file.exists(filo.path.string),filo.path.string,"Data/Filosofi/FakeFilo.fst")

# I. Preparation du zonage
#-------------------------
# I.1. Creation de la variable zonage
zonage <- readOGR(dsn = "Data/QPV/qpv.shp",encoding = "UTF-8",stringsAsFactors = FALSE)[1:2,]
zonage@data <- zonage@data %>% 
  rename(idZonage = CODE_QP,
         idZonage.name = NOM_QP) %>% 
  select(idZonage,idZonage.name)
zonage <- spTransform(zonage, "+init=epsg:3857")

# II. Adresses géolocalisées
#---------------------------
load("Data/Maps/Cities/cities.RData") # Cities map
# II.1 Communes dans lesquelles se trouvent une ou plusieurs zones
zoneInter <- gIntersects(zonage,com.dom,byid = TRUE)
test <- apply(zoneInter, 1, function(x){
  test <- sum(x)
  return(ifelse(test>0,TRUE,FALSE))}) 
com.dom.select <- com.dom@data$Codgeo[test]

# II.2. Adresses géolocalisées
rpa <- read_fst(rpaPath) %>%
  filter(com %in% com.dom.select)

rpa.geo <- rpa %>% 
  filter(!is.na(ril.millesime)) %>% 
  mutate(idx = C_IMM) %>% 
  select(idx,C_IMM,com,x,y,nb_logn.ril)

# II.3. Transformation du ril en objet spatial
coordinates(rpa.geo) <- ~x+y
rpa.geo@proj4string <- CRS("+init=epsg:3857")

# III. Données du recensement + identification de la zone
#--------------------------------------------------------

# III.1. Zone dans laquelle chaque logement se situe (MAJ : 19.03.2019)
incProgress(amount = 0.1,message = "Zone dans laquelle chaque logement se situe")
# rpa.geo <- rpa.geo[duplicated(rpa.geo@data$idx)==F,]
pts.sp <- zonaPts(pts.sp = rpa.geo,zonage = zonage)
pts.df <- pts.sp@data %>% 
  mutate(idZonage = ifelse(is.na(idZonage) | idZonage == "Hors zonage",paste0("horsZon",com), idZonage)) %>%   
  select(-idx,-com)

# III.2. Ajout de la zone aux données du rp individu (MAJ : 19.03.2019)
# Note : pour des raisons de performances, les données du RP sont préalablement filtrées selon les communes étudiées
incProgress(amount = 0.2,message = "Ajout de la zone aux données du RP")

rpi <- read_fst(rpiPath) %>% 
  filter(C_IMM %in% unique(pts.df$C_IMM)) %>% 
  left_join(pts.df, by = c("C_IMM"))

# III.3. Ajout de la zone aux données du rp logement (MAJ : 19.03.2019)
rpl <- read_fst(rplPath) %>% 
  filter(C_IMM %in% unique(pts.df$C_IMM)) %>% 
  left_join(pts.df, by = c("C_IMM")) %>% 
  mutate(idZonage = ifelse(is.na(idZonage) | idZonage == "Hors zonage",paste0("horsZon",com), idZonage))

# IV. Table multicommunes + bridage lié à la qualité de l'appariement
#-----------------------------------------------------------------------
# Qualite de l'appariement
seuil_qualite_appariement <- 30

qualiteAppariement <- read_fst("Data/Rp/appariementRil_Rp.fst") %>% 
  filter(com %in% com.dom.select & an == rp.an) %>% 
  mutate(appariement_diff = ifelse(nonApparie.pct < seuil_qualite_appariement,TRUE,FALSE))

zonage.com <- rpl %>% 
  group_by(dep,com,com.lib,idZonage,idZonage.name) %>% 
  summarise(freq=n()) %>% 
  ungroup() %>% 
  mutate(dep = substr(com,1,3)) %>% 
  left_join(qualiteAppariement,by="com")

# V. Ajout de la zone aux données fiscales
#------------------------------------------

# # V.1. Chargement des données fiscales filtrées selon les communes d'intérêts
# incProgress(amount = 0.3,message = "Ajout des données fiscales")
# filo <- read_fst(filoPath) %>% 
#   filter(com %in% com.dom.select)
# 
# # V.2. Transformation des données en objet spatial
# filo.sp <- SpatialPointsDataFrame(coords = filo[,c("x","y")],data = filo,proj4string = CRS("+init=epsg:3857"))
# 
# # V.3. Zone dans laquelle chaque foyer fiscal se situe
# filo.sp <- zonaPts(pts.sp = filo.sp,zonage = zonage)
# 
# # V.4. Ajout de la zone aux données fiscales
# typmen.label <- c("famille monoparentale","couple sans enfant","couple avec enfant(s)","menage complexe",
#                   "femme seule","homme seul")
# 
# filo <- filo.sp@data %>% 
#   left_join(data.frame(unique(rpl[,c("com","com.lib")])),"com") %>% 
#   mutate(dep = substr(com,1,3),
#          idZonage = ifelse(is.na(idZonage) | idZonage == "Hors zonage",paste0("horsZon",com), idZonage),
#          typmenR.lib = factor(typmenR,labels = typmen.label)) 
# rm(filo.sp)

# VI. Calcul des indicateurs statistiques
#---------------------------------------
incProgress(amount = 0.4,message = "Calcul des statistiques")

# VI.1. Statistiques dans la zone
group_var <- c("idZonage","idZonage.name") # Attention utilisé plusieurs fois
statZone <- statistics_zone(group_var = group_var,zone = zonage,rpi = rpi,rpl = rpl, 
                            lstCategorie = lstCategorie,
                            sourceRp = rp.an,
                            rpi.weight = "IPONDI.cal",
                            rpl.weight = "IPONDL.cal")

# VI.2. Objets pour page "statistiques"
df.zone <- statZone$indicateur_stat
source <- unique(df.zone$source)

# VII. Qualité des données du RP (Travaux Baptiste Raimbaud)
#----------------------------------------------------------
incProgress(amount = 0.5,message = "Qualité des données du rp")

ril <- read_fst("Data/Ril/ril_leger.fst") %>% 
  select(idx,x,y,nb_logn) %>% 
  mutate(com = substr(idx,1,5)) %>%
  filter(com %in% com.dom.select)
coordinates(ril) <- ~x+y
ril@proj4string <- CRS("+init=epsg:3857")
ril.geo <- ril[!duplicated(ril@data$idx),]
ril.geo <- zonaPts(pts.sp = ril.geo,zonage = zonage)
ril <- ril@data %>% 
  left_join(ril.geo@data %>% select(idx,idZonage),by="idx") %>% 
  mutate(idZonage = ifelse(is.na(idZonage) | idZonage == "Hors zonage",paste0("horsZon",com), idZonage))


# VII.1. Chargement de la base adresses (MAJ : 26.03.2019)
rpa.qualite <- rpa %>% 
  left_join(pts.df %>% select(C_IMM,idZonage,idZonage.name), by = c("C_IMM")) %>% 
  filter(ril.millesime == 1) %>% 
  mutate(IPOND = IPOND.cal)

# VII.2. Liste des variables à calculer
group_var.qualite <- c("INPER",lstIndicateur$qualiteIndicateur[paste0(lstIndicateur$nomVariable,lstIndicateur$nomIndicateur) %in% colnames(rpa.qualite)])

# VII.3. Estimation de la qualité
seuil_diffusion <- 5 # Seuil de diffusion de la valeur du coefficient de variation
qualityZone <- Qlfinal(rpa.qualite,group_var.qualite,ril = ril) %>% 
  mutate(val.qualite = ifelse(CoefVariation <= seuil_diffusion & !is.nan(CoefVariation),EstVariable,IntervalConf.)) 
Sys.time() - t1

# VII.4. Ajout des données à la table finale
df.zone <- qualityZone %>% 
  rename(idZonage = zonage,
         qualiteIndicateur = Variable) %>% 
  left_join(lstIndicateur %>% 
              select(domaine,categorie,nomVariable,nomIndicateur,qualiteIndicateur,source),
            by = "qualiteIndicateur") %>% # Ajout de variables
  left_join(zonage@data %>% 
              select(idZonage,idZonage.name)) %>% 
  mutate(source = paste0(source,rp.an)) %>% 
  select(-qualiteIndicateur) %>% 
  gather("type.indicateur","value",-group_var,-nomVariable,-nomIndicateur,-domaine,-categorie,-source) %>% # Transformation de la base
  bind_rows(df.zone %>% mutate(value = as.character(value))) # Ajout des données sur la qualité dans un format simple a exploiter

# VIII. Secret statistique
#-------------------------
incProgress(amount = 0.8,message = "Secret statistique")

# Utiliser la règle des 11 observations minimum par case.
seuil_secret_stat <- 11

# VIII.1. Table de travail pour le secret statistique
df.zone.secret <- df.zone %>% 
  filter(type.indicateur %in% c("freq","n"))

# VIII.2. Catégorie à secretiser
var.secret <- lstCategorie$nomVariable[lstCategorie$typeVar == "pct" & substr(lstCategorie$source,1,2) == "rp"]

# VIII.3. Secret statistique
indicateur.secret <- bind_rows(lapply(var.secret,secret_stat,df.zone.secret = df.zone.secret,seuil_secret_stat = seuil_secret_stat))
Sys.time() - t1

# VIII.4. Ajout du secret aux indicateurs calculés
df.zone <- df.zone.secret %>% 
  left_join(indicateur.secret,by = c("idZonage","nomVariable","nomIndicateur")) %>% # Ajout de la variable diffusable
  mutate(value = ifelse(is.na(diff.secret),"diffusable",diff.secret),
         type.indicateur = "secret_stat") %>% 
  select(-diff.secret) %>% 
  bind_rows(df.zone)

Sys.time() - t1

# VIII.5 Valeur diffusable
df.zone <- df.zone %>% 
  filter(type.indicateur %in% c("val.qualite","secret_stat")) %>% 
  spread(key = type.indicateur, value = value) %>% 
  mutate(secret_stat = ifelse(is.na(secret_stat),"n_diffusable",secret_stat),
         valeur.diffusable = ifelse(secret_stat == "diffusable",val.qualite,"c")) %>%  # c : données confidencielles
  gather("type.indicateur","value",-group_var,-nomVariable,-nomIndicateur,-domaine,-categorie,-source) %>% 
  bind_rows(df.zone)

# IX. Enregistrement temporaire pour test
#----------------------------------------
save(zonage,df.zone,statZone,zonage.com,file = "Data/Tmp/qpv_stat_tmp.RData")



