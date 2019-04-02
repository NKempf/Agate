#----------------------------------------------------------------------------------------------------------------------#
#                                   Agata - Statistical calculation improvement                                        #
#----------------------------------------------------------------------------------------------------------------------#

# 01.04.2019

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
# source("Other programs/2 Fonctions Agate/2 Cartographie/Agate - Cartographie fct.R",encoding = "UTF-8")
source("Other programs/2 Fonctions Agate/3 Indicateurs statistiques/Agate - Statistics Zonage_v9.R",encoding = "UTF-8")
# source("Other programs/2 Fonctions Agate/6 Census Quality/Agate - Qualite du RP.R",encoding = "UTF-8")
# source("Other programs/2 Fonctions Agate/7 Statistical disclosure/Agate - statistiscal disclosure.R",encoding = "UTF-8")
# 


# I. Import des données
#---------------------------------------------------------------------------------------------------------------------------------------------
# Zonage
zonage <- readOGR(dsn = "Data/QPV/qpv.shp",encoding = "UTF-8",stringsAsFactors = FALSE)[1:2,]
zonage@data <- zonage@data %>% 
  rename(idZonage = CODE_QP,
         idZonage.name = NOM_QP) %>% 
  select(idZonage,idZonage.name)

# Villes
load("Data/Maps/Cities/cities.RData") # Cities map

# Listes des indicateurs statistiques
load("Data/Liste indicateurs statistiques/lstIndicateur.RData")



# Paramètres fonction
rp.an <- "13"
zonage <- zonage
com.dom <- com.dom
zoneType <- "QPV"
seuil_qualite_appariement <- 30
group_var <- c("idZonage","idZonage.name") # Attention utilisé plusieurs fois
rpi.weight = "IPONDI.cal"
rpl.weight = "IPONDL.cal"
seuil_diffusion_qualite <- 5 # Seuil de diffusion de la valeur du coefficient de variation
seuil_secret_stat <- 11 # Seuil fixé à 11 observations minimum par case conformément aux recommandations d'utilisation des fichiers filosofi

# II. Agate statistiques du Recensement de la population
#--------------------------------------------------------------------------------------------------------------------------------------------
t1 <- Sys.time()
test <- agate_statRp(rp.an = "13",zonage = zonage,group_var = c("idZonage","idZonage.name"),com.dom = com.dom,
                     zoneType = "QPV",rpi.weight = "IPONDI.cal",rpl.weight = "IPONDL.cal")
Sys.time() - t1






agate_statRp <- function(rp.an,zonage,group_var,com.dom,rpi.weight,rpl.weight,
                         zoneType = "",seuil_qualite_appariement = 30,seuil_diffusion_qualite = 5,seuil_secret_stat=11){


# Selection des bases du RP
rpi.path.string <- paste0("Data/Rp/rpi",rp.an,".fst")
rpl.path.string <- paste0("Data/Rp/rpl",rp.an,".fst")
rpa.path.string <- paste0("Data/Rp/rpa",rp.an,".fst")
rpiPath <- ifelse(file.exists(rpi.path.string),rpi.path.string,"Data/Rp/FakeRpi.fst")
rplPath <- ifelse(file.exists(rpl.path.string),rpl.path.string,"Data/Rp/FakeRpl.fst")
rpaPath <- ifelse(file.exists(rpa.path.string),rpa.path.string,NA)




#--------------------------------------------------------------------------------------#
#                                   Cartographie                                       #
#--------------------------------------------------------------------------------------#

zonage <- spTransform(zonage, "+init=epsg:3857")

# 1. Intersection zone + communes
zoneInter <- gIntersects(zonage,com.dom,byid = TRUE)
test <- apply(zoneInter, 1, function(x){
  test <- sum(x)
  return(ifelse(test>0,TRUE,FALSE))}) 
com.dom.select <- com.dom@data$Codgeo[test]

# 2. adresses
rpa <- read_fst(rpaPath) %>%
  filter(com %in% com.dom.select)

rpa.geo <- rpa %>% 
  filter(!is.na(ril.millesime)) %>% 
  mutate(idx = C_IMM) %>% 
  select(idx,C_IMM,com,x,y,nb_logn.ril)

coordinates(rpa.geo) <- ~x+y
rpa.geo@proj4string <- CRS("+init=epsg:3857")

# incProgress(amount = 0.1,message = "Zone dans laquelle chaque logement se situe")

# 3. Intersection entre le recensement de la population géolocalisé et les zones d'études
pts.sp <- zonaPts(pts.sp = rpa.geo,zonage = zonage)
pts.df <- pts.sp@data %>% 
  mutate(idZonage = ifelse(is.na(idZonage) | idZonage == "Hors zonage",paste0("horsZon",com,zoneType), idZonage)) %>%   
  select(-idx,-com)

# incProgress(amount = 0.2,message = "Ajout de la zone aux données du RP")

rpi <- read_fst(rpiPath) %>% 
  select(-com.lib) %>% 
  filter(C_IMM %in% unique(pts.df$C_IMM)) %>% 
  left_join(pts.df, by = c("C_IMM")) %>% 
  left_join(com.dom@data %>% select(Codgeo,Libgeo), by = c("com"="Codgeo")) %>% 
  mutate(com.lib = Libgeo,
         idZonage = ifelse(is.na(idZonage) | idZonage == "Hors zonage",paste0("horsZon",com,zoneType), idZonage),
         idZonage.name = ifelse(is.na(idZonage.name) & substr(idZonage,1,7) == "horsZon",
                                paste0(com.lib," (hors zone ",zoneType,")"),idZonage.name))

rpl <- read_fst(rplPath) %>% 
  select(-com.lib) %>% 
  filter(C_IMM %in% unique(pts.df$C_IMM)) %>% 
  left_join(pts.df, by = c("C_IMM")) %>% 
  left_join(com.dom@data %>% select(Codgeo,Libgeo), by = c("com"="Codgeo")) %>% 
  mutate(com.lib = Libgeo,
         idZonage = ifelse(is.na(idZonage) | idZonage == "Hors zonage",paste0("horsZon",com,zoneType), idZonage),
         idZonage.name = ifelse(is.na(idZonage.name) & substr(idZonage,1,7) == "horsZon",
                                paste0(com.lib," (hors zone ",zoneType,")"),idZonage.name))

# 4. Qualite de l'appariement
qualiteAppariement <- read_fst("Data/Rp/appariementRil_Rp.fst") %>% 
  filter(com %in% com.dom.select & an == rp.an) %>% 
  mutate(appariement_diff = ifelse(nonApparie.pct < seuil_qualite_appariement,TRUE,FALSE))

zonage.com <- rpl %>% 
  group_by(dep,com,com.lib,idZonage,idZonage.name) %>% 
  summarise(freq=n()) %>% 
  ungroup() %>% 
  left_join(qualiteAppariement,by="com")

#--------------------------------------------------------------------------------------#
#                                   Statistiques                                       #
#--------------------------------------------------------------------------------------#

# incProgress(amount = 0.4,message = "Calcul des statistiques")

# 1. Indicateurs statistiques dans chaque zone et hors des zones
#---------------------------------------------------------------
statZone <- statistics_zone(group_var = group_var,zone = zonage,rpi = rpi,rpl = rpl, 
                            lstCategorie = lstCategorie,
                            sourceRp = rp.an,
                            rpi.weight = rpi.weight,
                            rpl.weight = rpl.weight)

df.zone <- statZone$indicateur_stat


# 2. Qualité des données du RP (Travaux Baptiste Raimbaud)
#---------------------------------------------------------
# incProgress(amount = 0.5,message = "Qualité des données du rp")

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
  mutate(idZonage = ifelse(is.na(idZonage) | idZonage == "Hors zonage",paste0("horsZon",com,zoneType), idZonage))

rpa.qualite <- rpa %>% 
  left_join(rpl %>% 
              filter(!duplicated(C_IMM)) %>%
            select(C_IMM,idZonage,idZonage.name), by = c("C_IMM")) %>% 
  filter(ril.millesime == 1) %>% 
  mutate(IPOND = IPOND.cal)

# Indicateurs de qualité (variance, coefficient de variation, intervalle de confiance, etc.)
group_var.qualite <- c("INPER",lstIndicateur$qualiteIndicateur[paste0(lstIndicateur$nomVariable,lstIndicateur$nomIndicateur) %in% colnames(rpa.qualite) & 
                         lstIndicateur$calculQualite == 1])
qualityZone <- Qlfinal(rpa.qualite,group_var.qualite,ril = ril) %>% 
  mutate(val.qualite = ifelse(CoefVariation <= seuil_diffusion_qualite & !is.nan(CoefVariation),EstVariable,IntervalConf.)) 

df.zone <- qualityZone %>% 
  rename(idZonage = zonage,
         qualiteIndicateur = Variable) %>% 
  left_join(rpl %>% filter(!duplicated(idZonage)) %>% 
              select(idZonage,idZonage.name),by="idZonage") %>% 
  left_join(lstIndicateur %>% 
              select(domaine,categorie,nomVariable,nomIndicateur,qualiteIndicateur,source),
            by = "qualiteIndicateur") %>% # Ajout de variables
  mutate(source = paste0(source,rp.an)) %>% 
  select(-qualiteIndicateur) %>% 
  gather("type.indicateur","value",-group_var,-nomVariable,-nomIndicateur,-domaine,-categorie,-source) %>% # Transformation de la base
  bind_rows(df.zone %>% mutate(value = as.character(value))) # Ajout des données sur la qualité dans un format simple a exploiter

# 3 Secret statistique
#---------------------
# incProgress(amount = 0.8,message = "Secret statistique")

df.zone.secret <- df.zone %>% 
  filter(type.indicateur %in% c("freq","n"))

var.secret <- lstCategorie$nomVariable[lstCategorie$typeVar == "pct" & substr(lstCategorie$source,1,2) == "rp"]

# Secret statistique
indicateur.secret <- bind_rows(lapply(var.secret,secret_stat,df.zone.secret = df.zone.secret,seuil_secret_stat = seuil_secret_stat))

df.zone <- df.zone.secret %>% 
  left_join(indicateur.secret,by = c("idZonage","nomVariable","nomIndicateur")) %>% # Ajout de la variable diffusable
  mutate(value = ifelse(is.na(diff.secret),"diffusable",diff.secret),
         type.indicateur = "secret_stat") %>% 
  select(-diff.secret) %>% 
  bind_rows(df.zone)

df.zone <- df.zone %>% 
  filter(type.indicateur %in% c("freq_p","superficie","val.qualite","secret_stat")) %>% 
  spread(key = type.indicateur, value = value) %>% 
  mutate(secret_stat = case_when(is.na(secret_stat) & nomVariable == "superficie" ~ "diffusable",
                                 is.na(secret_stat) & nomVariable != "superficie" ~ "n_diffusable",
                                 TRUE ~ secret_stat),
         valeur.diffusable = case_when(secret_stat == "diffusable" & !is.na(val.qualite) ~ val.qualite,
                                       secret_stat == "diffusable" & is.na(val.qualite) & nomVariable %in% c("log_tot") ~ freq_p,
                                       secret_stat == "diffusable" & is.na(val.qualite) & nomVariable %in% c("superficie") ~ superficie,
                                       TRUE ~ "c")) %>%  # c : données confidencielles
  select(-secret_stat,-val.qualite,-freq_p,-superficie) %>% 
  gather("type.indicateur","value",-group_var,-nomVariable,-nomIndicateur,-domaine,-categorie,-source) %>% 
  bind_rows(df.zone)


return(list(df.zone = df.zone,
            pyramide = statZone$pyramide_tr,
            zonage.com = zonage.com))

}

# IX. Enregistrement temporaire pour test
#----------------------------------------
save(zonage,df.zone,statZone,zonage.com,file = "Data/Tmp/qpv_stat_tmp.RData")







