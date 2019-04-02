#----------------------------------------------------------------------------------------------------------------------#
#                                   Agate - Dashboard Improvement                                                      #
#----------------------------------------------------------------------------------------------------------------------#

# 01.04.2019

# Amélioration du tableau de bord en trois parties 
# 1) Affichage de chaque widget indépendemment 
# 2) Construction d'une petite application qui affiche le tableau de bord interactif
# 3) intégration dans l'application principale

# Packages nécessaires
#---------------------
library(fst)
library(tidyverse)
library(plotly)
library(DT)

# Fonctions  particulières
source("Other programs/2 Fonctions Agate/4 Dashboard/Agate - Dashboard fct.R",encoding = "UTF-8")

# Paramètres
zone.etude <- "QP971002"
zone.compare <- "QP971001"

# I. Import des données
#-------------------------------------------------------------------------------------------------------------------------

# 0. Label tableau
#-----------------
load("Data/Liste indicateurs statistiques/lstIndicateur.RData")

# I.1. Zone d'étude
#------------------
load("Data/Tmp/qpv_stat_tmp.RData") # Indicateurs statistiques sur deux QPV

an <- "13"
zone.pred.etude <- 4
zone.etude <- "QP971002"

zone.pred.compare <- 1
zone.compare <- "971"

if(zone.pred.compare != 4){
  df <- read_fst("Data/Stats/Prefine aera/Real/fst/indicateur_stat.fst") %>% 
    filter(zone.pred == zone.pred.compare & idZonage == zone.compare & substr(source,4,5) == an) 
  
  df2 <- df %>% 
    filter(nomVariable %in% c("emp_typeActivite","sco_popSco2","sco_diplome","log_cat","log_ach_constru",
                              "log_bati","res_nbPiece","res_surface")) %>% 
    mutate(type.indicateur = "part_p")
  df <- bind_rows(df,df2,df.zone %>% filter(idZonage == zone.etude))
  
  pyr <- read_fst("Data/Stats/Prefine aera/Real/fst/pyramide.fst") %>% 
    filter(zone.pred == zone.pred.compare & idZonage == zone.compare & substr(source,4,5) == an) %>% 
    bind_rows(pyramide %>% filter(idZonage == zone.etude))
}else{
  df <- df.zone %>% filter(idZonage %in% c(zone.etude,zone.compare))
  pyr <- pyramide %>% filter(idZonage %in% c(zone.etude,zone.compare))
}

# II. Lancement
#--------------------------------------------------------------------------------------------------------------------------------
dash.indicateur <- stat.dashboard_agate(df = df,zone.etude = zone.etude, zone.compare = zone.compare, lstIndicateur = lstIndicateur,
                                        pyramide_tr = pyr)

save(dash.indicateur,file = "Data/Tmp/dashboard_tmp.RData")










