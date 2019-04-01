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

# II. Lancement
#--------------------------------------------------------------------------------------------------------------------------------
dash.indicateur <- stat.dashboard_agate(df = df.zone,zone.etude = zone.etude, zone.compare = zone.compare, lstIndicateur = lstIndicateur,
                                        pyramide_tr = statZone$pyramide_tr)

save(dash.indicateur,file = "Data/Tmp/dashboard_tmp.RData")










