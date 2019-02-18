#----------------------------------------------------------------------------------------------------------------------#
#                                   Agate - Dashboard                                                                  #
#----------------------------------------------------------------------------------------------------------------------#

# 18.02.2019

# Amélioration du tableau de bord en trois parties 
# 1) Affichage de chaque widget indépendemment 
# 2) Construction d'une petite application qui affiche le tableau de bord interactif
# 3) intégration dans l'application principale

# Packages nécessaires
#---------------------
library(tidyverse)

# Fonction necessaire


# I. Import des données
#-------------------------------------------------------------------------------------------------------------------------
load("Data/Tmp/qpv_stat_tmp.RData") # Indicateurs statistiques sur deux QPV

# Selection d'une zone
zone.selection <- "QP971002"
df.zone <- indStat$indicateur_stat %>% 
  filter(idZonage == zone.selection)

# II. Thème Territoire
#-------------------------------------------------------------------------------------------------------------------------

# II.1. Population
#-----------------
df.zone %>% 
  filter(indicateur == "population" & type.indicateur == "freq_p") %>% 
  select(value)

# II.2. Densité de population (Attention pour l'instant ce chiffre est faux pour les zones à cheval sur plusieurs communes)
#----------------------------
df.zone %>% 
  filter(indicateur == "population" & type.indicateur == "densitepop") %>% 
  select(value)

# II.3. Superficie
#-----------------
df.zone %>% 
  filter(indicateur == "population" & type.indicateur == "superficie") %>% 
  select(value)

# II.4 Tableau gauche

