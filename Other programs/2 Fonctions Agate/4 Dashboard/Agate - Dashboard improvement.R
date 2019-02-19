#----------------------------------------------------------------------------------------------------------------------#
#                                   Agate - Dashboard Improvement                                                      #
#----------------------------------------------------------------------------------------------------------------------#

# 18.02.2019

# Amélioration du tableau de bord en trois parties 
# 1) Affichage de chaque widget indépendemment 
# 2) Construction d'une petite application qui affiche le tableau de bord interactif
# 3) intégration dans l'application principale

# Packages nécessaires
#---------------------
library(tidyverse)
library(plotly)

# Fonction necessaire


# I. Import des données
#-------------------------------------------------------------------------------------------------------------------------

# Paramètre temporaire
zone.selection <- 3

# 0. Label tableau
#-----------------
load("Data/Liste indicateurs statistiques/lstIndicateur.RData")
dmn <- lstDomaine$idDomaine
names(dmn) <- lstDomaine$labelDomaine

# I.1. Zone d'étude
#------------------
load("Data/Tmp/qpv_stat_tmp.RData") # Indicateurs statistiques sur deux QPV

# Selection d'une zone
zone.etude <- "QP971002"
df.etude <- indStat$indicateur_stat %>% 
  filter(idZonage == zone.etude & type.indicateur != "part_np") %>% 
  select(source,domaine,categorie,idZonage,idZonage.name,indicateur,type.indicateur,value) %>% 
  mutate(dashboard = "zone.etude1")

# Source utilisée pour les indicateurs de l'étude
source.etude <- unique(df.etude$source)

# I.2. Zone de comparaison
#-------------------------
if(zone.selection == 4){ #Si la zone de comparaison fait partie des zones d'études
  zone.compare <- "QP971001"
  df.compare <- indStat$indicateur_stat %>% 
    filter(idZonage == zone.compare & type.indicateur != "part_np") %>% 
    select(source,domaine,categorie,idZonage,idZonage.name,indicateur,type.indicateur,value)
}else{ # Sinon, elle fait partie des zones predefinies
  zone.compare <- "971"
  df.compare <- read_fst("Data/Stats/Prefine aera/Real/fst/indicateur_stat.fst") %>%
    select(zone.predefine,source,domaine,categorie,idZonage,idZonage.name,indicateur,type.indicateur,value) %>%
    filter(type.indicateur != "part_np" & zone.predefine == 1 & idZonage == zone.compare & source %in% source.etude) %>% 
    select(-zone.predefine)
  }

# Table de travail
df.dashboard <- bind_rows(df.etude,df.compare %>% mutate(dashboard = "zone.etude2"))

# Label des colonnes 
dash.label <- c("indicateur","zone.etude1","zone.etude2")
names(dash.label) <- c("Indicateurs",
                       unique(df.etude$idZonage.name[df.etude$idZonage == zone.etude]),
                       unique(df.compare$idZonage.name[df.compare$idZonage == zone.compare]))

# II. Thème Territoire
#-------------------------------------------------------------------------------------------------------------------------

# II.0. Nom de la zone
#---------------------
unique(df.etude$idZonage.name)[1]

# II.1. Population
#-----------------
df.dashboard %>% 
  filter(idZonage %in% zone.etude & indicateur == "population" & type.indicateur == "freq_p") %>% 
  select(value)

# II.2. Densité de population (Attention pour l'instant ce chiffre est faux pour les zones à cheval sur plusieurs communes)
#----------------------------
df.dashboard %>% 
  filter(idZonage %in% zone.etude & indicateur == "population" & type.indicateur == "densitepop") %>% 
  select(value)

# II.3. Superficie
#-----------------
df.dashboard %>% 
  filter(idZonage %in% zone.etude & indicateur == "population" & type.indicateur == "superficie") %>% 
  select(value)

# II.4 Tableau gauche 
#--------------------
df.tab <- df.dashboard %>% 
  filter(domaine == 1 & categorie %in% c(1) & indicateur == "population" & type.indicateur %in% c("freq_p","densitepop","superficie")) %>% 
  select(dashboard,type.indicateur,value) %>% 
  spread(key = dashboard,value = value) %>% 
  left_join(lstTypeIndicateur %>% select(typeIndicateur,labelTypeIndicateur),c("type.indicateur" = "typeIndicateur")) %>% 
  rename(indicateur = type.indicateur) %>% 
  mutate(indicateur = labelTypeIndicateur) %>% 
  select(-labelTypeIndicateur)
df.tab

# Affichage
datatable(df.tab,colnames = dash.label,
          rownames = FALSE, options = list(dom = 't'))

# TODO
# Ajouter la liste des communes, la part des habitants dans la ou les communes, la part des habitants dans le département
# Liste commune
zonage.com %>% 
  ungroup() %>% 
  filter(idZonage == zone.etude) %>% 
  select(com,com.lib)

# II.5 Tableau de droite
#-----------------------
df.tab <- df.dashboard %>% 
  filter(domaine == 1 & categorie %in% c(2,3) & indicateur %in% c("femme","[0,20)","[65,120]","[75,120]") & type.indicateur %in% c("part_p")) %>% 
  select(dashboard,indicateur,value) %>% 
  spread(key = dashboard,value = value) %>% 
  left_join(lstIndicateur %>% select(nomIndicateur,labelIndicateur),c("indicateur" = "nomIndicateur")) %>% 
  mutate(indicateur = labelIndicateur) %>% 
  select(-labelIndicateur)
df.tab

# Affichage
datatable(df.tab,colnames = dash.label,
          rownames = FALSE, options = list(dom = 't'))

# II.6. Carte
#------------
# TODO

# II.7. Composition des ménages
#------------------------------
# TODO

# II. Thème Revenu
#-------------------------------------------------------------------------------------------------------------------------

# II.1. Niveau de vie moyen
#--------------------------
df.dashboard %>% 
  filter(idZonage %in% zone.etude & domaine == 2 & categorie == 1 & indicateur == "nivviem" & type.indicateur == "avg") %>% 
  select(value)

# II.2. Taux de pauvreté à 60 % du seuil métropolitain
#-----------------------------------------------------
df.dashboard %>% 
  filter(idZonage %in% zone.etude & domaine == 2 & categorie == 3 & indicateur == "pauvrete" & type.indicateur == "tx_pauv60.ind.metro") %>% 
  select(value)

# II.3. Nombre de personnes sous le seuil de pauvreté
#----------------------------------------------------
# TODO
# Problème du nombre de personnes sous le seuil de pauvreté car filosofi sous estime le nombre de personnes. Il faudrait donc le calculer
# en utilisant la population du RP
df.dashboard %>% 
  filter(idZonage %in% zone.etude & domaine == 2 & categorie == 3 & indicateur == "pauvrete" & type.indicateur == "freq.pauv60.ind.metro") %>% 
  select(value)

# II.4. Boxplot du niveau de vie
#-------------------------------
# TODO

# II.5.Tableau haut droit
# -----------------------
# TODO : calculer l'indice de Gini 
df.tab <- df.dashboard %>% 
  filter(domaine == 2 & categorie %in% c(1,3) & indicateur %in% c("pauvrete","nivviem") & 
           type.indicateur %in% c("tx_pauv40.ind.metro","tx_pauv50.ind.metro","tx_pauv60.ind.metro","d9_d1","q4_q1")) %>% 
  select(dashboard,type.indicateur,value) %>% 
  spread(key = dashboard,value = value) %>% 
  left_join(lstTypeIndicateur %>% select(typeIndicateur,labelTypeIndicateur),c("type.indicateur" = "typeIndicateur")) %>% 
  rename(indicateur = type.indicateur) %>% 
  mutate(indicateur = labelTypeIndicateur) %>% 
  select(-labelTypeIndicateur)
df.tab

# Affichage
datatable(df.tab,colnames = dash.label,
          rownames = FALSE, options = list(dom = 't'))

# II.6. Courbe de lorenz ou composition du revenu
#------------------------------------------------
# TODO

# II.7. Tableau bas droit
#------------------------
df.tab <- df.dashboard %>% 
  filter(domaine == 2 & categorie %in% c(4) & 
           indicateur %in% c("nivviem_femme seule","nivviem_homme seul","nivviem_famille monoparentale","nivviem_couple sans enfant",
                             "nivviem_couple avec enfant(s)","nivviem_menage complexe") & type.indicateur %in% c("avg")) %>% 
  select(dashboard,indicateur,value) %>% 
  spread(key = dashboard,value = value) %>% 
  left_join(lstIndicateur %>% select(nomIndicateur,labelIndicateur),c("indicateur" = "nomIndicateur")) %>% 
  mutate(indicateur = labelIndicateur) %>% 
  select(-labelIndicateur)
df.tab

# Affichage
datatable(df.tab,colnames = dash.label,
          rownames = FALSE, options = list(dom = 't'))

# III. Thème Emploi
#-------------------------------------------------------------------------------------------------------------------------

# III.1. Taux d'emploi
#---------------------
df.dashboard %>% 
  filter(idZonage %in% zone.etude & domaine == 3 & categorie %in% c(1) & 
           indicateur %in% c("actifocc") & type.indicateur %in% c("part_p")) %>% 
  select(value)

# III.2. Taux de chômeur
#-----------------------
df.dashboard %>% 
  filter(idZonage %in% zone.etude & domaine == 3 & categorie %in% c(1) & 
           indicateur %in% c("chomeur") & type.indicateur %in% c("part_p")) %>% 
  select(value)

# III.3. Taux d'actif
#--------------------
df.dashboard %>% 
  filter(idZonage %in% zone.etude & domaine == 3 & categorie %in% c(1) & 
           indicateur %in% c("actif") & type.indicateur %in% c("part_p")) %>% 
  select(value)

# III.4. Graphique à determiner
#------------------------------
# TODO

# III.5. Tableau droit
#---------------------
df.tab <- bind_rows(df.dashboard %>% 
  filter(domaine == 3 & categorie %in% c(1,2) & indicateur %in% c("actif","inactif","chomeur") & 
           type.indicateur %in% c("freq_p")),
  df.dashboard %>% 
    filter(domaine == 3 & categorie %in% c(2,3) & indicateur %in% c("actif femme","actif homme","cadre_prof_inter") & 
             type.indicateur %in% c("part_p"))) %>% 
  select(dashboard,indicateur,value) %>% 
  spread(key = dashboard,value = value) %>% 
  left_join(lstIndicateur %>% select(nomIndicateur,labelIndicateur),c("indicateur" = "nomIndicateur")) %>% 
  mutate(indicateur = labelIndicateur) %>% 
  select(-labelIndicateur)
df.tab

# Affichage
datatable(df.tab,colnames = dash.label,
          rownames = FALSE, options = list(dom = 't'))
