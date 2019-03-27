#----------------------------------------------------------------------------------------------------------------------#
#                                   Agate - Dashboard Improvement                                                      #
#----------------------------------------------------------------------------------------------------------------------#

# 27.03.2019

# Amélioration du tableau de bord en trois parties 
# 1) Affichage de chaque widget indépendemment 
# 2) Construction d'une petite application qui affiche le tableau de bord interactif
# 3) intégration dans l'application principale

# Packages nécessaires
#---------------------
library(tidyverse)
library(plotly)

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
dmn <- lstDomaine$domaine
names(dmn) <- lstDomaine$labelDomaine

# I.1. Zone d'étude
#------------------
load("Data/Tmp/qpv_stat_tmp.RData") # Indicateurs statistiques sur deux QPV

df.etude <- df.zone %>% 
  filter(idZonage == zone.etude & type.indicateur == "valeur.diffusable") %>% 
  mutate(dashboard = "zone.etude1")

table(df.etude$nomIndicateur)

# Source utilisée pour les indicateurs de l'étude
source.etude <- unique(df.etude$source)

# I.2. Zone de comparaison
#-------------------------
df.compare <- df.zone %>% 
  filter(idZonage == zone.compare & type.indicateur == "valeur.diffusable") %>% 
  mutate(dashboard = "zone.etude2")

# Table de travail
df.dashboard <- bind_rows(df.etude,df.compare) %>% 
  left_join(lstIndicateur %>% select(nomVariable,nomIndicateur,labelIndicateur),by = c("nomVariable","nomIndicateur"))

# Label des colonnes 
dash.label <- c("labelIndicateur","zone.etude1","zone.etude2")
names(dash.label) <- c(" ",
                       unique(df.etude$idZonage.name[df.etude$idZonage == zone.etude]),
                       unique(df.compare$idZonage.name[df.compare$idZonage == zone.compare]))


# II. Thème Territoire
#-------------------------------------------------------------------------------------------------------------------------



# III. Thème Démographie
#-------------------------------------------------------------------------------------------------------------------------

# II.0. Nom de la zone
#---------------------
titreDash <- unique(df.etude$idZonage.name)[1]

# II.1. Part des femmes
#----------------------
vb.dem.fem <- df.dashboard %>% 
  filter(idZonage %in% zone.etude & nomIndicateur == "b_femme") %>% 
  select(value)
vb.dem.fem

# II.2. Population
#-----------------
vb.dem.pop <- df.dashboard %>% 
  filter(idZonage %in% zone.etude & nomIndicateur == "a_population") %>% 
  select(value)
vb.dem.pop <- format(round(as.numeric(vb.dem.pop),digits = 0),digits = 9,decimal.mark=",", big.mark=" ")
vb.dem.pop

# II.3. Superficie 
#-----------------
# TODO
# df.dashboard %>% 
#   filter(idZonage %in% zone.etude & indicateur == "population" & type.indicateur == "superficie") %>% 
#   select(value)

# II.4. Tableau haut gauche : Démographie
#----------------------------------------
df.dem.tab.hg <- df.dashboard %>% 
  filter(nomVariable %in% c("population","dem_sexe","men_npers","dem_lieuNais") & 
           nomIndicateur %in% c("a_population","b_femme","b_1pers","a_depResid")) %>% 
  select(-domaine,-categorie,-nomVariable,-type.indicateur,-source,-idZonage,-idZonage.name,-nomIndicateur) %>% 
  spread(key = dashboard,value = value) 

df.dem.tab.hg <- df.dem.tab.hg[c(4,1,2,3),]

# Affichage
datatable(df.dem.tab.hg,colnames = dash.label,
          rownames = FALSE, options = list(dom = 't'))

# II.5. Tableau haut droit : Ménages
#-----------------------------------
df.dem.tab.hd <- bind_rows(df.dashboard %>% 
  filter(nomVariable %in% c("men_statOcc","dem_residenceAnterieur","men_voiture") & 
           nomIndicateur %in% c("b_proprio","b_mmLog","b_n_voit")),
  df.dashboard %>% 
    filter(nomVariable %in% c("men_statOcc_detail") & 
             nomIndicateur %in% c("d_locataireVide_hlm"))) %>% 
  select(-domaine,-categorie,-nomVariable,-type.indicateur,-source,-idZonage,-idZonage.name,-nomIndicateur) %>% 
  spread(key = dashboard,value = value)
df.dem.tab.hd <- df.dem.tab.hd[c(3,1,4,2),]
df.dem.tab.hd

# Affichage
datatable(df.dem.tab.hd,colnames = dash.label,
          rownames = FALSE, options = list(dom = 't'))

# II.6. Tableau bas droit : Immigration
#--------------------------------------
df.dem.tab.bd <- df.dashboard %>% 
  filter(nomVariable %in% c("dem_immigration","dem_etranger","dem_lieuNais","dem_ancienArrive") & 
           nomIndicateur %in% c("a_immigre","b_etranger","c_etranger","a_moins5","d_20plus")) %>% 
  select(-domaine,-categorie,-nomVariable,-type.indicateur,-source,-idZonage,-idZonage.name,-nomIndicateur) %>% 
  spread(key = dashboard,value = value)
df.dem.tab.bd <- df.dem.tab.bd[c(1,5,4,3,2),]
df.dem.tab.bd

# Affichage
datatable(df.dem.tab.bd,colnames = dash.label,
          rownames = FALSE, options = list(dom = 't'))

# II.7. Graphique bas gauche : pyramide des âges
#-----------------------------------------------
pyramide <- statZone$pyramide_tr %>% 
  select(-type.indicateur,-nomVariable)
g.dem.pyramide <- pyramide_Agate(pyramide = pyramide, zone.etude = zone.etude, zone.compare = zone.compare, lstIndicateur = lstIndicateur)
ggplotly(g.dem.pyramide) 


# III. Thème Emploi
#-------------------------------------------------------------------------------------------------------------------------

# III.1. Population en âge de travailler
#---------------------------------------
vb.emp.popTrav <- df.dashboard %>% 
  filter(idZonage %in% zone.etude & nomIndicateur == "a_de15a64") %>% 
  select(value)
vb.emp.popTrav

# III.2. Taux de chômage
#-----------------------
vb.emp.chom <- df.dashboard %>% 
  filter(idZonage %in% zone.etude & nomIndicateur == "b_chomeur") %>% 
  select(value)
vb.emp.chom 

# III.3. Taux d'inactif 
#----------------------
vb.emp.inact <- df.dashboard %>% 
  filter(idZonage %in% zone.etude & nomIndicateur == "b_autreInactif") %>% 
  select(value)
vb.emp.inact 

# III.4. tableau haut gauche : Marché de l'emploi
#------------------------------------------------
df.emp.tab.hg <- df.dashboard %>% 
  filter(nomVariable %in% c("emp_popTrav","emp_popActive","emp_typeActivite") & 
           nomIndicateur %in% c("a_de15a64","b_autreInactif","a_actifocc","c_retraite")) %>% 
  select(-domaine,-categorie,-nomVariable,-type.indicateur,-source,-idZonage,-idZonage.name,-nomIndicateur) %>% 
  spread(key = dashboard,value = value) 
df.emp.tab.hg <- df.emp.tab.hg[c(2,3,1,4),]
df.emp.tab.hg

# Affichage
datatable(df.emp.tab.hg,colnames = dash.label,
          rownames = FALSE, options = list(dom = 't'))


# III.5. tableau haut droit : Chômage
#------------------------------------
df.emp.tab.hd <- df.dashboard %>% 
  filter(nomVariable %in% c("emp_typeActivite","emp_ancienRech","emp_tempsPartiel") & 
           nomIndicateur %in% c("b_chomeur","c_plus1","b_tempsPartiel")) %>% 
  select(-domaine,-categorie,-nomVariable,-type.indicateur,-source,-idZonage,-idZonage.name,-nomIndicateur) %>% 
  spread(key = dashboard,value = value) 
df.emp.tab.hd

# Affichage
datatable(df.emp.tab.hd,colnames = dash.label,
          rownames = FALSE, options = list(dom = 't'))

# III.6. tableau bas droit : Travail
#-----------------------------------
df.emp.tab.bd <- df.dashboard %>% 
  filter(nomVariable %in% c("emp_statPro","emp_positionActuelle","emp_lieuTravail","emp_modeTransport") & 
           nomIndicateur %in% c("a_salarie","d_cadreEmplo","a_communeResid","b_autreComDep","d_vehicule","e_transportCommun")) %>% 
  select(-domaine,-categorie,-nomVariable,-type.indicateur,-source,-idZonage,-idZonage.name,-nomIndicateur) %>% 
  spread(key = dashboard,value = value) 
df.emp.tab.bd
df.emp.tab.bd <- df.emp.tab.bd[c(4,3,2,1,6,5),]
df.emp.tab.bd

# Affichage
datatable(df.emp.tab.bd,colnames = dash.label,
          rownames = FALSE, options = list(dom = 't'))

# III.7. graphique bas gauche : Type d'activité
#----------------------------------------------
# TODO
# df.dashboard %>% 
#   filter(idZonage %in% zone.etude & indicateur == "population" & type.indicateur == "superficie") %>% 
#   select(value)



















# Enregistrement
save(df.dashboard,titreDash,dash.label,
     vb.dem.fem,vb.dem.pop,df.dem.tab.hg,df.dem.tab.hd,df.dem.tab.bd,g.dem.pyramide,
     vb.emp.popTrav,vb.emp.chom,vb.emp.inact,df.emp.tab.hg,df.emp.tab.hd,df.emp.tab.bd,
     
     file = "Data/Tmp/dashboard_tmp.RData")




