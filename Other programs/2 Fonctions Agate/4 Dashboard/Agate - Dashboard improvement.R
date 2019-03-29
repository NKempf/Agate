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
dmn <- lstDomaine$domaine
names(dmn) <- lstDomaine$labelDomaine

# I.1. Zone d'étude
#------------------
load("Data/Tmp/qpv_stat_tmp.RData") # Indicateurs statistiques sur deux QPV

df.etude <- df.zone %>% 
  filter(idZonage == zone.etude & type.indicateur == "valeur.diffusable") %>% 
  mutate(dashboard = "zone.etude1")

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


# II. Thème Synthèse
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
g.dem.pyramide
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
g.emp.typeAct <- barChart_agate(df = df.zone,var.barChart = "emp_typeActivite",
                                zone.etude = zone.etude,zone.compare = zone.compare,lstIndicateur = lstIndicateur)
# Affichage
ggplotly(g.emp.typeAct)

# IV. Thème Scolarisation
#-------------------------------------------------------------------------------------------------------------------------

# IV.1. Population en âge d'être scolarisée
#------------------------------------------
vb.sco.popSco <- df.dashboard %>% 
  filter(idZonage %in% zone.etude & nomIndicateur == "a_popSco") %>% 
  select(value)
vb.sco.popSco

# IV.2. Jeunes en études
#-----------------------
vb.sco.etud <- df.dashboard %>% 
  filter(idZonage %in% zone.etude & nomVariable == "sco_etude" & nomIndicateur == "a_etud") %>% 
  select(value)
vb.sco.etud 

# IV.3. Taux de décrocheur
#-------------------------
vb.sco.decrocheur <- df.dashboard %>% 
  filter(idZonage %in% zone.etude & nomVariable == "sco_decrocheur" & nomIndicateur == "a_decrocheur") %>% 
  select(value)
vb.sco.decrocheur 

# IV.4. tableau haut gauche : Jeunes scolarisés
#----------------------------------------------
df.sco.tab.hg <- df.dashboard %>% 
  filter(nomVariable %in% c("sco_popSco","sco_etude","sco_lieuEtude") & 
           nomIndicateur %in% c("a_popSco","a_etud","a_communeResid","b_autreComDep")) %>% 
  select(-domaine,-categorie,-nomVariable,-type.indicateur,-source,-idZonage,-idZonage.name,-nomIndicateur) %>% 
  spread(key = dashboard,value = value) 
df.sco.tab.hg <- df.sco.tab.hg[c(3,4,2,1),]
df.sco.tab.hg

# Affichage
datatable(df.sco.tab.hg,colnames = dash.label,
          rownames = FALSE, options = list(dom = 't'))

# IV.5. graphique haut droit : Population scolarisée
#---------------------------------------------------
g.sco.pop <- barChart_agate(df = df.zone,var.barChart = "sco_popSco2",
                            zone.etude = zone.etude,zone.compare = zone.compare,lstIndicateur = lstIndicateur)
# Affichage
ggplotly(g.sco.pop)

# IV.6. graphique bas gauche : Dîplome
#-------------------------------------
g.sco.diplome <- barChart_agate(df = df.zone,var.barChart = "sco_diplome",
                                zone.etude = zone.etude,zone.compare = zone.compare,lstIndicateur = lstIndicateur)
# Affichage
ggplotly(g.sco.diplome)

# IV.7. tableau bas droit : Jeunes non scolarisés
#------------------------------------------------
df.sco.tab.bd <- df.dashboard %>% 
  filter(nomVariable %in% c("sco_decrocheur","sco_scolarisation2a5","sco_scolarisation6a15","sco_scolarisation16a24") & 
           nomIndicateur %in% c("a_decrocheur","b_n_scolarise2a5","b_n_scolarise6a15","b_n_scolarise16a24")) %>% 
  select(-domaine,-categorie,-nomVariable,-type.indicateur,-source,-idZonage,-idZonage.name,-nomIndicateur) %>% 
  spread(key = dashboard,value = value) 
df.sco.tab.bd <- df.sco.tab.bd[c(1,3,4,2),]
df.sco.tab.bd

# Affichage
datatable(df.sco.tab.hg,colnames = dash.label,
          rownames = FALSE, options = list(dom = 't'))


# V. Thème Logement
#-------------------------------------------------------------------------------------------------------------------------

# V.1. Nombre de logements
#--------------------------
#TODO (Revoir le calcul)

# V.2. HLM
#----------
vb.log.hlm <- df.dashboard %>% 
  filter(idZonage %in% zone.etude & nomVariable == "log_hlm" & nomIndicateur == "a_hlm") %>% 
  select(value)
vb.log.hlm 

# V.3. Maisons
#--------------
vb.log.maison <- df.dashboard %>% 
  filter(idZonage %in% zone.etude & nomVariable == "log_type" & nomIndicateur == "a_maison") %>% 
  select(value)
vb.log.maison 

# V.4. tableau haut gauche : 
#----------------------------------------------
#TODO
df.log.tab.hg <- df.dashboard %>% 
  filter(nomVariable %in% c("log_hlm","log_type","log_emm") & 
           nomIndicateur %in% c("a_hlm","b_appart","b_av60","e_ap99")) %>% 
  select(-domaine,-categorie,-nomVariable,-type.indicateur,-source,-idZonage,-idZonage.name,-nomIndicateur) %>% 
  spread(key = dashboard,value = value) 
df.log.tab.hg <- df.log.tab.hg[c(4,1,3,2),]
df.log.tab.hg

# Affichage
datatable(df.sco.tab.hg,colnames = dash.label,
          rownames = FALSE, options = list(dom = 't'))

# V.5. graphique haut droit : Categorie de logement
#--------------------------------------------------
g.log.cat <- barChart_agate(df = df.zone,var.barChart = "log_cat",
                            zone.etude = zone.etude,zone.compare = zone.compare,lstIndicateur = lstIndicateur)
# Affichage
ggplotly(g.log.cat)

# V.6. graphique bas gauche : Année d'achevement
#-----------------------------------------------
g.log.ach <- barChart_agate(df = df.zone,var.barChart = "log_ach_constru",
                                zone.etude = zone.etude,zone.compare = zone.compare,lstIndicateur = lstIndicateur)
# Affichage
ggplotly(g.log.ach)

# V.7. graphique bas droit : Aspect du bati
#-------------------------------------------
g.log.bati <- barChart_agate(df = df.zone,var.barChart = "log_bati",
                            zone.etude = zone.etude,zone.compare = zone.compare,lstIndicateur = lstIndicateur)
# Affichage
ggplotly(g.log.bati)


# VI. Thème Résidences principales
#-------------------------------------------------------------------------------------------------------------------------

# VI.1. Part des résidences principales
#--------------------------------------
vb.res.part <- df.dashboard %>% 
  filter(idZonage %in% zone.etude & nomVariable == "log_cat" & nomIndicateur == "a_res_princ") %>% 
  select(value)
vb.res.part

# VI.2. Logements collectifs
#--------------------------
vb.res.collectif <- df.dashboard %>% 
  filter(idZonage %in% zone.etude & nomVariable == "res_type" & nomIndicateur == "b_collectif") %>% 
  select(value)
vb.res.collectif 

# VI.3. TODO
#--------------
#TODO (Revoir le calcul)

# VI.4. tableau haut gauche : Caractéristiques des résidences
#-----------------------------------------------------------
df.res.tab.hg <- df.dashboard %>% 
  filter(nomVariable %in% c("log_cat","res_type","res_eau","res_wc") & 
           nomIndicateur %in% c("a_res_princ","b_collectif","a_eauFroide","a_wc")) %>% 
  select(-domaine,-categorie,-nomVariable,-type.indicateur,-source,-idZonage,-idZonage.name,-nomIndicateur) %>% 
  spread(key = dashboard,value = value) 
df.res.tab.hg
df.res.tab.hg <- df.res.tab.hg[c(4,1,2,3),]
df.res.tab.hg

# Affichage
datatable(df.res.tab.hg,colnames = dash.label,
          rownames = FALSE, options = list(dom = 't'))

# VI.5. graphique haut droit : Nombre de pièces
#---------------------------------------------
g.res.nbp <- barChart_agate(df = df.zone,var.barChart = "res_nbPiece",
                            zone.etude = zone.etude,zone.compare = zone.compare,lstIndicateur = lstIndicateur)
# Affichage
ggplotly(g.res.nbp)

# VI.6. graphique bas gauche : Surface
#------------------------------------
g.res.surf <- barChart_agate(df = df.zone,var.barChart = "res_surface",
                            zone.etude = zone.etude,zone.compare = zone.compare,lstIndicateur = lstIndicateur)
# Affichage
ggplotly(g.res.surf)

# VI.7. tableau bas droit : Équipements
#--------------------------------------
df.res.tab.bd <- df.dashboard %>% 
  filter(nomVariable %in% c("res_bain","res_cuis","res_clim","res_garage") & 
           nomIndicateur %in% c("b_n_bain","b_n_cuis","a_clim","a_garage")) %>% 
  select(-domaine,-categorie,-nomVariable,-type.indicateur,-source,-idZonage,-idZonage.name,-nomIndicateur) %>% 
  spread(key = dashboard,value = value) 
df.res.tab.bd

# Affichage
datatable(df.res.tab.bd,colnames = dash.label,
          rownames = FALSE, options = list(dom = 't'))

# Enregistrement
save(df.dashboard,titreDash,dash.label,
     vb.dem.fem,vb.dem.pop,df.dem.tab.hg,df.dem.tab.hd,df.dem.tab.bd,g.dem.pyramide,
     vb.emp.popTrav,vb.emp.chom,vb.emp.inact,df.emp.tab.hg,df.emp.tab.hd,df.emp.tab.bd,g.emp.typeAct,
     vb.sco.popSco,vb.sco.etud,vb.sco.decrocheur,df.sco.tab.hg,df.sco.tab.bd,g.sco.pop,g.sco.diplome,
     vb.log.hlm,vb.log.maison,df.log.tab.hg,g.log.cat,g.log.ach,g.log.bati,
     vb.res.part,vb.res.collectif,df.res.tab.hg,g.res.nbp,g.res.surf,df.res.tab.bd,
     file = "Data/Tmp/dashboard_tmp.RData")




