#----------------------------------------------------------------------------------------------------------------------------------------#
#                     Agate - Evaluation de la qualité de l'appariement RIL _ RP                                                         #
#----------------------------------------------------------------------------------------------------------------------------------------#

# Nicolas Kempf

# Derniere MAJ : 07.11.2018

# Evaluation de la qualité entre RIL et le RP. Utilisation de la base corrective du CRIEM visant à corriger certains identifiant du ril. 
# Calage éventuel.


# Packages nécessaires
#---------------------
library(sp)
library(tidyverse) # Transformation des données
library(haven) # Lecture de table SAS
library(sampling) # Calage sur marges
library(fst) # Lecture partielle des données 
library(fstplyr) # dplyr pour tables fst

# Fonctions utiles
#-----------------
source("Other programs/Quality/Appariement Ril RP/Catalogue de fonctions/Agate - calage du RP.R")

# I. Import des bases
#-----------------------------------------------------------------------------------------------------------------------------------------

# I.1. Ril 2015 (Ancienne version)
#--------------------------------
load("Data/Ril/ril15.RData")

ril.ancien <- rilhab15@data %>% 
  mutate(ril.ancien = 1,
         x = rilhab15@coords[,1],
         y = rilhab15@coords[,2],
         epsg = "3857") %>% 
  rename(nb_logn.ancien = nb_logn,
         gr.ancien = gr)
rm(rilhab15)

# Vérifications 
sum(is.na(ril$x))
sum(is.na(ril$y))
sum(is.na(ril$epsg))
sum(is.na(ril$idx))
sum(duplicated(ril$idx)==T)

# I.2. Ril millesimé
#-------------------
# Idee : on ne charge que les données necessaire au rp2013. On doit normalement retrouver le même appariement qu'avec la base ril2015
# On ne charge que les années 2011,2012,2013,2014,2015
ril.new <- read_fst("Data/Ril/ril_leger.fst") %>% 
  filter(millesime %in% c(2011,2012,2013,2014,2015)) %>% 
  mutate(ril.new = 1,
         millesime = as.character(millesime)) %>% 
  rename(gr.new = GR,
         nb_logn.new = NB_LOG) 

# Vérifications 
sum(is.na(ril.new$x))
sum(is.na(ril.new$y))
sum(is.na(ril.new$epsg))
sum(is.na(ril.new$idx))
sum(duplicated(ril.new$idx)==T)

# RP 2013
#--------
rpl <- read_fst("Data/Rp/rp13l.fst") %>% 
  mutate(rp = 1)
# rm(rp13i,Rp13dico,rp13l)

str(rpl)


# II. Appariement entre le RP et le RIL
#-------------------------------------------------------------------------------------------------------------------------------------------

# II.1. Appariement entre l'ancien et le nouveau ril
#---------------------------------------------------
ril <- full_join(ril.new[,c("idx","millesime","ril.new","gr.new","nb_logn.new")],
                  ril.ancien[,c("idx",'ril.ancien',"gr.ancien","nb_logn.ancien")],
                  "idx")

# Quelques chiffres rapides
sum(is.na(ril$ril.ancien)) # 930 adresses vides
sum(is.na(ril$ril.new)) # 998 adresses vides
sum(duplicated(ril$idx)==T) # 13 adresses dupliquées

# II.2. Appariement sans correction avec le ril
#----------------------------------------------
ril_rpl <- full_join(ril.ancien,rpl,c("idx"="idrp")) %>% # Yes !!
  select(-com,-com.lib) %>% 
  mutate(com = as.character(substr(idx,1,5)),
         dep = substr(idx,1,3),
         rp = ifelse(is.na(rp),0,rp),
         ril.ancien = ifelse(is.na(ril.ancien),0,ril.ancien),
         riletRp = ifelse(ril.ancien == 1 & rp == 1,1,0),
         rilNonRp = ifelse(ril.ancien == 1 &  rp == 0,1,0),
         nonRilRp = ifelse(ril.ancien == 0 & rp ==1,1,0),
         nonRilnonRp = ifelse(ril.ancien == 0 & rp == 0,1,0)) %>% 
  left_join(rpl[duplicated(rpl$com)==F,c("com","com.lib")],"com") # Ajout de la variable com.lib







# II.3. Appariement sans correction avec le noveau ril
#-----------------------------------------------------
ril_rpl.new <- full_join(ril.new,rpl,c("millesime"="C_ANNEE_COL","idx"="idrp")) %>% # Yes !!
  select(-com,-com.lib) %>% 
  mutate(com = as.character(substr(idx,1,5)),
         dep = substr(idx,1,3),
         rp = ifelse(is.na(rp),0,rp),
         ril.new = ifelse(is.na(ril.new),0,ril.new),
         riletRp = ifelse(ril.new == 1 & rp == 1,1,0),
         rilNonRp = ifelse(ril.new == 1 &  rp == 0,1,0),
         nonRilRp = ifelse(ril.new == 0 & rp ==1,1,0),
         nonRilnonRp = ifelse(ril.new == 0 & rp == 0,1,0)) %>% 
  left_join(rpl[duplicated(rpl$com)==F,c("com","com.lib")],"com") # Ajout de la variable com.lib


# II.2 Statistiques sur l'appariement avant correction avec la base du CRIEM
#---------------------------------------------------------------------------

# Niveau département
tab1 <- ril_rpl %>% 
  group_by(dep) %>% 
  summarise(riletRp = sum(riletRp),
            rilNonRp = sum(rilNonRp),
            nonRilRp = sum(nonRilRp),
            nonRilnonRp = sum(nonRilnonRp)) %>% 
  ungroup() %>% 
  mutate(logRil.total = riletRp + rilNonRp + nonRilRp + nonRilnonRp,
         logRp.total = riletRp + nonRilRp,
         part_riletRp = round(100 * riletRp / logRp.total),
         part_nonRilRp = round(100 * nonRilRp / logRp.total))

tab1.bis <- ril_rpl.new %>% 
  group_by(dep) %>% 
  summarise(riletRp = sum(riletRp),
            rilNonRp = sum(rilNonRp),
            nonRilRp = sum(nonRilRp),
            nonRilnonRp = sum(nonRilnonRp)) %>% 
  ungroup() %>% 
  mutate(logRil.total = riletRp + rilNonRp + nonRilRp + nonRilnonRp,
         logRp.total = riletRp + nonRilRp,
         part_riletRp = round(100 * riletRp / logRp.total),
         part_nonRilRp = round(100 * nonRilRp / logRp.total))

# Niveau communal
tab2 <- ril_rpl %>% 
  group_by(dep,com,com.lib) %>% 
  summarise(riletRp = sum(riletRp),
            rilNonRp = sum(rilNonRp),
            nonRilRp = sum(nonRilRp),
            nonRilnonRp = sum(nonRilnonRp)) %>% 
  ungroup() %>% 
  mutate(logRil.total = riletRp + rilNonRp + nonRilRp + nonRilnonRp,
         logRp.total = riletRp + nonRilRp,
         part_riletRp = round(100 * riletRp / logRp.total),
         part_nonRilRp = round(100 * nonRilRp / logRp.total))


# III. Analyse spécifique de la table de passage fournie par le CRIEM
#--------------------------------------------------------------------------------------------------------------------------------------------

# III.1. Import de la table
#--------------------------
pont <- read_sas("../../../Bdd/SAS/Passage RIL_RP CRIEM/basedfa.sas7bdat")
pont <- pont[substr(pont$id_geo_rp,1,3) %in% c("971","972","973"),]

# Modification de la base
pont2 <- pont %>% 
  filter(substr(id_geo_rp,1,3) %in% c("971","972","973")) %>% 
  rename(C_ANNEE_COL = annee_col) %>% 
  mutate(dep = substr(id_geo_rp,1,3),
         com = substr(id_geo_rp,1,5),
         id.diff = ifelse(id_geo_rp != id_geo_carto,1,0), # Repérage des identifiants différents. 
         id.ril.manquant = ifelse(id_geo_carto == "",1,0),
         id.rp.manquant = ifelse(id_geo_rp == "",1,0),
         C_ANNEE_COL.manquant = ifelse(C_ANNEE_COL=="",1,0),
         id.rp.duplicated = duplicated(id_geo_rp),
         id.ril.duplicated = duplicated(id_geo_carto),
         id.rilRp.duplicated = duplicated(paste0(id_geo_carto,id_geo_rp)),
         id.rilRpAn.duplicated = duplicated(paste0(id_geo_carto,id_geo_rp,C_ANNEE_COL)),
         id.carto = paste(substr(id_geo_carto,1,5),"___",substr(id_geo_carto,6,9),"_",substr(id_geo_carto,10,length(id_geo_carto)),sep = ""),
         id.rp = paste(substr(id_geo_rp,1,5),"___",substr(id_geo_rp,6,9),"_",substr(id_geo_rp,10,length(id_geo_rp)),sep = ""),
         pont = 1
         )

# Indicateurs statistiques
tab3 <- pont2 %>% 
  group_by(dep) %>% 
  summarise(id.diff = sum(id.diff),
            id.ril.manquant = sum(id.ril.manquant),
            id.rp.manquant = sum(id.rp.manquant),
            id.rp.duplicated = sum(id.rp.duplicated),
            id.ril.duplicated = sum(id.ril.duplicated),
            id.rilRp.duplicated = sum(id.rilRp.duplicated),
            id.rilRpAn.duplicated = sum(id.rilRpAn.duplicated),
            nbAdresses = n()) %>% 
  mutate(p_id.diff = round(100 * id.diff / nbAdresses),
         p_id.ril.manquant = round(100 * id.ril.manquant / nbAdresses),
         p_id.rp.manquant = round(100 * id.rp.manquant / nbAdresses)) %>% 
  ungroup()

# III.2. Nettoyage de la base
#----------------------------
pont3 <- pont2 %>% 
  filter(id.diff == 1 & id.ril.manquant == 0 & !id.rilRpAn.duplicated)

# IV. Amélioration de l'appariement RIL et RP
#-------------------------------------------------------------------------------------------------------------------------------------------------------------

# Selection des logements du rp non apparié avec le RP
rp.nonril <- ril_rpl %>% 
  filter(nonRilRp == 1) %>% 
  select(-nb_logn,-gr,-ril,-x,-y,-epsg,-idx.y)

# Appariement rp.nonril et pont3
rp.nonril2 <- rp.nonril %>% 
  select(idx,C_ANNEE_COL,C_GEO,C_IMM,C_LOG) %>% 
  left_join(pont3,c("idx" = "id.rp")) %>% 
  filter(pont == 1)

test <- left_join(rp.nonril2,ril,c("id_geo_carto" = "idx"))

test2 <- test %>% 
  filter(ril == 1) # Conclusion : la table pont n'apporte rien


# V. Calage sur marge à la commune
#---------------------------------------------------------------------------------------------------------------------------------------------------------------

# V.1. Statistiques de cadrages
#------------------------------

# Nombre d'individu par commune
stat.ind <- rp13i %>% 
  group_by(com) %>% 
  summarise(popInd.np = n(),
            popInd.p = sum(IPONDI))

# Nombre de logements par commune
stat.log <- rp13l %>% 
  group_by(com) %>% 
  summarise(popLog.np = n(),
            popLog.p = sum(IPONDL))

# Nombre de logements par commune sur base appariée
stat.apparie <- ril_rpl %>% 
  filter(riletRp == 1) %>% 
  group_by(com) %>% 
  summarise(popLogApa.np = n(),
            popLogApa.p = sum(IPONDL))

# Ecart entre vrai données et données appariées
tab4 <- stat.log %>% 
  left_join(stat.apparie,"com") %>% 
  mutate(part.np = round(100 * (popLog.np - popLogApa.np)/popLog.np,2),
         part.p = round(100 *(popLog.p - popLogApa.p)/popLog.p,2))
  
# V.2. Calage sur marges
#-----------------------------------------------------------------------------------------------------------------

# V.2.1. Niveau logement

# P1 : Preparation des données de calage
# Variables de calage et indicatrices
rpl.calage <- rpl %>% 
  select(idrp,C_ANNEE_COL,C_GEO,C_IMM,C_LOG,com,SURF,TYPL,NBPI,IPONDL) %>% 
  mutate(surf.cal = case_when(SURF %in% c("1","2") ~ "1",
                              SURF %in% c("3","4","5") ~ "2",
                              TRUE ~ "3"),
         typ.cal = case_when(TYPL == "1" ~ "1",
                             TYPL == "2" ~ "2",
                             TRUE ~ "3"),
         nbpi.cal = as.numeric(NBPI),
         
         # Indicatrice de calage
         log.tot = 1,
         surf.cal_1 = 1 * (surf.cal == "1"),
         surf.cal_2 = 1 * (surf.cal == "2"),
         surf.cal_3 = 1 * (surf.cal == "3"),
         typ.cal_1 = 1 * (typ.cal == "1"),
         typ.cal_2 = 1 * (typ.cal == "2"),
         typ.cal_3 = 1 * (typ.cal == "3")
         )

# Totaux des variables
totaux.cal.log <- rpl.calage %>% 
  group_by(com) %>% 
  summarise(log.tot = round(sum(IPONDL)),
            surf.cal_1 = round(sum(surf.cal_1 * IPONDL)),
            surf.cal_2 = round(sum(surf.cal_2 * IPONDL)),
            surf.cal_3 = round(sum(surf.cal_3 * IPONDL)),
            typ.cal_1 = round(sum(typ.cal_1 * IPONDL)),
            typ.cal_2 = round(sum(typ.cal_2 * IPONDL)),
            typ.cal_3 = round(sum(typ.cal_3 * IPONDL)),
            nbpi.cal = round(sum(nbpi.cal * IPONDL))
            )

# Table à caler
ril_rpl.cal <- ril_rpl %>% 
  filter(riletRp == 1) %>% 
  left_join(rpl.calage[,c("idrp","C_LOG","log.tot","surf.cal_1","surf.cal_2","surf.cal_3","typ.cal_1","typ.cal_2","typ.cal_3","nbpi.cal")],c("idx" = "idrp","C_LOG"))

# Calage du poids du RP
list_var <- c("log.tot","typ.cal_1","typ.cal_2","typ.cal_3","nbpi.cal")
rpPoids.cal <- lapply(unique(rpl$com), calage.agate,df = ril_rpl.cal,list_var_cal = list_var,poids = "IPONDL",totaux.cal = totaux.cal.log)
rpPoids.cal <- bind_rows(rpPoids.cal)

# Ajout de la variable poids (IPONDL.agate) à la table ril_rpl
ril_rpl.cal <- ril_rpl.cal %>% 
  left_join(rpPoids.cal[,c("idx","C_LOG","IPOND.agate","calage")],c("idx","C_LOG"))

# Totaux avec le nouveau poids de calage
totaux.cal.ap <- ril_rpl.cal %>% 
  group_by(com) %>% 
  summarise(popLogApa.cal = round(sum(IPOND.agate)),
            surf.cal_1.cal = round(sum(surf.cal_1 * IPOND.agate)),
            surf.cal_2.cal = round(sum(surf.cal_2 * IPOND.agate)),
            surf.cal_3.cal = round(sum(surf.cal_3 * IPOND.agate)),
            typ.cal_1.cal = round(sum(typ.cal_1 * IPOND.agate)),
            typ.cal_2.cal = round(sum(typ.cal_2 * IPOND.agate)),
            typ.cal_3.cal = round(sum(typ.cal_3 * IPOND.agate)),
            nbpi.cal.cal = round(sum(nbpi.cal * IPOND.agate))
  )

cal.tmp <- ril_rpl.cal %>% 
  group_by(com) %>% 
  summarise(calage = sum(calage)) %>% 
  mutate(calage = 1 * (calage>0))

# Statistiques sur l'efficacité du calage
tab4 <- tab4 %>% 
  left_join(totaux.cal.ap[,c("com","popLogApa.cal")],"com") %>% 
  mutate(part.cal = round(100 *(popLog.p - popLogApa.cal)/popLog.p,2),
         amelioration.part = part.p - part.cal) %>% 
  select(com,popLog.np,popLogApa.np,popLog.p,popLogApa.cal,part.p,part.cal,amelioration.part) %>% 
  left_join(cal.tmp,"com") 


# V.2.2. Niveau individu

# Variable de calage : tranche d'age, sexe et ???

colnames(rp13i)

# P1 : Preparation des données de calage
# Variables de calage et indicatrices
rpi <- rp13i %>% 
  select(C_ANNEE_COL,C_GEO,C_IMM,C_LOG,C_IND,idx,com,SEXE,AGEREV,IPONDI) %>% 
  mutate(ind.tot = 1,
         AGEREV = as.numeric(AGEREV),
         trAgerev = case_when(AGEREV < 20 ~ "1",
                              AGEREV >= 20 & AGEREV <65 ~ "2",
                              TRUE ~ "3"),
         # Indicatrices
         trage_1 = 1 * (trAgerev == "1"),
         trage_2 = 1 * (trAgerev == "2"),
         trage_3 = 1 * (trAgerev == "3"),
         sexe_1 = 1 * (SEXE == "homme"),
         sexe_2 = 1 * (SEXE =="femme")
         )

# Totaux des variables
totaux.cal.ind <- rpi %>% 
  group_by(com) %>% 
  summarise(ind.tot = round(sum(ind.tot)),
            trage_1 = round(sum(trage_1 * IPONDI)),
            trage_2 = round(sum(trage_2 * IPONDI)),
            trage_3 = round(sum(trage_3 * IPONDI)),
            sexe_1 = round(sum(sexe_1 * IPONDI)),
            sexe_2 = round(sum(sexe_2 * IPONDI)),
  )


# Table a caler
ril_rpi.cal <- left_join(rpi,ril_rpl[,c("C_ANNEE_COL","C_GEO","C_IMM","C_LOG","riletRp","ril","x","y")],c("C_ANNEE_COL","C_GEO","C_IMM","C_LOG")) %>% 
  filter(riletRp == 1)
  

# Calage du poids du RP
list_var <- c("ind.tot","trage_1","trage_2","trage_3","sexe_1","sexe_2")
rpPoids.cal <- lapply(unique(rpi$com), calage.agate,df = ril_rpi.cal,list_var_cal = list_var,poids = "IPONDI",totaux.cal = totaux.cal.ind)
rpPoids.cal <- bind_rows(rpPoids.cal)

# Ajout de la variable poids (IPONDL.agate) à la table ril_rpl
ril_rpi.cal <- ril_rpi.cal %>% 
  left_join(rpPoids.cal[,c("C_ANNEE_COL","C_GEO","C_IMM","C_LOG","C_IND","IPOND.agate","calage")],c("C_ANNEE_COL","C_GEO","C_IMM","C_LOG","C_IND"))


# Totaux avec le nouveau poids de calage
totauxInd.cal.ap <- ril_rpi.cal %>% 
  group_by(com) %>% 
  summarise(popIndApa.cal = round(sum(IPOND.agate)),
            trage_1.cal = round(sum(trage_1 * IPOND.agate)),
            trage_2.cal = round(sum(trage_2 * IPOND.agate)),
            trage_3.cal = round(sum(trage_3 * IPOND.agate)),
            sexe_1.cal = round(sum(sexe_1 * IPOND.agate)),
            sexe_2.cal = round(sum(sexe_2 * IPOND.agate))
            )

cal.tmp <- ril_rpi.cal %>% 
  group_by(com) %>% 
  summarise(calage = sum(calage)) %>% 
  mutate(calage = 1 * (calage>0))

# Nombre de logements par commune sur base appariée
stat.apparie <- ril_rpi.cal %>% 
  filter(riletRp == 1) %>% 
  group_by(com) %>% 
  summarise(popIndApa.np = n(),
            popIndApa.p = sum(IPONDI))

# Ecart entre vrai données et données appariées
tab5 <- stat.ind %>% 
  left_join(stat.apparie,"com") %>% 
  mutate(part.np = round(100 * (popInd.np - popIndApa.np)/popInd.np,2),
         part.p = round(100 *(popInd.p - popIndApa.p)/popInd.p,2))

# Statistiques sur l'efficacité du calage
tab5 <- tab5 %>% 
  left_join(totauxInd.cal.ap[,c("com","popIndApa.cal")],"com") %>% 
  mutate(part.cal = round(100 *(popInd.p - popIndApa.cal)/popInd.p,2),
         amelioration.part = part.p - part.cal) %>% 
  select(com,popInd.np,popIndApa.np,popInd.p,popIndApa.cal,part.p,part.cal,amelioration.part) %>% 
  left_join(cal.tmp,"com") 



# VI. Enregistrement des bases
#---------------------------------------------------------------------------------------------------------------------------------------------
save(tab1,tab2,tab3,tab4,tab5,file = "../../../Bdd/RData/Qualite/RilRp_appariement.RData")













