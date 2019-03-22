#--------------------------------------------------------------------------------------------------------------------------------#
#                 Agate - Gestion de la liste des indicateurs                                                                    #
#--------------------------------------------------------------------------------------------------------------------------------#

# MAJ : 18.02.2019

# Nicolas Kempf

# Transformation de la liste des indicateurs en base fst

# Packages
library(readxl) # Load excel files
library(fst) # fst tables
library(tidyverse) # Transform data
library(openxlsx)


# I. Data import
#------------------------------------------------------------------------------------------------------------------------------
lstDomaine <- read_xlsx("Data/Liste indicateurs statistiques/Agate - indicateurs statistiques.xlsx",sheet = "domaine")
lstCategorie <- read_xlsx("Data/Liste indicateurs statistiques/Agate - indicateurs statistiques.xlsx",sheet = "categorie")
lstIndicateur <- read_xlsx("Data/Liste indicateurs statistiques/Agate - indicateurs statistiques.xlsx",sheet = "indicateur")
lstTypeIndicateur <- read_xlsx("Data/Liste indicateurs statistiques/Agate - indicateurs statistiques.xlsx",sheet = "type indicateur")
lstPredefine <- read_xlsx("Data/Liste indicateurs statistiques/Agate - indicateurs statistiques.xlsx",sheet = "Zone predefinie")


typInd <- lstTypeIndicateur$typeIndicateur
names(typInd) <- lstTypeIndicateur$labelTypeIndicateur

pred.choice <- lstPredefine$idPredefine
names(pred.choice) <- lstPredefine$labelPredefine

ind.label <- lstIndicateur$nomIndicateur
names(ind.label) <- lstIndicateur$labelIndicateur

# Départements
load("Data/Maps/Region/region.RData")
dep.label <- dep.dom@data %>% 
  select(Codgeo, Libgeo) %>% 
  rename(idDep = Codgeo,
         labelDep = Libgeo) %>% 
  add_row(idDep = "977",labelDep = "Saint-Barthélemy") %>% 
  add_row(idDep = "978",labelDep = "Saint-Martin")

# Communes
load("Data/Maps/Cities/cities.RData")
com.label <- com.dom@data %>% 
  select(Codgeo, Libgeo) %>% 
  rename(idCom = Codgeo,
         labelCom = Libgeo)

# Variables issues de la qualité (Baptiste Raimbaud)
# qualiteIndicateur=c("INPER","NbFemme / INPER","NbHomme / INPER","NbJeune / INPER","NbMoyen / INPER","NbVieux / INPER", #Territoire
#             "NbVieux2 / INPER","NbMoyen2 / INPER","NbVieux3 / INPER",
#             "ACTIF / NbAgeTravaille","- ACTIF / NbAgeTravaille","INPCM / ACTIF","- INPCM / ACTIF","HommeActif / NbHomme", # Emploi
#             "- HommeActif / NbHomme","FemmeActif / NbFemme","- FemmeActif / NbFemme","NbCadre / NbAgeTravaille","- NbCadre / NbAgeTravaille",
#             "NbEtudian1825 / INPER","NbEtudian0206 / INPER","NbEtudian0614 / Nb0614","NbDecrocheur / Nb1625","NbScole_15plus / NbScole", #Scolarité
#             "- NbEtudian1825 / INPER","- NbEtudian0206 / INPER","- NbEtudian0614 / Nb0614","- NbDecrocheur / Nb1625","- NbScole_15plus / NbScole",
#             "NbImmigre / INPER","- NbImmigre / INPER","NbEtranger / INPER","- NbEtranger / INPER", # Immigration
#             "CATL1 / X","CATL2 / X","CATL3 / X","CATL4 / X","NbLocataire / X","- NbLocataire / X","NblocHLM / X", #Logement
#             "- NblocHLM / X","NbAppartement / X","- NbAppartement / X",
#             "NbHLM / CATL1","Surface / CATL1","- Surface / CATL1","NbBain / CATL1","NbEAU / CATL1","NbEGOUL / CATL1", # Residence principal
#             "- NbHLM / CATL1","- NbBain / CATL1","- NbEAU / CATL1","- NbEGOUL / CATL1"
# )
# TablePassage <- data.frame(qualiteIndicateur)
# TablePassage$nomIndicateur <- c("population","femme","homme","[0,20)" ,"[20,65)","[65,120]","[75,120]","[20,60)","[60,75)", #Territoire
#                          "actif","inactif","chomeur","actifocc","actif homme","inactif homme","actif femme","inactif femme","cadre_prof_inter","autre", #Emploi
#                          "etudi[18,25)","etudi[2,6)","etudi","decrocheur","nScola_15plus","n_etudi[18,25)","n_etudi[2,6)","n_etudi","n_decrocheur","autre", #Scolarité
#                          "immigre","non_immigre","etranger","francais", #Immigration
#                          "res_princ","log_occa","res_second","log_vacants","locataire","autre","locataireHlm","autre","appartement","autre", #Logement
#                          "hlm","surf100etplus","surf100moins","bain_douche","Eau_chaude","tout_egout","n_hlm","N_bain_douche","N_eau_chaude","N_tout_egout") # Residence principal
# TablePassage$idDomaine <-   c(1,1,1,1,1,1,1,1,1,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,5,5,5,5,6,6,6,6,6,6,6,6,6,6,7,7,7,7,7,7,7,7,7,7)
# TablePassage$idCategorie <- c(1,2,2,3,3,3,3,3,3,1,1,1,1,2,2,2,2,3,3,1,1,2,3,4,1,1,2,3,4,1,1,2,2,1,1,1,1,2,2,3,3,4,4,1,2,2,3,4,5,1,3,4,5)
# 
# # Ajout des noms des indicateurs qualités à la liste des indicateurs
# test <- lstIndicateur %>% 
#   left_join(TablePassage,
#             by = c("idDomaine","idCategorie","nomIndicateur")
#             )
# 
# write.xlsx(test,"Data/Liste indicateurs statistiques/test.xlsx")


test <- lstIndicateur %>% 
  # left_join(lstCategorie %>% 
  #             select(idDomaine,idCategorie,nomVariable),by=c("idDomaine","idCategorie")) %>% 
  mutate(qualiteIndicateur2 = paste0(nomVariable,nomIndicateur,"/",qualiteIndicateurDenom))
write.xlsx(test,"Data/Liste indicateurs statistiques/test.xlsx")

# II. Enregistrement
#------------------------------------------------------------------------------------------------------------------------------
save(lstDomaine,lstCategorie,lstIndicateur,typInd,pred.choice,ind.label,lstTypeIndicateur,dep.label,com.label,
     file = "Data/Liste indicateurs statistiques/lstIndicateur.RData")




