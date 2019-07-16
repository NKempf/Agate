#--------------------------------------------------------------------------------------------------------------------------------#
#                 Agate - Gestion de la liste des indicateurs                                                                    #
#--------------------------------------------------------------------------------------------------------------------------------#

# MAJ : 02.04.2019

# Nicolas Kempf

# Transformation de la liste des indicateurs en base fst

# Packages
library(readxl) # Load excel files
library(fst) # fst tables
library(tidyverse) # Transform data
library(openxlsx)


# I. Data import
#------------------------------------------------------------------------------------------------------------------------------
fichierExcel <- "Data/Liste indicateurs statistiques/Agate - indicateurs statistiques_v3.xlsx"
lstDomaine <- read_xlsx(fichierExcel,sheet = "domaine")
lstCategorie <- read_xlsx(fichierExcel,sheet = "categorie")
lstIndicateur <- read_xlsx(fichierExcel,sheet = "indicateur")
lstTypeIndicateur <- read_xlsx(fichierExcel,sheet = "type indicateur")
lstPredefine <- read_xlsx(fichierExcel,sheet = "zone predefinie")
lstZonePreType <- read_xlsx(fichierExcel,sheet = "zonePreType")


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

# Gestion des tables
lstIndicateur <- lstIndicateur %>% 
  left_join(lstCategorie %>% 
              select(domaine,categorie,nomVariable,source,typeVar,ssChamp,variableChamp,modaliteChamp),
            by = "nomVariable") %>% 
  mutate(qualiteIndicateur = case_when(substr(source,1,2) == "rp" & typeVar == "effectif" ~ as.character(variableChamp),
                                       substr(source,1,2) == "rp" & typeVar == "pct" & 
                                         ssChamp == 0 ~ paste0(nomVariable,nomIndicateur," / ",variableChamp),
                                       substr(source,1,2) == "rp" & typeVar == "pct" & 
                                         ssChamp == 1 ~ paste0(nomVariable,nomIndicateur," / ",variableChamp,modaliteChamp),
                                       TRUE ~ "nc"))
                                       
# write.xlsx(lstIndicateur,"Data/Liste indicateurs statistiques/testLstIndicateur.xlsx")

# II. Enregistrement
#------------------------------------------------------------------------------------------------------------------------------
save(lstDomaine,lstCategorie,lstIndicateur,typInd,pred.choice,ind.label,lstTypeIndicateur,dep.label,com.label,lstZonePreType,lstPredefine,
     file = "Data/Liste indicateurs statistiques/lstIndicateur.RData")




