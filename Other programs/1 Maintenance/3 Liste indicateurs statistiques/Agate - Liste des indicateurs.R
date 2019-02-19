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

# II. Enregistrement
#------------------------------------------------------------------------------------------------------------------------------
save(lstDomaine,lstCategorie,lstIndicateur,typInd,pred.choice,ind.label,lstTypeIndicateur,dep.label,com.label,
     file = "Data/Liste indicateurs statistiques/lstIndicateur.RData")




