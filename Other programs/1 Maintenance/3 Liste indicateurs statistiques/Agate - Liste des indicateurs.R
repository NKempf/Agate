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


# II. Enregistrement
#------------------------------------------------------------------------------------------------------------------------------
save(lstDomaine,lstCategorie,lstIndicateur,typInd,pred.choice,ind.label,file = "Data/Liste indicateurs statistiques/lstIndicateur.RData")




