#--------------------------------------------------------------------------------------------------------------------------------#
#                 Agate - Gestion de la liste des indicateurs                                                                    #
#--------------------------------------------------------------------------------------------------------------------------------#

# MAJ : 15.02.2019

# Nicolas Kempf

# Transformation de la liste des indicateurs en base fst

# Packages
library(readxl) # Load excel files
library(fst) # fst tables
library(tidyverse) # Transform data


# I. Data import
#------------------------------------------------------------------------------------------------------------------------------
lstDomaine <- read.xlsx("Data/Liste indicateurs statistiques/Agate - indicateurs statistiques.xlsx",sheet = "domaine")
lstCategorie <- read.xlsx("Data/Liste indicateurs statistiques/Agate - indicateurs statistiques.xlsx",sheet = "categorie")
lstIndicateur <- read.xlsx("Data/Liste indicateurs statistiques/Agate - indicateurs statistiques.xlsx",sheet = "indicateur")
lstTypeIndicateur <- read.xlsx("Data/Liste indicateurs statistiques/Agate - indicateurs statistiques.xlsx",sheet = "type indicateur")

typInd <- lstTypeIndicateur$typeIndicateur
names(typInd) <- lstTypeIndicateur$labelTypeIndicateur


# II. Enregistrement
#------------------------------------------------------------------------------------------------------------------------------
save(lstDomaine,lstCategorie,lstIndicateur,typInd,file = "Data/Liste indicateurs statistiques/lstIndicateur.RData")




