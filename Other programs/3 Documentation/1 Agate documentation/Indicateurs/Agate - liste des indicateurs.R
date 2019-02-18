#-------------------------------------------------------------------------------------------------------------------------------#
#                     Agate - liste des indicateurs                                                                             #
#-------------------------------------------------------------------------------------------------------------------------------#

# Nicolas Kempf

# 29.11.2018

# Objectif : Liste des indicateurs présents dans l'application

# Packages necessaires
library(tidyverse) # transformation des données
library(DT) # Affichage d'un tableau intéractif

# I. Construction de la base de données
#--------------------------------------------------------------------------------------------------------------------------------
indicateurs <- read.csv(file = "Html pages/Indicateurs/Agate - liste indicateurs.csv",header = TRUE,sep = ";")

# II. Tableau interactif
#--------------------------------------------------------------------------------------------------------------------------------

datatable(indicateurs,
          extensions = 'Buttons',
          options = list(
            scrollX = TRUE,
            # fixedColumns = TRUE,
            # autoWidth = TRUE,
            ordering = FALSE,
            dom = 'lBfrtip',
            buttons = c(I('colvis'),'excel', 'pdf')),
          rownames= FALSE,
          class = "display" #if you want to modify via .css
) 
