#-------------------------------------------------------------------------------------#
#                            Agate - lancement Distancier                             #
#-------------------------------------------------------------------------------------#

# MAJ : 26.03.2018

### Distancier infracommunal utilisant les données du rp 2013 (Ne fonctionne que sur le 
### territoire guyanais)

# Paramétrage du WorkSpace
setwd(dir=dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Chargement des fonctions developpées par NK
# Catalogue de fonctions spécifiques aux calculs de distances
source("Catalogue de fonctions/Agate - Distancier.R")

### Librairies necessaires ###
# Répertoire d'installation des librairies
#.libPaths('Library')

# Chargement dans R
library(rgdal) # Importer de nombreux formats de données spatiales
library(spdep) # Determine le plus proche voisin d'un point
library(igraph) # Graphe et calcul de la distance la plus courte
library(plyr)
library(lubridate) # Convertir secondes en temps formaté

#-----------------------------------------------#
#         Chargement des RData                  #
#-----------------------------------------------#

# Chargement du ril
load("../../Bdd/RData/Ril/ril15.RData")


# Chargement du reseau routier de Guyane
load("../../Bdd/RData/Reseau routier/Rte973.RData")
rte <- list(rte=rte973,node=nodeRte973)

#-----------------------------------------------#
#         Chargement des cartes                 #
#-----------------------------------------------#

# Chargement de la carte des medecins guyanais
med973 <- readOGR(dsn = "../../Cartes/Specialistes sante/973/medecin.geojson",
                       layer = "medecin",encoding = "GeoJSON")

# Reprojection dans le systeme de coordonnees utilisees aux antilles
med973 <- spTransform(x = med973,CRSobj = CRS("+init=epsg:3857"))

#-----------------------------------------------#
#                Paramètres                     #
#-----------------------------------------------#

# Utilisation du distancier
test <- distancierDirag(equipement = med973,nomEqui = "med",dep = "973",
                ril = rilhab15,rte = rte)

summary(test$TpHp_med)



# Calcul des temps de déplacement le plus court. Renvoie une liste de deux objets : d'une part,
# l'objet ril qui contient les temps de déplacement pour se rendre à l'équipement le plus court pour chaque logement du Ril
# l'objet node contient les temps de déplacements pour se rendre à l'équipement le plus proche à partir du noeud routier
# considéré. 
ChefL <- zonaTemp(equipement = guyChefLieu,nomEqui = "med")

# Enregistrement
save(ChefL,file = "Bdd/RData/chefL.Rdata")
