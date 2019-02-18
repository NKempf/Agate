#-------------------------------------------------------------------------------------#
#                      Acces aux services - Temps de trajet par services              #
#-------------------------------------------------------------------------------------#

# MAJ : 28.03.2018

# Nicolas Kempf

# Ce programme calcule les temps d'accès à chaque service présent dans la BPE. Il calcul pour chaque logement le trajet le plus court pour se rendre
# à un service donné. Trois scénarii sont disponibles : le temps en heures pleines, le temps en heures creuses et la distance la plus courte.
# En sortie, les bases sont stockees dans un RData. 

# Remarque : Ce programme necessite un temps très long pour réaliser les calculs. Il est conseillé de le faire tourner la nuit. 

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

# Chargement de la bpe
load("../../Bdd/RData/Bpe/Bpe.Rdata")

# Reprojection dans le systeme de coordonnees utilisees aux antilles
bpe <- spTransform(x = bpe,CRSobj = CRS("+init=epsg:3857"))





# I. Temps de trajets en Guadeloupe
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
    # dep <- "971"
    # 
    # # Chargement du reseau routier
    # load("../../Bdd/RData/Reseau routier/rte971.RData")
    # rte <- list(rte=rte971,node=rte971.node)
    # 
    # # Bpe en Guadeloupe
    # bpe971 <- bpe[bpe@data$dep == dep,]
    # 
    # t1 <- Sys.time()
    # # Calcul des temps d'accès aux services (Temps de calcul très long environ 9h)
    # bpeTps971 <- distancierDiragBpe(equipement = bpe971,dep = dep,ril = rilhab15,rte = rte)
    # t2 <- Sys.time()
    # print("Temps de calcul total : " )
    # t2-t1
    # 
    # # Enregistrement des temps d'acces 
    # save(bpeTps971,file=paste("../../Bdd/RData/Acces aux services/bpeTps971.RData",sep=""))





# II. Temps de trajets en Martinique
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
    dep <- "972"
    
    # Chargement du reseau routier
    load("../../Bdd/RData/Reseau routier/rte972.RData")
    rte <- list(rte=rte972,node=rte972.node)
    
    # Bpe en martinique
    bpe972 <- bpe[bpe@data$dep == dep,]
    
    t1 <- Sys.time()
    # Calcul des temps d'accès aux services (temps de calcul d'environ 2h30)
    bpeTps972 <- distancierDiragBpe(equipement = bpe972,dep = dep,ril = rilhab15,rte = rte)
    t2 <- Sys.time()
    
    print("Temps de calcul total : " )
    t2-t1
    
    # Enregistrement des temps d'acces 
    save(bpeTps972,file=paste("../../Bdd/RData/Acces aux services/bpeTps972.RData",sep=""))



# III. Temps de trajets en Guyane
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
    dep <- "973"
    
    # Chargement du reseau routier
    load("../../Bdd/RData/Reseau routier/Rte973.RData")
    rte <- list(rte=rte973,node=nodeRte973)
    
    # Bpe en guyane
    bpe973 <- bpe[bpe@data$dep == dep,]
    
    t1 <- Sys.time()
    # Calcul des temps d'accès aux services
    bpeTps973 <- distancierDiragBpe(equipement = bpe973,dep = dep,ril = rilhab15,rte = rte)
    t2 <- Sys.time()
    print("Temps de calcul total : " )
    t2-t1
    
    # Enregistrement des temps d'acces 
    save(bpeTps973,file=paste("../../Bdd/RData/Acces aux services/bpeTps973.RData",sep=""))

  


