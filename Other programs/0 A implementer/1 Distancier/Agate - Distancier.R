#-------------------------------------------------------------------------------------#
#                            Agate - Distancier                                       #
#-------------------------------------------------------------------------------------#

# Nicolas Kempf

# MAJ : 28.03.2018

#-------------------------------------------------------------#
#                      Distancier DIRAG                       #
#-------------------------------------------------------------#
# Equipement: SpatialPointDataFrame : contenant une couche de point indiquant la position et 
# les caracteristiques de l'équipement. 
# nomEqui : libelle de l'equipement (type string)
# ril : ril utilise (type spatialpointdataframe)
# rte : liste d'objet spatiaux contenant les routes (type spatiallinesdataframe) 
#   et les noeuds routiers (type spatialpointdataframe)
# metrique : nom de la variable metrique utilisee (type string). 
#   Par exemple : le temps en heure creuse
distancierDirag <- function(equipement,nomEqui = NULL,dep="973",ril,rte,metrique=NA,vMarche=4){
  
  T1 <- Sys.time()
  # Chargement des fonctions developpées par NK
  # Catalogue de fonctions spécifiques aux calculs de distances
  source("Catalogue de fonctions/Agate - fonctions Distancier.R")

  # Chargement dans R
  #library(rgdal) # Importer de nombreux formats de données spatiales
  #library(haven) # Importer des tables SAS
  #library(plyr) # Pour notamment sa fonction rename()
  #library(spdep) # Determine le plus proche voisin d'un point
  #library(igraph) # Graphe et calcul de la distance la plus courte
  #library(btb) # PSAR AU : lissage et carroyage
  
    # Changement du système de projection : Utilisation de epsg:3857 
    equipement <- spTransform(x = equipement,CRSobj = CRS("+init=epsg:3857"))
    
    #### Recherche du noeud routier le plus proche de chaque equipement ####
    equipement <- distVoisinProche(points =  equipement,node = rte$node)
    
    # Calcul de la moyenne du temps et de la distance de marche pour chaque noeud proche d'un médecin
    # cela permet de gérer le cas où plusieurs médecins ont le même noeud routier le plus proche.
    equipement <- merge(equipement,ddply(.data = equipement@data,
                                         .variables = "node_nearest",
                                         .fun = summarise,
                                         dMarche_equi = mean(dMarche),
                                         tMarche_equi = mean(tMarche)),
                        by="node_nearest",all.x=T)
  
  ###############################################################################
    # Selection du ril sur la Guyane
    ril <- ril[substr(ril@data$idx,1,3) == dep,]
    
    #### Recherche du noeud routier le plus proche de chaque habitation du RIL ####
    ril <- distVoisinProche(points =  ril,node = rte$node,vMarche = vMarche)
  
  #--------------------------------------------------------------------#
  #            Temps de trajet à l'equipement le plus proche           #
  #--------------------------------------------------------------------#
  
  # Calcul pour chaque noeud routier de la distance à parcourir pour atteindre l'équipement le plus proche. 
  # Heures pleines (temps en secondes)
  ril.df1 <- distTrajetCourt(route = rte$rte,noeud = rte$node,ril=ril,
                            equipement = equipement,distance = "TpHp",
                            nomVar = paste("TpHp",nomEqui,sep="_"))
  
  # Heure creuse(temps en secondes)
  ril.df2 <- distTrajetCourt(route = rte$rte,noeud = rte$node,ril=ril,
                            equipement = equipement,distance = "TpHc",
                            nomVar = paste("TpHc",nomEqui,sep="_"))
  # Distance en km
  ril.df3 <- distTrajetCourt(route = rte$rte,noeud = rte$node,ril=ril,
                            equipement = equipement,distance = "longueur",
                            nomVar = paste("longueur",nomEqui,sep="_"))
  # Fusions
  ril.df <- merge(ril.df1,ril.df2[,colnames(ril.df2)!="equi_id" ],by="idx",all.x=TRUE)
  ril.df <- merge(ril.df,ril.df3[,colnames(ril.df3)!="equi_id" ],by="idx",all.x=TRUE)
  
  if(!is.na(metrique)){
    # Metrique personnalisable
    ril.df4 <- distTrajetCourt(route = rte$rte,noeud = rte$node,ril=ril,
                              equipement = equipement,distance = metrique,
                              nomVar = paste(metrique,nomEqui,sep="_"))
    ril.df <- merge(ril.df,ril.df4[,colnames(ril.df4)!="equi_id" ],by="idx",all.x=TRUE)
  }

  T2 <- Sys.time()
  print(T2-T1)
  
  return(ril.df)
  
}

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Fonctions spéciales pour l'accès aux services

#-------------------------------------------------------------#
#                      Distancier DIRAG BPE                   #
#-------------------------------------------------------------#
# Calcul pour chaque type d'équipement de la bpe les temps d'accès à l'équipement le plus proche
# Equipement: SpatialPointDataFrame : contenant une couche de point indiquant la position et 
# les caracteristiques de l'équipement. 
# nomEqui : libelle de l'equipement (type string)
# ril : ril utilise (type spatialpointdataframe)
# rte : liste d'objet spatiaux contenant les routes (type spatiallinesdataframe) 
#   et les noeuds routiers (type spatialpointdataframe)
# metrique : nom de la variable metrique utilisee (type string). 
#   Par exemple : le temps en heure creuse
distancierDiragBpe <- function(equipement,nomEqui = NULL,dep="973",ril,rte,metrique=NA,vMarche=4){
  
  T1 <- Sys.time()
  # Chargement des fonctions developpées par NK
  # Catalogue de fonctions spécifiques aux calculs de distances
  source("Catalogue de fonctions/Agate - fonctions Distancier_v2.R")
  
  # Liste des équipements de la bpe
  lst_equi <- names(table(equipement$TYPEQU))
  
  # Changement du système de projection : Utilisation de epsg:3857 
  equipement <- spTransform(x = equipement,CRSobj = CRS("+init=epsg:3857"))
  
  #### Recherche du noeud routier le plus proche de chaque equipement ####
  equipement <- distVoisinProche(points =  equipement,node = rte$node)
  
  # Liste des noeuds routiers connectés à au moins un équipement
  node.equi <- ddply(.data = equipement@data,
                     .variables = c("TYPEQU","node_nearest"),
                     .fun = summarise,
                     dMarche_equi = mean(dMarche),
                     tMarche_equi = mean(tMarche))
  
  # Calcul de la moyenne du temps et de la distance de marche pour chaque noeud proche d'un médecin
  # cela permet de gérer le cas où plusieurs médecins ont le même noeud routier le plus proche.
  equipement <- merge(equipement,node.equi,
                      by=c("TYPEQU","node_nearest"),all.x=T)
  
  ###############################################################################
  # Selection du ril sur la Guyane
  ril <- ril[substr(ril@data$idx,1,3) == dep,]
  
  #### Recherche du noeud routier le plus proche de chaque habitation du RIL ####
  ril <- distVoisinProche(points =  ril,node = rte$node)
  
  #--------------------------------------------------------------------#
  #            Temps de trajet à l'equipement le plus proche           #
  #--------------------------------------------------------------------#
  # Calcul des temps de trajet le plus court pour chaque équipement de la BPE
  bpeTps <- lapply(lst_equi[76:78], bpeTps,equipement=equipement,ril=ril)
  
  # Fusion des listes
  bpeTps <- Reduce(function(...) merge(..., by = c('idx')), bpeTps)
  
  return(bpeTps)
  
}

# Calcul les temps et les distances d'accès à l'équipement le plus proche pour chaque logement du RIL. 
# Utiliser dans un apply

bpeTps <- function(equipement.select,equipement,ril){
  
  equipement.tmp <- equipement[equipement@data$TYPEQU == equipement.select & equipement@data$dep == dep,]
  
  # Calcul pour chaque noeud routier de la distance à parcourir pour atteindre l'équipement le plus proche. 
  # Heures pleines (temps en secondes)
  ril.df1 <- distTrajetCourt(route = rte$rte,noeud = rte$node,ril=ril,
                             equipement = equipement.tmp,distance = "TpHp",
                             nomVar = paste(equipement.select,"TpHp",sep="_"))
  
  # Heure creuse(temps en secondes)
  ril.df2 <- distTrajetCourt(route = rte$rte,noeud = rte$node,ril=ril,
                             equipement = equipement.tmp,distance = "TpHc",
                             nomVar = paste(equipement.select,"TpHc",sep="_"))
  # Distance en km
  ril.df3 <- distTrajetCourt(route = rte$rte,noeud = rte$node,ril=ril,
                             equipement = equipement.tmp,distance = "longueur",
                             nomVar = paste(equipement.select,"longueur",sep="_"))
  # Fusions
  ril.df <- merge(ril.df1,ril.df2[,colnames(ril.df2)!="equi_id" ],by="idx",all.x=TRUE)
  ril.df <- merge(ril.df,ril.df3[,colnames(ril.df3)!="equi_id" ],by="idx",all.x=TRUE)
  
  # Renommage la variable equi_id
  ril.df <- rename(ril.df,replace = c("equi_id" = paste(equipement.select,"equi_id",sep="_")))
  
  return(ril.df)
}




