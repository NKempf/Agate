# MAJ : 27.03.2018

#-------------------------------------------------------------#
#              Recherche du trajet le plus court              #
#-------------------------------------------------------------#
# Recherche le trajet le plus court entre un logement et l'equipement le plus proche
# route : reseau routier (type spatiallinedataframe)
# noeud : noeuds associes au reseau routier (type spatialpointdataframe)
# equipement : equipement considere (type spatialpointdataframe)
# distance : variable de distance utilisee
# nomVar : libelle de la variable en sortie
distTrajetCourt <- function(route,noeud,equipement,ril,distance,nomVar="nearestEquip"){
  
  # Temps de calcul
  T1 <- Sys.time()
  
  #### Chargement des librairies necessaires ####
  #library(igraph) # Graphe et calcul de la distance la plus courte
  #library(plyr) # Gestion des data.frame
  
  # Création indispensable
  route@data$dist <- route@data[,distance]
  
  # Selection des noeuds routiers les plus proches des logements du RIL
  node_ril <- ril@data$node_nearest[duplicated(ril@data$node_nearest)==FALSE]
  
  # Les sommets (Vertex) sont nos noeuds (node) et les routes (edges) 
  # Transformation des données issues de routes
  edgex <- route@data[,c("node_from","node_to","dist","route")]
  vertex <- noeud@data[ ,c("node_id","x","y")]
  # Création du graph
  st_graph <- graph_from_data_frame(d=edgex, vertices=vertex, directed=F) 
  
  ### Calcul de la matrice de distance entre chaque equipement et chaque noeud routier
  matCourte <- shortest.paths(st_graph, 
                              # Depart : les noeuds routiers les plus proches des equipements
                              v = V(st_graph)[name %in% equipement@data$node_nearest], 
                              # Arrivé : les noeuds routiers les plus proches des logements
                              to  = V(st_graph)[name %in% node_ril],
                              # Metrique utilisee : distance ou temps de parcours
                              weights = E(st_graph)$dist)
  
  # Suppression des noeuds routiers non connectés à un equipement
  # c'est a dire suppression des noeuds routiers ou il est impossible de calculer une distance ou temps
  # Reperage des noeuds non connectés
  testInf <- apply(matCourte,2,FUN = function(x){
    return(min(x) < Inf)})
  # Suppression des noeuds dans la matrice de distance
  matCourte <- as.matrix(matCourte[,testInf])
  # En resume, les lignes de matCourte correspondent aux noeuds routiers des equipements
  # les colonnes aux logements.

  # S'il n'y a qu'un seul equipement...
  if(ncol(matCourte) == 1){
    # Construction d'une data.frame en sortie
    matCourte <- data.frame(node_id=rownames(matCourte),equi_id=equipement@data$node_nearest,dist=matCourte[,1])
  
  # Sinon...    
  }else{
    # Selection de l'equipement le plus proche pour chaque noeud routiers
    matCourte <- apply(matCourte,2,function(x){
      medMin <- min(x)
      inds <- which(x == min(x), arr.ind=TRUE)
      medNom <- names(inds)
      med <- data.frame(equi_id=medNom,dist=medMin)
      return(med)
    })
    # Construction d'une data.frame en sortie en fusionnant la liste de data.frame
    matCourte <- do.call(rbind, matCourte)
    matCourte$node_id <- rownames(matCourte)
  }
  
  # Fusion avec le ril
  ril.df <- merge(ril@data,matCourte,
                  by.x="node_nearest",by.y="node_id",all.x=TRUE)
  
  # Ajout de la distance de marche jusqu'au medecin
  ril.df <- merge(ril.df,equipement@data[duplicated(equipement@data$node_nearest)==F,
                                  c("node_nearest","dMarche_equi","tMarche_equi")],
                  by.x="equi_id",by.y="node_nearest",all.x=TRUE)
  
  # Calcul du temps de trajet total : Marche jusqu'au reseau routier + temps de trajet dans 
  # dans le reseau routier + temps de marche jusqu'a l'equipement
  # MAJ : 26.03.2018 : probleme de factor : le code ci-dessus crée un facteur pour la variable dist... Il faut donc le supprimer
  if(distance == "longueur"){
    ril.df$tpTrajet <- ril.df$dMarche + as.numeric(as.character(ril.df$dist)) + ril.df$dMarche_equi
  }else{
    ril.df$tpTrajet <- ril.df$tMarche + as.numeric(as.character(ril.df$dist)) + ril.df$tMarche_equi
    }
  
  # Selection des variables
  list_var <- c("idx","equi_id","tpTrajet")
  ril.df <- ril.df[,list_var]
  
  #Renommage de la variable
  ril.df <- rename(ril.df,replace = c("tpTrajet" = nomVar))
  
  # Temps de calcul  
  T2 <- Sys.time()
  tdiff <- T2 - T1
  print(tdiff)
  
  return(ril.df)
}

#-------------------------------------------------------------#
#              Recherche du plus proche voisin                #
#-------------------------------------------------------------#
# Recherche pour chaque point le noeud routier le plus proche en utilisant la distance 
# a vol d'oiseau
# points : couche de points (type spatialpointdataframe) 
# node : noeuds routiers (type spatialpointdataframe)

distVoisinProche <- function(points,node,vMarche=4){

  #### Chargement des librairies necessaires ####
  library(FNN) # Recherche du voisin le plus proche  
  
  # Création d'identifiants utiles
  points@data$idn <- seq(1:length(rownames(points@data)))
  node@data$node_rowid <- rownames(node@data)
  
  # Transformation des spatialPointsDataFrames en data.frame.
  # On va pouvoir ensuite faire tourner la fonction get.knnx
  pts <- data.frame(points@coords)
  nod <- data.frame(node@coords)
  
  # Recherche du plus proche voisin grace au package FNN et la fonction get.knnx
  nearestNeigh <- FNN::get.knnx(nod,pts,k=1)
  nearestNeigh <- data.frame(idn=seq(1:length(nearestNeigh[[1]])),
                             node_rowid=nearestNeigh[[1]],dMarche=nearestNeigh[[2]])
  # Fusion avec la couche de medecins
  points <- merge(points,nearestNeigh,by="idn",all.x=TRUE)
  # Recuperation de l'identifiant des noeuds routiers
  points <- merge(points,node@data[,c("node_id","node_rowid")],by="node_rowid",all.x=TRUE)
  
  # Nettoyage base
  points@data$node_nearest <- points@data$node_id
  points@data <- points@data[,!(colnames(points@data) %in% c("idn","node_rowid","node_id"))]
  
  # temps de marche en seconde
  points@data$tMarche <- 3600 * points@data$dMarche / vMarche / 1000

  return(points)
}


## Calcul le chemin le plus court (Fonctionne !) ##
# Retourne pour un trajet la liste des routes empruntées
# Node_from et Node_to : chaine de caractère
# graphx : graphe du réseau routier
# rte : carte des tronçons routiers de Guyane contenant la liste des noeuds routiers
chemin_court <- function(node_from,node_to,graphx,rte){
  
  # Calcul de distance la plus courte
  cheminCourt <- shortest_paths(graphx, 
                                
                                from = V(graphx)[name==as.character(node_from)], 
                                
                                to  = V(graphx)[name==as.character(node_to)],
                                
                                weights = E(graphx)$longueur,
                                
                                output = "both") # both path nodes and edges
  
  # Transformation en vecteur
  chemin <- unlist(cheminCourt$vpath)
  chemin1 <- names(chemin[-length(chemin)])
  chemin2 <- names(chemin[- 1])
  
  # retourne un vecteur contenant la liste des tronçons empruntés
  return (as.vector(rte@data[(rte@data$node_from %in% chemin1 & rte@data$node_to %in% chemin2) |
                               (rte@data$node_to %in% chemin1 & rte@data$node_from %in% chemin2),"route"]))
  
}



