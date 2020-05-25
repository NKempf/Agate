#-------------------------------------------------------------------------------------------------------------------------------------------------------#
#                                       Agate - Fonction cartographique                                                                                 #
#-------------------------------------------------------------------------------------------------------------------------------------------------------#

# MAJ : 20.06.2018

#########################################################
######### Fonction zonaRoute - spécifications ###########
# Objectif : prepare la couche de tronçons routiers, supprime les tronçons incorrectes et selectionne le
# type de route correcte.
# La fonction renvoie la couche carto route nettoyee et tronçonnee
# ROUTE
#   Couche carthographique (ATTENTION ne marche qu'avec une couche issue de l'IGN) contenant des routes
#   Obligatoirement un objet de type SpatialLineDataFrame
# 
# DEP 
#   Obligatoirement un objet de type String
#   Indiquer le département si autre que la Guyane. Ex : "971" ou "972"
#
#######################################################
zonaRoute <- function(route,dep="973",route_conserv = NULL){
  
  #### Chargement des librairies necessaires ####
  library(sp)
  library(rgeos) 
  library(rgdal) 
  
  # Changement du système de projection : Utilisation de epsg:3857 
  route <- spTransform(x = route,
                       CRSobj = CRS("+init=epsg:3857"))
  ###############################
  ### Gestion du cas Guyanais ###
  # Selection des routes sur le territoire Guyanais 
  if (dep == "973"){
    
    # Chargement de la carte de Guyane
    guyReg <- readOGR(dsn = "Cartes/Cartes brutes/DepD973.TAB",
                      layer = "DepD973",
                      encoding="utf8",
                      stringsAsFactors = FALSE)
    # Changement du système de projection : Utilisation de epsg:3857 
    guyReg <- spTransform(x = guyReg,
                          CRSobj = CRS("+init=epsg:3857"))
    
    # Selection des routes sur le territoire Guyanais
    # Routes : repérage des routes en intersection avec la Guyane
    joinroutes <- gIntersects(guyReg,route,byid = T)
    joinroutes <- data.frame(ID=rownames(joinroutes),joinroutes)
    # On conserve uniquement les routes qui sont en intersection
    joinroutes <- joinroutes[joinroutes[,2] == T,]
    # Création de la couche finale route sur le territoire Guyanais
    route <- route[rownames(route@data) %in% joinroutes$ID, ] 
  }
  
  ## Attention ne fonctionne que avec des cartes IGN ##
  if(!is.null(route_conserv)){
    # Selection des routes
    route <- route[route@data$nature %in% route_conserv,] 
  }

  
  # Selection des variables à conserver dans la table attributaire
  route@data$typeRte <- route@data$nature
  route@data <- route@data[,c("userid","length","typeRte","cl_admin","sens","inseecom_g","inseecom_d")]

  ### Calcul de la longueur de chaque tronçon ###
  route@data$longueur <- gLength(route,byid = T)
  route@data$id <- rownames(route@data)
  
  ### Détection des chemins non valide ###
  # gBoundary permet d'obtenir le début et la fin de chaque tronçon
  noeud_rout <- gBoundary(route,byid = T)
  
  # création de la table de correspondance entre les tronçons et les coordonnées de chaque noeuds.
  dat <- data.frame(Id=seq(1:length(noeud_rout)),route=rownames(noeud_rout@coords),noeud_rout@coords[,c(1,2)])
  # On prend tous les impaires
  dat_from <- dat[dat$Id%%2!=0, c("route","x","y") ]
  colnames(dat_from) <- c("route","x_from","y_from")
  # On prend tous les pairs
  dat_to <- dat[dat$Id%%2==0,c("route","x","y")]
  colnames(dat_to) <- c("route","x_to","y_to")
  # Création de la table de passage noeuds - tronçons
  node_tron <- merge(dat_from,dat_to, by="route",all=T)
  
  # Détection de 99 chemins qui posent problèmes car ils sont circulaires. Suppression de ces chemins dans 
  pbchemin <- node_tron[is.na(node_tron$x_to),]
  # Suppression de ces chemins dans la couche tronçons
  route <- route[!(rownames(route@data) %in% pbchemin$route),]
  
  return(route)
}

#############################################################
######### Fonction zonaRouteNode - spécifications ###########
# Objectif : Création d'une couche de points à chaque extremité des tronçons routiers 
# et ajout dans la table attributaire de la couche tronçon routier deux variables contenant l'identifiant du noeud de 
# départ et du noeud d'arrivé
# La fonction renvoie une liste de deux couches carto : un spatialLineDataFrame (route) et un SpatialPointDataFrame 
# (noeuds routiers)
#
# ROUTE
#   Couche carthographique contenant des tronçons routiers correctements découpés
#   Obligatoirement un objet de type SpatialLineDataFrame
#
#############################################################
zonaRouteNode <- function(route){
  #### Chargement des librairies necessaires ####
  library(sp) # Base de tous les autres packages spatiaux
  library(rgeos) # Manipulation des géométries 
  library(maptools) # Fusion de couches carthographiques
  library(plyr) # Fonction de mise en forme de data frame. Exemple rename()
  
  ### Création de la couche noeud_routier ### 
  # gBoundary permet d'obtenir le début et la fin de chaque tronçon
  noeud_rout <- gBoundary(route,byid = T)
  # création de la table de correspondance entre les tronçons et les coordonnées de chaque noeuds.
  dat <- data.frame(Id=seq(1:length(noeud_rout)),route=rownames(noeud_rout@coords),noeud_rout@coords[,c(1,2)])
  # On prend tous les impaires
  dat_from <- dat[dat$Id%%2!=0, c("route","x","y") ]
  colnames(dat_from) <- c("route","x_from","y_from")
  # On prend tous les pairs
  dat_to <- dat[dat$Id%%2==0,c("route","x","y")]
  colnames(dat_to) <- c("route","x_to","y_to")
  # Création de la table de passage noeuds - tronçons
  node_tron <- merge(dat_from,dat_to, by="route",all=T)  
  
  # Suppression des points doublons dans noeud_rout
  noeud_rout <- remove.duplicates(noeud_rout)
  # Modification du nom des lignes pour les coordonnées
  rownames(noeud_rout@coords) <- NULL
  # Création de la table attributaire
  node <- data.frame(node_id=paste("node",seq(1:length(noeud_rout)),sep=""),noeud_rout@coords[,c(1,2)])
  # Ajout de la table attributaire a la couche noeud
  noeud_rout <- SpatialPointsDataFrame(coords = noeud_rout@coords,data = node)
  # Ajout du système de projection adéquat
  noeud_rout@proj4string <-CRS("+init=epsg:3857")
  
  
### Ajout des informations des noeuds à la couche route ### 
  
  # Ajout des node_id à la table tronçon
  node_tron <- merge(node_tron,noeud_rout@data, by.x=c("x_from","y_from"),by.y=c("x","y"),all.x=T)
  # On renomme "node_id"
  node_tron <- plyr::rename(node_tron, c("node_id"="node_from")) # Attention package plyr 
  # Ajout des node_id à la table tronçon
  node_tron <- merge(node_tron,noeud_rout@data, by.x=c("x_to","y_to"),by.y=c("x","y"),all.x=T)
  # On renomme "node_id"
  node_tron <- plyr::rename(node_tron, c("node_id"="node_to")) # Attention package plyr 
  
  # Renommage des lignes de node_tron
  rownames(node_tron) <- node_tron$route
  
  # Trie des lignes par nom de ligne
  node_tron <- node_tron[order(rownames(node_tron)),]
  route <- route[order(rownames(route@data)),]
  
  # Jointure entre la couche route et la data frame node_tron
  route <- spCbind(route,node_tron)
  
### Création de la liste contenant les couches node et route ###
  liste <- list(route=route,node = noeud_rout )
  
  return(liste)
}

#######################################################
######### Fonction zonaRil - spécifications ###########
# Objectif : apparier le RIL avec n'importe quel zonage
# La fonction renvoie la couche RIL dans laquelle se trouve une ou plusieures variables indiquant dans quel zonage
# se trouve chaque point du RIL
# pts.sp
#   
#   Obligatoirement un objet de type SpatialPointDataFrame
#   Variables indispensables
#     idx : construit de la manière suivante : Codecommune + 3 espaces + identifiant Ilot + 1 espace + numéro de rang
#          Ex : "97120   AH01 001" (attention à bien conserver les espaces)  
# 
# ZONAGE 
#   Obligatoirement un objet de type SpatialPolygonDataFrame
#   Aucune variable n'est obligatoire. Conserver dans la table attributaire les variables d'intérêts
#
# MAJ : 20.06.2018
#
#######################################################
zonaPts <- function(pts.sp,zonage,CRSagate = "+init=epsg:3857"){
  
  #### Changement du système de projection
  pts.sp <- spTransform(x = pts.sp,CRSobj = CRS(CRSagate))
  zonage <- spTransform(x = zonage,CRSobj = CRS(CRSagate))
  
  #########################################     
  ### Zonage : préparation de la base #####
  #########################################  
  # Gestion des eventuels chevauchements de polygones
  zonage <- simplifyZonage(zonage)
  
  ######################################     
  ### JOINTURE Spatiale pts.sp Zonage #####
  ######################################
  
  ### Jointure spatiale entre le pts.sp et les communes de guyane ###
  zonageDB <- over(pts.sp,zonage)
  # Ajout des identifiants des points
  zonageDB <- data.frame(idx=pts.sp@data$idx,zonageDB)
  
  # Ajout des variables d'interets de zonage au pts.sp
  pts.sp <- merge(pts.sp,zonageDB,by="idx",all.x=T)

  # Modification de l'idZonage
  pts.sp@data$idZonage <- ifelse(is.na(pts.sp@data$idZonage),"Hors zonage", as.character(pts.sp@data$idZonage))
  
  return(pts.sp)
}




# Calcul le nombre de points dans un zonage
# points : obligatoirement un spatialPointDataframe
# poly : obligatoirement un spatialPolygonDataFrame
zonaGridSecret <- function(points,poly,seuil=5){
  #### Chargement des librairies necessaires ####
  library(sp)
  library(rgeos) 
  library(plyr)
  
  #### Changement du système de projection : Utilisation de epsg:3857 
  points <- spTransform(x = points,CRSobj = CRS("+init=epsg:3857"))
  poly <- spTransform(x = poly,CRSobj = CRS("+init=epsg:3857"))
  
  #######################################     
  ### Poly : préparation de la base #####
  #######################################  
  # Gestion des eventuels chevauchements de polygones
  poly <- simplifyZonage(poly)
  
  # identifiant poly maison
  poly@data$idpoly <- paste("poly",rownames(poly@data),sep="")
  
  #########################################     
  ### Points : préparation de la base #####
  #########################################  

    # identifiant maison
  points@data$idpts <- paste("pts",rownames(points@data),sep="")
  
  
  ######################################     
  ### JOINTURE Spatiale RIL Zonage #####
  ######################################
  
  ### Jointure spatiale entre le ril et les communes de guyane ###
  zonageDB <- over(points,poly)
  # Ajout des identifiants des points
  zonageDB <- data.frame(idx=points@data$idpts,zonageDB)
  
  zonageDB$compt <- 1
  
  # Comptage des points dans chaque carreaux
  secret <- ddply(.data = zonageDB,
                    .variables = "idpoly",
                    .fun = summarise,
                    comptNk = sum(compt))

  # Fusion avec la couche poly
  poly2 <- merge(poly,secret,by="idpoly",all.y=T)
  
  # Mise au secret selon le seuil 
  poly2 <- poly2[poly2@data$comptNk >= seuil,]
  
  return(poly2)
}






#######################################################
######### Fonction zonaIlot - spécifications ##########
# Objectif : Vérifie si un ilot est recouvert à plus de XX % par un zonage alors 
# on affecte le nom du zonage à l'ilot. Le but de rechercher est de connaitre les
# ilots qui sont grandement recouvert par un zonage. Ainsi, les logements rejetés
# par RIl seront quand même comptabilisés dans le zonage.
#
# Renvoie un objet du type SpatialPolygonDataFrame.
#
# ILOT
#   Obligatoirement un objet de type SpatialpolygonDataFrame
#   Variables indispensables 
#       depcom
#       ilot99
#
# ZONAGE 
#   Obligatoirement un objet de type SpatialPolygonDataFrame
#   Aucune variable n'est obligatoire. Conserver dans la table attributaire les variables d'intérêts
#
# Recouvrement
#   Obligatoirement une valeur numérique comprise entre 0 et 100
#   C'est le pourcentage de recouvrement que l'on desire. Par défaut : 90 %
#
################################################
zonaIlot <- function(ilot,zonage,recouvrement = 90){
  #### Chargement des librairies necessaires ####
  library(sp)
  library(rgeos)
  library(raster)
  
  #### Changement du système de projection : Utilisation de epsg:3857 
  ilot <- spTransform(x = ilot,CRSobj = CRS("+init=epsg:3857"))
  zonage <- spTransform(x = zonage,CRSobj = CRS("+init=epsg:3857"))
  
  # Selection des variables  
  ilot@data <- ilot@data[,c("depcom","ilot99")]
  
  ### Simplification des couches ###
  ilot <- simplifyZonage(ilot)
  zonage <- simplifyZonage(zonage)
  
  # Calcul de la surface de chaque ilot
  ilot@data$surf_ilot <- gArea(ilot, byid = T)
  
  # Stratégie : calculer la surface de l'intersection entre un zonage et un ilot
  # Pour les ilots qui sont recouvert à plus de XX % par un zonage, on considère que tous les habitants
  # font partie du zonage concerné. 
  # Création des polygones d'intersections entre les ilots et les qpv
  pi <- raster::intersect(ilot, zonage) # Fonction issue du package raster
  
  # Calcul de la surface de chaque polygone
  pi@data$surf_poly <- gArea(pi,byid = T)
  
  # Calcul de part de la surface du poly dans la surface de l'ilot
  pi@data$part_surf <- round(100*pi@data$surf_poly/pi@data$surf_ilot,digits = 1)
  
  # Selection des ilots dont la surface est supérieure à recouvrement
  pi2 <- pi@data[pi@data$part_surf>=recouvrement,]
  
  # Suppression des doublons et suppression de la variables surf_ilot
  pi2 <- pi2[duplicated(paste(pi2$depcom,pi2$ilot99,sep="_")) == F, !(colnames(pi2) %in% c("surf_ilot"))]
  
  # Fusion des données avec la couche ilots
  ilot2 <- merge(ilot,pi2,by=c("depcom","ilot99"),all.x=T)
  
  return(ilot2)
}


#####################################################
######### Fonction zonaRP - spécifications ##########
# Objectif : nettoyer la base du RP et éventuellement corriger certain ilot/rang 
# modifier au cours de la collecte. 
#
# Renvoie un objet du type Data.frame
#
# RP
#   Obligatoirement un objet de type Data.frame
#   Variables indispensables 
#       C_ANNEE_COL : année de collecte
#       C_GEO : Localisation géographique
#       C_IMM : Numéro d'adresse
#       C_LOG : Numéro du logement 
#       C_IND : Numéro de l'individu
#       IPONDI : Variable de poids
# 
# PONT
#  Obligatoirement un objet de type data.frame
#  Variables indispensables 
#   id_geo_rp
#   id_geo_carto
# 
# 
################################################  
zonaRP <- function(rp,pont=NULL){
  
  #####################################     
  ### RP : préparation de la base #####
  #####################################      
  
  ##### Niveau individu ##### 
  # Selection des individus
  rp <- rp[substr(rp$C_IMM,1,3) %in% c("971","972","973"),]
  
  # CrÃ©ation de l'identifiant necessaire à la fusion avec le RIL
  rp$idrp <- paste(substr(rp$C_IMM,1,5),"___",substr(rp$C_IMM,9,12),"_",substr(rp$C_IMM,14,16),sep = "")
  # Création de l'identifiant Rp unique pour chaque logement (cf mad RP)
  rp$ident_log <- paste(rp$C_ANNEE_COL,rp$C_GEO,rp$C_IMM,rp$C_LOG,sep="")
  
  # Département
  rp$dep <- substr(rp$C_IMM,1,3)
  # Commune
  rp$com <- substr(rp$C_IMM,1,5)
  # Ilot
  rp$ilot <- substr(rp$C_IMM,9,12)
  
  ######################################     
  ### PONT : préparation de la base ####
  ######################################    
  # Si la variable pont est renseignée, alors
  if (!is.null(pont)){
    # Selection des logements uniquement en Guadeloupe, Martinique et Guyane
    pont <- pont[substr(pont$id_geo_rp,1,3) %in% c("971","972","973"),]
    
    # Transformation de l'identifiant pour qu'il concorde avec celui prÃ©sent dans le RP et le RIL
    pont$idrp <- paste(substr(pont$id_geo_rp,1,5),"___",substr(pont$id_geo_rp,6,9),
                       "_",substr(pont$id_geo_rp,10,length(pont$id_geo_rp)),sep = "")
    # Attention, il y a des identifiants "rp_carto" a vide... 
    # Traitement de ces cas...
    pont$idril <- ifelse(pont$id_geo_carto != "",
                         paste(substr(pont$id_geo_carto,1,5),"___",substr(pont$id_geo_carto,6,9),
                               "_",substr(pont$id_geo_carto,10,length(pont$id_geo_carto)),sep = ""),
                         pont$idrp)
    # Indicatrice d'appartenance
    pont$pont <- 1
    
    # Suppression des doublons dans la table pont
    pont <- pont[duplicated(pont$idrp)==F,]  
    
    ### Correction des identifiants dans le RP ###
    # Ajout des données de la table du CRIEM : Objectif : corriger certains logements du RP non retrouvé dans le RIL
    # Fusion Pont - RP13
    rp <- merge(rp,pont[,c("idrp","idril","pont")], by="idrp", all=T)
    # Création de l'identifiant final 
    rp$idx <- ifelse(is.na(rp$idril),rp$idrp,rp$idril)
    # Suppression des variables inutiles
    rp <- rp[, !(colnames(rp) %in% c("idrp","idril"))]
    
  }else{
    # Si pas de pont alors on crée quand même la variable idx
    rp$idx <- rp$idrp
    # Suppression des variables inutiles
    rp <- rp[, !(colnames(rp) %in% c("idrp"))]      
  } 
  
  return(rp) 
  
}







##############################################################
######### Fonction simplifyZonage - spécifications ###########
# Objectif : Gère les problèmes de chevauchement de polygones. 
# Renvoie un SpatialPolygoneDataFrame 
# 
# ZONAGE 
#   Obligatoirement un objet de type SpatialPolygonDataFrame
#
#######################################################
simplifyZonage <- function(zonage){
  
  #### Chargement des librairies necessaires ####
  library(rgeos)
  
  # Suppression du chevauchement de certains ilots 
  # simplify the polgons a tad (tweak 0.00001 to your liking)
  zonage1 <- gSimplify(zonage, tol = 0.00001)
  
  # this is a well known R / GEOS hack (usually combined with the above) to 
  # deal with "bad" polygons
  zonage1 <- gBuffer(zonage1, byid=TRUE, width=0)
  
  # any bad polys?
  sum(gIsValid(zonage1, byid=TRUE)==FALSE)
  
  # MAJ 20/06/2018 : changement des identifiants
  zonage1 <- spChFIDs(zonage1,as.character(rownames(zonage@data)))
  
  # Ajout de la table attributaire
  zonage <- SpatialPolygonsDataFrame(zonage1,zonage@data)
  
  return(zonage)
  
}






