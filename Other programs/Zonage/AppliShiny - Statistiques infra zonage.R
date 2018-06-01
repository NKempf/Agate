##############################################################################################################
#################### Zonage - Chaine de production ###########################################################
##############################################################################################################

##############################################################
######### Fonction zonaRilRp - spécifications ###########
# Objectif : POur un zonage, cette méthode réalise l'appariement avec le RIL, puis avec le RP, corrige
# éventuellement les cas de rejet grâce à la couche ilot. Calcule toutes les statistiques du RP sur le zonage
# Créer plusieurs fichiers dans le repertoire de sortie :
#     1) Couche cartographique contenant le zonage et les stats issues du RP
#     2) Une base csv contenant les stats du RP sur le zonage
#     3) Un fichier excel contenant les résultats en détails des stats du RP
#     4) une base csv au niveau RP individu pouvant être fusionnée avec la table archivée du RP
#
# Précision sur la correction à l'ilot
#   Si un ilot est recouvert à XX% par un zonage alors
#   tous les individus de l'ilot sont considérés comme appartenant au zonage. 
# 
# zonage
#   Zonage sur lequel réaliser l'analyse
#   Obligatoirement un objet de type SpatialPolygonDataFrame
#   
# idZonage 
#   Valeur par défaut 1 qui indique la premiere colonne de la table attributaire
#   Obligatoirement un objet de type String ou numérique
# 
# nomZonageFinal
#   Indiquer une chaine de caractère courte qui sera utilisée comme nom pour les fichiers à enregistrer
#
# repSortie
#   Obligatoirement une chaine de caractère indiquant le répertoire de sortie pour l'enregistrement des 
#   différents fichiers. 
#
# ilotRecouvrement
#   Pourcentage compris entre 1 à 99 qui indique la surface de l'ilot à partir de laquelle
#   on considère que tous les individus doivent être ajouter au zonage
#
##############################################################
zonaRilRp <- function(zonage,idZonage=1,nomZonageFinal="zone",repSortie="Sorties",ilotRecouvrement=90){
  # Temps de calcul
  T1 <- Sys.time()
  
  ### Chargement des fonctions developpées par NK
  # Catalogue de fonctions spécifiques aux traitements cartographiques
  source("Catalogue fonctions/AMV - Fonctions Carthographiques_v2.R")
  # Catalogue de fonctions permettant de calculer les statistiques du RP à partir d'une variable
  source("Catalogue fonctions/AMV - Fonctions StatRP_v2.R")
  # Catalogue de fonctions permettant d'enregistrer les stats dans un rapport excel mis en forme 
  source("Catalogue fonctions/AMV - Fonctions StatRP_export_xlsx_v2.R")
  # Catalogue de fonctions spécifiques aux QPV (ne marche que pour le zonage QPV)  
  source("Catalogue fonctions/AMV - Fonctions special QPV.R")
  
  # Chargement des librairies dans R
  library(rgdal) # Importer de nombreux formats de données spatiales
  library(haven) # Importer des tables SAS
  library(plyr) # Pour notamment sa fonction rename()
  
  ### Bases de données ###
    # Tentative rapide : Chargement des fichiers RData du RP
    if(file.exists("Bdd/RData/rp13.RData") & (!exists("pont") | !exists("rp13i") | !exists("rp13l"))){
      load("Bdd/RData/rp13.RData")
      rp13i <<- rp13i
      rp13l <<- rp13l
      pont <<- pont
    }
  
    # Si RData n'existe par alors chargement des fichiers du RP via les fichiers leur format d'origine
      # Table CRIEM correction de la localisation de certains logements
      if(!exists("pont")){pont <<- read_sas("Bdd/Passage RIL_RP CRIEM/basedfa.sas7bdat")}
      
      # RP2013 cumulé, exploitation principale au niveau individu en SAS
      if(!exists("rp13i")){rp13i <<- read_sas("Bdd/RP13/rp13i.sas7bdat")}
      
      # RP2013 au niveau logement en SAS
      if(!exists("rp13l")){rp13l <<- read_sas("Bdd/RP13/rp13l.sas7bdat")}
  
  # Correspondance communes du RP
  if(!exists("com_rp")){com_rp <<- read.csv2(file = "Bdd/RP13/RP13_Communes_DFA.csv",header = TRUE)}
  
  ### Fond de Cartes ###  
    # Tentative rapide : Chargement des fichiers RData du RP
    if(file.exists("Bdd/RData/rilhab15.RData") & !exists("rilhab15")){
      load("Bdd/RData/rilhab15.RData")
      rilhab15 <<- rilhab15
    }
    # Si RData n'existe par alors chargement des fichiers du RP via les fichiers leur format d'origine
    # Ril habitation 2015
    if(!exists("rilhab15")){
      # Chargement de la carte
      rilhab15 <- readOGR(dsn = "Bdd/RIL/97_2015.sqlite",
                          layer = "rilhab",
                          encoding="utf8",
                          stringsAsFactors = FALSE)
      # Reprojection dans le système de coordonnées 3857
      rilhab15 <<- spTransform(x = rilhab15,
                               CRSobj = CRS("+init=epsg:3857"))
    }

### Carte Zonage ###
  # Reprojection dans le système de coordonnées 3857
  zonage <- spTransform(x = zonage,CRSobj = CRS("+init=epsg:3857"))
  
  ###################################################################################################
  ################# Traitments statistiques sur Bases et Cartes #####################################
  ###################################################################################################
  
  ### Préparation du fonc de carte zonage
  zonage@data$idZonage <- zonage@data[,idZonage]
  
  ### Jointure spatiale ril et zonage ###
  # Determine pour chaque logement du ril s'il se trouve dans le zonage. Si oui, lequel.
  ril <- zonaRil(ril = rilhab15,zonage = zonage)

  ### Preparation de la table RP et correction du code ilot et numero de rang pour certains individus 
  # a l'aide de la table pont fournie par le CRIEM
  rpi <- zonaRP(rp = rp13i,pont = pont)
  rpl <- zonaRP(rp = rp13l)
  
  ### Fusion du ril avec le RP logement ###
  # Objectif : obtenir l idZonage pour chaque logement du RP
  rpl <- merge(rpl,ril@data[,c("idx","rilNK","idZonage")],by="idx",all.x=T)
  
  # Ajout des variables issues du zonage à la table RP individu
  rpi <- merge(rpi,rpl[,c("ident_log","idZonage","rilNK")], by="ident_log",all.x=T)

  ###############################################################################
  ############# Calcul de statistiques issues du RP #############################
  ###############################################################################    
  
  # Génère les tableaux des statistiques issues du RP pour un zonage à choisir
  # Cette procédure génére un tableau par chaque statistique calculée et un tableau récapitulatif contenant
  # toutes les statistiques d'interêts
  # Attention : cette procédure renvoie une liste de data.frame
  zona_statRP <- zonaStatRp(rpi = rpi,rpl = rpl,id = "idZonage")

  # Association des statistiques du RP à la couche cartographique zonage
  zonage2 <- merge(zonage,zona_statRP[["tStatRP"]],by.x="idZonage",by.y="id",all.x=T)
  # La couche zonage2 contient donc toutes les statistiques calculées
  
  ###############################################################################
  ################# Enregistrement des bases et des couches #####################
  ###############################################################################      
  
  # Table de passage RP - zonage
  # Permet d'ajouter les informations sur les QPV au RP
  # Niveau individu #
  chemin <- paste(repSortie,"rp13i_",nomZonageFinal,".csv",sep="")
  write.csv2(rpi[,c("com","ilot","idx","C_ANNEE_COL","C_GEO","C_IMM","C_LOG","C_IND",
                    "ident_log","pont","rilNK","idZonage")]
             ,file = chemin)
  
  # Enregistrement de la couche Cartographique qpv2 en format ShapeFile #
  layerNK <- paste("c_",nomZonageFinal,sep="")
  writeOGR(zonage2, 
           dsn=paste(repSortie,layerNK,".geojson",sep=""), layer=layerNK, driver="GeoJSON")

  # Export de la base StatRp en csv
  chemin <- paste(repSortie,"stat_",nomZonageFinal,".csv",sep="")
  write.csv2(zona_statRP[["tStatRP"]],file=chemin)
  
  ###############################################################################
  ############################ Reporting ########################################
  ###############################################################################    
  
  # Création du rapport excel des indicateurs Stats
  chemin <- paste(repSortie,"Ind_details_",nomZonageFinal,".xlsx",sep="")
  zonaStatRp_Xls(zona_statRP,file = chemin) 
  
  ### Affichage du temps de calcul ###
  T2<-Sys.time()
  print(difftime(T2, T1) )
  
  
}


