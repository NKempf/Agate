##############################################################################################################
#################### Nicolas Kempf - Fonctions statistiques du RP export en xlsx #############################
##############################################################################################################

# MAJ : 18.01.2018
#-----------------

zonaQpvReporting <- function(liste_df,
                           file="Sortie/QPV - Statistiques RP détails.xlsx",
                           sourceInsee="Insee, RP 2013"){
  
  # Librairie indispensable
  library(xlsx) # Lire et ecrire en Excel
  
  
  # Création du classeur Excel
  #---------------------------------------------------------------------------------------------------------------------------------------------------
  wb<<-createWorkbook(type="xlsx")
  # Chargement des styles
  excel.styleNk(wb)
  
  
  # FEUILLE Sommaire
  #---------------------------------------------------------------------------------------------------------------------------------------------------
      sommaireXls <<- createSheet(wb, sheetName = "Sommaire")
      somXlsLigne <<- 1
      xlsx.addTitle(sommaireXls,rowIndex = somXlsLigne,"Table des matieres",TITLE_STYLE)
      somXlsLigne <<- somXlsLigne + 1
      # On commence à la deuxième ligne
      excel.sommaireNk(sommaireXls,texte1 = "Feuille",
                       texte2 = "Titre Tableau", rowIndex = somXlsLigne,titleStyle = TABLE_COLNAMES_STYLE)
      
  # FEUILLE  Synthèse
  #---------------------------------------------------------------------------------------------------------------------------------------------------    
      # Créer une nouvelle feuille dans le classeur
      sheet <<- createSheet(wb, sheetName = "Synthese")
      
      # Ajout du tableau1
      excel.ajoutTabNk(sheet,tableau = liste_df[["tStatRP"]],
                       titre1 = "Synthese : Indicateurs statistiques du RP selon le zonage",
                       sources = sourceInsee,
                       champ = "Depend de la statistique consideree. Regarder les resultats en details pour connaitre le champ",
                       ligne = 1)
      
  # FEUILLE  Population
  #----------------------------------------------------------------------------------------------------------------------------------------------------
      # Créer une nouvelle feuille dans le classeur
      sheet <<- createSheet(wb, sheetName = "Population")
      
      # Ajout du tableau1
      excel.ajoutTabNk(sheet,tableau = liste_df[["t0a"]],
                       titre1 = "Population des qpv par Communes",
                       sources = sourceInsee,
                       champ = "Individus",
                       ligne = 1)
      
      # Ajout du tableau
      excel.ajoutTabNk(sheet,tableau = liste_df[["t0b"]],
                       titre1 = "Population par QPV",
                       sources = sourceInsee,
                       champ = "Individus",
                       ligne = ligneMax)
  
  # FEUILLE  Age
  #----------------------------------------------------------------------------------------------------------------------------------------------------
      # Créer une nouvelle feuille dans le classeur
      sheet <- createSheet(wb, sheetName = "Age")
      
      # Ajout du tableau1
      excel.ajoutTabNk(sheet,tableau = liste_df[["t1"]],
                       titre1 = "Part des individus par tranche d'age selon le QPV",
                       sources = sourceInsee,
                       ligne = 1)
      
      # Ajout du tableau
      excel.ajoutTabNk(sheet,tableau = liste_df[["t1b"]],
                       titre1 = "Part des individus de 60 ans et plus selon le QPV",
                       sources = sourceInsee,
                       champ = "Individus ages entre 18 et 24 ans",
                       ligne = ligneMax)
      
      
  # FEUILLE  Sexe
  #-----------------------------------------------------------------------------------------------------------------------------------------------------
      # Créer une nouvelle feuille dans le classeur
      sheet <- createSheet(wb, sheetName = "Sexe")
      
      # Ajout du tableau1
      excel.ajoutTabNk(sheet,tableau = liste_df[["t1c"]],
                       titre1 = "Part des individus par sexe selon le QPV",
                       sources = sourceInsee,
                       ligne = 1)
      
  # FEUILLE  Scolarisation
  #-----------------------------------------------------------------------------------------------------------------------------------------------------
      # Créer une nouvelle feuille dans le classeur
      sheet <- createSheet(wb, sheetName = "Scolarisation")
      
      # Ajout du tableau
      excel.ajoutTabNk(sheet,tableau = liste_df[["t2a"]],
                       titre1 = "Taux de scolarisation des 2 a 5 ans",
                       sources = sourceInsee,
                       champ = "Individus ages entre 2 et 5 ans",
                       ligne = 1)
      
      # Ajout du tableau
      excel.ajoutTabNk(sheet,tableau = liste_df[["t2b"]],
                       titre1 = "Taux de scolarisation des 18 a 24 ans",
                       sources = sourceInsee,
                       champ = "Individus ages entre 18 et 24 ans",
                       ligne = ligneMax)
      
      # Ajout du tableau
      excel.ajoutTabNk(sheet,tableau = liste_df[["t2c"]],
                       titre1 = "Taux de non scolarisation des sans diplome de plus de 15 ans",
                       champ = "Individus sans diplome et ages de 15 ans et plus",
                       sources = sourceInsee,
                       ligne = ligneMax)
      
      # Ajout du tableau
      excel.ajoutTabNk(sheet,tableau = liste_df[["t2d"]],
                       titre1 = "Part des non scolarises de 6 a 14 ans",
                       champ = "Individus sans diplome et ages de 6 a 14 ans.",
                       sources = sourceInsee,
                       ligne = ligneMax)
      
      # Ajout du tableau
      excel.ajoutTabNk(sheet,tableau = liste_df[["t2e"]],
                       titre1 = "Taux de decrocheurs scolaires",
                       champ = "Individus ages de 16 a 25 ans.",
                       sources = sourceInsee,
                       ligne = ligneMax)
      
  # FEUILLE  Emploi
  #-----------------------------------------------------------------------------------------------------------------------------------------------------
      # Créer une nouvelle feuille dans le classeur
      sheet <- createSheet(wb, sheetName = "Emploi")
      
      # Ajout du tableau
      excel.ajoutTabNk(sheet,tableau = liste_df[["t3a"]],
                       titre1 = "Taux de chomage",
                       sources = sourceInsee,
                       champ = "Individus actif ages entre 15 et 64 ans",
                       ligne = 1)
      
      # Ajout du tableau
      excel.ajoutTabNk(sheet,tableau = liste_df[["t3b"]],
                       titre1 = "Taux d'actif et d'inactif",
                       champ = "Individus ages entre 15 et 64 ans",
                       sources = sourceInsee,
                       ligne = ligneMax)
      
      # Ajout du tableau
      excel.ajoutTabNk(sheet,tableau = liste_df[["t3c"]],
                       titre1 = "Taux d'activite des femmes",
                       champ = "Femmes agees entre 15 et 64 ans",
                       sources = sourceInsee,
                       ligne = ligneMax)
      
      # Ajout du tableau
      excel.ajoutTabNk(sheet,tableau = liste_df[["t3d"]],
                       titre1 = "Taux d'activite des hommes",
                       champ = "Hommes ages entre 15 et 64 ans",
                       sources = sourceInsee,
                       ligne = ligneMax)
      
      # Ajout du tableau
      excel.ajoutTabNk(sheet,tableau = liste_df[["t3e"]],
                       titre1 = "Part des actifs cadres ou profession intermediaire",
                       champ = "Individus ages entre 15 et 64 ans",
                       sources = sourceInsee,
                       ligne = ligneMax)
      
  # FEUILLE  Immigration  
  #----------------------------------------------------------------------------------------------------------------------------------------------------
      # Créer une nouvelle feuille dans le classeur
      sheet <- createSheet(wb, sheetName = "Immigration")
      
      # Ajout du tableau
      excel.ajoutTabNk(sheet,tableau = liste_df[["t4"]],
                       titre1 = "Part des etrangers",
                       sources = sourceInsee,
                       champ = "Individus",
                       ligne = 1)
      
      # Ajout du tableau
      excel.ajoutTabNk(sheet,tableau = liste_df[["t5"]],
                       titre1 = "Taux des immigres",
                       champ = "individus",
                       sources = sourceInsee,
                       ligne = ligneMax)
      
      
  # FEUILLE  Logement
  #----------------------------------------------------------------------------------------------------------------------------------------------------
      # Créer une nouvelle feuille dans le classeur
      sheet <- createSheet(wb, sheetName = "Logement")
      
      # Ajout du tableau
      excel.ajoutTabNk(sheet,tableau = liste_df[["tl0"]],
                       titre1 = "Part des logements selon la categorie du logement",
                       sources = sourceInsee,
                       champ = "Logement",
                       ligne = 1)
      
  # FEUILLE  Residence principale
  #----------------------------------------------------------------------------------------------------------------------------------------------------
      # Créer une nouvelle feuille dans le classeur
      sheet <- createSheet(wb, sheetName = "Residence principale")
      
      # Ajout du tableau
      excel.ajoutTabNk(sheet,tableau = liste_df[["tl1"]],
                       titre1 = "Part des residences de 100 m2 et plus",
                       sources = sourceInsee,
                       champ = "Residences principales",
                       ligne = 1)
      
      # Ajout du tableau
      excel.ajoutTabNk(sheet,tableau = liste_df[["tl1b"]],
                       titre1 = "Part des residences en HLM",
                       champ = "Residences principales",
                       sources = sourceInsee,
                       ligne = ligneMax)
      
      # Ajout du tableau
      excel.ajoutTabNk(sheet,tableau = liste_df[["tl1c"]],
                       titre1 = "Part des résidences sans eau chaude",
                       champ = "Residences principales",
                       sources = sourceInsee,
                       ligne = ligneMax)
      
      # Ajout du tableau
      excel.ajoutTabNk(sheet,tableau = liste_df[["tl1d"]],
                       titre1 = "Part des residences sans bain ni douche",
                       champ = "Residences principales",
                       sources = sourceInsee,
                       ligne = ligneMax)
      
  
      # Ajout du tableau
      excel.ajoutTabNk(sheet,tableau = liste_df[["tl1e"]],
                       titre1 = "Part des residences sans tout a l'egout",
                       champ = "Residences principales",
                       sources = sourceInsee,
                       ligne = ligneMax)
      
  # FEUILLE  Locataire
  #-----------------------------------------------------------------------------------------------------------------------------------------------------
      # Créer une nouvelle feuille dans le classeur
      sheet <- createSheet(wb, sheetName = "Locataire")            
      
      # Ajout du tableau
      excel.ajoutTabNk(sheet,tableau = liste_df[["tl2"]],
                       titre1 = "Part des locataires",
                       champ = "Menages",
                       sources = sourceInsee,
                       ligne = 1)
      
      # Ajout du tableau
      excel.ajoutTabNk(sheet,tableau = liste_df[["tl3"]],
                       titre1 = "Part des locataires en HLM",
                       champ = "Menages",
                       sources = sourceInsee,
                       ligne = ligneMax)
      
      # Ajout du tableau
      excel.ajoutTabNk(sheet,tableau = liste_df[["tl4"]],
                       titre1 = "Part des appartements",
                       champ = "Logements",
                       sources = sourceInsee,
                       ligne = ligneMax)
      
      # # Ajout du tableau
      # excel.ajoutTabNk(sheet,tableau = liste_df[["tl5"]],
      #                  titre1 = "Part des menages composes d'une seule personne",
      #                  champ = "Menages",
      #                  sources = sourceInsee,
      #                  ligne = ligneMax)
      
      
  # FEUILLE  Filosofi
  #------------------------------------------------------------------------------------------------------------------------------------------------------
      # Créer une nouvelle feuille dans le classeur
      sheet <- createSheet(wb, sheetName = "Filosofi")  
      
      # Ajout du tableau
      tmp <- liste_df[["filoStat.nivvie"]]
      colnames(tmp) <- c("idZonage","Nombre de menages fiscaux","Niveau de vie moyen","Niveau de vie median",
                                       "Niveau de vie minimum","1er decile du niveau de vie","1er quintile du niveau de vie","1er quartile du niveau de vie",
                                       "3eme quartile du niveau de vie","4eme quintile du niveau de vie","9eme decile du niveau de vie",
                                       "95eme percentile du niveau de vie","Niveau de vie maximum")
      excel.ajoutTabNk(sheet,tableau = tmp,
                       titre1 = "Niveau de vie annuel",
                       champ = "Menages fiscaux",
                       sources = "Insee, filosofi 2014.",
                       ligne = 1)
      rm(tmp)
      
      # Ajout du tableau
      tmp <- liste_df[["filoStat.revdec"]]
      colnames(tmp) <-  c("idZonage","Nombre de menages fiscaux","Revenu declare moyen","Revenu declare median",
                          "Revenu declare minimum","1er decile du revenu declare","1er quintile du revenu declare",
                          "1er quartile du revenu declare",
                          "3eme quartile du revenu declare","4eme quintile du revenu declare","9eme decile du revenu declare",
                          "95eme percentile du revenu declare","Revenu declare maximum")
      excel.ajoutTabNk(sheet,tableau = tmp,
                       titre1 = "Revenu declare annuel",
                       champ = "Menages fiscaux",
                       sources = "Insee, filosofi 2014.",
                       ligne = ligneMax)
      rm(tmp)
      # Ajout du tableau
      tmp <- liste_df[["filoStat.revdisp"]]
      colnames(tmp) <-  c("idZonage","Nombre de menages fiscaux","Revenu disponible moyen","Revenu disponible median",
                          "Revenu disponible minimum","1er decile du revenu disponible","1er quintile du revenu disponible",
                          "1er quartile du revenu disponible",
                          "3eme quartile du revenu disponible","4eme quintile du revenu disponible","9eme decile du revenu disponible",
                          "95eme percentile du revenu disponible","Revenu disponible maximum")
      
      excel.ajoutTabNk(sheet,tableau = tmp,
                       titre1 = "Revenu disponible annuel",
                       champ = "Menages fiscaux",
                       sources = "Insee, filosofi 2014.",
                       ligne = ligneMax)
      rm(tmp)
      # Ajout du tableau
      excel.ajoutTabNk(sheet,tableau = liste_df[["statFilo.typmenR"]],
                       titre1 = "Revenus annuels selon le type de menage",
                       champ = "Menages fiscaux",
                       sources = "Insee, filosofi 2014.",
                       ligne = ligneMax)
      
      
      
      # Changer la largeur des colonnes
      setColumnWidth(sheet, colIndex=c(1:ncol(state.x77)), colWidth=11)
      
  # ETAPE FINALE
  #----------------------------------------------------------------------------------------------------------------------------------------------------
      # Enregistrer le classeur dans un fichier
      saveWorkbook(wb, file)   
  
}


############### Export des résultats en excel #########################

#++++++++++++++++++++++++
# Fonction helper pour ajouter des titres
#++++++++++++++++++++++++
# - sheet : la feuille Excel pour contenir le titre
# - rowIndex : numéro de la ligne pour contenir le titre 
# - title : texte du titre
# - titleStyle : l'objet style pour le titre
xlsx.addTitle<-function(sheet, rowIndex, title, titleStyle){
  rows <-createRow(sheet,rowIndex=rowIndex)
  sheetTitle <-createCell(rows, colIndex=1)
  setCellValue(sheetTitle[[1,1]], title)
  setCellStyle(sheetTitle[[1,1]], titleStyle)
}

## Style des titres et des sous-titres (version Nk)
# wb : worbook package (xlsx)
excel.styleNk <- function(wb){
  TITLE_STYLE <<- CellStyle(wb)+ Font(wb,  heightInPoints=16 
                                      , isBold = , underline=1)
  SUB_TITLE_STYLE <<- CellStyle(wb) + 
    Font(wb,  heightInPoints=12, 
         isItalic=TRUE, isBold=FALSE)
  # Styles pour les noms de lignes/colonnes
  TABLE_ROWNAMES_STYLE <<- CellStyle(wb) + Font(wb, isBold=TRUE) +
    Alignment(wrapText=TRUE, horizontal="ALIGN_CENTER") +
    Border(color="black", position=c("RIGHT"))
  TABLE_COLNAMES_STYLE <<- CellStyle(wb) + Font(wb, isBold=TRUE) +
    Alignment(wrapText=TRUE, horizontal="ALIGN_CENTER") +
    Border(color="black", position=c("BOTTOM"))
  
  LABEL_COL_STYLE <<- CellStyle(wb) + Font(wb, heightInPoints=12,isBold=TRUE) +
    Alignment(wrapText=TRUE, horizontal="ALIGN_CENTER")
  
  SIMPLE_TEXTE <<- CellStyle(wb) + 
    Font(wb,  heightInPoints=10, 
         isItalic=F, isBold=FALSE)
  
}

# Gestion des labels de colonnes
excel.addLabelCol <- function(sheet,tableau,rowIndex,colIndex=1,titleStyle=LABEL_COL_STYLE){
  #Création d'une ligne dans la feuille
  rows <-createRow(sheet,rowIndex=rowIndex)
  #Position de la dernière cellule
  fin <- colIndex + length(colnames(tableau)) - 1
  
  for (i in colIndex:fin){
    # Création d'une cellule
    sheetTitle <-createCell(rows, colIndex=i+1)
    #Création du texte
    texte <- " "
    if (!is.null(attr(tableau[,c(i)], "label"))){
      texte <- as.character(attr(tableau[,c(i)], "label"))
    }
    setCellValue(sheetTitle[[1,1]],texte)
    setCellStyle(sheetTitle[[1,1]], titleStyle)
  }
}

# Pour construire des sommaires
excel.sommaireNk <- function(sheet,texte1 ,texte2=NA ,rowIndex = 1 , titleStyle= SIMPLE_TEXTE){
  
  rows <-createRow(sheet,rowIndex=rowIndex)
  if(!is.na(texte1)){
    sheetTitle1 <-createCell(rows, colIndex=1)
    setCellValue(sheetTitle1[[1,1]],texte1)
    setCellStyle(sheetTitle1[[1,1]], titleStyle)
  }
  if(!is.na(texte2)){
    sheetTitle2 <-createCell(rows, colIndex=2)
    setCellValue(sheetTitle2[[1,1]],texte2)
    setCellStyle(sheetTitle2[[1,1]], titleStyle)
  }
  # Saut de ligne
  somXlsLigne <<- rowIndex + 1
}



excel.ajoutTabNk <- function(sheet,tableau,ligne,titre1,
                             titre2=NA,
                             champ=NA,sources=NA){
  
###########################  
### Gestion des styles ####
###########################   
  # Chargement des styles  
  excel.styleNk(wb)
  
###########################  
### Gestion du sommaire ###
###########################   
  
  # Feuille sommaire : création si elle n'existe pas
  if(!exists("sommaireXls")){
    sommaireXls <<- createSheet(wb, sheetName = "Sommaire")
    somXlsLigne <<- 1
    xlsx.addTitle(sommaireXls,rowIndex = somXlsLigne,"Table des matieres",TITLE_STYLE)
    somXlsLigne <<- somXlsLigne + 1
    # On commence à la deuxième ligne
    excel.sommaireNk(sommaireXls,texte1 = "Feuille",
                     texte2 = "Titre Tableau", rowIndex = somXlsLigne,titleStyle = TABLE_COLNAMES_STYLE)
  }
   # ajout du tableau au sommaire
    excel.sommaireNk(sommaireXls,texte1 = sheet$getSheetName(),
                     texte2 = titre1, rowIndex = somXlsLigne)
 
 
###########################  
### Gestion du tableau ####
###########################    
  # Ajouter un titre
  xlsx.addTitle(sheet, rowIndex=ligne, title=titre1,
                titleStyle = TITLE_STYLE)
  # Ajouter un sous-titre
  if(!is.na(titre2)){
    # incrémentation de la ligne
    ligne <- ligne+1
    xlsx.addTitle(sheet, rowIndex=ligne, 
                  title=titre2,
                  titleStyle = SUB_TITLE_STYLE)
  }
  # incrémentation de la ligne
  ligne <- ligne+1
  
  # Ajout des labels des colonnes
  excel.addLabelCol(sheet = sheet,tableau = tableau,rowIndex = ligne)
  
  # incrémentation de la ligne
  ligne <- ligne+1
  
  # Ajouter une table
  addDataFrame(tableau, sheet, startRow=ligne, startColumn=1, 
               colnamesStyle = TABLE_COLNAMES_STYLE,
               rownamesStyle = TABLE_ROWNAMES_STYLE)
  
  
  ligne <- ligne + length(tableau[,1])
  
  # Ajout du champ
  if(!is.na(champ)){
    # incrémentation de la ligne
    ligne <- ligne+1
    xlsx.addTitle(sheet, rowIndex=ligne, 
                  title=paste("Champ :",champ,sep=" "),
                  titleStyle = SIMPLE_TEXTE)
  }
  
  # Ajout de la source
  if(!is.na(sources)){
    # incrémentation de la ligne
    ligne <- ligne+1
    xlsx.addTitle(sheet, rowIndex=ligne, 
                  title=paste("Source :",sources,sep=" "),
                  titleStyle = SIMPLE_TEXTE)
  }
  
  # Enregistrement de la ligne maximale (pratique si on veut ajouter plusieurs tableaux à la suite)
  ligneMax <<- ligne + 2
  
}
