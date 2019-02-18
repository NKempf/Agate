report_excel <- function(list_tab,
                             file="Sortie/QPV - Statistiques RP détails.xlsx",
                             sourceInsee="Insee, RP 2013"){
  
  # Création du classeur Excel
  #---------------------------------------------------------------------------------------------------------------------------------------------------
  wb <- createWorkbook(type='xlsx')
  
  # Création du classeur Excel
  #---------------------------------------------------------------------------------------------------------------------------------------------------
  sheet.1 <- createSheet(wb, sheetName = "Sommaire")
  
  sheet.1
  
  
  
  # # A=as.data.frame(matrix(2,2,2))
  # sheet.1 <- createSheet(Results_Workbook, sheetName = "Data frame")
  # addDataFrame(mtcars, sheet=sheet.1, startRow=4, 
  #              startColumn=2,row.names=FALSE)
  # setColumnWidth(sheet.1,colIndex=c(1:100),colWidth=30)
  # sheet.2 <- createSheet(Results_Workbook, sheetName = "Plot")
  # addDataFrame(rock, sheet=sheet.2, startRow=4, 
  #              startColumn=2,row.names=FALSE)
  # 
  # # ggsave("plot",example_plot, device="emf")
  # # addImage(file = "plot.emf", sheet = sheet.2, scale = 55,
  # #          startRow = 4, startColumn = 4)
  # saveWorkbook(Results_Workbook,file)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
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
  excel.ajoutTabNk(sheet,tableau = mtcars,
                   titre1 = "Synthese : Indicateurs statistiques du RP selon le zonage",
                   sources = sourceInsee,
                   champ = "Depend de la statistique consideree. Regarder les resultats en details pour connaitre le champ",
                   ligne = 1)
  
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
  