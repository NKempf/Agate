#--------------------------------------------------------------------------------------------------------------------------------#
#                      Agate - Export Excel fct                                                                                  #
#--------------------------------------------------------------------------------------------------------------------------------#

# MAJ : 26.06.2018

# Nicolas Kempf

# Utilise principalement le package openxlsx

# Convertit un nombre d'objet en chaine de caractere
var_string <- function(var) {
  deparse(substitute(var))
}


addTableaux <- function(wb, sheet,tab_list,tab_name,ligne,champ,source){
  
  ligneMax <<- ligne
  tab <- tab_list[[tab_name]]
  
  if(is.null(tab)){ return(NULL)
  }
  
    writeData(wb,sheet = sheet, x = tab_list$tab_lib[tab_name], startRow = ligneMax,headerStyle = titre.style)
    addStyle(wb, sheet, style = titre.style, rows= ligneMax,cols = 1)
    writeData(wb,sheet = sheet, tab, startRow = ligneMax + 1, headerStyle = head.style,
              borders = "rows", borderStyle = "medium")
    writeData(wb,sheet = sheet, x = champ, startRow = ligneMax + 1 + nrow(tab) + 1,headerStyle = source.style)
    writeData(wb,sheet = sheet, x = source,startRow = ligneMax + 1 + nrow(tab) + 2, headerStyle = source.style)
    
    addStyle(wb, sheet, style = source.style, rows= c(ligneMax + 1 + nrow(tab)+1 , ligneMax + 1 + nrow(tab) + 2),cols = 1 )
    
    ligneMax <<-  ligneMax + 1 + nrow(tab) + 2 + 2 
  
}



report_stat_zone <- function(tab_list, file){
  
  # Libellé des tableaux
  tab_lib <- tab_list$tab_lib
  
  # Creation du classeur
  wb <- createWorkbook()
  
  # Style
  # options("openxlsx.borderColour" = "#4F80BD")
  options("openxlsx.borderStyle" = "thin")
  options("openxlsx.borderColour" = "#4F80BD")
  modifyBaseFont(wb, fontSize = 10, fontName = "Arial")
  
  # Style du début du tableau
  head.style <<- createStyle(fgFill = "#4F81BD", halign = "CENTER", textDecoration = "Bold",
                            border = "Bottom", fontColour = "white")
  # Style des titres
  titre.style <<- createStyle(fontSize=14, textDecoration=c("bold", "italic"))
  
  # Style du champ et de la source
  source.style <<- createStyle(fontSize=9, textDecoration=c("italic"))
  
  
  
  #----------------------------------------------------------------------------------------------------------------------------------------------#
  #                                             0. Sommaire                                                                                      #
  #----------------------------------------------------------------------------------------------------------------------------------------------# 
  sheet <- "Sommaire"
  addWorksheet(wb, sheetName = sheet, gridLines = FALSE)
  setColWidths(wb, sheet =sheet,cols = c(1:3) ,widths = 20)
  setColWidths(wb, sheet =sheet,cols = c(4) ,widths = 50)
  
  sommaire.df <- data.frame(feuille = NA,Source = NA, tableau = names(tab_list[!names(tab_list) %in% c("tab_lib")]),
                            nom_tableau = tab_list$tab_lib[names(tab_list$tab_lib) %in% names(tab_list)])
  sommaire.df <- sommaire.df %>% 
    mutate(Source = case_when(substr(tableau,2,3) == "Rp" ~ "Rp",
                              substr(tableau,2,5) == "Filo" ~ "Filosofi"),
           feuille = case_when(substr(tableau,2,6) == "Rp.I." ~ "Population",
                               substr(tableau,2,7) == "Rp.II." ~ "Age et Sexe",
                               substr(tableau,2,8) == "Rp.III." ~ "Scolarisation",
                               substr(tableau,2,7) == "Rp.IV." ~ "Emploi",
                               substr(tableau,2,6) == "Rp.V." ~ "Immigration",
                               substr(tableau,2,7) == "Rp.VI." ~ "Logements",
                               substr(tableau,2,8) == "Rp.VII." ~ "Residences principales",
                               substr(tableau,2,8) == "Filo.I." ~ "Niveau de vie",
                               substr(tableau,2,9) == "Filo.II." ~ "Pauvrete"))
  
  writeData(wb,sheet = sheet, x = "Sommaire", startRow = 1)
  addStyle(wb, sheet, style = titre.style, rows= 1,cols = 1)
  writeData(wb,sheet = sheet, sommaire.df, startRow = 2, headerStyle = head.style,
            borders = "rows", borderStyle = "medium")

  #----------------------------------------------------------------------------------------------------------------------------------------------#
  #                                             I. Recensement de la population                                                                  #
  #----------------------------------------------------------------------------------------------------------------------------------------------#
  
  # Champ et Source
  source <- "Source : Insee, RP 2014."
  champ <- "Champ : Individus."
  
  # I. Population
  #--------------------------------------------------------------------------------------------------------------------------------------------------
  sheet <- "rp_Population"
  addWorksheet(wb, sheetName = sheet, gridLines = FALSE)
  setColWidths(wb, sheet =sheet,cols = c(1:10) ,widths = 20)
  
  # I.1. Population communale
  #--------------------------
  if(!is.null(tab_list$tRp.I.1)){
    addTableaux(wb,sheet,tab_list,tab_name = "tRp.I.1",ligne = 1,champ = champ,source = source)
  }else{
    ligneMax <<- 1
  }
  
  # I.2. Population par zone
  #-------------------------
  addTableaux(wb,sheet,tab_list,tab_name = "tRp.I.2",ligne = ligneMax,champ = champ,source = source)
  setColWidths(wb, sheet =  sheet,cols = c(1:10) ,widths = 20)
  
  # II. Age et Sexe
  #--------------------------------------------------------------------------------------------------------------------------------------------------
  sheet <- "Age et Sexe"
  addWorksheet(wb, sheetName = sheet, gridLines = FALSE)
  setColWidths(wb, sheet =  sheet,cols = c(1:10) ,widths = 20)
  
  # II.1. Part des moins de 20 ans et des 20 à 64 ans
  #--------------------------------------------------
  addTableaux(wb,sheet,tab_list,tab_name = "tRp.II.1",ligne = 1,champ = champ,source = source)
  
  # II.2. Part des 60 ans et plus
  #------------------------------
  addTableaux(wb,sheet,tab_list,tab_name = "tRp.II.2",ligne = ligneMax,champ = champ,source = source)
  
  # II.3. Part des femmes
  #----------------------
  addTableaux(wb,sheet,tab_list,tab_name = "tRp.II.3",ligne = ligneMax,champ = champ,source = source)
  
  # # II.4. Pyramide des ages par sexe et par zonage
  # #-----------------------------------------------
  # addTableaux(wb,sheet,tab_name = "tRp.II.4",ligne = ligneMax,champ = champ,source = source)
  # 
  # # II.5. Pyramide par sexe, zonage et tranche d'age
  # #------------------------------------------------
  # addTableaux(wb,sheet,tab_name = "tRp.II.5",ligne = ligneMax,champ = champ,source = source)
  
  # III. Scolarisation
  #--------------------------------------------------------------------------------------------------------------------------------------------------
  sheet <- "Scolarisation"
  addWorksheet(wb, sheetName = sheet, gridLines = FALSE)
  setColWidths(wb, sheet =  sheet,cols = c(1:10) ,widths = 20)
  
  # III.1. Taux de scolarisation des 2 à 5 ans et des 18 à 25 ans
  #--------------------------------------------------------------
  addTableaux(wb,sheet,tab_list,tab_name = "tRp.III.1",ligne = 1,champ = "Champ : Individus ages de 2 à 5 ans et de 18 à 25 ans.",source = source)
  
  # III.2. Part de non scolarisation de plus de 15 ans sans diplome
  #----------------------------------------------------------------
  addTableaux(wb,sheet,tab_list,tab_name = "tRp.III.2",ligne = ligneMax,champ = "Champ : Individus sans diplomes et non scolarisés ages de 15 ans et plus.",source = source)
  
  # III.3. Taux de non scolarisation des 6 - 14 ans
  #-----------------------------------------------
  addTableaux(wb,sheet,tab_list,tab_name = "tRp.III.3",ligne = ligneMax,champ = champ,source = source)
  
  # III.4. Taux de décrocheur : jeune non scolarisé de 16 - 25 ans sans diplome
  #----------------------------------------------------------------------------
  addTableaux(wb,sheet,tab_list,tab_name = "tRp.III.4",ligne = ligneMax,champ = "Champ : Individus non scolarisé sans diplome ages de 16 à 25 ans.",source = source)
  
  # IV. Emploi
  #-----------------------------------------------------------------------------------------------------------------------------------------------
  sheet <- "Emploi"
  addWorksheet(wb, sheetName = sheet, gridLines = FALSE)
  setColWidths(wb, sheet =  sheet,cols = c(1:10) ,widths = 20)
  
  # IV.1. Taux de chomage pour les 15 - 64 ans (parmi les actifs)
  #--------------------------------------------------------------
  addTableaux(wb,sheet,tab_list,tab_name = "tRp.IV.1",ligne = 1,champ = "Champ : Individus ages de 15 à 64 ans.",source = source)
  
  # IV.2. Taux d'actif  - d'inactif pour les 15 - 64 ans
  #-----------------------------------------------------
  addTableaux(wb,sheet,tab_list,tab_name = "tRp.IV.2",ligne = ligneMax,champ = "Champ : Individus ages de 15 à 64 ans.",source = source)
  
  # IV.3. Taux d'actif selon le sexe pour les 15 - 64 ans
  #------------------------------------------------------
  addTableaux(wb,sheet,tab_list,tab_name = "tRp.IV.3",ligne = ligneMax,champ = "Champ : Individus ages de 15 à 64 ans.",source = source)
  
  # IV.4. Part des actifs de 15-64 ans cadres et professions intermédiaire
  #-----------------------------------------------------------------------
  addTableaux(wb,sheet,tab_list,tab_name = "tRp.IV.4",ligne = ligneMax,champ = "Champ : Individus ages de 15 à 64 ans.",source = source)
  
  # V. Immigration
  #-----------------------------------------------------------------------------------------------------------------------------------------------
  sheet <- "Immigration"
  addWorksheet(wb, sheetName = sheet, gridLines = FALSE)
  setColWidths(wb, sheet =  sheet,cols = c(1:10) ,widths = 20)
  
  # V.1. Part des etrangers
  #------------------------
  addTableaux(wb,sheet,tab_list,tab_name = "tRp.V.1",ligne = 1,champ = champ,source = source)
  
  # V.2. Part des immigres
  #-----------------------
  addTableaux(wb,sheet,tab_list,tab_name = "tRp.V.2",ligne = ligneMax,champ = champ,source = source)
  
  # VI. Logements
  #-----------------------------------------------------------------------------------------------------------------------------------------------
  sheet <- "Logements"
  addWorksheet(wb, sheetName = sheet, gridLines = FALSE)
  setColWidths(wb, sheet =  sheet,cols = c(1:10) ,widths = 20)
  
  # VI.1. Part de logements selon la catégorie de logement
  #-------------------------------------------------------
  addTableaux(wb,sheet,tab_list,tab_name = "tRp.VI.1",ligne = 1,champ = champ,source = source)
  
  # VI.2. Part des appartements
  #----------------------------
  addTableaux(wb,sheet,tab_list,tab_name = "tRp.VI.2",ligne = ligneMax,champ = champ,source = source)
  
  # VI.3. Part des locataires
  #--------------------------
  addTableaux(wb,sheet,tab_list,tab_name = "tRp.VI.3",ligne = ligneMax,champ = champ,source = source)
  
  # VI.4. Part des locatairesHlm
  #-----------------------------
  addTableaux(wb,sheet,tab_list,tab_name = "tRp.VI.4",ligne = ligneMax,champ = champ,source = source)
  
  # VII. Résidences principales
  #-----------------------------------------------------------------------------------------------------------------------------------------------
  sheet <- "Residences principales"
  addWorksheet(wb, sheetName = sheet, gridLines = FALSE)
  setColWidths(wb, sheet =  sheet,cols = c(1:10) ,widths = 20)
  
  # VII.1. Part des résidences principales de plus de 100m²
  #--------------------------------------------------------
  addTableaux(wb,sheet,tab_list,tab_name = "tRp.VII.1",ligne = 1,champ = "Champ : Résidences principales.",source = source)
  
  # VII.2. Part des résidences principales en HLM
  #----------------------------------------------
  addTableaux(wb,sheet,tab_list,tab_name = "tRp.VII.2",ligne = ligneMax,champ = "Champ : Résidences principales.",source = source)
  
  # VII.3. Part des résidences principales sans eau chaude
  #-------------------------------------------------------  
  addTableaux(wb,sheet,tab_list,tab_name = "tRp.VII.3",ligne = ligneMax,champ = "Champ : Résidences principales.",source = source)
  
  # VII.4. Part des résidences principales sans bain ni douche 
  #-----------------------------------------------------------
  addTableaux(wb,sheet,tab_list,tab_name = "tRp.VII.4",ligne = ligneMax,champ = "Champ : Résidences principales.",source = source)
  
  # VII.5. Part des résidences principales sans tout a l'egout
  #-----------------------------------------------------------
  addTableaux(wb,sheet,tab_list,tab_name = "tRp.VII.5",ligne = ligneMax,champ = "Champ : Résidences principales.",source = source)
  
  #----------------------------------------------------------------------------------------------------------------------------------------------#
  #                                             II. Statistiques issues de Filosofi                                                              #
  #----------------------------------------------------------------------------------------------------------------------------------------------#
  
  # Champ et Source
  source <- "Source : Insee, filosofi 2015."
  champ <- "Champ : ménages fiscaux."
  
  # I. Statistiques du niveau de vie
  #-----------------------------------------------------------------------------------------------------------------------------------------------
  sheet <- "Niveau de vie"
  addWorksheet(wb, sheetName = sheet, gridLines = FALSE)
  setColWidths(wb, sheet =  sheet,cols = c(1:10) ,widths = 20)
  
  # I.1. Distribution du niveau de vie selon le zonage
  #---------------------------------------------------
  addTableaux(wb,sheet,tab_list,tab_name = "tFilo.I.1",ligne = 1,champ = champ,source = source)
  
  # I.2. Distribution du niveau de vie selon le type de ménage et le zonage
  #------------------------------------------------------------------------
  addTableaux(wb,sheet,tab_list,tab_name = "tFilo.I.2",ligne = ligneMax,champ = champ,source = source)
  
  # I.3. Part des ménages selon le type de ménage et le zonage
  #-----------------------------------------------------------
  addTableaux(wb,sheet,tab_list,tab_name = "tFilo.I.3",ligne = ligneMax,champ = champ,source = source)
  
  # I.4. Part des ménages selon le décile de niveau de vie métro et le zonage
  #--------------------------------------------------------------------------
  addTableaux(wb,sheet,tab_list,tab_name = "tFilo.I.4",ligne = ligneMax,champ = champ,source = source)
  
  # II. Pauvreté
  #-----------------------------------------------------------------------------------------------------------------------------------------------
  sheet <- "Pauvrete"
  addWorksheet(wb, sheetName = sheet, gridLines = FALSE)
  setColWidths(wb, sheet =  sheet,cols = c(1:10) ,widths = 20)
  
  # II.1 Taux de pauvreté selon le seuil métropolitain et départemental
  #--------------------------------------------------------------------
  addTableaux(wb,sheet,tab_list,tab_name = "tFilo.II.1",ligne = 1,champ = champ,source = source)
  
  # II.2 Courbe de lorenz
  #----------------------
  addTableaux(wb,sheet,tab_list,tab_name = "tFilo.II.2",ligne = ligneMax,champ = champ,source = source)
  
  # Enregistrement du classeur Excel
  #----------------------------------------------------------------------------------------------------------------------------------------------
  saveWorkbook(wb, file = file, overwrite = TRUE)
  
  # openXL(wb)

}





