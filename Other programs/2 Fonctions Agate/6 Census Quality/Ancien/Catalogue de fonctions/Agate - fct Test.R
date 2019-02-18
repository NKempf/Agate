precision_analytique_test <- function(rpa,Y,zonage,idZonage){
  
  # I. Statistiques et des variances analytiques associées, par QPV
  #---------------------------------------------------------------------------------------------------------------------------------------------------
  
  # Les chiffres de populations légales des communes sont obtenus par estimation par le ratio.
  # En fait  On a d'une part le total exhaustif de logements une année donnée et d'autre part 
  # la taille moyenne des ménages recensés cette même année. Le produit des deux donne la population estimée.
  # Cela revient à attribuer aux ménages recencés des poids différents des poids théoriques initiaux
  # de 2.5 en grandes communes et de 1 en petites communes. 
  # Ces pondérations effectives, fournies dans le fichier du RP correspondent au rapport du nombre de logements 
  # connus exhaustivement sur le nombre de logements effectivement recensés parmi ceux qui étaient connus initialement.  
  
  # En pratique, on retrouve simplement la bonne estimation grace aux pondérations fournies dans le fichier RP.
  # On utilise également ces memes poids pour toute autre statistique que le nombre d'habitants.
  
  # Les ratios par année de recensement et commune sont par contre nécessaires
  # pour le calcul de la variance analytique.
  
  Y <- substitute(Y)
  
  # rpa_save <- rpa
  # rpa$Y <- rpa$INPER
  
  rpa$comptage <- 1
  
  # I.1. Calcul des ratios estimés de logements par commune
  #--------------------------------------------------------
  rpa <- rpa %>% 
    group_by(C_ANNEE_COL,com) %>% 
    summarise(estim_totalCOM_log = sum(IPONDL*NLOG, na.rm = TRUE),
              estim_totalCOM_Y = sum(IPONDL * eval(Y), na.rm = TRUE),
              sample_size = sum(comptage, na.rm = TRUE)) %>% 
    mutate(Rchapeau_Y = estim_totalCOM_Y/estim_totalCOM_log) %>% 
    right_join(rpa,by=c("C_ANNEE_COL", "com"))
  
  # I.2. Nombre d'adresses par zone
  #--------------------------------
  rpa <- rpa %>% 
    group_by(com,C_ANNEE_COL,idZonage) %>% 
    summarise(sample_sizeZon = sum(comptage)) %>% 
    right_join(rpa,by=c("com", "C_ANNEE_COL", "idZonage"))
  
  
  # II. Estimations de population dans chaque zone et de densités
  #-----------------------------------------------------------------------------------------------------------------------------------------------------
  
  # La variance de l'estimateur du total d'une variable Y sur un domaine dépend, sans équilibrage sur ce domaine,
  # des valeurs prises par cette variable hors du domaine. 
  
  # Avec un équilibrage au niveau communal sur le nombre de logements, on s'intéresse à la variance des résidus de Y au niveau communal.
  # Equation avec estimateur par le ratio : Y = R.X + residu (X: nombre de logements)
  # Hors du domaine, la variable d'intérêt Y est nulle mais le résidu ne l'est pas car X est non nul
  # Pour chaque zone, on s'intéresse à la variance des résidus qui valent sur le domaine (ici la zone) : Y-R.X (par exemple NPER -  Rchapeau_pop*NLOG)
  # et hors du domaine : -R.X
  
  # Dans "estim" calculs de différents termes qui entrent en compte dans le calcul de la variance 
  
  # Astuce utilisée : Calcul au niveau communal de la somme des (-RX - ubarre)² de laquelle est retranchée
  # les termes correspondants aux adresses en QPV, remplacés par les termes (Y-R.X  - ubarre)²
  
  # II.1. Calcul de RX par commune
  #-------------------------------
  rpa <- rpa %>%
    group_by(com,C_ANNEE_COL) %>% 
    summarise(RX_sum = sum(NLOG*Rchapeau_Y),
              RX_carre_sum = sum((NLOG*Rchapeau_Y)^2)) %>% 
    right_join(rpa,by=c("com", "C_ANNEE_COL"))
  
  # II.2. Calcul de ubarre_Y
  #-------------------------
  rpa <- rpa %>% 
    group_by(com,C_ANNEE_COL,idZonage) %>% 
    summarise(ubarre_Y = sum(( eval(Y) - RX_sum / sample_sizeZon) / sample_size)) %>% 
    right_join(rpa,by=c("com", "C_ANNEE_COL", "idZonage"))
  
  # II.3. Estimations brutes de population et de densités de chomeurs au sein des différentes zones avec les variances associées
  #-----------------------------------------------------------------------------------------------------------------------------
  RPstats <- rpa %>%
    group_by(idZonage) %>% 
    summarise(estimRP_log_Zon = round(sum(IPONDL * NLOG),0),
              estimRP_total_Y_Zon = round(sum(IPONDL * eval(Y)),0),
              # Variance totale de Y sur la zone
              var_total_Y_Zon = 3.75 * (sum((RX_carre_sum + 2 * ubarre_Y *RX_sum + ubarre_Y^2 * sample_size) / sample_sizeZon 
                                            - (-1 * NLOG * Rchapeau_Y - ubarre_Y)^2 + (eval(Y) - Rchapeau_Y * NLOG - ubarre_Y)^2) )
    ) %>% 
    # Coefficients de variation
    mutate(CV_Y = round(100*sqrt(var_total_Y_Zon)/estimRP_total_Y_Zon, 1)) %>% 
    # Ajout de la superficie
    left_join(zonage@data[,c("idZonage","superficie")], by = "idZonage") %>% 
    mutate(
      # Densite de Y
      densite_Y = round(estimRP_total_Y_Zon * 10^6/superficie,0),
      # Coefficients de variation (densite) 
      
      # ATTENTION : A VERIFIER : semble y avoir une erreur
      CV_densite_Y = round(100*sqrt(var_total_Y_Zon)/estimRP_total_Y_Zon, 1)
    )
  
  RPstats <- RPstats[!RPstats[,"idZonage"] == "horsZon", ] 
  
  # III. Calage sur marges
  #----------------------------------------------------------------------------------------------------------------------------------------------------- 
  
  # III.1. Statistiques à la commune
  #---------------------------------
  com_RPstats <- rpa %>% 
    group_by(com) %>% 
    summarise(sample_size = sum(comptage),
              estimRP_totalCOM_log = round(sum(IPONDL * NLOG),0),
              estimRP_totalCOM_pop = round(sum(IPONDL * INPER),0),
              estimRP_totalCOM_Y = round(sum(IPONDL * eval(Y)), 0),
              var_total_Y = 3.75 * (sum( (eval(Y) - Rchapeau_Y * NLOG)^2)))
  
  # L'idée de départ du calage sur marges à partir de l'information exhaustive de l'effectif de logements
  # est d'effectuer une extension au niveau QPV de ce qui est classiquement fait au niveau communal pour le RP;
  # De fait, les poids modifiés IPONDL (sensiblement différents de 2.5) sont censés permettre de calculer
  # quasi exactement le nombre de logements au niveau communal puisqu'il y a eu calage à ce niveau.
  # Dans les faits, le nombre de logements NLOG_ril cartographiés est nettement différent de l'estimation
  # RP du nombre de logements. C'est pourquoi on peut difficilement utiliser l'information brute du ril.
  # L'idée alternative est de considérer comme exact l'effectif communal de logements fourni via le RP,
  # et de l'adapter en informations quasi-exactes au niveau QPV en maintenant constant le poids structurel de chaque
  # QPV au sein de la commune. Cela est acceptable sous réserve qu'il n'y ait pas de déformation structurelle 
  # trop importante en termes de poids relatifs des différents QPV; ce qui n'est pas toujours vrai.
  
  # III.2. Marges de calage
  #------------------------
  effectifs_Zon <- sondage %>% 
    group_by(com) %>% 
    summarise(NLOG_ril_commune = sum(NLOG_ril)) %>% 
    right_join(sondage,by= "com") %>% 
    mutate(
      #calcul des poids relatifs des QPV selon le nombre de logements cartographiés
      poidsZon = NLOG_ril/NLOG_ril_commune)
  
  effectifs_Zon <- effectifs_Zon %>% 
    left_join(com_RPstats[,c("com", "sample_size", "estimRP_totalCOM_log", "estimRP_totalCOM_pop")], by = c("com")) %>% 
    mutate(
      # Marges de calage par QPV, à partir du total communal de logements et des poids structurels des QPV
      NLOG_Zon = round(poidsZon * estimRP_totalCOM_log,0)
    )
  
  # III.3. Preparation de la base à  caler
  #---------------------------------------
  # Selection des communes comportant au moins une zone et selection des variables d'interet
  communes_avec_zon <- unique(rpa$com[!is.na(rpa[,idZonage])])
  
  rpa_calage_zon <- rpa %>% 
    select(com,idZonage,C_ANNEE_COL,IPONDL,idx,NLOG,INPER,sample_sizeZon) %>% 
    filter(com %in% communes_avec_zon) %>% 
    left_join(effectifs_Zon[, c("com", "idZonage", "poidsZon")], by=c("com", "idZonage")) %>% 
    left_join(com_RPstats[,c("com", "estimRP_totalCOM_log"),], by="com")
  
  #Retrait de la seule adresse recensée aux Abymes pour le QP971013
  # rpa_97101_QP971013 <- rpa_qpv[(rpa_qpv$com == "97101" & rpa_qpv$idZonageV == "QP971013"),]
  # rpa_calage_zon <- rpa_calage_zon[!(rpa_calage_zon$com == "97101" & rpa_calage_zon$idZonageV == "QP971013"),]
  
  # III.4. Calage sur marge par commune
  #------------------------------------
  rpa_calage_zon <- as.data.frame(rpa_calage_zon)
  rpa <- as.data.frame(rpa)
  effectifs_Zon <- as.data.frame(effectifs_Zon)
  rpa_cale <- lapply(communes_avec_zon, calage_sur_marges, rpa_calage = rpa_calage_zon, rpa_nc = rpa,list_com = communes_avec_zon, 
                     idZonage = idZonage,effectifs_Zon = effectifs_Zon) %>% 
    bind_rows()
  
  
  
return(rpa_cale)
}
  