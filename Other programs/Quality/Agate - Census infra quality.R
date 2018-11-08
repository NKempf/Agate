#-------------------------------------------------------------------------------------#
#                     Agate - Fonctions estimation de la qualite du RP infra          #
#-------------------------------------------------------------------------------------#

# Nicolas Kempf

# Derniere MAJ : 31.10.2018

# Les fonctions ci-dessus sont de légères modifications des travaux de Lionel Delta (stage ENSAE)


# Base sondage (special calage)
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
sondageZon <- function(rpa){

  # Calcul des effectifs d'adresses ril, rp et rp-ril par commune et par zone
  sondage <- rpa %>% 
    group_by(com, idZonage) %>% 
    summarise(NLOG_ril = sum(NLOG_ril),
              NLOG_recensement = sum(NLOG_recensement,na.rm = TRUE),
              NBadresses_ril = sum(carto_ril),
              NBadresses_40prcent = round(sum(carto_ril*.4),0),
              NBadresses_recensement = sum(recensement,na.rm = TRUE),
              NBadresses_ril_et_recensement = sum(rETc,na.rm = TRUE)
              )

  return(sondage)
}



precision_analytique_nc <- function(rpa,Y,zonage,idZonage, sondage){
  
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

  # Y <- substitute(INPCM)
  Y <- substitute(Y)

  # rpa_save <- rpa
  # rpa$Y <- rpa$INPER
  rpa$comptage <- 1
  rpa <- rpa %>% 
    filter(recensement == 1) %>%
    rename(NLOG = NLOG_recensement)
  
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
    mutate(CV_Y = round(100*sqrt(var_total_Y_Zon)/estimRP_total_Y_Zon, 1)) 
  
  
  # ATTENTION : A VERIFIER : semble y avoir une erreur
  # %>% 
  #   # Ajout de la superficie
  #   left_join(zonage@data[,c("idZonage","superficie")], by = "idZonage") %>% 
  #   mutate(
  #     # Densite de Y
  #     densite_Y = round(estimRP_total_Y_Zon * 10^6/superficie,0),
  #     # Coefficients de variation (densite) 
  #     
  #     
  #     CV_densite_Y = round(100*sqrt(var_total_Y_Zon)/estimRP_total_Y_Zon, 1)
  #   )
  
  RPstats <- RPstats[!RPstats[,"idZonage"] == "horsZon", ] 

  # renommage des variables
  # colnames(RPstats) <- c(idZonage, "estimRP_log_Zon", paste0("estimRP_total_", Y, "_Zon"),
                         # paste0("var_total_", Y, "_Zon"), paste0("CV_", Y),"superficie_Zon", paste0("densite_", Y), paste0("CV_densite_", Y))
  
  # III. Calage sur marges
  #----------------------------------------------------------------------------------------------------------------------------------------------------- 
  
  # Pour simplifier les opérations par la suite : restriction aux communes qui ont au moins un zonage.
  commune_acZonage <- unique(rpa$com[rpa$idZonage != "horsZon"])
  # commune_acZonage <- c("97101", "97104", "97105", "97107", "97117", "97120", "97129",
  #                        "97209", "97213", "97222", "97228", "97302", "97304", "97305", 
  #                        "97307", "97309", "97311")
  
  # III.1. Statistiques à la commune
  #---------------------------------
  com_RPstats <- rpa %>% 
    # filter(com %in% commune_acZonage) %>% 
    group_by(com) %>% 
    summarise(sample_size = sum(comptage),
              estimRP_totalCOM_log = round(sum(IPONDL * NLOG),0),
              estimRP_totalCOM_pop = round(sum(IPONDL * INPER),0),
              estimRP_totalCOM_Y = round(sum(IPONDL * eval(Y)), 0),
              var_total_Y = 3.75 * (sum( (eval(Y) - Rchapeau_Y * NLOG)^2))) %>% 
    mutate(CV_total_Y = round(100*sqrt(var_total_Y)/estimRP_totalCOM_Y, 1))
  
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
      poidsZon = NLOG_ril/NLOG_ril_commune) %>% 
    filter(com %in% commune_acZonage)
 
  effectifs_Zon <- effectifs_Zon %>% 
    left_join(com_RPstats[,c("com", "sample_size", "estimRP_totalCOM_log", "estimRP_totalCOM_pop")], by = c("com")) %>% 
    mutate(
      # Marges de calage par QPV, à partir du total communal de logements et des poids structurels des QPV
      NLOG_Zon = round(poidsZon * estimRP_totalCOM_log,0)
             )
  
  effectifs_Zon <- effectifs_Zon[!(effectifs_Zon$com == "97101" & effectifs_Zon$idZonage == "QP971013"),] 
  
  # III.3. Preparation de la base à  caler
  #---------------------------------------
  # Selection des communes comportant au moins une zone et selection des variables d'interet
  rpa_calage_zon <- rpa %>% 
    select(com,idZonage,C_ANNEE_COL,IPONDL,idx,NLOG,INPER,sample_sizeZon,carto_ril) %>% 
    filter(com %in% commune_acZonage) %>% 
    left_join(effectifs_Zon[, c("com", "idZonage", "poidsZon")], by=c("com", "idZonage")) %>% 
    left_join(com_RPstats[,c("com", "estimRP_totalCOM_log"),], by="com") %>% 
    as.data.frame()
  
  
  #Retrait de la seule adresse recensée aux Abymes pour le QP971013
  rpa_calage_zon <- rpa_calage_zon[!(rpa_calage_zon$com == "97101" & rpa_calage_zon$idZonage == "QP971013"),]
  
  # rpa_calage_zon <- as.data.frame(rpa_calage_zon) %>% 
  #   filter(idZonage != "horsZon")
  # rpa <- as.data.frame(rpa) %>% 
  #   filter(idZonage != "horsZon")
  # effectifs_Zon <- as.data.frame(effectifs_Zon)
  
  # III.4. Calage sur marge par commune
  #------------------------------------
  rpa_calage_zon <- rpa_calage_zon[order(rpa_calage_zon$idx),]
  rpa <- rpa[order(rpa$idx),]
  
  
  rpa_cale <- lapply(commune_acZonage, calage_sur_marges, rpa_calage = rpa_calage_zon, rpa_nc = rpa,list_com = commune_acZonage, 
                     idZonage = idZonage,effectifs_Zon = effectifs_Zon) %>% 
    bind_rows()
  
  # III.5. Statistiques avec le poids cale
  #---------------------------------------
  RPstats_cale <- rpa_cale %>% 
    group_by(idZonage) %>% 
    summarise(estimRP_log_Zon = round(sum(poids_cales * NLOG),0),
              estimRP_pop_Zon = round(sum(poids_cales * INPER),0),
              estimRP_total_Y_Zon = round(sum(poids_cales * eval(Y)),0),
              var_total_Y_Zon = 3.75 * sum(( eval(Y) - Rchapeau_Y * NLOG )^2)
              ) %>% 
    mutate(
      # coefficients de variation correspondants
      CV_Y_cale = round(100*sqrt(var_total_Y_Zon)/estimRP_total_Y_Zon, 1))
  
  RPstats_cale <- RPstats_cale[!RPstats_cale[,"idZonage"] == "horsZon", ] 
  
  RPstats <- left_join(RPstats,RPstats_cale[,c("idZonage","CV_Y_cale")],by = "idZonage")
  
  return(RPstats)
}

# Calcul de la superficie d'un zonage
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
superficieZon <- function(zonage,idZonage){
  zonage@data <- as.data.frame(cbind(zonage@data, gArea(zonage, byid = T)), stringsAsFactors = FALSE)
  colnames(zonage@data) = c(colnames(zonage@data)[1:length(zonage@data)-1], "superficie")
  zonage@data$superficie <- as.numeric(zonage@data$superficie)
  return(zonage)
}


# Fonction Calage sur marges (Lionel Delta)
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# fonction calage_sur_marges qui prend en entrée un tableau RP agrégé à l'adresse
# et un code commune correspondant au zonage. 
# Pour chaque commune, génération d'autant de variables qu'il y a de QPV. Chacune d'entre elles,
# relative à un QPV,
# prend pour valeur le nombre de logements pour une observation (adresse) si l'adresse en question
# fait partie d'un QPV considéré et 0 sinon. 
# Une variable supplémentaire de calage correspond au nombre de personnes pour chaque observation (adresse)
# afin que le calage sur marges ne modifie pas l'estimation du dénombrement de la population communale.
# En sortie de la fonction, le tableau RP agrégé à l'adresse avec les nouveaux poids calés
calage_sur_marges <- function(zonage, rpa_calage, rpa_nc,list_com,idZonage,effectifs_Zon){
  
  # Problème avec les objets du type tibble
  rpa_calage <- as.data.frame(rpa_calage) 
  rpa_nc <- rpa_nc %>% 
    as.data.frame() 
  effectifs_Zon <- effectifs_Zon %>% 
    as.data.frame()
  
  # Pour chaque commune
  rpax <- rpa_calage[rpa_calage$com == zonage, ] %>% 
    as.data.frame()
  # Recupération de la liste des zonages de la commune
  list_zonage <- unique(rpax$idZonage[rpax$com == zonage])
  list_zonage <- list_zonage[order(list_zonage)]
  
  # Pour chaque zonage de la commune, 
  for (i in 1:length(list_zonage)){
    rpax$newvar <- rpax$NLOG*(rpax[,idZonage] == list_zonage[i])
    colnames(rpax)[length(rpax)] <- paste("NLOG", zonage, list_zonage[i], sep = "_")
  }
  
  rpax$NPER_commune <- rpax$INPER*(rpax$com == zonage)
  
  # Si il n'y a aucun logement du RIL dans la commune (gestion du cas particulier de la commune 97306 qui comporte un logement dans le QPV)
  if(effectifs_Zon$NLOG_ril[effectifs_Zon$com == zonage][1] == 0 | zonage == "97306"){
    rpax$poids_cales <- rpax$IPONDL
    rpax$calage <- 0
    
  }else{
    # Sinon, on effectue le calage
    # On effectue le calage sur marge
    g <- calib(Xs = rpax[,(length(rpa_calage)+1):length(rpax)], 
               d = rpax$IPONDL, 
               total = c(effectifs_Zon[effectifs_Zon$com == zonage, c("NLOG_Zon") ], unique(effectifs_Zon[effectifs_Zon$com ==zonage, c("estimRP_totalCOM_pop") ])), 
               # total = c(effectifs_Zon[effectifs_Zon$com == zonage, c("NLOG_Zon") ], unique(effectifs_Zon[effectifs_Zon$com ==zonage, c("estimRP_totalCOM_pop") ])), 
               method = "logit", 
               bounds = c(0.4, 10))
    
    rpax <- rpa_nc[rpa_nc$com == zonage, ]
    rpax <- rpax[rpax$com %in% list_com, ]
    rpax <- rpax[!(rpax$com == "97101" & rpax$idZonage == "QP971013"),]
    rpax$poids_cales <- g*rpax$IPONDL
    rpax$calage <- 1
  }
  
  return(rpax)
}







