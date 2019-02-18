#-------------------------------------------------------------------------------------#
#                AppliShiny - Fonctions estimation de la qualite du RP infra          #
#-------------------------------------------------------------------------------------#

# Nicolas Kempf

# Derniere MAJ : 16.03.2018

# Les fonctions ci-dessus sont de légères modifications des travaux de Lionel Delta (stage ENSAE)

# Estimation de la qualite des donnees du RP de manière analytique (Normal + densite)
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
precision_analytique_nc <- function(rpa,Y,zonage,idZonage){
  ########## Calcul des statistiques et des variances analytiques associées, par QPV   ##########
  
  ## Les chiffres de populations légales des communes sont obtenus par estimation par le ratio.
  ## En fait  On a d'une part le total exhaustif de logements une année donnée et d'autre part 
  ## la taille moyenne des ménages recensés cette même année. Le produit des deux donne la population estimée.
  ## Cela revient à attribuer aux ménages recencés des poids différents des poids théoriques initiaux
  ## de 2.5 en grandes communes et de 1 en petites communes. 
  ## Ces pondérations effectives, fournies dans le fichier du RP correspondent au rapport du nombre de logements 
  ## connus exhaustivement sur le nombre de logements effectivement recensés parmi ceux qui étaient connus initialement.  
  
  ## En pratique, on retrouve simplement la bonne estimation grace aux pondérations fournies dans le fichier RP.
  ## On utilise également ces memes poids pour toute autre statistique que le nombre d'habitants.
  
  ## Les ratios par année de recensement et commune sont par contre nécessaires
  ## pour le calcul de la variance analytique.
  rpa$comptage <- 1
  ## Calcul des ratios estimés
  ratio <- ddply(.data = rpa,
                 .variables = c("C_ANNEE_COL", "com"), .fun = function(df){
                   c(estim_totalCOM_log = sum(df[,"IPONDL"]*df[,"NLOG"], na.rm = TRUE),
                     estim_totalCOM_Y = sum(df[,"IPONDL"]*df[,Y], na.rm = TRUE),
                     sample_size = sum(df[,"comptage"], na.rm = TRUE))})
  
  ratio$Rchapeau_Y <- ratio$estim_totalCOM_Y/ratio$estim_totalCOM_log
  
  rpa <- merge(rpa, ratio, by=c("C_ANNEE_COL", "com"))
  
  qpv_ech <- ddply(.data = rpa,
                   .variables = c("com", "C_ANNEE_COL", idZonage), .fun = summarise,
                   sample_sizeZon = sum(comptage))
  
  rpa <- merge(rpa, qpv_ech, by=c("com", "C_ANNEE_COL", idZonage), all.x = T)
  rm(ratio, qpv_ech)
  
  ########## Partie relative aux estimations de population dans les QPV et de densités de chômeurs #########
  
  #La variance de l'estimateur du total d'une variable Y sur un domaine dépend, sans équilibrage sur ce domaine,
  #des valeurs prises par cette variable hors du domaine. 
  #Avec un équilibrage au niveau communal sur le nombre de logements, on s'intéresse à la variance des résidus de Y au niveau communal.
  # Equation avec estimateur par le ratio : Y = R.X + residu (X: nombre de logements)
  #Hors du domaine, la variable d'intérêt Y est nulle mais le résidu ne l'est pas car X est non nul
  #Pour chaque QPV on s'intéresse à la variance des résidus qui valent sur le domaine QPV : Y-R.X (par exemple NPER -  Rchapeau_pop*NLOG)
  #et hors du domaine QPV : -R.X
  
  #Dans "estim" calculs de différents termes qui entrent en compte dans le calcul de la variance 
  
  #Astuce utilisée : Calcul au niveau communal de la somme des (-RX - ubarre)² de laquelle est retranchée
  #les termes correspondants aux adresses en QPV, remplacés par les termes (Y-R.X  - ubarre)²
  
  estim <- ddply(.data = rpa,
                 .variables = c("com", "C_ANNEE_COL"), .fun = summarise,
                 RX_sum = sum(NLOG*Rchapeau_Y))
  
  rpa <- merge(rpa, estim, by=c("com", "C_ANNEE_COL"), all.x = T)
  
  estim <- ddply(.data = rpa,
                 .variables = c("com", "C_ANNEE_COL", idZonage), .fun = function(df){
                   c(ubarre_Y = sum((df[,Y]-df[,"RX_sum"]/df[,"sample_sizeZon"])/df[,"sample_size"]))})
  
  rpa <- merge(rpa, estim, by=c("com", "C_ANNEE_COL", idZonage), all.x = T)
  
  estim <- ddply(.data =rpa,
                 .variables = c("com", "C_ANNEE_COL"), .fun = summarise,
                 RX_carre_sum = sum((NLOG*Rchapeau_Y)^2))
  
  rpa <- merge(rpa, estim, by=c("com", "C_ANNEE_COL"), all.x = T)
  rm(estim)
  
  #Resultats bruts des estimations de population et de densités de chomeurs au sein des différents QPV
  # avec les variances associées
  RPstats <- ddply(.data = rpa,
                       .variables = c(idZonage), .fun = function(df){
                         c(estimRP_log_Zon = round(sum(df[,"IPONDL"]*df[,"NLOG"]),0),
                           estimRP_total_Y_Zon = round(sum(df[,"IPONDL"]*df[,Y]),0),
                           var_total_Y_Zon = 3.75*(sum((df[,"RX_carre_sum"]+2*df[,"ubarre_Y"]*df[,"RX_sum"]+df[,"ubarre_Y"]^2*df[,"sample_size"])/df[,"sample_sizeZon"] - ((-1)*df[,"NLOG"]*df[,"Rchapeau_Y"]-df[,"ubarre_Y"])^2 + (df[,Y]-df[,"Rchapeau_Y"]*df[,"NLOG"]-df[,"ubarre_Y"])^2) ))})
  
  # calcul des coefficients de variation correspondants
  RPstats$CV_Y <- round(100*sqrt(RPstats$var_total_Y_Zon)/RPstats$estimRP_total_Y_Zon, 1)
  
  # Ajout de la superficie
  RPstats <- merge(RPstats, zonage@data[,c(idZonage,"superficie")], by = idZonage, all.x = T)
  # Calcul de la densite de Y
  RPstats$densite_Y <- round(RPstats$estimRP_total_Y_Zon*10^6/RPstats$superficie,0)
  # Calcul des coefficients de variation (densite) correspondants
  RPstats$CV_densite_Y <- round(100*sqrt(RPstats$var_total_Y_Zon)/RPstats$estimRP_total_Y_Zon, 1)

  # renommage des variables
  colnames(RPstats) <- c(idZonage, "estimRP_log_Zon", paste0("estimRP_total_", Y, "_Zon"),
                             paste0("var_total_", Y, "_Zon"), paste0("CV_", Y),"superficie_Zon", paste0("densite_", Y), paste0("CV_densite_", Y))
  
  RPstats <- RPstats[!RPstats[,idZonage] == "horsZon", ]
  
  #-------------------------------#
  #       Calage sur marges       #
  #-------------------------------#

  # Statistiques à la commune
  com_RPstats <- ddply(.data = rpa,
                       .variables = c("com"), .fun = function(df){
                         c(sample_size = sum(df[,"comptage"], na.rm = TRUE),
                           estimRP_totalCOM_log = round(sum(df[,"IPONDL"]*df[,"NLOG"], na.rm = TRUE),0),
                           estimRP_totalCOM_pop = round(sum(df[,"IPONDL"]*df[,"INPER"], na.rm = TRUE),0),
                           estimRP_totalCOM_Y = round(sum(df[,"IPONDL"]*df[,Y], na.rm = TRUE), 0),
                           var_total_Y = 3.75*(sum( (df[,Y]-df[,"Rchapeau_Y"]*df[,"NLOG"])^2, na.rm = TRUE ))
                         )})
  
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
  
  #calcul du nombre de logements cartographiés par commune
  total_ril <- ddply(.data = sondage,
                     .variables = c("com"), .fun = summarise,
                     NLOG_ril_commune = sum(NLOG_ril) )
  # Ajout de variables
  effectifs_Zon <- merge(sondage, total_ril, by= "com", all.x = T)
  
  #calcul des poids relatifs des QPV selon le nombre de logements cartographiés
  effectifs_Zon$poidsZon <- effectifs_Zon$NLOG_ril/effectifs_Zon$NLOG_ril_commune
  rm(total_ril)
  
  # Ajout de variables
  effectifs_Zon <- merge(effectifs_Zon, com_RPstats[,c("com", "sample_size", "estimRP_totalCOM_log", "estimRP_totalCOM_pop")], by = c("com"))
  
  # calculs des marges de calage par QPV, à partir du total communal de logements et des poids structurels des QPV
  effectifs_Zon$NLOG_Zon <- round(effectifs_Zon$poidsZon * effectifs_Zon$estimRP_totalCOM_log,0)
  
  
  # Selection des communes comportant au moins une zone et selection des variables d'interet
  communes_avec_zon <- unique(rpa$com[!is.na(rpa[,idZonage])])
  rpa_calage_zon <- rpa[rpa$com %in% communes_avec_zon ,c("com", idZonage, "C_ANNEE_COL", "IPONDL", "idx", "NLOG", "INPER", 
                               "sample_sizeZon")]
  # Ajout de variables
  rpa_calage_zon <- merge(rpa_calage_zon, effectifs_Zon[, c("com", idZonage, "poidsZon")], by=c("com", idZonage), all.x = T)
  # Ajout de variables
  rpa_calage_zon <- merge(rpa_calage_zon, com_RPstats[,c("com", "estimRP_totalCOM_log"),], by="com", all.x = T)
  
  #Retrait de la seule adresse recensée aux Abymes pour le QP971013
  # rpa_97101_QP971013 <- rpa_qpv[(rpa_qpv$com == "97101" & rpa_qpv$idZonageV == "QP971013"),]
  # rpa_calage_zon <- rpa_calage_zon[!(rpa_calage_zon$com == "97101" & rpa_calage_zon$idZonageV == "QP971013"),]
  
  # Calage sur marge par commune
  #-----------------------------
  rpa_cale <- lapply(communes_avec_zon, calage_sur_marges, rpa_calage = rpa_calage_zon, rpa_nc = rpa,list_com = communes_avec_zon, idZonage = idZonage)
  rpa_cale <- rbindlist(rpa_cale)
  
  # Calcul des statistiques avec le poids cale
  RPstats_cale <- ddply(.data = rpa_cale,
                   .variables = c(idZonage), .fun = function(df){
                     c(estimRP_log_Zon = round(sum(df[,"poids_cales"]*df[,"NLOG"]),0),
                       estimRP_total_Y_Zon = round(sum(df[,"poids_cales"]*df[,Y]),0),
                       var_total_Y_Zon = 3.75*(sum((df[,"RX_carre_sum"]+2*df[,"ubarre_Y"]*df[,"RX_sum"]+df[,"ubarre_Y"]^2*df[,"sample_size"])/df[,"sample_sizeZon"] - ((-1)*df[,"NLOG"]*df[,"Rchapeau_Y"]-df[,"ubarre_Y"])^2 + (df[,Y]-df[,"Rchapeau_Y"]*df[,"NLOG"]-df[,"ubarre_Y"])^2) ))})
  
  # calcul des coefficients de variation correspondants
  RPstats_cale$CV_Y <- round(100*sqrt(RPstats_cale$var_total_Y_Zon)/RPstats_cale$estimRP_total_Y_Zon, 1)
  
  # colnames(rpa_cale)

  return(list(RPstats = RPstats,RPstats_cale = RPstats_cale))
}

# Calcul de la superficie d'un zonage
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
superficieZon <- function(zonage,idZonage){
  zonage@data <- as.data.frame(cbind(zonage@data, gArea(zonage, byid = T)), stringsAsFactors = FALSE)
  colnames(zonage@data) = c(colnames(zonage@data)[1:length(zonage@data)-1], "superficie")
  zonage@data$superficie <- as.numeric(zonage@data$superficie)
  return(zonage)
}

# Base sondage (special calage)
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
sondageZon <- function(eff_rilRp,rpa,idZonage){
  # Ajout de la variable idZonage
  eff_rilRp <- merge(eff_rilRp,rpa[,c("idx","com",idZonage)],by="idx",all.x=TRUE)
  
  # Calcul des effectifs d'adresses ril, rp et rp-ril par commune, par qpv
  sondage <- ddply(.data = eff_rilRp, .variables = c("com", idZonage),
                       .fun = summarise, NLOG_ril = sum(NLOG),
                       NLOG_recensement = sum(NLOG_recensement),
                       NBadresses_ril = sum(carto_ril),
                       NBadresses_40prcent = round(sum(carto_ril*.4),0),
                       NBadresses_recensement = sum(recensement),
                       NBadresses_ril_et_recensement = sum(rETc))
  return(sondage)
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
calage_sur_marges <- function(zonage, rpa_calage, rpa_nc,list_com,idZonage){
  # Pour chaque commune
  rpax <- rpa_calage[rpa_calage$com == zonage, ]
  # Recupération de la liste des zonages de la commune
  list_zonage <- unique(rpax[rpax$com == zonage,idZonage])

  # Pour chaque zonage de la commune, 
  for (i in 1:length(list_zonage)){
    rpax$newvar <- rpax$NLOG*(rpax[,idZonage] == list_zonage[i])
    colnames(rpax)[length(rpax)] <- paste("NLOG", zonage, list_zonage[i], sep = "_")
  }

  rpax$NPER_commune <- rpax$INPER*(rpax$com == zonage)
  # On effectue le calage sur marge
  g <- calib(Xs = rpax[,(length(rpa_calage)+1):length(rpax)], 
             d = rpax$IPONDL, 
             total = c(effectifs_Zon[effectifs_Zon$com ==zonage, c("NLOG_Zon") ], unique(effectifs_Zon[effectifs_Zon$com ==zonage, c("estimRP_totalCOM_pop") ])), 
             method = "logit", 
             bounds = c(0.4, 10))
  
  rpax <- rpa_nc[rpa_nc$com == zonage, ]
  rpax <- rpax[rpax$com %in% list_com, ]
  #rpa <- rpa[!(rpa$com == "97101" & rpa$idZonageV == "QP971013"),]
  rpax$poids_cales <- g*rpax$IPONDL
  return(rpax)
}










# Estimation de la qualité des données du RP infra avec Calage
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------


precision_analytique_cale <- function(Y){

  
  #restriction aux communes avec QPV
  # effectifs_Zon <- effectifs_Zon[effectifs_Zon$com %in% communes_avec_qpv, ]
  
  #Retrait de la seule adresse recensée aux Abymes pour le QP971013
  # effectifs_Zon <- effectifs_Zon[!(effectifs_Zon$com == "97101" & effectifs_Zon$idZonageV == "QP971013"),] 
  
 
  
  
  
  
  liste_qpv <- effectifs_Zon[, c("com", "idZonageV")]
  
  
  
  
 

  ######## Nouvelles estimations de population et de densités de chômeurs avec les variables calées ########
  
  v <- rep(0, 2*length(Y)) 
  qpv_RPstats_cales <- ddply(.data = rpa_qpv_cale,
                             .variables = c("idZonageV"), .fun = function(df){
                               c(estim_log_RPcalage = round(sum(df[,"poids_cales"]*df[,"NLOG"]),0),
                                 estim_pop_RPcalage = round(sum(df[,"poids_cales"]*df[,"INPER"]),0),
                                 for(i in 1:length(Y)){
                                   v[2*i-1] = round(sum(df[,"poids_cales"]*df[,Y[i]]),0)
                                   v[2*i] = 3.75*(sum( (df[,Y[i]]-df[,paste0("Rchapeau_", Y[i])]*df[,"NLOG"])^2 ))
                                 },v)})
  
  for(i in 1:length(Y)){
    qpv_RPstats_cales[,3+2*length(Y)+i] <- round(100*sqrt(qpv_RPstats_cales[,3+2*i])/qpv_RPstats_cales[,3+2*i-1], 1)
    colnames(qpv_RPstats_cales)[3+2*i-1] <- paste0("estimQPVcale_", Y[i])
    colnames(qpv_RPstats_cales)[3+2*i] <- paste0("var_", Y[i], "_QPVcale")
    colnames(qpv_RPstats_cales)[3+2*length(Y)+i] <- paste0("CV_", Y[i], "_QPVcale")
  }
  
  qpv_RPstats_cales <- qpv_RPstats_cales[!qpv_RPstats_cales$idZonageV == "horsQPV", ]
  return(qpv_RPstats_cales)
}  






