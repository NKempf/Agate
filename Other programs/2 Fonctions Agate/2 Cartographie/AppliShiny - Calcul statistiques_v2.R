#--------------------------------------------------------------------------------------------------------------------------------#
#                 AppliShiny - Fonction calcul de statistiques selon un zonage                                                   #
#--------------------------------------------------------------------------------------------------------------------------------#

# MAJ : 13.03.2018


#######################################################
######### Fonction rpAutZon - spécifications ##########
# Objectif : Prepare la table pour calculer les indicateurs des autres zonages du departement à l'exception du zonage considere.
# rp : data.frame du rp (niveau individu ou logement)
# zonage : spatialPolygonDataFrame du zonage. Doit obligatoirement contenir la variable idZonage qui est l'identifiant des polygons. 
rpAutZon <- function(rp,zonage){
  library(plyr)
  
  autZon <- lapply(X = unique(zonage@data$idZonage),FUN = function(x){
    # 1. reperage du département
    depZon <- rp$dep[rp$idZonage %in% x][1]
    # 2. selection de la base
    rpZ <- rp[rp$dep%in% depZon & !is.na(rp$idZonage) & !(rp$idZonage %in% x),]
    # 3. Reperage du zonage
    rpZ$idZonageAutZon <- x
    return(rpZ)
  })
  # Fusion des tables
  rpZ <- rbind.fill(autZon)
  return(rpZ)
}

#########################################################
######### Fonction zonaStatFilo - spécifications ##########
# Objectif : Calculer l'ensemble des statistiques issues de Filosif pour un zonage 
#
# Renvoie une liste de data.frame
#
# filo
#   base niveau menage fiscal 
#   Obligatoirement un objet de type Data.frame
#
# ID
#   Variable identifiant du zonage (ex : qpv => "CODE_QPV")
#
#
################################################  
id <- "idZonage"

zonaStatFilo <- function(filo,id){
  # Librairies indispensables
  library(plyr)
  
  # Variable temporaire de calcul de population
  filo$pop <- 1 
  
  # I. Statistiques du niveau de vie
  #-----------------------------------------------------------------------------------------------------------------------------------------------
  statFilo <- ddply(.data = filo,.variables = c(id),.fun = summarise,
                    eff.filo=sum(pop),
                    nivvie.mean=round(mean(nivviem),0),
                    nivvie.med=round(quantile(nivviem,probs = 0.5),0),
                    nivvie.min = min(nivviem),
                    nivvie.p10=round(quantile(nivviem,probs = 0.1),0),
                    nivvie.p20=round(quantile(nivviem,probs = 0.2),0),
                    nivvie.p25=round(quantile(nivviem,probs = 0.25),0),
                    nivvie.p75=round(quantile(nivviem,probs = 0.75),0),
                    nivvie.p80=round(quantile(nivviem,probs = 0.80),0),
                    nivvie.p90=round(quantile(nivviem,probs = 0.90),0),
                    nivvie.p95=round(quantile(nivviem,probs = 0.95),0),
                    nivvie.max = max(nivviem),
                    
                    revdec.mean=round(mean(revdecm),0),
                    revdec.med=round(quantile(revdecm,probs = 0.5),0),
                    revdec.min = min(revdecm),
                    revdec.p10=round(quantile(revdecm,probs = 0.1),0),
                    revdec.p20=round(quantile(revdecm,probs = 0.2),0),
                    revdec.p25=round(quantile(revdecm,probs = 0.25),0),
                    revdec.p75=round(quantile(revdecm,probs = 0.75),0),
                    revdec.p80=round(quantile(revdecm,probs = 0.80),0),
                    revdec.p90=round(quantile(revdecm,probs = 0.90),0),
                    revdec.p95=round(quantile(revdecm,probs = 0.95),0),
                    revdec.max = max(revdecm),
                    
                    revdisp.mean=round(mean(revdispm),0),
                    revdisp.med=round(quantile(revdispm,probs = 0.5),0),
                    revdisp.min = min(revdispm),
                    revdisp.p10=round(quantile(revdispm,probs = 0.1),0),
                    revdisp.p20=round(quantile(revdispm,probs = 0.2),0),
                    revdisp.p25=round(quantile(revdispm,probs = 0.25),0),
                    revdisp.p75=round(quantile(revdispm,probs = 0.75),0),
                    revdisp.p80=round(quantile(revdispm,probs = 0.80),0),
                    revdisp.p90=round(quantile(revdispm,probs = 0.90),0),
                    revdisp.p95=round(quantile(revdispm,probs = 0.95),0),
                    revdisp.max = max(revdispm)
  )
  
  # II. Statistiques du niveau de vie par type de ménage
  #----------------------------------------------------------------------------------------------------------------------------------------------
  statFilo.typmenR <- ddply(.data = filo,.variables = c(id,"typmenR"),.fun = summarise,
                            eff.filo=sum(pop),
                            nivvie.mean=round(mean(nivviem),0),
                            nivvie.med=round(quantile(nivviem,probs = 0.5),0),
                            nivvie.min = min(nivviem),
                            nivvie.p10=round(quantile(nivviem,probs = 0.1),0),
                            nivvie.p20=round(quantile(nivviem,probs = 0.2),0),
                            nivvie.p25=round(quantile(nivviem,probs = 0.25),0),
                            nivvie.p75=round(quantile(nivviem,probs = 0.75),0),
                            nivvie.p80=round(quantile(nivviem,probs = 0.80),0),
                            nivvie.p90=round(quantile(nivviem,probs = 0.90),0),
                            nivvie.p95=round(quantile(nivviem,probs = 0.95),0),
                            nivvie.max = max(nivviem),
                            
                            revdec.mean=round(mean(revdecm),0),
                            revdec.med=round(quantile(revdecm,probs = 0.5),0),
                            revdec.min = min(revdecm),
                            revdec.p10=round(quantile(revdecm,probs = 0.1),0),
                            revdec.p20=round(quantile(revdecm,probs = 0.2),0),
                            revdec.p25=round(quantile(revdecm,probs = 0.25),0),
                            revdec.p75=round(quantile(revdecm,probs = 0.75),0),
                            revdec.p80=round(quantile(revdecm,probs = 0.80),0),
                            revdec.p90=round(quantile(revdecm,probs = 0.90),0),
                            revdec.p95=round(quantile(revdecm,probs = 0.95),0),
                            revdec.max = max(revdecm),
                            
                            revdisp.mean=round(mean(revdispm),0),
                            revdisp.med=round(quantile(revdispm,probs = 0.5),0),
                            revdisp.min = min(revdispm),
                            revdisp.p10=round(quantile(revdispm,probs = 0.1),0),
                            revdisp.p20=round(quantile(revdispm,probs = 0.2),0),
                            revdisp.p25=round(quantile(revdispm,probs = 0.25),0),
                            revdisp.p75=round(quantile(revdispm,probs = 0.75),0),
                            revdisp.p80=round(quantile(revdispm,probs = 0.80),0),
                            revdisp.p90=round(quantile(revdispm,probs = 0.90),0),
                            revdisp.p95=round(quantile(revdispm,probs = 0.95),0),
                            revdisp.max = max(revdispm)
  )

  # III. Statistiques par type de ménage
  #----------------------------------------------------------------------------------------------------------------------------------------------
  # Label du type de ménage
  typmen.label <- c("famille monoparentale","couple sans enfant","couple avec enfant(s)","menage complexe",
                    "personne seule")
  # Recodage de la variable type de ménages
  filo$typmen <- filo$typmenR
  filo$typmen[filo$typmen == "6"] <- "5"
  filo$typmen <- factor(x = filo$typmen,labels = typmen.label)
  
  # Effectif par zonage et type de menage
  t1a <- as.data.frame(xtabs(data = filo,formula = ~idZonage + typmen))
  colnames(t1a)[colnames(t1a) == "Freq"] <- "eff"
  # PCT par zonage et type de menage
  t1b <-as.data.frame(lprop(xtabs(data = filo,formula = ~idZonage + typmen),2,digits = 2,total = F))
  colnames(t1b)[colnames(t1b) == "Freq"] <- "zonage"
  # Fusion
  t1 <- merge(t1a,t1b,by=c("idZonage","typmen"),all.x=TRUE)
  
  # PCT par commune
  t2 <-as.data.frame(lprop(xtabs(data = filo,formula = ~depcom + typmen),2,digits = 2,total = F))
  colnames(t2)[colnames(t2) == "Freq"] <- "commune"
  
  # PCT des zonages par département
  filo$dep <- substr(filo$depcom,1,3)
  t3 <-as.data.frame(lprop(xtabs(data = filo[!is.na(filo$idZonage),],formula = ~ dep + typmen),2,digits = 2,total = F))
  colnames(t3)[colnames(t3) == "Freq"] <- "zonage.moy"
  
  # PCT des hors zonages par département
  t4 <-as.data.frame(lprop(xtabs(data = filo[is.na(filo$idZonage),],formula = ~ dep + typmen),2,digits = 2,total = F))
  colnames(t4)[colnames(t4) == "Freq"] <- "Hzonage.moy"
  
  # Creation des tables à conserver
  # Niveau de vie
  filoStat.nivvie <- statFilo[,c("idZonage","eff.filo","nivvie.mean","nivvie.med","nivvie.min","nivvie.p10","nivvie.p20","nivvie.p25",
                                 "nivvie.p75","nivvie.p80","nivvie.p90","nivvie.p95","nivvie.max")]
  
  # Revenu declare
  filoStat.revdec <- statFilo[,c("idZonage","eff.filo","revdec.mean","revdec.med","revdec.min","revdec.p10","revdec.p20","revdec.p25",
                                 "revdec.p75","revdec.p80","revdec.p90","revdec.p95","revdec.max")]

  # Revenu disponible
  filoStat.revdisp <- statFilo[,c("idZonage","eff.filo","revdisp.mean","revdisp.med","revdisp.min","revdisp.p10","revdisp.p20","revdisp.p25",
                                  "revdisp.p75","revdisp.p80","revdisp.p90","revdisp.p95","revdisp.max")]
  
  # Liste récapitulative
  liste <- list(filoStat.nivvie = filoStat.nivvie,filoStat.revdec=filoStat.revdec,filoStat.revdisp=filoStat.revdisp,
                statFilo.typmenR=statFilo.typmenR,typmen.zon=t1,typmen.com=t2,typmen.aZon=t3,typmen.hZon=t4) 
  
  return(liste)
  
}



#########################################################
######### Fonction zonaStatRP - spécifications ##########
# Objectif : Calculer l'ensemble des statistiques du RP pour un zonage 
#
# Renvoie un objet du type Data.frame
#
# RPI
#   Base cumulée du recensement de la population au niveau individu (exploitation principale) 
#   Obligatoirement un objet de type Data.frame
#   
# RPL
#   Base cumulée du recensement de la population au niveau logement (exploitation principale) 
#   Obligatoirement un objet de type Data.frame 
#
# ID
#   Variable identifiant du zonage (ex : qpv => "CODE_QPV")
#
#
#
#
# 
################################################  
zonaStatRp <- function(rpi,rpl=NA,id){
  # Package necessaire
  library(plyr) 
  
  # Identifiant temporaire
  rpi$idnk <- rpi[,id]
  rpl$idnk <- rpl[,id]
  rpi$rpi <- 1
  rpi$zonage <- ifelse(!is.na(rpi[,id]),1,0)
  
  #--------------------------------------------------------------------------------------#  
  #                    Niveau individu : Calcul de Stat                                  #
  #--------------------------------------------------------------------------------------#  
  
  #-----------------#
  # 0. Population   #
  #-----------------#
  
  # Correction à la marge pour les logements des qpv tombant au gosier(971) et à mana(973)
  rpi$com <- ifelse(rpi$com == "97113","97101",rpi$com)
  rpi$com <- ifelse(rpi$com == "97306","97311",rpi$com)
  
  # 0.1. Population communale et zonage
  #----------------------------------------------------------------------------------------------------------------------------------------------
  t0a <- merge(ddply(.data = rpi,
                     .variables = c("dep","com"),
                     .fun = summarise,
                     pop_p = round(sum(IPONDI),0),
                     pop_np = sum(rpi)) ,
               ddply(.data = rpi[!is.na(rpi$idnk),],
                     .variables = c("dep","com"),
                     .fun = summarise,
                     pop_qpv_p = round(sum(IPONDI),0),
                     pop_qpv_np = sum(rpi)), by=c("dep","com"),
               all.x=T)
  
  # 0.2.Population par qpv
  #----------------------------------------------------------------------------------------------------------------------------------------------
  t0b <- ddply(.data = rpi[!is.na(rpi$idnk),],
               .variables = c("idnk"),
               .fun = summarise,
               pop_qpv_p = round(sum(IPONDI),0),
               pop_qpv_np = sum(rpi)) 
  
  t0b <- rbind(t0b,
               data.frame(idnk="sum",pop_qpv_p=sum(t0b$pop_qpv_p),pop_qpv_np=sum(t0b$pop_qpv_np))
  )
  colnames(t0b)[1] <- "id"
  
  #-------------------------------#
  # I. Description population     #
  #-------------------------------#
  
  # I.1. Part des moins de 20 ans et des 20 à 64 ans
  #-----------------------------------------------------------------------------------------------------------------------------------------------
  # Recodage de la variable age
  rpi$ageNK <-  cut(as.numeric(rpi$AGEREV),
                    breaks = c(0,20,65,120),
                    include.lowest = TRUE,
                    right = FALSE)
  # Calcul des stats
  t1 <- freqNk(rpi$idnk,rpi$ageNK,poids = rpi$IPONDI,stat = c("rowpct"))
  t1 <- plyr::rename(t1,c('[20,65)(p)(rowpct)'='p_20_65'))
  t1 <- plyr::rename(t1,c('[0,20)(p)(rowpct)'='p_moins20'))
  t1 <- plyr::rename(t1,c('[65,120](p)(rowpct)'='p_65plus'))
  
  # I.2. Part des 60 ans et plus
  #-----------------------------------------------------------------------------------------------------------------------------------------------
  # Recodage de la variable age
  rpi$ageNK <-  cut(as.numeric(rpi$AGEREV),
                    breaks = c(0,20,60,120),
                    include.lowest = TRUE,
                    right = FALSE)
  # Calcul des stats
  t1b <- freqNk(rpi$idnk,rpi$ageNK,poids = rpi$IPONDI,stat = c("rowpct"))
  t1b <- keepNK(t1b,val = "[60,120]")
  t1b <- plyr::rename(t1b,c('[60,120](p)(rowpct)'='p_60plus'))
  
  # I.3. Part des femmes
  #-----------------------------------------------------------------------------------------------------------------------------------------------
  # Calcul des stats
  t1c <- freqNk(rpi$idnk,rpi$SEXE,poids = rpi$IPONDI,stat = c("rowpct"))
  t1c <- plyr::rename(t1c,c('femme(p)(rowpct)'='p_femmes'))
  t1c <- plyr::rename(t1c,c('homme(p)(rowpct)'='p_hommes'))
  
  # I.4. Pyramide des ages par sexe et par zonage
  #-----------------------------------------------------------------------------------------------------------------------------------------------
  t1d_pyramide <- as.data.frame(xtabs(data = rpi,formula = IPONDI ~  idnk + SEXE + AGEREV))
  t1d_pyramide <- plyr::rename(t1d_pyramide,c('Freq'='pop','idnk'=id,'AGEREV'="age")) 
  t1d_pyramide$pop <- round(t1d_pyramide$pop,0)
  
  # I.5. Pyramide par sexe, zonage et tranche d'age
  #-----------------------------------------------------------------------------------------------------------------------------------------------
  rpi$ageNK <-  cut(as.numeric(rpi$AGEREV),
                    breaks = c(seq(0,90,2),120),
                    include.lowest = TRUE,
                    right = FALSE)
  t1e_pyramide <- as.data.frame(xtabs(data = rpi,formula = IPONDI ~  idnk + SEXE + ageNK))
  t1e_pyramide <- plyr::rename(t1e_pyramide,c('Freq'='pop','idnk'=id,'ageNK'="age")) 
  t1e_pyramide$pop <- round(t1e_pyramide$pop,0)
  
  #-----------------------#
  # II. Scolarisation     #
  #-----------------------# 
  
  # Recodage de la variable age
  rpi$ageNK <-  cut(as.numeric(rpi$AGEREV),
                    breaks = c(0,2,6,18,25,120),
                    include.lowest = TRUE,
                    right = FALSE)
  
  # II.1. Taux de scolarisation des 2 à 5 ans
  #-------------------------------------------------------------------------------------------------------------------------------------------------
  tmp <- rpi[rpi$ageNK =="[2,6)",]
  t2a <- freqNk(tmp$idnk,tmp$ETUD,poids = tmp$IPONDI,stat = c("rowpct"))
  t2a <- keepNK(t2a,val="etudi")
  t2a <- plyr::rename(t2a,c('etudi(p)(rowpct)'='p_scola_2_5'))
  
  # II.2. Taux de scolarisation des 18 à 24 ans
  #-------------------------------------------------------------------------------------------------------------------------------------------------
  tmp <- rpi[rpi$ageNK =="[18,25)",]
  # Calcul de stat descriptives
  t2b <- freqNk(tmp$idnk,tmp$ETUD,poids = tmp$IPONDI,stat = c("rowpct"))
  # Suppression des variables inutiles
  t2b <- keepNK(t2b,val="etudi")
  # Renommage statistique d'interêt
  t2b <- plyr::rename(t2b,c('etudi(p)(rowpct)'='p_scola_18_24'))
  
  # II.3. Part de non scolarisation de plus de 15 ans sans diplome
  #--------------------------------------------------------------------------------------------------------------------------------------------------
  # Recodage de la variable age
  rpi$ageNK <-  cut(as.numeric(rpi$AGEREV),
                    breaks = c(0,15,120),
                    include.lowest = TRUE,
                    right = FALSE)
  
  # Calcul du taux 
  tmp <- rpi[rpi$ageNK =="[15,120]"& rpi$DIPL == "A",]
  tmp$nonScola <- ifelse(tmp$ageNK =="[15,120]"& tmp$DIPL == "A"& tmp$ETUD=="n_etudi","nScola_15plus","autre")
  t2c <- freqNk(tmp$idnk,tmp$nonScola,poids = tmp$IPONDI,stat = c("rowpct"))
  # Suppression des variables inutiles
  t2c <- keepNK(t2c,val="nScola_15plus")
  t2c <- plyr::rename(t2c,c('nScola_15plus(p)(rowpct)'='p_Nscola_15plus'))
  
  # II.4. Taux de non scolarisation des 6 - 14 ans
  #---------------------------------------------------------------------------------------------------------------------------------------------------
  # Recodage de la variable age
  rpi$ageNK <-  cut(as.numeric(rpi$AGEREV),
                    breaks = c(0,6,15,120),
                    include.lowest = TRUE,
                    right = FALSE)
  # Calcul du taux 
  tmp <- rpi[rpi$ageNK =="[6,15)",]
  t2d <- freqNk(tmp$idnk,tmp$ETUD,poids = tmp$IPONDI,stat = c("rowpct"))
  # Suppression des variables inutiles
  t2d <- keepNK(t2d,val="n_etudi")
  t2d <- plyr::rename(t2d,c('n_etudi(p)(rowpct)'='p_Nscola_6_14'))
  
  # II.5. Taux de non scolarisation des 16 à 25 ans
  #---------------------------------------------------------------------------------------------------------------------------------------------------
  # Recodage de la variable age
  rpi$ageNK <-  cut(as.numeric(rpi$AGEREV),
                    breaks = c(0,16,26,120),
                    include.lowest = TRUE,
                    right = FALSE) 
  # Calcul du taux 
  tmp <- rpi[rpi$ageNK =="[16,26)",]
  tmp$decrocheur <- ifelse(tmp$ageNK == "[16,26)" & tmp$DIPL == "A" & tmp$ETUD == "n_etudi",
                           "decrocheur","n_decrocheur")
  t2e <- freqNk(tmp$idnk,tmp$decrocheur,poids = tmp$IPONDI,stat = c("rowpct"))
  # Suppression des variables inutiles
  t2e <- keepNK(t2e,val="decrocheur")
  t2e <- plyr::rename(t2e,c('decrocheur(p)(rowpct)'='t_decrocheur'))
  
  
  #-----------------------#
  #   III. Emploi         #
  #-----------------------# 
  
  # III.1. Taux de chomage pour les 15 - 64 ans
  #---------------------------------------------------------------------------------------------------------------------------------------------------
  # Recodage de la variable age
  rpi$ageNK <-  cut(as.numeric(rpi$AGEREV),
                    breaks = c(0,15,65,120),
                    include.lowest = TRUE,
                    right = FALSE)
  # Selection des actifs de 15 à 64 ans
  tmp <- rpi[rpi$TACT %in% c("actifocc","chomeur") & rpi$ageNK == "[15,65)",]
  # Taux de chomage des 15 - 64 ans
  t3a <- freqNk(tmp$idnk,tmp$TACT,poids = tmp$IPONDI,stat = c("rowpct"))
  t3a <- plyr::rename(t3a,c('chomeur(p)(rowpct)'='t_chom_15_64'))
  t3a <- plyr::rename(t3a,c('actifocc(p)(rowpct)'='t_actifOcc_15_64'))
  
  # III.2. Taux d'actif  - d'inactif pour les 15 - 64 ans
  #---------------------------------------------------------------------------------------------------------------------------------------------------
  # Selection des actifs
  tmp <- rpi[rpi$ageNK == "[15,65)",]
  # Taux d'actif et d'inactif parmi les 15 - 64 ans
  t3b <- freqNk(tmp$idnk,tmp$inactif,poids = tmp$IPONDI,stat = c("rowpct"))
  t3b <- plyr::rename(t3b,c('actif(p)(rowpct)'='t_act_15_64'))
  t3b <- plyr::rename(t3b,c('inactif(p)(rowpct)'='t_inact_15_64'))
  
  # III.3. Taux d'actif selon le sexe pour les 15 - 64 ans
  #--------------------------------------------------------------------------------------------------------------------------------------------------
  # Femmes #
  tmp <- rpi[rpi$ageNK == "[15,65)" & rpi$SEXE == "femme",]
  # Taux d'actif et d'inactif parmi les 15 - 64 ans
  t3c <- freqNk(tmp$idnk,tmp$inactif,poids = tmp$IPONDI,stat = c("rowpct"))
  t3c <- plyr::rename(t3c,c('actif(p)(rowpct)'='t_actF_15_64'))
  t3c <- plyr::rename(t3c,c('inactif(p)(rowpct)'='t_inactF_15_64'))
  
  # Hommes #
  tmp <- rpi[rpi$ageNK == "[15,65)" & rpi$SEXE == "homme",]
  # Taux d'actif et d'inactif parmi les 15 - 64 ans
  t3d <- freqNk(tmp$idnk,tmp$inactif,poids = tmp$IPONDI,stat = c("rowpct"))
  t3d <- plyr::rename(t3d,c('actif(p)(rowpct)'='t_actH_15_64'))
  t3d <- plyr::rename(t3d,c('inactif(p)(rowpct)'='t_inactH_15_64'))
  
  # III.4. Part des actifs de 15-64 ans cadres et professions intermédiaire
  #--------------------------------------------------------------------------------------------------------------------------------------------------
  # Selection des actifs de 15 à 64 ans
  tmp <- rpi[rpi$TACT %in% c("actifocc","chomeur") & rpi$ageNK == "[15,65)",]
  tmp$profInter <-ifelse(tmp$POSP %in% c("1J","1I","1H","1G","1F"),"cadre_prof_inter","autre")
  # Taux d'actif et d'inactif parmi les 15 - 64 ans
  t3e <- freqNk(tmp$idnk,tmp$profInter,poids = tmp$IPONDI,stat = c("rowpct"))
  t3e <- plyr::rename(t3e,c('cadre_prof_inter(p)(rowpct)'='p_cadre_prof_inter'))
  
  #-----------------------#
  #   IV. Immigration     #
  #-----------------------# 
  
  # IV.1. Part des etrangers
  #---------------------------------------------------------------------------------------------------------------------------------------------------
  t4 <- freqNk(rpi$idnk,rpi$etranger,poids = rpi$IPONDI,stat = c("rowpct"))
  t4 <- plyr::rename(t4,c('etranger(p)(rowpct)'='p_etranger'))
  
  # IV.2. Part des immigres
  #---------------------------------------------------------------------------------------------------------------------------------------------------
  t5 <- freqNk(rpi$idnk,rpi$immigre,poids = rpi$IPONDI,stat = c("rowpct"))
  t5 <- plyr::rename(t5,c('immigre(p)(rowpct)'='p_immigre'))
  
  #--------------------------------------------------------------------------------------#  
  #                    Niveau logement : Calcul de Stat                                  #
  #--------------------------------------------------------------------------------------# 
  
  #-------------------#
  #   0. Logements    #
  #-------------------#
  
  # 0.1. Part de logements selon la catégorie de logement
  #----------------------------------------------------------------------------------------------------------------------------------------------------
  tl0 <- freqNk(var1 = rpl$idnk,var2 = rpl$catLog,poids = rpl$IPONDL,stat = c("rowpct"))
  
  tl0 <- plyr::rename(tl0,c('CATL_1(p)(rowpct)'='p_resid_princ'))
  tl0 <- plyr::rename(tl0,c('CATL_2(p)(rowpct)'='p_log_occas'))
  tl0 <- plyr::rename(tl0,c('CATL_3(p)(rowpct)'='p_resid_second'))
  tl0 <- plyr::rename(tl0,c('CATL_4(p)(rowpct)'='p_log_vacants'))
  
  #--------------------------------#
  #   I. Résidences principales    #
  #--------------------------------#
  
  # I.1. Part des résidences principales de plus de 100m²
  #----------------------------------------------------------------------------------------------------------------------------------------------------
  tmp <- rpl[rpl$CATL=="1",]
  tl1 <- freqNk(tmp$idnk,tmp$surf100,poids = tmp$IPONDL,stat = c("rowpct"))
  tl1 <- plyr::rename(tl1,c('surf100etplus(p)(rowpct)'='p_surf100p'))
  
  # I.2. Part des résidences principales en HLM
  #----------------------------------------------------------------------------------------------------------------------------------------------------
  tl1b <- freqNk(tmp$idnk,tmp$hlm,poids = tmp$IPONDL,stat = c("rowpct"))
  tl1b <- plyr::rename(tl1b,c('hlm(p)(rowpct)'='p_residHlm')) 
  
  # I.3. Part des résidences principales sans eau chaude
  #----------------------------------------------------------------------------------------------------------------------------------------------------
  tmp <- rpl[rpl$CATL=="1" & rpl$EAU %in% c("1","2","3"),]
  tmp$eauchaude <- ifelse(tmp$EAU =="2","Eau_chaude","N_eau_chaude")
  tl1c <- freqNk(tmp$idnk,tmp$eauchaude,poids = tmp$IPONDL,stat = c("rowpct"))
  tl1c <- plyr::rename(tl1c,c('N_eau_chaude(p)(rowpct)'='p_N_eau_chaude')) 
  
  # I.4. Part des résidences principales sans bain ni douche 
  #----------------------------------------------------------------------------------------------------------------------------------------------------
  tmp <- rpl[rpl$CATL=="1" & rpl$BAIN %in% c("1","2"),]
  tmp$douche <- ifelse(tmp$BAIN == "1","bain_douche","N_bain_douche")
  tl1d <- freqNk(tmp$idnk,tmp$douche,poids = tmp$IPONDL,stat = c("rowpct"))
  tl1d <- plyr::rename(tl1d,c('N_bain_douche(p)(rowpct)'='p_N_Bain_douche')) 
  
  # I.5. Part des résidences principales sans tout a l'egout
  #----------------------------------------------------------------------------------------------------------------------------------------------------
  tmp <- rpl[rpl$CATL=="1" & rpl$EGOUL %in% c("1","2","3","4"),]
  tmp$egout <- ifelse(tmp$EGOUL == "1","tout_egout","N_tout_egout")
  tl1e <- freqNk(tmp$idnk,tmp$egout,poids = tmp$IPONDL,stat = c("rowpct"))
  tl1e <- plyr::rename(tl1e,c('N_tout_egout(p)(rowpct)'='p_N_tout_egout')) 
  
  #--------------------------------#
  #   II. Logement                 #
  #--------------------------------#
  
  # II.1. Part des locataires
  #---------------------------------------------------------------------------------------------------------------------------------------------------
  tl2 <- freqNk(rpl$idnk,rpl$locataire,poids = rpl$IPONDL,stat = c("rowpct"))
  tl2 <- plyr::rename(tl2,c('locataire(p)(rowpct)'='p_locataire'))
  
  # II.2. Part des locatairesHlm
  #---------------------------------------------------------------------------------------------------------------------------------------------------
  tl3 <- freqNk(rpl$idnk,rpl$locataireHlm,poids = rpl$IPONDL,stat = c("rowpct"))
  tl3 <- plyr::rename(tl3,c('locataireHlm(p)(rowpct)'='p_locataireHLM'))
  
  # II.3. Part des appartements
  #---------------------------------------------------------------------------------------------------------------------------------------------------
  tl4 <- freqNk(rpl$idnk,rpl$appartement,poids = rpl$IPONDL,stat = c("rowpct"))
  tl4 <- plyr::rename(tl4,c('appartement(p)(rowpct)'='p_appartement'))
  
  #--------------------------------------------------------------------------------------#  
  #                           Table synthétique                                          #
  #--------------------------------------------------------------------------------------# 
  
  # Création de la table contenant toutes les statistiques pondérées calculées à partir du 
  # RP exploitation principale
  tfinal <- merge(t0b[,c("id","pop_qpv_p")],
                  t1[,c("id","p_moins20","p_20_65","p_65plus")], by="id",all=T)
  tfinal <- merge(tfinal,
                  t1b[,c("id","p_60plus")],by="id",all=T) 
  tfinal <- merge(tfinal,
                  t1c[,c("id","p_femmes","p_hommes")],by="id",all=T) 
  tfinal <- merge(tfinal,
                  t2a[,c("id","p_scola_2_5")],by="id",all=T) 
  tfinal <- merge(tfinal,
                  t2b[,c("id","p_scola_18_24")],by="id",all=T) 
  tfinal <- merge(tfinal,
                  t2c[,c("id","p_Nscola_15plus")],by="id",all=T) 
  tfinal <- merge(tfinal,
                  t2d[,c("id","p_Nscola_6_14")],by="id",all=T) 
  tfinal <- merge(tfinal,
                  t2e[,c("id","t_decrocheur")],by="id",all=T) 
  tfinal <- merge(tfinal,
                  t3a[,c("id","t_chom_15_64","t_actifOcc_15_64")],by="id",all=T) 
  tfinal <- merge(tfinal,
                  t3b[,c("id","t_act_15_64","t_inact_15_64")],by="id",all=T) 
  tfinal <- merge(tfinal,
                  t3c[,c("id","t_actF_15_64","t_inactF_15_64")],by="id",all=T) 
  tfinal <- merge(tfinal,
                  t3d[,c("id","t_actH_15_64","t_inactH_15_64")],by="id",all=T) 
  tfinal <- merge(tfinal,
                  t3e[,c("id","p_cadre_prof_inter")],by="id",all=T)       
  tfinal <- merge(tfinal,
                  t4[,c("id","p_etranger")],by="id",all=T) 
  tfinal <- merge(tfinal,
                  t5[,c("id","p_immigre")],by="id",all=T) 
  tfinal <- merge(tfinal,
                  tl0[,c("id","p_resid_princ","p_log_occas","p_resid_second","p_log_vacants")],by="id",all=T) 
  tfinal <- merge(tfinal,
                  tl1[,c("id","p_surf100p")],by="id",all=T) 
  tfinal <- merge(tfinal,
                  tl1b[,c("id","p_residHlm")],by="id",all=T)
  tfinal <- merge(tfinal,
                  tl1c[,c("id","p_N_eau_chaude")],by="id",all=T)
  tfinal <- merge(tfinal,
                  tl1d[,c("id","p_N_Bain_douche")],by="id",all=T)
  tfinal <- merge(tfinal,
                  tl1e[,c("id","p_N_tout_egout")],by="id",all=T)
  tfinal <- merge(tfinal,
                  tl2[,c("id","p_locataire")],by="id",all=T) 
  tfinal <- merge(tfinal,
                  tl3[,c("id","p_locataireHLM")],by="id",all=T) 
  tfinal <- merge(tfinal,
                  tl4[,c("id","p_appartement")],by="id",all=T)
  
  #--------------------------------------------------------------------------------------#  
  #                      Enregistrement des tables dans une liste                        #
  #--------------------------------------------------------------------------------------# 
  
  liste <- list(t0a = t0a,tStatRP=tfinal,t0b = t0b,t1 = t1,t1b = t1b,t1c = t1c,t1d_pyramide=t1d_pyramide,t1e_pyramide = t1e_pyramide,
                t2a=t2a,t2b=t2b,t2c = t2c,t2d = t2d,t2e=t2e,t3a = t3a,t3b=t3b,t3c=t3c,t3d=t3d,t3e=t3e,
                t4=t4,t5=t5,tl0 = tl0, tl1=tl1,tl1b=tl1b,tl1c=tl1c,tl1d=tl1d,tl1e=tl1e,
                tl2=tl2,tl3=tl3,tl4=tl4
  )  
  return(liste)
}

#----------------------------------------------------------------------------------------------------------------#
# A voir si on conserve la suite #






















## Recoder les variables caractères en numérique afin d'appliquer le lissage du PSAR AU ##
zonaRpGridRecod <- function(rpi,rpl=NA){
  
  #####################
  ## Niveau individu ##
  #####################
  
  # Sexe 
  rpi$SEXEf <- ifelse(rpi$SEXE=="2",rpi$IPONDI,0)
  rpi$SEXEh <- ifelse(rpi$SEXE=="1",rpi$IPONDI,0)
  
  # Age
  rpi$ageNK <-  cut(as.numeric(rpi$AGEREV),
                    breaks = c(0,20,65,120),
                    include.lowest = TRUE,
                    right = FALSE)
  rpi$Age_0_20 <- ifelse(rpi$ageNK=="[0,20)",rpi$IPONDI,0)
  rpi$Age_20_64 <- ifelse(rpi$ageNK=="[20,65)",rpi$IPONDI,0)
  rpi$Age_65_120 <- ifelse(rpi$ageNK=="[65,120]",rpi$IPONDI,0)
  
  rpi$ageNK <-  cut(as.numeric(rpi$AGEREV),
                    breaks = c(0,2,6,20,60,120),
                    include.lowest = TRUE,
                    right = FALSE)
  rpi$Age_60_120 <- ifelse(rpi$ageNK=="[60,120]",rpi$IPONDI,0)
  rpi$Age_2_5 <- ifelse(rpi$ageNK=="[2,6)",rpi$IPONDI,0)
  
  # Etude
  rpi$etudi <- ifelse(rpi$ETUD=="1",rpi$IPONDI,0)
  rpi$Netudi <- ifelse(rpi$ETUD=="2",rpi$IPONDI,0)
  rpi$etudi2_5 <- ifelse(rpi$ETUD=="1" & !is.na(rpi$Age_2_5),rpi$IPONDI,0)
  rpi$Netudi2_5 <- ifelse(rpi$ETUD=="2"& !is.na(rpi$Age_2_5),rpi$IPONDI,0)
  
  return(rpi)
}

# Calcul des statistiques pour les Grilles
zonaRPGridStat <- function(rpi,rpl=NA){
  
  ########
  # SEXE #
  ########
  rpi$p_sexef <- round(100*rpi$SEXEf/rpi$IPONDI,digits = 3)
  rpi$p_sexeh <- round(100*rpi$SEXEh/rpi$IPONDI,digits = 3)
  
  
  #######
  # AGE #
  #######
  rpi$p_Age_0_20 <- round(100*rpi$Age_0_20/rpi$IPONDI,digits = 3)
  rpi$p_Age_20_64 <- round(100*rpi$Age_20_64/rpi$IPONDI,digits = 3)
  rpi$p_Age_65_120 <- round(100*rpi$Age_65_120/rpi$IPONDI,digits = 3)
  rpi$p_Age_60_120 <- round(100*rpi$Age_60_120/rpi$IPONDI,digits = 3)
  rpi$p_Age_2_5 <- round(100*rpi$Age_2_5/rpi$IPONDI,digits = 3)
  
  
  #########
  # ETUDE #
  #########  
  rpi$p_etudi <- round(100*rpi$etudi/rpi$IPONDI,digits = 3)
  rpi$p_Netudi <- round(100*rpi$Netudi/rpi$IPONDI,digits = 3)
  # Champ : ind age 2 à 5 
  rpi$p_Netudi2_5 <- round(100*rpi$Netudi2_5/rpi$Age_2_5,digits = 3)
  
  return(rpi[,!(colnames(rpi) %in% c("SEXEf","SEXEh","Age_0_20","Age_20_64","Age_65_120","Age_60_120",
                                     "Age_2_5","etudi","Netudi","etudi2_5","Netudi2_5"))])
}






















### Fonction qui ajoute la commune et le nom de la commune à chaque tableau ###
zonaTabMiseForme <- function(list_tab,rpi){
  
  if(!exists("com_rp")){
    # Correspondance communes du RP
    com_rp <<- read.csv2(file = "Bdd/RP13/RP13_Communes_DFA.csv",header = T)
  }
  
  
  for(i in 2:length(list_tab)){
    # Ajout de colonnes issues du RP pour chaque tableau
    list_tab[[i]] <- merge(list_tab[[i]],rpi[duplicated(rpi$idZonageV)==F,c("com","idZonageV")],
                           by.x="id",by.y ="idZonageV",all.x=T)
    
    # Ajout du nom des communes
    list_tab[[i]] <- merge(list_tab[[i]],com_rp[,c("cod_mod","lib_mod")],by.x="com",by.y="cod_mod",all.x=T)
    
    # Reorganisation du tableau
    list_tab[[i]] <- list_tab[[i]][,c(1,length(colnames(list_tab[[i]])),
                                      2:(length(colnames(list_tab[[i]]))-1))]
  }
  
  return(list_tab)
}

############################################################################################################
########## Equivalent Proc freq en SAS #####################################################################
############################################################################################################

# pondération : np = non pondéré
freq_form <-function(tab,nbdec=2,p="np",stat=c("rowpct","colpct","percent")){
  
  # Ajout des marges
  tab1 <- addmargins(tab,1)
  # pourcentage en lignes
  tab1_rpct <- prop.table(tab1,1)
  # pourcentage en colonnes
  
  tab1_cpct <- prop.table(tab,2)
  tab1_cpct
  # pourcentage en total
  tab1_ppct <- prop.table(tab)
  
  #Transformation en data.frame
  tab1 <- as.data.frame.matrix(tab1)
  tab1_rpct <- as.data.frame.matrix(tab1_rpct)
  tab1_cpct <- as.data.frame.matrix(tab1_cpct)
  tab1_ppct <- as.data.frame.matrix(tab1_ppct)
  
  # Arrondi des pourcentages
  tab1_rpct <- round(100 * tab1_rpct,nbdec)
  tab1_cpct <- round(100 * tab1_cpct,nbdec)
  tab1_ppct <- round(100 * tab1_ppct,nbdec)
  
  # On renomme les colonnes en vue d'une fusion de table
  colnames(tab1) <- paste(colnames(tab1),"(",p,")",sep="")
  colnames(tab1_rpct) <- paste(colnames(tab1_rpct),"(",p,")","(rowpct)",sep="")
  colnames(tab1_cpct) <- paste(colnames(tab1_cpct),"(",p,")","(colpct)",sep="")
  colnames(tab1_ppct) <- paste(colnames(tab1_ppct),"(",p,")","(percent)",sep="")   
  
  # Création des identifiants pour la fusion des tables
  tab1$id <- rownames(tab1)
  tab1_rpct$id <- rownames(tab1_rpct)
  tab1_cpct$id <- rownames(tab1_cpct)
  tab1_ppct$id <- rownames(tab1_ppct)
  
  # Fusion des deux data.frame
  
  tab1f <- tab1 
  
    if("rowpct" %in% stat){
      tab1f <- merge(tab1f,tab1_rpct,by="id",all.x=T)
    }
    
    if("colpct" %in% stat){
      tab1f <- merge(tab1f,tab1_cpct,by="id",all.x=T)
    }
    
    if("percent" %in% stat){
      tab1f <- merge(tab1f,tab1_ppct,by="id",all.x=T)
    }
 
  return(tab1f)
  
}


# Tableau croisé de deux variables discrètes effectif non pondérés
# A venir :  traiter les cas des pourcentages en colonnes et par rapport au total
# nbdec : nombre de décimale

freqNk <- function(var1,var2=NULL,poids=NULL, nbdec=2,stat=c("rowpct","colpct","percent")){
  
  if(!is.null(poids)){
    # chargement de la librarie questionr
    library(questionr)
    
    # Stat descriptives en non pondérées
    tab1a <-table(var1,var2)
    tab1a <- freq_form(tab = tab1a,nbdec = nbdec,stat = stat)
    
    # Stat descriptives en pondérées
    tab1b <-questionr::wtd.table(var1,var2,weights = poids)
    tab1b <- freq_form(tab = tab1b,p = "p",nbdec = nbdec,stat = stat)
    
    # fusion des deux tables
    tab1 <- merge(tab1a,tab1b,by="id",all=F)
    
  }else{
    # Stat descriptives en non pondérées
    tab1a <-table(var1,var2)
    tab1 <- freq_form(tab = tab1a,nbdec = nbdec)
  }
  
  return(tab1)
  
}

# Selection automatique d'une seule variable dans un tableau
keepNK <- function(tableau,val){
  return(
    tableau[, substr(colnames(tableau),1,nchar(val)) %in% substr(c("id",val),1,nchar(val))]
  )
}


