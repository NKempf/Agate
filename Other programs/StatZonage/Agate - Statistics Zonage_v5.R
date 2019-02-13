#--------------------------------------------------------------------------------------------------------------------------------#
#                 AppliShiny - Fonction calcul de statistiques selon un zonage                                                   #
#--------------------------------------------------------------------------------------------------------------------------------#

# MAJ : 23.01.2019

# Use Dplyr language into functions : https://dplyr.tidyverse.org/articles/programming.html#different-data-sets

# Nicolas Kempf

statistics_zone <- function(group_var,zone,rpi,rpl,filo,sourceRpi,sourceRpl,sourceFilo,rpi.weight,rpl.weight,filo.weight){
  
  #----------------------------------------------------------------------------------------------------------------------------------------------#
  #                                             A. Recensement de la population                                                                  #
  #----------------------------------------------------------------------------------------------------------------------------------------------#
  
  # I. Territoire
  #-----------------------------------------------------------------------------------------------------------------------------------------------
  domaine <- "Territoire"
  # Strategie deux focntions imbriqués :
  # Fct la plus basse permet de generer le maximum de calcul simple pour n'importe quelle zone
  # Fct agregée permet le calcul spécifique selon le type de zone (infracommunale ou autre)
  
  # I.1. Population et densité de population par zonage
  #----------------------------------------------------
  
  # Superficie en km²
  zone@data$superficie <- gArea(zone,byid = T)/1000000
  zone <- zone@data
  
  indicateur_stat <- rpi %>%
    group_by(!!! syms(group_var)) %>% 
    summarise(freq = n(),
              freq_p = round(sum(!!! syms(rpi.weight),na.rm = TRUE),0)) %>% 
    left_join(zone %>% select(idZonage,superficie),"idZonage") %>% 
    mutate(densitepop = round(freq_p / superficie,0)) %>%
    gather("type.indicateur","value",-group_var) %>% 
    mutate(indicateur = "population",
           source = sourceRpi,
           # unite = "nb",
           domaine = domaine,
           categorie = "population")
  
  
  # indicateur_stat <- rpi %>%
  #   group_by(!!! syms(group_var)) %>% 
  #   summarise(freq = n(),
  #             freq_p = round(sum(!!! syms(rpi.weight),na.rm = TRUE),0)) %>%
  #   gather("type.indicateur","value",-group_var) %>% 
  #   mutate(indicateur = "population",
  #          source = sourceRpi,
  #          # unite = "nb",
  #          domaine = "territoire",
  #          categorie = "population") 
  
  # II.1. Part des moins de 20 ans et des 20 a 64 ans
  #--------------------------------------------------
  indicateur_stat <- rpi %>%
    mutate(indicateur = cut(as.numeric(AGEREV),
                            breaks = c(0,20,65,120),
                            include.lowest = TRUE,
                            right = FALSE)) %>%
    statRp_agate(df_final = indicateur_stat,group_var = group_var,poids = rpi.weight,
                 domaine = domaine,categorie = "age",source = sourceRpi)
  
  # II.2. Part des 60 ans et plus (!)
  #------------------------------
  indicateur_stat <- rpi %>%
    mutate(indicateur = cut(as.numeric(rpi$AGEREV),
                            breaks = c(0,20,60,120),
                            include.lowest = TRUE,
                            right = FALSE)) %>%
    group_by(!!! syms(group_var),indicateur) %>%
    weighted_frequency(rpi.weight) %>% 
    filter(indicateur != "[0,20)") %>% 
    majDf(indicateur_stat,group_var = group_var,domaine = domaine,source = sourceRpi,categorie = "age")
  
  # II.3. Part des femmes
  #----------------------
  indicateur_stat <- rpi %>%
    mutate(indicateur = SEXE) %>% 
    statRp_agate(df_final = indicateur_stat,group_var = group_var,poids = rpi.weight,
                 domaine = domaine,categorie = "sexe",source = sourceRpi)
  
  # III. Scolarisation
  #-----------------------------------------------------------------------------------------------------------------------------------------------
  domaine <- "Scolarisation"
  
  # III.1. Taux de scolarisation des 2 a 5 ans et des 18 a 25 ans (!)
  #--------------------------------------------------------------
  indicateur_stat <- rpi %>%
    mutate(age = cut(as.numeric(rpi$AGEREV),
                     breaks = c(0,2,6,18,25,120),
                     include.lowest = TRUE,
                     right = FALSE),
           indicateur = paste0(ETUD,age)) %>%
    group_by(!!! syms(group_var),indicateur) %>%
    weighted_frequency(rpi.weight) %>%
    filter(indicateur %in% c("etudi[2,6)","n_etudi[2,6)","etudi[18,25)","n_etudi[18,25)")) %>% 
    majDf(indicateur_stat,group_var = group_var,domaine = domaine,source = sourceRpi,categorie = "scolarisation")
  
  # III.2. Part de non scolarisation de plus de 15 ans sans diplome (!)
  #----------------------------------------------------------------
  indicateur_stat <- rpi %>%
    filter(as.numeric(AGEREV) >= 15 & DIPL == "A") %>% 
    mutate(indicateur = ifelse(ETUD == "n_etudi","nScola_15plus","autre")) %>%
    group_by(!!! syms(group_var),indicateur) %>% 
    weighted_frequency(rpi.weight) %>% 
    majDf(indicateur_stat,group_var = group_var,domaine = domaine, source = sourceRpi,categorie = "non scolarise sans diplome [15,+[")
  
  # III.3. Taux de non scolarisation des 6 - 14 ans
  #-----------------------------------------------
  indicateur_stat <- rpi %>%
    filter(as.numeric(AGEREV) %in% c(6:14)) %>% 
    mutate(indicateur = ETUD) %>% 
    statRp_agate(df_final = indicateur_stat,group_var = group_var,poids = rpi.weight,
                 domaine = domaine,categorie = "non scolarise [6,14]",source = sourceRpi)
  
  # III.4. Taux de decrocheur : jeune non scolarise de 16 - 25 ans sans diplome (!)
  #----------------------------------------------------------------------------
  indicateur_stat <- rpi %>%
    filter(as.numeric(AGEREV) %in% c(16:25)) %>%  
    mutate(indicateur = ifelse(DIPL == "A" & ETUD == "n_etudi","decrocheur","n_decrocheur")) %>% 
    statRp_agate(df_final = indicateur_stat,group_var = group_var,poids = rpi.weight,
                 domaine = domaine,categorie = "decrocheur",source = sourceRpi)
  
  
  # IV. Emploi
  #-----------------------------------------------------------------------------------------------------------------------------------------------
  domaine <- "Emploi"
  
  # IV.1. Taux de chomage pour les 15 - 64 ans (parmi les actifs)
  #--------------------------------------------------------------
  indicateur_stat <- rpi %>%
    filter(as.numeric(AGEREV) %in% c(16:64) & TACT %in% c("actifocc","chomeur")) %>%
    mutate(indicateur = TACT) %>% 
    statRp_agate(df_final = indicateur_stat,group_var = group_var,poids = rpi.weight,
                 domaine = domaine,categorie = "chomage",source = sourceRpi)
  
  # IV.2. Taux d'actif  - d'inactif pour les 15 - 64 ans
  #-----------------------------------------------------
  indicateur_stat <- rpi %>%
    filter(as.numeric(AGEREV) %in% c(16:64)) %>%
    mutate(indicateur = inactif) %>%
    statRp_agate(df_final = indicateur_stat,group_var = group_var,poids = rpi.weight,
                 domaine = domaine,categorie = "chomage",source = sourceRpi)
  
  
  # IV.3. Taux d'actif selon le sexe pour les 15 - 64 ans
  #------------------------------------------------------
  indicateur_stat <- rpi %>%
    filter(as.numeric(AGEREV) %in% c(16:64)) %>%
    mutate(indicateur = paste(inactif,SEXE,sep=" ")) %>% 
    statRp_agate(df_final = indicateur_stat,group_var = group_var,poids = rpi.weight,
                 domaine = domaine,categorie = "actif par sexe",source = sourceRpi)
  
  # IV.4. Part des actifs de 15-64 ans cadres et professions intermediaire
  #-----------------------------------------------------------------------
  indicateur_stat <- rpi %>%
    filter(as.numeric(AGEREV) %in% c(16:64) & TACT %in% c("actifocc","chomeur")) %>%
    mutate(indicateur = ifelse(POSP %in% c("1J","1I","1H","1G","1F"),"cadre_prof_inter","autre")) %>%
    statRp_agate(df_final = indicateur_stat,group_var = group_var,poids = rpi.weight,
                 domaine = domaine,categorie = "cadre et profession inter",source = sourceRpi)
  
  # V. Immigration
  #-----------------------------------------------------------------------------------------------------------------------------------------------
  domaine <- "Immigration"
  # V.1. Part des etrangers
  #------------------------
  indicateur_stat <- rpi %>%
    mutate(indicateur = etranger) %>% 
    statRp_agate(df_final = indicateur_stat,group_var = group_var,poids = rpi.weight,
                 domaine = domaine,categorie = "etranger",source = sourceRpi)
  
  # V.2. Part des immigres
  #-----------------------
  indicateur_stat <- rpi %>%
    mutate(indicateur = immigre) %>% 
    statRp_agate(df_final = indicateur_stat,group_var = group_var,poids = rpi.weight,
                 domaine = domaine,categorie = "immigre",source = sourceRpi)
  
  # VI. Logements
  #-----------------------------------------------------------------------------------------------------------------------------------------------
  domaine <- "Logements"
  
  # VI.1. Part de logements selon la categorie de logement ( A faire modifier libelle CATL)
  #-------------------------------------------------------
  indicateur_stat <- rpl %>%
    mutate(indicateur = catLog) %>% 
    statRp_agate(df_final = indicateur_stat,group_var = group_var,poids = rpl.weight,
                 domaine = domaine,categorie = "categorie logement",source = sourceRpl)
  
  # VI.2. Part des appartements
  #----------------------------
  indicateur_stat <- rpl %>%
    mutate(indicateur = appartement) %>% 
    statRp_agate(df_final = indicateur_stat,group_var = group_var,poids = rpl.weight,
                 domaine = domaine,categorie = "appartement",source = sourceRpl)
  
  # VI.3. Part des locataires
  #--------------------------
  indicateur_stat <- rpl %>%
    mutate(indicateur = locataire) %>% 
    statRp_agate(df_final = indicateur_stat,group_var = group_var,poids = rpl.weight,
                 domaine = domaine,categorie = "locataire",source = sourceRpl)
  
  # VI.4. Part des locatairesHlm
  #-----------------------------
  indicateur_stat <- rpl %>%
    mutate(indicateur = locataireHlm) %>% 
    statRp_agate(df_final = indicateur_stat,group_var = group_var,poids = rpl.weight,
                 domaine = domaine,categorie = "locataire Hlm",source = sourceRpl)
  
  # VII. Residences principales
  #-----------------------------------------------------------------------------------------------------------------------------------------------
  domaine <- "Residences Principales"
  
  # VII.1. Part des residences principales de plus de 100m²
  #--------------------------------------------------------
  indicateur_stat <- rpl %>%
    filter(CATL == "1") %>%
    mutate(indicateur = surf100) %>% 
    statRp_agate(df_final = indicateur_stat,group_var = group_var,poids = rpl.weight,
                 domaine = domaine,categorie = "surface logement",source = sourceRpl)
  
  # VII.2. Part des residences principales en HLM
  #----------------------------------------------
  indicateur_stat <- rpl %>%
    filter(CATL == "1") %>% 
    mutate(indicateur = hlm) %>% 
    statRp_agate(df_final = indicateur_stat,group_var = group_var,poids = rpl.weight,
                 domaine = domaine,categorie = "hlm",source = sourceRpl)
  
  # VII.3. Part des residences principales sans eau chaude
  #-------------------------------------------------------  
  indicateur_stat <- rpl %>%
    filter(CATL == "1" & EAU %in% c("1","2","3")) %>% 
    mutate(indicateur = ifelse(EAU =="2","Eau_chaude","N_eau_chaude")) %>% 
    statRp_agate(df_final = indicateur_stat,group_var = group_var,poids = rpl.weight,
                 domaine = domaine,categorie = "presence eau chaude",source = sourceRpl)
  
  # VII.4. Part des residences principales sans bain ni douche 
  #-----------------------------------------------------------
  indicateur_stat <- rpl %>%
    filter(CATL == "1" & BAIN %in% c("1","2")) %>% 
    mutate(indicateur = ifelse(BAIN == "1","bain_douche","N_bain_douche")) %>% 
    statRp_agate(df_final = indicateur_stat,group_var = group_var,poids = rpl.weight,
                 domaine = domaine,categorie = "presence douche ou baignoire",source = sourceRpl)
  
  # VII.5. Part des residences principales sans tout a l'egout
  #-----------------------------------------------------------
  indicateur_stat <- rpl %>%
    filter(CATL == "1" & EGOUL %in% c("1","2","3","4")) %>% 
    mutate(indicateur = ifelse(EGOUL == "1","tout_egout","N_tout_egout")) %>% 
    statRp_agate(df_final = indicateur_stat,group_var = group_var,poids = rpl.weight,
                 domaine = domaine,categorie = "presence egout",source = sourceRpl)
  
  
  #----------------------------------------------------------------------------------------------------------------------------------------------#
  #                                             II. Statistiques issues de Filosofi                                                              #
  #----------------------------------------------------------------------------------------------------------------------------------------------#
  
  # I. Statistiques du niveau de vie
  #-----------------------------------------------------------------------------------------------------------------------------------------------
  domaine <- "Niveau de vie"
  # I.1. Distribution du niveau de vie selon le zonage
  #---------------------------------------------------
  indicateur_stat <- filo %>%
    group_by(!!! syms(group_var)) %>% 
    income_distrib("nivviem") %>% 
    mutate(indicateur = "nivviem") %>% 
    majDf(indicateur_stat,group_var = group_var,domaine = domaine,source = sourceFilo,categorie = "Distribution") 
  
  
  # I.2. Distribution du niveau de vie selon le type de menage et le zonage
  #------------------------------------------------------------------------
  indicateur_stat <- filo %>% 
    mutate(indicateur = paste("nivviem",typmenR.lib,sep="_")) %>% 
    group_by(!!! syms(group_var),indicateur) %>% 
    income_distrib("nivviem") %>% 
    majDf(indicateur_stat,group_var = group_var,domaine = domaine,source = sourceFilo,categorie = "Type de menage") 
  
  
  # I.3. Part des menages selon le type de menage et le zonage
  #-----------------------------------------------------------
  indicateur_stat <- filo %>%
    rename(indicateur = typmenR.lib) %>% 
    statRp_agate(df_final = indicateur_stat,group_var = group_var,poids = filo.weight,
                 domaine = "Territoire",categorie = "Type de menage",source = sourceFilo)
  
  # I.4. Part des menages selon le decile de niveau de vie metro et le zonage
  #--------------------------------------------------------------------------
  indicateur_stat <- filo %>%
    mutate(indicateur = paste0("D_",decile_nivvie)) %>% 
    statRp_agate(df_final = indicateur_stat,group_var = group_var,poids = filo.weight,
                 domaine = domaine,categorie = "Richesse",source = sourceFilo)
  
  # I.5 Taux de pauvrete selon le seuil metropolitain et departemental
  #--------------------------------------------------------------------
  seuil_pauv.dfa <- filo %>%
    group_by(dep) %>% 
    summarise(seuil_pauv.dfa = median(nivviem))
  
  indicateur_stat <- filo %>%
    left_join(seuil_pauv.dfa,by = "dep") %>% 
    group_by(!!! syms(group_var)) %>% 
    mutate(i_pauvre60.dfa = ifelse(nivviem <= 0.6 * seuil_pauv.dfa,1,0),
           i_pauvre50.dfa = ifelse(nivviem <= 0.5 * seuil_pauv.dfa,1,0),
           i_pauvre40.dfa = ifelse(nivviem <= 0.4 * seuil_pauv.dfa,1,0)) %>% 
    summarise(freq.ind = round(sum(nbpersm ,na.rm = TRUE),0),
              freq.pauv60.ind.metro = round(sum(i_pauvre60 * nbpersm,na.rm = TRUE),0),
              freq.pauv60.ind.dfa = round(sum(i_pauvre60.dfa * nbpersm,na.rm = TRUE),0),
              freq.pauv50.ind.metro = round(sum(i_pauvre50 * nbpersm,na.rm = TRUE),0),
              freq.pauv50.ind.dfa = round(sum(i_pauvre50.dfa * nbpersm,na.rm = TRUE),0),
              freq.pauv40.ind.metro = round(sum(i_pauvre40 * nbpersm,na.rm = TRUE),0),
              freq.pauv40.ind.dfa = round(sum(i_pauvre40.dfa * nbpersm,na.rm = TRUE),0)) %>% 
    mutate(tx_pauv60.ind.metro = round(100 * freq.pauv60.ind.metro / freq.ind,2),
           tx_pauv60.ind.dfa = round(100 * freq.pauv60.ind.dfa / freq.ind,2),
           tx_pauv50.ind.metro = round(100 * freq.pauv50.ind.metro / freq.ind,2),
           tx_pauv50.ind.dfa = round(100 * freq.pauv50.ind.dfa / freq.ind,2),
           tx_pauv40.ind.metro = round(100 * freq.pauv40.ind.metro / freq.ind,2),
           tx_pauv40.ind.dfa = round(100 * freq.pauv40.ind.dfa / freq.ind,2),
           indicateur = "pauvrete") %>% 
    majDf(indicateur_stat,group_var = group_var,domaine = domaine,source = sourceFilo,categorie = "Pauvrete")
  
  
  #----------------------------------------------------------------------------------------------------------------------------------------------#
  #                                             III. Statistiques issues de FIDELI                                                               #
  #----------------------------------------------------------------------------------------------------------------------------------------------#  
  
  #----------------------------------------------------------------------------------------------------------------------------------------------#
  #                                             IV. Statistiques issues de BPE                                                                   #
  #----------------------------------------------------------------------------------------------------------------------------------------------#    
  
  #----------------------------------------------------------------------------------------------------------------------------------------------#
  #                                             V. Statistiques issues de la CAF                                                                 #
  #----------------------------------------------------------------------------------------------------------------------------------------------# 
  
  
  #----------------------------------------------------------------------------------------------------------------------------------------------#
  #                                             VI. Objets speciaux                                                                              #
  #----------------------------------------------------------------------------------------------------------------------------------------------#  
  
  # VI.1. Pyramide des ages par sexe et par zonage
  #-----------------------------------------------
  pyramide_detail <- rpi %>%
    group_by(!!! syms(group_var),SEXE,AGEREV) %>%
    weighted_frequency(rpi.weight)
  
  # VI.2. Pyramide par sexe, zonage et tranche d'age 
  #------------------------------------------------
  pyramide_tr <- rpi %>%
    mutate(age = cut(as.numeric(rpi$AGEREV),
                     breaks = c(seq(0,90,4),120),
                     include.lowest = TRUE,
                     right = FALSE)) %>%
    group_by(!!! syms(group_var),SEXE,age) %>%
    weighted_frequency(rpi.weight)
  
  # II.2 Courbe de lorenz
  #----------------------
  prob <- seq(0,1,by = 0.01)
  
  c_lorenz <- filo %>% 
    group_by(!!! syms(group_var)) %>% 
    do(data.frame(Q=prob, stat=quantile(.$nivviem, probs=prob))) %>%
    mutate(stat = ifelse(stat<0,0,stat)) 
  
  #----------------------------------------------------------------------------------------------------------------------------------------------#
  #                                             VII. Listes de tableaux finaux                                                                   #
  #----------------------------------------------------------------------------------------------------------------------------------------------#
  list_tab <- list(indicateur_stat = indicateur_stat,pyramide_detail = pyramide_detail, pyramide_tr = pyramide_tr, c_lorenz = c_lorenz)
  
  return(list_tab)  
  
} 








# Calcul les frequences et la part ponderee
weighted_frequency <- function(df,weight){
  df %>% 
    summarise(freq = n(),
              freq_p = round(sum(!!! syms(weight),na.rm = TRUE),0)) %>%
    mutate(part_np = round(100*freq / sum(freq),2),
           part_p = round(100*freq_p / sum(freq_p),2)) 
}

majDf <- function(df_init,df_final,group_var = group_var,domaine,source,categorie){
  df_init  %>% 
    gather("type.indicateur","value",-group_var,-indicateur) %>% 
    mutate(domaine = domaine,source = source,categorie = categorie) %>% 
    ungroup() %>% 
    bind_rows(df_final)
}

# Distribution d'un revenu
income_distrib <- function(df,income,
                           proba = c(0.01,0.05,0.1,0.2,0.25,0.5,0.75,0.80,0.90,0.95,0.99)){
  # Programming tips about do function : https://www.r-bloggers.com/dplyr-do-some-tips-for-using-and-programming/
  df %>%
    do(data.frame(Q=proba, n = length(.[[income]]), avg = mean(.[[income]]), stat=quantile(.[[income]], probs=proba))) %>% 
    spread(Q, stat,sep = "_") %>%
    mutate(d9_d1 = round(Q_0.9 / Q_0.1,2),
           q4_q1 = round(Q_0.8 / Q_0.2,2),
           d5_d1 = round(Q_0.5 / Q_0.1,2),
           d9_d5 = round(Q_0.9 / Q_0.5,2))
}

# Statistiques discretes RP et mise à jour table finale
statRp_agate <- function(df,df_final,group_var,poids,domaine,categorie,source){
  df %>% 
    group_by(!!! syms(group_var),indicateur) %>%
    weighted_frequency(poids) %>% 
    majDf(df_final,group_var = group_var,domaine = domaine,source = source,categorie = categorie)
}



