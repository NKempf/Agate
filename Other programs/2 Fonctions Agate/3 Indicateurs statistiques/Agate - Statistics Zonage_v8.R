#--------------------------------------------------------------------------------------------------------------------------------#
#                 Agate - Indicateurs statistiques par zonage                                                   #
#--------------------------------------------------------------------------------------------------------------------------------#

# MAJ : 25.03.2019 : ajout de nombreux indicateurs

# Use Dplyr language into functions : https://dplyr.tidyverse.org/articles/programming.html#different-data-sets

# Nicolas Kempf

statistics_zone <- function(group_var,zone,rpi,rpl,lstCategorie,sourceRp,rpi.weight,rpl.weight){
  
  #----------------------------------------------------------------------------------------------------------------------------------------------#
  #                                             A. Recensement de la population                                                                  #
  #----------------------------------------------------------------------------------------------------------------------------------------------#
  
  # I. Territoire
  #-----------------------------------------------------------------------------------------------------------------------------------------------
  # domaine <- 1

  # I.1. Superficie et densité de population
  #-----------------------------------------
  
  # Superficie en km²
  # zone@data$superficie <- round(gArea(zone,byid = T)/1000000,1)
  # zone <- zone@data

  # II. Individu
  #-----------------------------------------------------------------------------------------------------------------------------------------------
  
  # II.1. population totale
  #------------------------
  rpi.popTot <- rpi %>%
    group_by(!!! syms(group_var)) %>% 
    summarise(freq = n(),
              freq_p = round(sum(!!! syms(rpi.weight),na.rm = TRUE),0)) %>% 
    gather("type.indicateur","value",-group_var) %>% 
    mutate(nomVariable = "population",
           nomIndicateur = "a_population")
  
  # II.2. Champ : population totale
  #--------------------------------
  
  # II.2.1. Selection des variables
  lst_var <- lstCategorie$nomVariable[lstCategorie$source == "rpi" & lstCategorie$typeVar == "pct" & lstCategorie$ssChamp == 0]
  
  # II.2.2. Calcul indicateurs
  rpi.popTot.champ <- bind_rows(lapply(lst_var, agate_qualitative,df = rpi,group_var = group_var, poids = rpi.weight))
  
  # II.3. Sous-champs
  #------------------
  
  # II.3.1. Informations sur les champs
  df_ssChamp <- lstCategorie %>% 
    filter(source == "rpi" & typeVar == "pct" & ssChamp == 1) %>% 
    select(nomVariable,variableChamp,modaliteChamp) %>% 
    mutate(varmod.champ = paste0(variableChamp,modaliteChamp))
  
  # II.3.2. Liste des sous-champs à calculer
  lst_champ <- unique(df_ssChamp$varmod.champ)
  
  # II.3.3. Calcul des indicateurs
  rpi.sschamp <- bind_rows(lapply(lst_champ, agate_qualitative.ssChamp,df_ssChamp = df_ssChamp,rp = rpi, poids = rpi.weight))
  
  # III. Logements
  #-----------------------------------------------------------------------------------------------------------------------------------------------
  
  # III.1. Champ : Logements
  #-------------------------
  # III.1.1. Selection des variables
  lst_var <- lstCategorie$nomVariable[lstCategorie$variableChamp %in% "X"]
  
  # III.1.2. Calcul indicateurs
  rpl.log.champ <- bind_rows(lapply(lst_var, agate_qualitative,df = rpl,group_var = group_var, poids = rpl.weight))
  
  # II.2. Sous-champs
  #------------------
  
  # II.2.1. Informations sur les champs
  df_ssChamp <- lstCategorie %>% 
    filter(source == "rpl" & !is.na(modaliteChamp)) %>% 
    select(nomVariable,variableChamp,modaliteChamp) %>% 
    mutate(varmod.champ = paste0(variableChamp,modaliteChamp))
  
  # II.2.2. Liste des sous-champs à calculer
  lst_champ <- unique(df_ssChamp$varmod.champ)
  
  # II.2.3. Calcul des indicateurs
  rpl.sschamp <- bind_rows(lapply(lst_champ, agate_qualitative.ssChamp,df_ssChamp = df_ssChamp,rp = rpl, poids = rpl.weight))
  
  # IV. Ajout à la table finale
  #-----------------------------------------------------------------------------------------------------------------------------------------------
  indicateur_stat <- bind_rows(rpi.popTot,rpi.popTot.champ,rpi.sschamp,rpl.log.champ,rpl.sschamp) %>% 
    left_join(lstCategorie %>% select(nomVariable,domaine,categorie,source),by="nomVariable") %>% 
    mutate(source = paste0(source,sourceRp))
    
  #----------------------------------------------------------------------------------------------------------------------------------------------#
  #                                             B. Filosofi : Fichiers fiscaux                                                                   #
  #----------------------------------------------------------------------------------------------------------------------------------------------#
  
  # TODO : A revoir 26.03.2019
  
  # # I. Statistiques du niveau de vie
  # #-----------------------------------------------------------------------------------------------------------------------------------------------
  # domaine <- 2
  # # I.1. Distribution du niveau de vie selon le zonage
  # #---------------------------------------------------
  # indicateur_stat <- filo %>%
  #   group_by(!!! syms(group_var)) %>% 
  #   income_distrib("nivviem") %>% 
  #   mutate(indicateur = "nivviem") %>% 
  #   majDf(indicateur_stat,group_var = group_var,domaine = domaine,source = sourceFilo,categorie = 1) 
  # 
  # 
  # # I.2. Distribution du niveau de vie selon le type de menage et le zonage
  # #------------------------------------------------------------------------
  # indicateur_stat <- filo %>% 
  #   mutate(indicateur = paste("nivviem",typmenR.lib,sep="_")) %>% 
  #   group_by(!!! syms(group_var),indicateur) %>% 
  #   income_distrib("nivviem") %>% 
  #   majDf(indicateur_stat,group_var = group_var,domaine = domaine,source = sourceFilo,categorie = 4) 
  # 
  # 
  # # I.3. Part des menages selon le type de menage et le zonage
  # #-----------------------------------------------------------
  # indicateur_stat <- filo %>%
  #   rename(indicateur = typmenR.lib) %>% 
  #   statRp_agate(df_final = indicateur_stat,group_var = group_var,poids = filo.weight,
  #                domaine = 1,categorie = 4,source = sourceFilo)
  # 
  # # I.4. Part des menages selon le decile de niveau de vie metro et le zonage
  # #--------------------------------------------------------------------------
  # indicateur_stat <- filo %>%
  #   mutate(indicateur = paste0("D_",decile_nivvie)) %>% 
  #   statRp_agate(df_final = indicateur_stat,group_var = group_var,poids = filo.weight,
  #                domaine = domaine,categorie = 2,source = sourceFilo)
  # 
  # # I.5 Taux de pauvrete selon le seuil metropolitain et departemental
  # #--------------------------------------------------------------------
  # seuil_pauv.dfa <- filo %>%
  #   group_by(dep) %>% 
  #   summarise(seuil_pauv.dfa = median(nivviem))
  # 
  # indicateur_stat <- filo %>%
  #   left_join(seuil_pauv.dfa,by = "dep") %>% 
  #   group_by(!!! syms(group_var)) %>% 
  #   mutate(i_pauvre60.dfa = ifelse(nivviem <= 0.6 * seuil_pauv.dfa,1,0),
  #          i_pauvre50.dfa = ifelse(nivviem <= 0.5 * seuil_pauv.dfa,1,0),
  #          i_pauvre40.dfa = ifelse(nivviem <= 0.4 * seuil_pauv.dfa,1,0)) %>% 
  #   summarise(freq.ind = round(sum(nbpersm ,na.rm = TRUE),0),
  #             freq.pauv60.ind.metro = round(sum(i_pauvre60 * nbpersm,na.rm = TRUE),0),
  #             # freq.pauv60.ind.dfa = round(sum(i_pauvre60.dfa * nbpersm,na.rm = TRUE),0),
  #             freq.pauv50.ind.metro = round(sum(i_pauvre50 * nbpersm,na.rm = TRUE),0),
  #             # freq.pauv50.ind.dfa = round(sum(i_pauvre50.dfa * nbpersm,na.rm = TRUE),0),
  #             freq.pauv40.ind.metro = round(sum(i_pauvre40 * nbpersm,na.rm = TRUE),0)) %>% 
  #             # freq.pauv40.ind.dfa = round(sum(i_pauvre40.dfa * nbpersm,na.rm = TRUE),0)) %>% 
  #   mutate(tx_pauv60.ind.metro = round(100 * freq.pauv60.ind.metro / freq.ind,2),
  #          # tx_pauv60.ind.dfa = round(100 * freq.pauv60.ind.dfa / freq.ind,2),
  #          tx_pauv50.ind.metro = round(100 * freq.pauv50.ind.metro / freq.ind,2),
  #          # tx_pauv50.ind.dfa = round(100 * freq.pauv50.ind.dfa / freq.ind,2),
  #          tx_pauv40.ind.metro = round(100 * freq.pauv40.ind.metro / freq.ind,2),
  #          # tx_pauv40.ind.dfa = round(100 * freq.pauv40.ind.dfa / freq.ind,2),
  #          indicateur = "pauvrete") %>% 
  #   majDf(indicateur_stat,group_var = group_var,domaine = domaine,source = sourceFilo,categorie = 3)
  
  
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
    weighted_frequency(rpi.weight) %>% 
    mutate(source = paste0("rpi",sourceRp))
  
  # VI.2. Pyramide par sexe, zonage et tranche d'age 
  #------------------------------------------------
  pyramide_tr <- rpi %>%
    mutate(age = cut(as.numeric(rpi$AGEREV),
                     breaks = c(seq(0,90,4),120),
                     include.lowest = TRUE,
                     right = FALSE)) %>%
    group_by(!!! syms(group_var),SEXE,age) %>%
    weighted_frequency(rpi.weight) %>% 
    mutate(source = paste0("rpi",sourceRp))
  
  # II.2 Courbe de lorenz
  #----------------------
  # prob <- seq(0,1,by = 0.01)
  # 
  # c_lorenz <- filo %>% 
  #   group_by(!!! syms(group_var)) %>% 
  #   do(data.frame(Q=prob, stat=quantile(.$nivviem, probs=prob))) %>%
  #   mutate(stat = ifelse(stat<0,0,stat),
  #          source = sourceFilo) 
  
  #----------------------------------------------------------------------------------------------------------------------------------------------#
  #                                             VII. Listes de tableaux finaux                                                                   #
  #----------------------------------------------------------------------------------------------------------------------------------------------#
  list_tab <- list(indicateur_stat = indicateur_stat,pyramide_detail = pyramide_detail, pyramide_tr = pyramide_tr)
  
  return(list_tab)  
} 

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#


# Calcul les frequences et la part ponderee
weighted_frequency <- function(df,weight){
  df %>% 
    summarise(freq = n(),
              freq_p = round(sum(!!! syms(weight),na.rm = TRUE),0)) %>%
    mutate(part_np = round(100*freq / sum(freq),2),
           part_p = round(100*freq_p / sum(freq_p),2)) 
}

# Indicateurs statistiques à partir de variables qualitatives
agate_qualitative <- function(indicateur,df,group_var,poids){
  df %>% 
    rename(nomIndicateur = indicateur) %>% 
    group_by(!!! syms(group_var),nomIndicateur) %>%
    weighted_frequency(poids) %>% 
    gather("type.indicateur","value",-group_var,-nomIndicateur) %>%
    mutate(nomVariable = indicateur) %>%
    ungroup()
}

# Automatisation de la fonction agate_qualitative pour les sous-schamps
agate_qualitative.ssChamp <- function(champ,df_ssChamp,rp, poids){
  
  lst_var <- df_ssChamp$nomVariable[df_ssChamp$varmod.champ == champ]
  var.filtre <- df_ssChamp$variableChamp[df_ssChamp$varmod.champ == champ]
  modalite.filtre <- df_ssChamp$modaliteChamp[df_ssChamp$varmod.champ == champ]
  
  bind_rows(lapply(lst_var, agate_qualitative,
                   df = rp %>% filter_at(vars(var.filtre), all_vars(. %in% modalite.filtre)),
                   group_var = group_var, poids = poids))
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
  df <- df %>%
    do(data.frame(Q=proba, n = length(.[[income]]), avg = mean(.[[income]]), stat=quantile(.[[income]], probs=proba))) %>% 
    spread(Q, stat,sep = "_") %>%
    mutate(d9_d1 = round(Q_0.9 / Q_0.1,2),
           q4_q1 = round(Q_0.8 / Q_0.2,2),
           d5_d1 = round(Q_0.5 / Q_0.1,2),
           d9_d5 = round(Q_0.9 / Q_0.5,2),
           avg = round(avg,0))
  
  # Arrondi
  df.select <- colnames(df)[substr(colnames(df),1,2) == "Q_"]
  df[,df.select] <- round(df[,df.select],0)
  
  return(df)
  
}

# Statistiques discretes RP et mise à jour table finale
statRp_agate <- function(df,df_final,group_var,poids,domaine,categorie,source){
  df %>% 
    group_by(!!! syms(group_var),indicateur) %>%
    weighted_frequency(poids) %>% 
    majDf(df_final,group_var = group_var,domaine = domaine,source = source,categorie = categorie)
}







