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
  zone@data$value <- round(gArea(zone,byid = T)/1000000,1)
  zone <- zone@data %>% 
    mutate(type.indicateur = "superficie",
           nomVariable = "superficie",
           nomIndicateur = "a_superficie")

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
           nomIndicateur = "a_population") %>% 
    bind_rows(zone)
  
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
  
  # III.1. population totale
  #------------------------
  rpl.popTot <- rpl %>%
    group_by(!!! syms(group_var)) %>% 
    summarise(freq = n(),
              freq_p = round(sum(!!! syms(rpl.weight),na.rm = TRUE),0)) %>% 
    gather("type.indicateur","value",-group_var) %>% 
    mutate(nomVariable = "log_tot",
           nomIndicateur = "a_log_tot")
  
  # III.2. Champ : Logements
  #-------------------------
  # III.2.1. Selection des variables
  lst_var <- lstCategorie$nomVariable[lstCategorie$variableChamp %in% "X" & lstCategorie$typeVar == "pct" & lstCategorie$ssChamp == 0]
  
  # III.2.2. Calcul indicateurs
  rpl.log.champ <- bind_rows(lapply(lst_var, agate_qualitative,df = rpl,group_var = group_var, poids = rpl.weight))
  
  # III.3. Sous-champs
  #------------------
  
  # III.3.1. Informations sur les champs
  df_ssChamp <- lstCategorie %>% 
    filter(source == "rpl" & !is.na(modaliteChamp)) %>% 
    select(nomVariable,variableChamp,modaliteChamp) %>% 
    mutate(varmod.champ = paste0(variableChamp,modaliteChamp))
  
  # III.3.2. Liste des sous-champs à calculer
  lst_champ <- unique(df_ssChamp$varmod.champ)
  
  # III.3.3. Calcul des indicateurs
  rpl.sschamp <- bind_rows(lapply(lst_champ, agate_qualitative.ssChamp,df_ssChamp = df_ssChamp,rp = rpl, poids = rpl.weight))
  
  # IV. Ajout à la table finale
  #-----------------------------------------------------------------------------------------------------------------------------------------------
  indicateur_stat <- bind_rows(rpi.popTot,rpi.popTot.champ,rpi.sschamp,rpl.popTot,rpl.log.champ,rpl.sschamp) %>% 
    left_join(lstCategorie %>% select(nomVariable,domaine,categorie,source),by="nomVariable") %>% 
    mutate(source = paste0(source,sourceRp),
           value = as.character(value))
    
  #----------------------------------------------------------------------------------------------------------------------------------------------#
  #                                             B.  Objets speciaux                                                                              #
  #----------------------------------------------------------------------------------------------------------------------------------------------#  

  # 1. Pyramide par sexe, zonage et tranche d'age (MAJ : 27.03.2019) 
  #----------------------------------------------------------------
  pyramide_tr <- bind_rows(lapply(c("a_homme","b_femme"),function(mod.sexe){
    rpi %>% 
      filter(dem_sexe == mod.sexe) %>% 
      agate_qualitative(df = .,indicateur = "dem_agerevTr",group_var = group_var,poids = rpi.weight) %>% 
      filter(type.indicateur == "part_p") %>% 
      mutate(dem_sexe = mod.sexe)
  })
  ) %>% 
    mutate(value = ifelse(dem_sexe == "a_homme",-value,value),
           source = paste0("rpi",sourceRp)) %>% 
    rename(age = nomIndicateur,
           sexe = dem_sexe,
           pop = value)
  
  #----------------------------------------------------------------------------------------------------------------------------------------------#
  #                                             VII. Listes de tableaux finaux                                                                   #
  #----------------------------------------------------------------------------------------------------------------------------------------------#
  list_tab <- list(indicateur_stat = indicateur_stat,pyramide_tr = pyramide_tr)
  
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







