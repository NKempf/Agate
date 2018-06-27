#--------------------------------------------------------------------------------------------------------------------------------#
#                 AppliShiny - Fonction calcul de statistiques selon un zonage                                                   #
#--------------------------------------------------------------------------------------------------------------------------------#

# MAJ : 26.06.2018

# Use Dplyr language into functions : https://dplyr.tidyverse.org/articles/programming.html#different-data-sets

# Nicolas Kempf

statistics_zone <- function(rpi,rpl,filo,group_var){
  
  
#----------------------------------------------------------------------------------------------------------------------------------------------#
#                                             A. Recensement de la population                                                                  #
#----------------------------------------------------------------------------------------------------------------------------------------------#
  
# I. Statistiques de cadrage
#-----------------------------------------------------------------------------------------------------------------------------------------------
  
  # I.1. Population communale, zonage et hors zonage par commune
  #-------------------------------------------------------------
  tRp.I.1 <- NULL
  
  if(!is.null(rpi$idZonage)){
    tmp <- rpi %>%
      group_by(!!! syms(group_var)[1]) %>% # More details : see the link below
      summarise(pop_np = n(),
                pop_p = round(sum(IPONDI,na.rm = TRUE),0))
    tmp2 <- rpi %>%
      filter(idZonage != "Hors zonage") %>%
      group_by(!!! syms(group_var)[1]) %>% 
      summarise(popZonage_np = n(),
                popZonage_p = round(sum(IPONDI,na.rm = TRUE),0))
    tRp.I.1 <- left_join(tmp,tmp2) %>%
      mutate(popHZonage_np = pop_np - popZonage_np,
             popHZonage_p = pop_p - popZonage_p) %>%
      filter(!is.na(popZonage_np))
    rm(tmp,tmp2)
  }

  
# I.2. Population par zonage
#---------------------------
  tRp.I.2 <- rpi %>%
    # filter(!is.na(zone)) %>%
    group_by(!!! syms(group_var)) %>% 
    summarise(popZonage_np = n(),
              popZonage_p = round(sum(IPONDI,na.rm = TRUE),0))

# II. Description de la population
#-----------------------------------------------------------------------------------------------------------------------------------------------
  
  # II.1. Part des moins de 20 ans et des 20 à 64 ans
  #--------------------------------------------------
  tRp.II.1 <- rpi %>%
    mutate(age = cut(as.numeric(AGEREV),
                     breaks = c(0,20,65,120),
                     include.lowest = TRUE,
                     right = FALSE)) %>%
    group_by(!!! syms(group_var),age) %>%
    weighted_frequency(IPONDI)
  
  # II.2. Part des 60 ans et plus
  #------------------------------
  tRp.II.2 <- rpi %>%
    mutate(age = cut(as.numeric(rpi$AGEREV),
                     breaks = c(0,20,60,120),
                     include.lowest = TRUE,
                     right = FALSE)) %>%
    group_by(!!! syms(group_var),age) %>%
    weighted_frequency(IPONDI)
  
  # II.3. Part des femmes
  #----------------------
  tRp.II.3 <- rpi %>%
    group_by(!!! syms(group_var),SEXE) %>%
    weighted_frequency(IPONDI)
  
  # II.4. Pyramide des ages par sexe et par zonage
  #-----------------------------------------------
  tRp.II.4 <- rpi %>%
    group_by(!!! syms(group_var),SEXE,AGEREV) %>%
    weighted_frequency(IPONDI)
  
  # II.5. Pyramide par sexe, zonage et tranche d'age
  #------------------------------------------------
  tRp.II.5 <- rpi %>%
    mutate(age = cut(as.numeric(rpi$AGEREV),
                     breaks = c(seq(0,90,2),120),
                     include.lowest = TRUE,
                     right = FALSE)) %>%
    group_by(!!! syms(group_var),SEXE,age) %>%
    weighted_frequency(IPONDI)
  
# III. Scolarisation
#-----------------------------------------------------------------------------------------------------------------------------------------------
  
  # III.1. Taux de scolarisation des 2 à 5 ans et des 18 à 25 ans
  #--------------------------------------------------------------
  tRp.III.1 <- rpi %>%
    mutate(age = cut(as.numeric(rpi$AGEREV),
                     breaks = c(0,2,6,18,25,120),
                     include.lowest = TRUE,
                     right = FALSE)) %>%
    group_by(!!! syms(group_var),age,ETUD) %>%
    weighted_frequency(IPONDI) %>%
    filter(age %in% c("[2,6)","[18,25)"))
  
  # III.2. Part de non scolarisation de plus de 15 ans sans diplome
  #----------------------------------------------------------------
  tRp.III.2 <- rpi %>%
    filter(as.numeric(AGEREV) >= 15 & DIPL == "A") %>% 
    mutate(nonScola = ifelse(ETUD == "n_etudi","nScola_15plus","autre")) %>%
    group_by(!!! syms(group_var),nonScola) %>% 
    weighted_frequency(IPONDI)
  
  # III.3. Taux de non scolarisation des 6 - 14 ans
  #-----------------------------------------------
  tRp.III.3 <- rpi %>%
    filter(as.numeric(AGEREV) %in% c(6:14)) %>%  
    group_by(!!! syms(group_var),ETUD) %>% 
    weighted_frequency(IPONDI)
  
  # III.4. Taux de décrocheur : jeune non scolarisé de 16 - 25 ans sans diplome
  #----------------------------------------------------------------------------
  tRp.III.4 <- rpi %>%
    filter(as.numeric(AGEREV) %in% c(16:25)) %>%  
    mutate(decrocheur = ifelse(DIPL == "A" & ETUD == "n_etudi","decrocheur","n_decrocheur")) %>% 
    group_by(!!! syms(group_var),decrocheur) %>% 
    weighted_frequency(IPONDI)

# IV. Emploi
#-----------------------------------------------------------------------------------------------------------------------------------------------
  
  # IV.1. Taux de chomage pour les 15 - 64 ans (parmi les actifs)
  #--------------------------------------------------------------
  tRp.IV.1 <- rpi %>%
    filter(as.numeric(AGEREV) %in% c(16:64) & TACT %in% c("actifocc","chomeur")) %>%
    group_by(!!! syms(group_var),TACT) %>% 
    weighted_frequency(IPONDI)
  
  # IV.2. Taux d'actif  - d'inactif pour les 15 - 64 ans
  #-----------------------------------------------------
  tRp.IV.2 <- rpi %>%
    filter(as.numeric(AGEREV) %in% c(16:64)) %>%
    group_by(!!! syms(group_var),inactif) %>% 
    weighted_frequency(IPONDI)
  
  # IV.3. Taux d'actif selon le sexe pour les 15 - 64 ans
  #------------------------------------------------------
  tRp.IV.3 <- rpi %>%
    filter(as.numeric(AGEREV) %in% c(16:64)) %>%
    group_by(!!! syms(group_var),SEXE,inactif) %>% 
    weighted_frequency(IPONDI)
  
  # IV.4. Part des actifs de 15-64 ans cadres et professions intermédiaire
  #-----------------------------------------------------------------------
  tRp.IV.4 <- rpi %>%
    filter(as.numeric(AGEREV) %in% c(16:64) & TACT %in% c("actifocc","chomeur")) %>%
    mutate(profInter = ifelse(POSP %in% c("1J","1I","1H","1G","1F"),"cadre_prof_inter","autre")) %>%
    group_by(!!! syms(group_var),profInter) %>% 
    weighted_frequency(IPONDI)
  
# V. Immigration
#-----------------------------------------------------------------------------------------------------------------------------------------------
  
  # V.1. Part des etrangers
  #------------------------
  tRp.V.1 <- rpi %>%
    group_by(!!! syms(group_var),etranger) %>% 
    weighted_frequency(IPONDI)
  
  # V.1. Part des immigres
  #-----------------------
  tRp.V.2 <- rpi %>%
    group_by(!!! syms(group_var),immigre) %>% 
    weighted_frequency(IPONDI)
  
# VI. Logements
#-----------------------------------------------------------------------------------------------------------------------------------------------
  
  # VI.1. Part de logements selon la catégorie de logement
  #-------------------------------------------------------
  tRp.VI.1 <- rpl %>%
    group_by(!!! syms(group_var),catLog) %>% 
    weighted_frequency(IPONDL)
  
  # VI.2. Part des appartements
  #----------------------------
  tRp.VI.2 <- rpl %>%
    group_by(!!! syms(group_var),appartement) %>% 
    weighted_frequency(IPONDL)
  
  # VI.3. Part des locataires
  #--------------------------
  tRp.VI.3 <- rpl %>%
    group_by(!!! syms(group_var),locataire) %>% 
    weighted_frequency(IPONDL)
  
  
  # VI.4. Part des locatairesHlm
  #-----------------------------
  tRp.VI.4 <- rpl %>%
    group_by(!!! syms(group_var),locataireHlm) %>% 
    weighted_frequency(IPONDL)
  
# VII. Résidences principales
#-----------------------------------------------------------------------------------------------------------------------------------------------
  
  # VII.1. Part des résidences principales de plus de 100m²
  #--------------------------------------------------------
  tRp.VII.1 <- rpl %>%
    filter(CATL == "1") %>% 
    group_by(!!! syms(group_var),surf100) %>% 
    weighted_frequency(IPONDL)
  
  # VII.2. Part des résidences principales en HLM
  #----------------------------------------------
  tRp.VII.2 <- rpl %>%
    filter(CATL == "1") %>% 
    group_by(!!! syms(group_var),hlm) %>% 
    weighted_frequency(IPONDL)
  
  # VII.3. Part des résidences principales sans eau chaude
  #-------------------------------------------------------  
  tRp.VII.3 <- rpl %>%
    filter(CATL == "1" & EAU %in% c("1","2","3")) %>% 
    mutate(eauchaude = ifelse(EAU =="2","Eau_chaude","N_eau_chaude")) %>% 
    group_by(!!! syms(group_var),eauchaude) %>% 
    weighted_frequency(IPONDL)
  
  # VII.4. Part des résidences principales sans bain ni douche 
  #-----------------------------------------------------------
  tRp.VII.4 <- rpl %>%
    filter(CATL == "1" & BAIN %in% c("1","2")) %>% 
    mutate(douche = ifelse(BAIN == "1","bain_douche","N_bain_douche")) %>% 
    group_by(!!! syms(group_var),douche) %>% 
    weighted_frequency(IPONDL)
  
  # VII.5. Part des résidences principales sans tout a l'egout
  #-----------------------------------------------------------
  tRp.VII.5 <- rpl %>%
    filter(CATL == "1" & EGOUL %in% c("1","2","3","4")) %>% 
    mutate(egout = ifelse(EGOUL == "1","tout_egout","N_tout_egout")) %>% 
    group_by(!!! syms(group_var),egout) %>% 
    weighted_frequency(IPONDL)
  
#----------------------------------------------------------------------------------------------------------------------------------------------#
#                                             II. Statistiques issues de Filosofi                                                              #
#----------------------------------------------------------------------------------------------------------------------------------------------#
  
  proba <- c(0.01,0.05,0.1,0.2,0.25,0.5,0.75,0.80,0.90,0.95,0.99)
  
  # I. Statistiques du niveau de vie
  #-----------------------------------------------------------------------------------------------------------------------------------------------
  
  # I.1. Distribution du niveau de vie selon le zonage
  #---------------------------------------------------
  tFilo.I.1 <- filo %>% 
    group_by(!!! syms(group_var)) %>% 
    income_distrib("nivviem",proba)
  
  # I.2. Distribution du niveau de vie selon le type de ménage et le zonage
  #------------------------------------------------------------------------
  tFilo.I.2 <- filo %>% 
    group_by(!!! syms(group_var),typmenR.lib) %>% 
    income_distrib("nivviem",proba)
  
  # I.3. Part des ménages selon le type de ménage et le zonage
  #-----------------------------------------------------------
  tFilo.I.3 <- filo %>%
    group_by(!!! syms(group_var),typmenR.lib) %>% 
    weighted_frequency(nbpersm) %>% 
    rename(freq.men = freq,
           freq.ind = freq_p,
           part.men = part_np,
           part.ind = part_p)
  
  # I.4. Part des ménages selon le décile de niveau de vie métro et le zonage
  #--------------------------------------------------------------------------
  tFilo.I.4 <- filo %>%
    group_by(!!! syms(group_var),decile_nivvie) %>% 
    weighted_frequency(nbpersm) %>% 
    rename(freq.men = freq,
           freq.ind = freq_p,
           part.men = part_np,
           part.ind = part_p)
  
# II. Pauvreté
#-----------------------------------------------------------------------------------------------------------------------------------------------
  seuil_pauv.dfa <- filo %>%
    group_by(dep) %>% 
    summarise(seuil_pauv.dfa = median(nivviem))
  
  # II.1 Taux de pauvreté selon le seuil métropolitain et départemental
  #--------------------------------------------------------------------
  tFilo.II.1 <- filo %>%
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
           tx_pauv40.ind.dfa = round(100 * freq.pauv40.ind.dfa / freq.ind,2))
  
  # II.2 Courbe de lorenz
  #----------------------
  prob <- seq(0,1,by = 0.01)
  
  tFilo.II.2 <- filo %>% 
    group_by(!!! syms(group_var)) %>% 
    do(data.frame(Q=prob, stat=quantile(.$nivviem, probs=prob))) %>%
    mutate(stat = ifelse(stat<0,0,stat)) 
  
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
#                                             VI. Synthèse des statistiques                                                                    #
#----------------------------------------------------------------------------------------------------------------------------------------------#
  
  # VI.1 Synthèse des indicateurs au niveau zonage
  #------------------------------------------------
  
  # VI.1. 
  # tSynth.I.1 <- tRp.II.1 %>% 
  #   select(-freq,-freq_p,-part_np) %>% 
  #   spread(age,part_p)
  # 
  # tSynth.I.2 <- tRp.II.2 %>% 
  #   select(-freq,-freq_p,-part_np) %>% 
  #   spread(age,part_p)  
  # 
  # tSynth.I.3 <- tRp.II.3 %>% 
  #   select(-freq,-freq_p,-part_np) %>% 
  #   spread(SEXE,part_p) 
  # 
  # tSynth.I.4 <- tRp.II.4 %>% 
  #   select(-freq,-freq_p,-part_np) %>% 
  #   spread(SEXE,part_p)
  
  
  # VI.2. Libellé des tableaux
  #---------------------------
  # Libellé des tableaux
  tab_lib <- c(tRp.I.1 = "Population communale",
               tRp.I.2 = "Population par zone",
               tRp.II.1 = "Part des moins de 20 ans et des 20 à 64 ans",
               tRp.II.2 = "Part des 60 ans et plus",
               tRp.II.3 = "Part des femmes",
               tRp.II.4 = "Pyramide des ages par sexe et par zonage",
               tRp.II.5 = "Pyramide par sexe, zonage et tranche d'age",
               tRp.III.1 = "Taux de scolarisation des 2 à 5 ans et des 18 à 25 ans",
               tRp.III.2 = "Part de non scolarisation de plus de 15 ans sans diplome",
               tRp.III.3 = "Taux de non scolarisation des 6 - 14 ans",
               tRp.III.4 = "Taux de décrocheur : jeune non scolarisé de 16 - 25 ans sans diplome",
               tRp.IV.1 = "Taux de chomage pour les 15 - 64 ans (parmi les actifs)",
               tRp.IV.2 = "Taux d'actif  - d'inactif pour les 15 - 64 ans",
               tRp.IV.3 = "Taux d'actif selon le sexe pour les 15 - 64 ans", 
               tRp.IV.4 = "Part des actifs de 15-64 ans cadres et professions intermédiaire",
               tRp.V.1 = "Part des etrangers",
               tRp.V.2 = "Part des immigres",
               tRp.VI.1 = "Part de logements selon la catégorie de logement",
               tRp.VI.2 = "Part des appartements",
               tRp.VI.3 = "Part des locataires",
               tRp.VI.4 = "Part des locatairesHlm",
               tRp.VII.1 = "Part des résidences principales de plus de 100m²",
               tRp.VII.2 = "Part des résidences principales en HLM",
               tRp.VII.3 = "Part des résidences principales sans eau chaude",
               tRp.VII.4 = "Part des résidences principales sans bain ni douche",
               tRp.VII.5 = "Part des résidences principales sans tout a l'egout",
               tFilo.I.1 = "Distribution du niveau de vie selon le zonage",
               tFilo.I.2 = "Distribution du niveau de vie selon le type de ménage et le zonage",
               tFilo.I.3 = "Part des ménages selon le type de ménage et le zonage",
               tFilo.I.4 = "Part des ménages selon le décile de niveau de vie métro et le zonage",
               tFilo.II.1 = "Taux de pauvreté selon le seuil métropolitain et départemental",
               tFilo.II.2 = "Courbe de lorenz"
  )
  
  
#----------------------------------------------------------------------------------------------------------------------------------------------#
#                                             VII. Listes de tableaux finaux                                                                   #
#----------------------------------------------------------------------------------------------------------------------------------------------#
  list_tab <- list(tRp.I.1 = tRp.I.1,
                   tRp.I.2 = tRp.I.2,
                   tRp.II.1 = tRp.II.1,
                   tRp.II.2 = tRp.II.2,
                   tRp.II.3 = tRp.II.3,
                   tRp.II.4 = tRp.II.4,
                   tRp.II.5 = tRp.II.5,
                   tRp.III.1 = tRp.III.1,
                   tRp.III.2 = tRp.III.2,
                   tRp.III.3 = tRp.III.3,
                   tRp.III.4 = tRp.III.4,
                   tRp.IV.1 = tRp.IV.1,
                   tRp.IV.2 = tRp.IV.2,
                   tRp.IV.3 = tRp.IV.3,
                   tRp.IV.4 = tRp.IV.4,
                   tRp.V.1 = tRp.V.1,
                   tRp.V.2 = tRp.V.2,
                   tRp.VI.1 = tRp.VI.1,
                   tRp.VI.2 = tRp.VI.2,
                   tRp.VI.3 = tRp.VI.3,
                   tRp.VI.4 = tRp.VI.4,
                   tRp.VII.1 = tRp.VII.1,
                   tRp.VII.2 = tRp.VII.2,
                   tRp.VII.3 = tRp.VII.3,
                   tRp.VII.4 = tRp.VII.4,
                   tRp.VII.5 = tRp.VII.5,
                   tFilo.I.1 = tFilo.I.1,
                   tFilo.I.2 = tFilo.I.2,
                   tFilo.I.3 = tFilo.I.3,
                   tFilo.I.4 = tFilo.I.4,
                   tFilo.II.1 = tFilo.II.1,
                   tFilo.II.2 = tFilo.II.2,
                   tab_lib = tab_lib
                   )
  
return(list_tab)  
  
} 


# Calcul les fréquences et la part pondérée
weighted_frequency <- function(df,weight){
  poids <- enquo(weight)
  
  df %>% 
  summarise(freq = n(),
            freq_p = round(sum(!! poids,na.rm = TRUE),0)) %>%
  mutate(part_np = round(100*freq / sum(freq),2),
         part_p = round(100*freq_p / sum(freq_p),2)) 
}

# Distribution d'un revenu
income_distrib <- function(df,income,proba){
  # Programming tips about do function : https://www.r-bloggers.com/dplyr-do-some-tips-for-using-and-programming/
  df %>%
    do(data.frame(Q=proba, n = length(.[[income]]), avg = mean(.[[income]]), stat=quantile(.[[income]], probs=proba))) %>% 
    spread(Q, stat,sep = "_") %>%
    mutate(d9_d1 = round(Q_0.9 / Q_0.1,2),
           q4_q1 = round(Q_0.8 / Q_0.2,2),
           d5_d1 = round(Q_0.5 / Q_0.1,2),
           d9_d5 = round(Q_0.9 / Q_0.5,2))
}



