#----------------------------------------------------------------------------------------------------------------------#
#                                   Agata - Statistical calculation improvement                                        #
#----------------------------------------------------------------------------------------------------------------------#

# 21.12.2018

# Improve statistical calculation of indicators

# Packages nécessaires
#---------------------
library(rgdal) # Input shapefile
library(rgeos) # Map tools
library(tidyverse) # Tidy data
library(fst) # Read partial data
library(fstplyr) # dplyr for fst object

source("Other programs/Zonage/Agate - Cartographie fct.R",encoding = "UTF-8")
source("Other programs/StatZonage/Agate - Statistics Zonage_v4.R",encoding = "UTF-8")

load("Data/Tmp/FakeDataImproveCalcul.RData")

# Idee : améliorer le data model. Rassembler un maximum de données dans une seule table avec un moyen d'y acceder
# A faire : gérer les zones à cheval sur plusieurs communes


# Ajout aux données finale
addIndicateurDiscret <- function(df_discret,df_indicateur,group_var,unite){
  # Transformation des données avant ajout
  df_discret %>% 
    select(!!! syms(group_var),domaine,source,categorie,indicateur,freq,part_p) %>% 
    rename(value_p = part_p) %>% 
    mutate(unite = unite) -> df
  # ajout à la table finale
  df_indicateur <- bind_rows(df_indicateur,df)
  return(df_indicateur)
}

majDiscret <- function(df_discret,var,domaine,source,categorie){
  df_discret  %>% 
    rename(indicateur = eval(var)) %>% 
    mutate(domaine = domaine,source = source,categorie = categorie) %>% 
    ungroup() %>% 
    bind_rows(stat_discrete_detail)
}

#----------------------------------------------------------------------------------------------------------------------------------------------#
#                                             A. Recensement de la population                                                                  #
#----------------------------------------------------------------------------------------------------------------------------------------------#

# Paramètre
sourceRpi <- "rpi14"
sourceRpl <- "rpl14"
sourceFilo <- "filo14"

# I. Territoire
#-----------------------------------------------------------------------------------------------------------------------------------------------

# Strategie deux focntions imbriqués :
# Fct la plus basse permet de generer le maximum de calcul simple pour n'importe quelle zone
# Fct agregée permet le calcul spécifique selon le type de zone (infracommunale ou autre)

group_var <- c("com","idZonage")

# I.1. Population par zonage
#---------------------------
indicateur_stat <- rpi %>%
  # filter(!is.na(zone)) %>%
  group_by(!!! syms(group_var)) %>% 
  summarise(freq = n(),
            value_p = round(sum(IPONDI,na.rm = TRUE),0)) %>% 
  mutate(indicateur = "population",
         source = sourceRpi,
         unite = "nb",
         domaine = "territoire",
         categorie = "population")

# II.1. Part des moins de 20 ans et des 20 a 64 ans
#--------------------------------------------------
stat_discrete_detail <- rpi %>%
  mutate(age = cut(as.numeric(AGEREV),
                   breaks = c(0,20,65,120),
                   include.lowest = TRUE,
                   right = FALSE)) %>%
  group_by(!!! syms(group_var),age) %>%
  weighted_frequency(IPONDI) %>% 
  rename(indicateur = age) %>% 
  mutate(domaine = "territoire",source = sourceRpi,categorie = "age") %>% 
  ungroup() 

  # II.2. Part des 60 ans et plus
  #------------------------------
stat_discrete_detail <- rpi %>%
  mutate(age = cut(as.numeric(rpi$AGEREV),
                          breaks = c(0,20,60,120),
                          include.lowest = TRUE,
                          right = FALSE)) %>%
  group_by(!!! syms(group_var),age) %>%
  weighted_frequency(IPONDI) %>% 
  filter(age != "[0,20)") %>% 
  majDiscret(var = "age",domaine = "territoire",source = sourceRpi,categorie = "age")
  
  # II.3. Part des femmes
  #----------------------
stat_discrete_detail <- rpi %>%
  group_by(!!! syms(group_var),SEXE) %>%
  weighted_frequency(IPONDI) %>% 
  majDiscret(var = "SEXE",domaine = "territoire",source = sourceRpi,categorie = "sexe")
  
  # III. Scolarisation
  #-----------------------------------------------------------------------------------------------------------------------------------------------
  
  # III.1. Taux de scolarisation des 2 a 5 ans et des 18 a 25 ans
  #--------------------------------------------------------------
stat_discrete_detail <- rpi %>%
  mutate(age = cut(as.numeric(rpi$AGEREV),
                   breaks = c(0,2,6,18,25,120),
                   include.lowest = TRUE,
                   right = FALSE)) %>%
  group_by(!!! syms(group_var),age,ETUD) %>%
  weighted_frequency(IPONDI) %>%
  filter(age %in% c("[2,6)","[18,25)")) %>% 
  mutate(indicateur = paste0(ETUD,age),
         domaine = "scolarisation",source = "rp14",categorie = "scolarisation") %>% 
  ungroup() %>% 
  select(-age,-ETUD) %>% 
bind_rows(stat_discrete_detail)

  
# III.2. Part de non scolarisation de plus de 15 ans sans diplome
#----------------------------------------------------------------
stat_discrete_detail <- rpi %>%
  filter(as.numeric(AGEREV) >= 15 & DIPL == "A") %>% 
  mutate(indicateur = ifelse(ETUD == "n_etudi","nScola_15plus","autre")) %>%
  group_by(!!! syms(group_var),indicateur) %>% 
  weighted_frequency(IPONDI) %>% 
  majDiscret(var= "indicateur",domaine = "scolarisation", source = sourceRpi, 
             categorie = "non scolarise sans diplome")
  
  # III.3. Taux de non scolarisation des 6 - 14 ans
  #-----------------------------------------------
stat_discrete_detail <- rpi %>%
    filter(as.numeric(AGEREV) %in% c(6:14)) %>%  
    group_by(!!! syms(group_var),ETUD) %>% 
    weighted_frequency(IPONDI) %>% 
  majDiscret(var= "ETUD",domaine = "scolarisation", source = sourceRpi, categorie = "non scolarise [6,14]")

  # III.4. Taux de decrocheur : jeune non scolarise de 16 - 25 ans sans diplome
  #----------------------------------------------------------------------------
stat_discrete_detail <- rpi %>%
    filter(as.numeric(AGEREV) %in% c(16:25)) %>%  
    mutate(indicateur = ifelse(DIPL == "A" & ETUD == "n_etudi","decrocheur","n_decrocheur")) %>% 
    group_by(!!! syms(group_var),indicateur) %>% 
    weighted_frequency(IPONDI) %>% 
  majDiscret(var= "indicateur",domaine = "scolarisation", source = sourceRpi, categorie = "decrocheur")
  
  # IV. Emploi
  #-----------------------------------------------------------------------------------------------------------------------------------------------
  
  # IV.1. Taux de chomage pour les 15 - 64 ans (parmi les actifs)
  #--------------------------------------------------------------
stat_discrete_detail <- rpi %>%
    filter(as.numeric(AGEREV) %in% c(16:64) & TACT %in% c("actifocc","chomeur")) %>%
    group_by(!!! syms(group_var),TACT) %>% 
    weighted_frequency(IPONDI) %>% 
  majDiscret(var= "TACT",domaine = "emploi", source = sourceRpi, categorie = "chomage")
  
  # IV.2. Taux d'actif  - d'inactif pour les 15 - 64 ans
  #-----------------------------------------------------
stat_discrete_detail <- rpi %>%
    filter(as.numeric(AGEREV) %in% c(16:64)) %>%
    group_by(!!! syms(group_var),inactif) %>% 
    weighted_frequency(IPONDI) %>% 
  majDiscret(var= "inactif",domaine = "emploi", source = sourceRpi, categorie = "chomage")
  
  # IV.3. Taux d'actif selon le sexe pour les 15 - 64 ans
  #------------------------------------------------------
stat_discrete_detail <- rpi %>%
    filter(as.numeric(AGEREV) %in% c(16:64)) %>%
    group_by(!!! syms(group_var),SEXE,inactif) %>% 
    weighted_frequency(IPONDI) %>% 
  mutate(indicateur = paste(inactif,SEXE,sep=" "),
         domaine = "emploi",source = "rp14",categorie = "actif par sexe") %>% 
  ungroup() %>% 
  select(-inactif,-SEXE) %>% 
  bind_rows(stat_discrete_detail)
  
  # IV.4. Part des actifs de 15-64 ans cadres et professions intermediaire
  #-----------------------------------------------------------------------
stat_discrete_detail <- rpi %>%
    filter(as.numeric(AGEREV) %in% c(16:64) & TACT %in% c("actifocc","chomeur")) %>%
    mutate(indicateur = ifelse(POSP %in% c("1J","1I","1H","1G","1F"),"cadre_prof_inter","autre")) %>%
    group_by(!!! syms(group_var),indicateur) %>% 
    weighted_frequency(IPONDI)  %>% 
  majDiscret(var= "indicateur",domaine = "emploi", source = sourceRpi, categorie = "cadre et profession inter")
  
  # V. Immigration
  #-----------------------------------------------------------------------------------------------------------------------------------------------
  
  # V.1. Part des etrangers
  #------------------------
stat_discrete_detail <- rpi %>%
    group_by(!!! syms(group_var),etranger) %>% 
    weighted_frequency(IPONDI) %>% 
  majDiscret(var= "etranger",domaine = "immigration", source = sourceRpi, categorie = "etranger")
  
  # V.2. Part des immigres
  #-----------------------
stat_discrete_detail <- rpi %>%
  group_by(!!! syms(group_var),immigre) %>% 
  weighted_frequency(IPONDI) %>% 
  majDiscret(var= "immigre",domaine = "immigration", source = sourceRpi, categorie = "immigre")
  
  # VI. Logements
  #-----------------------------------------------------------------------------------------------------------------------------------------------
  
  # VI.1. Part de logements selon la categorie de logement ( A faire modifier libelle CATL)
  #-------------------------------------------------------
stat_discrete_detail <- rpl %>%
  group_by(!!! syms(group_var),catLog) %>% 
  weighted_frequency(IPONDL) %>% 
  majDiscret(var= "catLog",domaine = "logement", source = sourceRpl, categorie = "categorie logement")
  
  # VI.2. Part des appartements
  #----------------------------
stat_discrete_detail <- rpl %>%
    group_by(!!! syms(group_var),appartement) %>% 
    weighted_frequency(IPONDL) %>% 
  majDiscret(var= "appartement",domaine = "logement", source = sourceRpl, categorie = "appartement")
  
  # VI.3. Part des locataires
  #--------------------------
stat_discrete_detail <- rpl %>%
  group_by(!!! syms(group_var),locataire) %>% 
  weighted_frequency(IPONDL) %>% 
  majDiscret(var= "locataire",domaine = "logement", source = sourceRpl, categorie = "locataire")
  
  # VI.4. Part des locatairesHlm
  #-----------------------------
stat_discrete_detail <- rpl %>%
    group_by(!!! syms(group_var),locataireHlm) %>% 
    weighted_frequency(IPONDL)  %>% 
  majDiscret(var= "locataireHlm",domaine = "logement", source = sourceRpl, categorie = "locataire Hlm")
  
  # VII. Residences principales
  #-----------------------------------------------------------------------------------------------------------------------------------------------
  
  # VII.1. Part des residences principales de plus de 100m²
  #--------------------------------------------------------
stat_discrete_detail <- rpl %>%
    filter(CATL == "1") %>% 
    group_by(!!! syms(group_var),surf100) %>% 
    weighted_frequency(IPONDL) %>% 
  majDiscret(var= "surf100",domaine = "residencePrinc", source = sourceRpl, categorie = "surface logement")
  
  # VII.2. Part des residences principales en HLM
  #----------------------------------------------
stat_discrete_detail <- rpl %>%
    filter(CATL == "1") %>% 
    group_by(!!! syms(group_var),hlm) %>% 
    weighted_frequency(IPONDL) %>% 
  majDiscret(var= "hlm",domaine = "residencePrinc", source = sourceRpl, categorie = "residence principale hlm")
  
  # VII.3. Part des residences principales sans eau chaude
  #-------------------------------------------------------  
stat_discrete_detail <- rpl %>%
    filter(CATL == "1" & EAU %in% c("1","2","3")) %>% 
    mutate(eauchaude = ifelse(EAU =="2","Eau_chaude","N_eau_chaude")) %>% 
    group_by(!!! syms(group_var),eauchaude) %>% 
    weighted_frequency(IPONDL) %>% 
  majDiscret(var= "eauchaude",domaine = "residencePrinc", source = sourceRpl, categorie = "presence eau chaude")
  
  # VII.4. Part des residences principales sans bain ni douche 
  #-----------------------------------------------------------
stat_discrete_detail <- rpl %>%
    filter(CATL == "1" & BAIN %in% c("1","2")) %>% 
    mutate(douche = ifelse(BAIN == "1","bain_douche","N_bain_douche")) %>% 
    group_by(!!! syms(group_var),douche) %>% 
    weighted_frequency(IPONDL) %>% 
  majDiscret(var= "douche",domaine = "residencePrinc", source = sourceRpl, categorie = "presence douche")
  
  # VII.5. Part des residences principales sans tout a l'egout
  #-----------------------------------------------------------
stat_discrete_detail <- rpl %>%
    filter(CATL == "1" & EGOUL %in% c("1","2","3","4")) %>% 
    mutate(egout = ifelse(EGOUL == "1","tout_egout","N_tout_egout")) %>% 
    group_by(!!! syms(group_var),egout) %>% 
    weighted_frequency(IPONDL) %>% 
  majDiscret(var= "egout",domaine = "residencePrinc", source = sourceRpl, categorie = "presence egout")
  

# Ajout au données synthethiques
#-------------------------------
indicateur_stat <- addIndicateurDiscret(df_discret = stat_discrete_detail,
                                        df_indicateur = indicateur_stat,group_var = group_var,unite = "%")


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
  
  # I.2. Distribution du niveau de vie selon le type de menage et le zonage
  #------------------------------------------------------------------------
  tFilo.I.2 <- filo %>% 
    group_by(!!! syms(group_var),typmenR.lib) %>% 
    income_distrib("nivviem",proba)
  
  # I.3. Part des menages selon le type de menage et le zonage
  #-----------------------------------------------------------
  tFilo.I.3 <- filo %>%
    group_by(!!! syms(group_var),typmenR.lib) %>% 
    weighted_frequency(nbpersm) %>% 
    rename(freq.men = freq,
           freq.ind = freq_p,
           part.men = part_np,
           part.ind = part_p)
  
  # I.4. Part des menages selon le decile de niveau de vie metro et le zonage
  #--------------------------------------------------------------------------
  tFilo.I.4 <- filo %>%
    group_by(!!! syms(group_var),decile_nivvie) %>% 
    weighted_frequency(nbpersm) %>% 
    rename(freq.men = freq,
           freq.ind = freq_p,
           part.men = part_np,
           part.ind = part_p)
  
  # II. Pauvrete
  #-----------------------------------------------------------------------------------------------------------------------------------------------
  seuil_pauv.dfa <- filo %>%
    group_by(dep) %>% 
    summarise(seuil_pauv.dfa = median(nivviem))
  
  # II.1 Taux de pauvrete selon le seuil metropolitain et departemental
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
  #                                             VI. Objets speciaux                                                                   #
  #----------------------------------------------------------------------------------------------------------------------------------------------#
  
  # VI.1. Pyramide des ages par sexe et par zonage
  #-----------------------------------------------
  pyramide_detail <- rpi %>%
    group_by(!!! syms(group_var),SEXE,AGEREV) %>%
    weighted_frequency(IPONDI)
  
  # VI.2. Pyramide par sexe, zonage et tranche d'age
  #------------------------------------------------
  pyramide_tr <- rpi %>%
    mutate(age = cut(as.numeric(rpi$AGEREV),
                     breaks = c(seq(0,90,2),120),
                     include.lowest = TRUE,
                     right = FALSE)) %>%
    group_by(!!! syms(group_var),SEXE,age) %>%
    weighted_frequency(IPONDI)
  
  # VI.2. Libelle des tableaux
  #---------------------------
  # Libelle des tableaux
  tab_lib <- c(tRp.I.1 = "Population communale",
               tRp.I.2 = "Population par zone",
               tRp.II.1 = "Part des moins de 20 ans et des 20 a 64 ans",
               tRp.II.2 = "Part des 60 ans et plus",
               tRp.II.3 = "Part des femmes",
               tRp.II.4 = "Pyramide des ages par sexe et par zonage",
               tRp.II.5 = "Pyramide par sexe, zonage et tranche d'age",
               tRp.III.1 = "Taux de scolarisation des 2 a 5 ans et des 18 a 25 ans",
               tRp.III.2 = "Part de non scolarisation de plus de 15 ans sans diplome",
               tRp.III.3 = "Taux de non scolarisation des 6 - 14 ans",
               tRp.III.4 = "Taux de decrocheur : jeune non scolarise de 16 - 25 ans sans diplome",
               tRp.IV.1 = "Taux de chomage pour les 15 - 64 ans (parmi les actifs)",
               tRp.IV.2 = "Taux d'actif  - d'inactif pour les 15 - 64 ans",
               tRp.IV.3 = "Taux d'actif selon le sexe pour les 15 - 64 ans", 
               tRp.IV.4 = "Part des actifs de 15-64 ans cadres et professions intermediaire",
               tRp.V.1 = "Part des etrangers",
               tRp.V.2 = "Part des immigres",
               tRp.VI.1 = "Part de logements selon la categorie de logement",
               tRp.VI.2 = "Part des appartements",
               tRp.VI.3 = "Part des locataires",
               tRp.VI.4 = "Part des locatairesHlm",
               tRp.VII.1 = "Part des residences principales de plus de 100m²",
               tRp.VII.2 = "Part des residences principales en HLM",
               tRp.VII.3 = "Part des residences principales sans eau chaude",
               tRp.VII.4 = "Part des residences principales sans bain ni douche",
               tRp.VII.5 = "Part des residences principales sans tout a l'egout",
               tFilo.I.1 = "Distribution du niveau de vie selon le zonage",
               tFilo.I.2 = "Distribution du niveau de vie selon le type de menage et le zonage",
               tFilo.I.3 = "Part des menages selon le type de menage et le zonage",
               tFilo.I.4 = "Part des menages selon le decile de niveau de vie metro et le zonage",
               tFilo.II.1 = "Taux de pauvrete selon le seuil metropolitain et departemental",
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





