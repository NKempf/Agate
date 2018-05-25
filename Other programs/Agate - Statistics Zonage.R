#--------------------------------------------------------------------------------------------------------------------------------#
#                 AppliShiny - Fonction calcul de statistiques selon un zonage                                                   #
#--------------------------------------------------------------------------------------------------------------------------------#

# MAJ : 25.05.2018

t1 <- Sys.time()

# Nicolas Kempf

# Ce programme calcule les statistiques issues du RP, de filosofi et prochainement : de Fideli et de la BPE. 

### Working directory 
# Paramètre automatiquement le dossier dans lequel se trouve le programme comme espace de travail
# Ne fonction que pour une version de R supérieure à 3.3
setwd(dir=dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# save(rpi,rpl,filo,file="../../Bdd/RData/Temp/rpTidyStat.RData")
load(file="../../Bdd/RData/Temp/rpTidyStat.RData")

load("../../Bdd/RData/Ancien/statTmp.RData")

#----------------------------------------------------------------------------------------------------------------------------------------------#
#                                             I. Statistiques issues du RP                                                                     #
#----------------------------------------------------------------------------------------------------------------------------------------------#

lstCom <- unique(rpi$com[!is.na(rpi$idZonage)])

# RP individu
rpi <- as_tibble(rpi) %>%
  filter(com %in% lstCom) %>%
  mutate(idZonage = ifelse(is.na(idZonage),"Hors zonage", idZonage))

# Rp logement
rpl <- as_tibble(rpl) %>%
  filter(com %in% lstCom) %>%
  mutate(idZonage = ifelse(is.na(idZonage),"Hors zonage", idZonage))

# Filosofi
filo <- as_tibble(filo) %>% 
  filter(depcom %in% lstCom) %>% 
  mutate(idZonage = ifelse(is.na(idZonage),"Hors zonage", idZonage),
         quantile = 1)

colnames(filo)

# I. Statistiques de cadrage
#-----------------------------------------------------------------------------------------------------------------------------------------------

# I.1. Population communale, zonage et hors zonage par commune
#-------------------------------------------------------------
tmp <- rpi %>%
  group_by(com) %>%
  summarise(pop_np = n(),
            pop_p = round(sum(IPONDI,na.rm = TRUE),0))
tmp2 <- rpi %>%
  filter(idZonage != "Hors zonage") %>%
  group_by(com) %>%
  summarise(popZonage_np = n(),
            popZonage_p = round(sum(IPONDI,na.rm = TRUE),0))
tRp.I.1 <- left_join(tmp,tmp2) %>%
  mutate(popHZonage_np = pop_np - popZonage_np,
         popHZonage_p = pop_p - popZonage_p) %>%
  filter(!is.na(popZonage_np))
rm(tmp,tmp2)

# I.2. Population par zonage
#---------------------------
tRp.I.2 <- rpi %>%
  # filter(!is.na(idZonage)) %>%
  group_by(com,idZonage) %>%
  summarise(popZonage_np = n(),
            popZonage_p = round(sum(IPONDI,na.rm = TRUE),0))

# II. Description de la population
#-----------------------------------------------------------------------------------------------------------------------------------------------

# II.1. Part des moins de 20 ans et des 20 à 64 ans
#--------------------------------------------------
tRp.II.1 <- rpi %>%
  mutate(age = cut(as.numeric(rpi$AGEREV),
                   breaks = c(0,20,65,120),
                   include.lowest = TRUE,
                   right = FALSE)) %>%
  group_by(com,idZonage,age) %>%
  summarise(freq = n(),
            freq_p = round(sum(IPONDI,na.rm = TRUE),0)) %>%
  mutate(part_p = round(100*freq_p / sum(freq_p),2))

# II.2. Part des 60 ans et plus
#------------------------------
tRp.II.2 <- rpi %>%
  mutate(age = cut(as.numeric(rpi$AGEREV),
                   breaks = c(0,20,60,120),
                   include.lowest = TRUE,
                   right = FALSE)) %>%
  group_by(com,idZonage,age) %>%
  summarise(freq = n(),
            freq_p = round(sum(IPONDI,na.rm = TRUE),0)) %>%
  mutate(part_p = round(100*freq_p / sum(freq_p),2))

# II.3. Part des femmes
#----------------------
tRp.II.3 <- rpi %>%
  group_by(com,idZonage,SEXE) %>%
  summarise(freq = n(),
            freq_p = round(sum(IPONDI,na.rm = TRUE),0)) %>%
  mutate(part_p = round(100*freq_p / sum(freq_p),2))

# II.4. Pyramide des ages par sexe et par zonage
#-----------------------------------------------
tRp.II.4 <- rpi %>%
  group_by(com,idZonage,SEXE,AGEREV) %>%
  summarise(freq = n(),
            freq_p = round(sum(IPONDI,na.rm = TRUE),0)) %>%
  mutate(part_p = round(100*freq_p / sum(freq_p),2))

# II.5. Pyramide par sexe, zonage et tranche d'age
#------------------------------------------------
tRp.II.5 <- rpi %>%
  mutate(age = cut(as.numeric(rpi$AGEREV),
                   breaks = c(seq(0,90,2),120),
                   include.lowest = TRUE,
                   right = FALSE)) %>%
  group_by(com,idZonage,SEXE,age) %>%
  summarise(freq = n(),
            freq_p = round(sum(IPONDI,na.rm = TRUE),0)) %>%
  mutate(part_p = round(100*freq_p / sum(freq_p),2))

# III. Scolarisation
#-----------------------------------------------------------------------------------------------------------------------------------------------

# III.1. Taux de scolarisation des 2 à 5 ans et des 18 à 25 ans
#--------------------------------------------------------------
tRp.III.1 <- rpi %>%
  mutate(age = cut(as.numeric(rpi$AGEREV),
                   breaks = c(0,2,6,18,25,120),
                   include.lowest = TRUE,
                   right = FALSE)) %>%
  group_by(com,age,idZonage,ETUD) %>%
  summarise(freq = n(),
            freq_p = round(sum(IPONDI,na.rm = TRUE),0)) %>%
  mutate(part_p = round(100*freq_p / sum(freq_p),2)) %>%
  filter(age %in% c("[2,6)","[18,25)"))

# III.2. Part de non scolarisation de plus de 15 ans sans diplome
#----------------------------------------------------------------
tRp.III.2 <- rpi %>%
  filter(as.numeric(AGEREV) >= 15 & DIPL == "A") %>% 
  mutate(nonScola = ifelse(ETUD == "n_etudi","nScola_15plus","autre")) %>%
  group_by(com,idZonage,nonScola) %>% 
  summarise(freq = n(),
            freq_p = round(sum(IPONDI,na.rm = TRUE),0)) %>%
  mutate(part_p = round(100*freq_p / sum(freq_p),2))

# III.3. Taux de non scolarisation des 6 - 14 ans
#-----------------------------------------------
tRp.III.3 <- rpi %>%
  filter(as.numeric(AGEREV) %in% c(6:14)) %>%  
  group_by(com,idZonage,ETUD) %>% 
  summarise(freq = n(),
            freq_p = round(sum(IPONDI,na.rm = TRUE),0)) %>%
  mutate(part_p = round(100*freq_p / sum(freq_p),2))

# III.4. Taux de décrocheur : jeune non scolarisé de 16 - 25 ans sans diplome
#----------------------------------------------------------------------------
tRp.III.4 <- rpi %>%
  filter(as.numeric(AGEREV) %in% c(16:25)) %>%  
  mutate(decrocheur = ifelse(DIPL == "A" & ETUD == "n_etudi","decrocheur","n_decrocheur")) %>% 
  group_by(com,idZonage,decrocheur) %>% 
  summarise(freq = n(),
            freq_p = round(sum(IPONDI,na.rm = TRUE),0)) %>%
  mutate(part_p = round(100*freq_p / sum(freq_p),2))

# IV. Emploi
#-----------------------------------------------------------------------------------------------------------------------------------------------

# IV.1. Taux de chomage pour les 15 - 64 ans (parmi les actifs)
#--------------------------------------------------------------
tRp.IV.1 <- rpi %>%
  filter(as.numeric(AGEREV) %in% c(16:64) & TACT %in% c("actifocc","chomeur")) %>%
  group_by(com,idZonage,TACT) %>% 
  summarise(freq = n(),
            freq_p = round(sum(IPONDI,na.rm = TRUE),0)) %>%
  mutate(part_p = round(100*freq_p / sum(freq_p),2))

# IV.2. Taux d'actif  - d'inactif pour les 15 - 64 ans
#-----------------------------------------------------
tRp.IV.2 <- rpi %>%
  filter(as.numeric(AGEREV) %in% c(16:64)) %>%
  group_by(com,idZonage,inactif) %>% 
  summarise(freq = n(),
            freq_p = round(sum(IPONDI,na.rm = TRUE),0)) %>%
  mutate(part_p = round(100*freq_p / sum(freq_p),2))

# IV.3. Taux d'actif selon le sexe pour les 15 - 64 ans
#------------------------------------------------------
tRp.IV.3 <- rpi %>%
  filter(as.numeric(AGEREV) %in% c(16:64)) %>%
  group_by(com,idZonage,SEXE,inactif) %>% 
  summarise(freq = n(),
            freq_p = round(sum(IPONDI,na.rm = TRUE),0)) %>%
  mutate(part_p = round(100*freq_p / sum(freq_p),2))

# IV.4. Part des actifs de 15-64 ans cadres et professions intermédiaire
#-----------------------------------------------------------------------
tRp.IV.4 <- rpi %>%
  filter(as.numeric(AGEREV) %in% c(16:64) & TACT %in% c("actifocc","chomeur")) %>%
  mutate(profInter = ifelse(POSP %in% c("1J","1I","1H","1G","1F"),"cadre_prof_inter","autre")) %>%
  group_by(com,idZonage,profInter) %>% 
  summarise(freq = n(),
            freq_p = round(sum(IPONDI,na.rm = TRUE),0)) %>%
  mutate(part_p = round(100*freq_p / sum(freq_p),2))

# V. Emploi
#-----------------------------------------------------------------------------------------------------------------------------------------------

# V.1. Part des etrangers
#------------------------
tRp.V.1 <- rpi %>%
  group_by(com,idZonage,etranger) %>% 
  summarise(freq = n(),
            freq_p = round(sum(IPONDI,na.rm = TRUE),0)) %>%
  mutate(part_p = round(100*freq_p / sum(freq_p),2))

# V.1. Part des immigres
#-----------------------
tRp.V.2 <- rpi %>%
  group_by(com,idZonage,immigre) %>% 
  summarise(freq = n(),
            freq_p = round(sum(IPONDI,na.rm = TRUE),0)) %>%
  mutate(part_p = round(100*freq_p / sum(freq_p),2))

# VI. Logements
#-----------------------------------------------------------------------------------------------------------------------------------------------

# VI.1. Part de logements selon la catégorie de logement
#-------------------------------------------------------
tRp.VI.1 <- rpl %>%
  group_by(com,idZonage,catLog) %>% 
  summarise(freq = n(),
            freq_p = round(sum(IPONDL,na.rm = TRUE),0)) %>%
  mutate(part_p = round(100*freq_p / sum(freq_p),2))

# VI.2. Part des appartements
#----------------------------
tRp.VI.2 <- rpl %>%
  group_by(com,idZonage,appartement) %>% 
  summarise(freq = n(),
            freq_p = round(sum(IPONDL,na.rm = TRUE),0)) %>%
  mutate(part_p = round(100*freq_p / sum(freq_p),2))

# VI.3. Part des locataires
#--------------------------
tRp.VI.3 <- rpl %>%
  group_by(com,idZonage,locataire) %>% 
  summarise(freq = n(),
            freq_p = round(sum(IPONDL,na.rm = TRUE),0)) %>%
  mutate(part_p = round(100*freq_p / sum(freq_p),2))


# VI.4. Part des locatairesHlm
#-----------------------------
tRp.VI.4 <- rpl %>%
  group_by(com,idZonage,locataireHlm) %>% 
  summarise(freq = n(),
            freq_p = round(sum(IPONDL,na.rm = TRUE),0)) %>%
  mutate(part_p = round(100*freq_p / sum(freq_p),2))


# VII. Résidences principales
#-----------------------------------------------------------------------------------------------------------------------------------------------

# VII.1. Part des résidences principales de plus de 100m²
#--------------------------------------------------------
tRp.VII.1 <- rpl %>%
  filter(CATL == "1")
  group_by(com,idZonage,surf100) %>% 
  summarise(freq = n(),
            freq_p = round(sum(IPONDL,na.rm = TRUE),0)) %>%
  mutate(part_p = round(100*freq_p / sum(freq_p),2))
  
# VII.2. Part des résidences principales en HLM
#----------------------------------------------
tRp.VII.2 <- rpl %>%
    filter(CATL == "1")
  group_by(com,idZonage,hlm) %>% 
    summarise(freq = n(),
              freq_p = round(sum(IPONDL,na.rm = TRUE),0)) %>%
    mutate(part_p = round(100*freq_p / sum(freq_p),2))

# VII.3. Part des résidences principales sans eau chaude
#-------------------------------------------------------  
tRp.VII.3 <- rpl %>%
  filter(CATL == "1" & EAU %in% c("1","2","3")) %>% 
  mutate(eauchaude = ifelse(EAU =="2","Eau_chaude","N_eau_chaude")) %>% 
  group_by(com,idZonage,eauchaude) %>% 
  summarise(freq = n(),
              freq_p = round(sum(IPONDL,na.rm = TRUE),0)) %>%
  mutate(part_p = round(100*freq_p / sum(freq_p),2))
  
# VII.4. Part des résidences principales sans bain ni douche 
#-----------------------------------------------------------
tRp.VII.4 <- rpl %>%
  filter(CATL == "1" & BAIN %in% c("1","2")) %>% 
  mutate(douche = ifelse(BAIN == "1","bain_douche","N_bain_douche")) %>% 
  group_by(com,idZonage,douche) %>% 
  summarise(freq = n(),
            freq_p = round(sum(IPONDL,na.rm = TRUE),0)) %>%
  mutate(part_p = round(100*freq_p / sum(freq_p),2))

# VII.5. Part des résidences principales sans tout a l'egout
#-----------------------------------------------------------
tRp.VII.5 <- rpl %>%
  filter(CATL == "1" & EGOUL %in% c("1","2","3","4")) %>% 
  mutate(egout = ifelse(EGOUL == "1","tout_egout","N_tout_egout")) %>% 
  group_by(com,idZonage,egout) %>% 
  summarise(freq = n(),
            freq_p = round(sum(IPONDL,na.rm = TRUE),0)) %>%
  mutate(part_p = round(100*freq_p / sum(freq_p),2))

#----------------------------------------------------------------------------------------------------------------------------------------------#
#                                             II. Statistiques issues de Filosofi                                                              #
#----------------------------------------------------------------------------------------------------------------------------------------------#

proba <- c(0.01,0.05,0.1,0.2,0.25,0.5,0.75,0.80,0.90,0.95,0.99)

# I. Statistiques du niveau de vie
#-----------------------------------------------------------------------------------------------------------------------------------------------

# I.1. Distribution du niveau de vie selon le zonage
#---------------------------------------------------
tFilo.I.1 <- filo %>% 
  group_by(idZonage) %>% 
  do(data.frame(Q=proba, stat=quantile(.$nivviem, probs=proba), 
                n = length(.$nivviem), avg = mean(.$nivviem),min=min(.$nivviem),max=max(.$nivviem))) %>%
  spread(Q, stat,sep = "_")

