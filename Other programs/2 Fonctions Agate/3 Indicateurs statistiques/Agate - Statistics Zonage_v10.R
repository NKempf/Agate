#--------------------------------------------------------------------------------------------------------------------------------#
#                 Agate - Indicateurs statistiques par zonage                                                                    #
#--------------------------------------------------------------------------------------------------------------------------------#

# MAJ : 25.03.2019 : ajout de nombreux indicateurs

# Use Dplyr language into functions : https://dplyr.tidyverse.org/articles/programming.html#different-data-sets

# Nicolas Kempf

# Fonctions supplémentaires (NK)
source("Other programs/2 Fonctions Agate/2 Cartographie/Agate - Cartographie fct.R",encoding = "UTF-8")
source("Other programs/2 Fonctions Agate/6 Census Quality/Agate - Qualite du RP.R",encoding = "UTF-8")
source("Other programs/2 Fonctions Agate/7 Statistical disclosure/Agate - statistiscal disclosure.R",encoding = "UTF-8")


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
agate_statRp <- function(rp.an,zonage,zone.pred,zoneType = "",group_var,com.dom,rpi.weight,rpl.weight,
                         seuil_qualite_appariement = 30,seuil_diffusion_qualite = 5,seuil_secret_stat=11,secret_stat_agate = FALSE){
  
  
  # Selection des bases du RP
  rpi.path.string <- paste0("Data/Rp/rpi",rp.an,".fst")
  rpl.path.string <- paste0("Data/Rp/rpl",rp.an,".fst")
  rpa.path.string <- paste0("Data/Rp/rpa",rp.an,".fst")
  rpiPath <- ifelse(file.exists(rpi.path.string),rpi.path.string,"Data/Rp/FakeRpi.fst")
  rplPath <- ifelse(file.exists(rpl.path.string),rpl.path.string,"Data/Rp/FakeRpl.fst")
  rpaPath <- ifelse(file.exists(rpa.path.string),rpa.path.string,NA)
  
  #--------------------------------------------------------------------------------------#
  #                                   Cartographie                                       #
  #--------------------------------------------------------------------------------------#
  
  zonage <- spTransform(zonage, "+init=epsg:3857")
  
  # 1. Intersection zone + communes
  zoneInter <- gIntersects(zonage,com.dom,byid = TRUE)
  test <- apply(zoneInter, 1, function(x){
    test <- sum(x)
    return(ifelse(test>0,TRUE,FALSE))}) 
  com.dom.select <- com.dom@data$idZonage[test]
  
  # 2. adresses
  rpa <- read_fst(rpaPath) %>%
    filter(com %in% com.dom.select)
  
  rpa.geo <- rpa %>% 
    filter(!is.na(ril.millesime)) %>% 
    mutate(idx = C_IMM) %>% 
    select(idx,C_IMM,com,x,y,nb_logn.ril)
  
  coordinates(rpa.geo) <- ~x+y
  rpa.geo@proj4string <- CRS("+init=epsg:3857")
  
  # incProgress(amount = 0.1,message = "Zone dans laquelle chaque logement se situe")
  
  # 3. Intersection entre le recensement de la population géolocalisé et les zones d'études
  pts.sp <- zonaPts(pts.sp = rpa.geo,zonage = zonage)
  pts.df <- pts.sp@data %>% 
    mutate(idZonage = ifelse(is.na(idZonage) | idZonage == "Hors zonage",paste0("horsZon",com,zoneType), idZonage)) %>%   
    select(-idx,-com)
  
  # incProgress(amount = 0.2,message = "Ajout de la zone aux données du RP")
  
  rpi <- read_fst(rpiPath) %>% 
    select(-com.lib) %>% 
    filter(C_IMM %in% unique(pts.df$C_IMM)) %>% 
    left_join(pts.df, by = c("C_IMM")) %>% 
    left_join(com.dom@data %>% 
                rename(Codgeo = idZonage,
                       Libgeo = idZonage.name) %>% 
                select(Codgeo,Libgeo), by = c("com"="Codgeo")) %>% 
    mutate(com.lib = Libgeo,
           idZonage = ifelse(is.na(idZonage) | idZonage == "Hors zonage",paste0("horsZon",com,zoneType), idZonage),
           idZonage.name = ifelse(is.na(idZonage.name) & substr(idZonage,1,7) == "horsZon",
                                  paste0(com.lib," (hors zone ",zoneType,")"),idZonage.name))
  
  rpl <- read_fst(rplPath) %>% 
    select(-com.lib) %>% 
    filter(C_IMM %in% unique(pts.df$C_IMM)) %>% 
    left_join(pts.df, by = c("C_IMM")) %>% 
    left_join(com.dom@data %>% 
                rename(Codgeo = idZonage,
                       Libgeo = idZonage.name) %>% 
                select(Codgeo,Libgeo), by = c("com"="Codgeo")) %>% 
    mutate(com.lib = Libgeo,
           idZonage = ifelse(is.na(idZonage) | idZonage == "Hors zonage",paste0("horsZon",com,zoneType), idZonage),
           idZonage.name = ifelse(is.na(idZonage.name) & substr(idZonage,1,7) == "horsZon",
                                  paste0(com.lib," (hors zone ",zoneType,")"),idZonage.name))
  
  # 4. Qualite de l'appariement
  qualiteAppariement <- read_fst("Data/Rp/appariementRil_Rp.fst") %>% 
    filter(com %in% com.dom.select & an == rp.an) %>% 
    mutate(appariement_diff = ifelse(nonApparie.pct < seuil_qualite_appariement,TRUE,FALSE))
  
  zonage.com <- rpl %>% 
    group_by(dep,com,com.lib,idZonage,idZonage.name) %>% 
    summarise(freq=n()) %>% 
    ungroup() %>% 
    left_join(qualiteAppariement,by="com")
  
  #--------------------------------------------------------------------------------------#
  #                                   Statistiques                                       #
  #--------------------------------------------------------------------------------------#

  # incProgress(amount = 0.4,message = "Calcul des statistiques")
  
  # 1. Indicateurs statistiques dans chaque zone et hors des zones
  #---------------------------------------------------------------
  statZone <- statistics_zone(group_var = group_var,zone = zonage,rpi = rpi,rpl = rpl, 
                              lstCategorie = lstCategorie,
                              sourceRp = rp.an,
                              rpi.weight = rpi.weight,
                              rpl.weight = rpl.weight)
  
  df.zone <- statZone$indicateur_stat
  
  # 2. Qualité des données du RP (Travaux Baptiste Raimbaud)
  #---------------------------------------------------------
  # incProgress(amount = 0.5,message = "Qualité des données du rp")
  
  ril <- read_fst("Data/Ril/ril_leger.fst") %>% 
    select(idx,x,y,nb_logn) %>% 
    mutate(com = substr(idx,1,5)) %>%
    filter(com %in% com.dom.select & C_ANNEE_COL %in% unique(rpi$C_ANNEE_COL)) %>% 
    select(-C_ANNEE_COL)
  coordinates(ril) <- ~x+y
  ril@proj4string <- CRS("+init=epsg:3857")
  ril.geo <- ril[!duplicated(ril@data$idx),]
  ril.geo <- zonaPts(pts.sp = ril.geo,zonage = zonage)
  ril <- ril@data %>% 
    left_join(ril.geo@data %>% select(idx,idZonage),by="idx") %>% 
    mutate(idZonage = ifelse(is.na(idZonage) | idZonage == "Hors zonage",paste0("horsZon",com,zoneType), idZonage))
  
  rpa.qualite <- rpa %>% 
    left_join(rpl %>% 
                filter(!duplicated(C_IMM)) %>%
                select(C_IMM,idZonage,idZonage.name), by = c("C_IMM")) %>% 
    filter(ril.millesime == 1) %>% 
    mutate(IPOND = IPOND.cal)
  
  # Indicateurs de qualité (variance, coefficient de variation, intervalle de confiance, etc.)
  group_var.qualite <- c("INPER",lstIndicateur$qualiteIndicateur[paste0(lstIndicateur$nomVariable,lstIndicateur$nomIndicateur) %in% colnames(rpa.qualite) & 
                                                                   lstIndicateur$calculQualite == 1])
  qualityZone <- Qlfinal(rpa.qualite,group_var.qualite,ril = ril) %>% 
    mutate(val.qualite = ifelse(CoefVariation <= seuil_diffusion_qualite & !is.nan(CoefVariation),EstVariable,IntervalConf.)) 
  
  df.zone <- qualityZone %>% 
    rename(idZonage = zonage,
           qualiteIndicateur = Variable) %>% 
    left_join(rpl %>% filter(!duplicated(idZonage)) %>% 
                select(idZonage,idZonage.name),by="idZonage") %>% 
    left_join(lstIndicateur %>% 
                filter(lstIndicateur$calculQualite == 1) %>% 
                select(domaine,categorie,nomVariable,nomIndicateur,qualiteIndicateur,source),
              by = "qualiteIndicateur") %>% # Ajout de variables
    mutate(source = paste0(source,rp.an)) %>% 
    select(-qualiteIndicateur) %>% 
    gather("type.indicateur","value",-group_var,-nomVariable,-nomIndicateur,-domaine,-categorie,-source) %>% # Transformation de la base
    bind_rows(df.zone %>% mutate(value = as.character(value))) # Ajout des données sur la qualité dans un format simple a exploiter
  
  # 3 Secret statistique
  #---------------------
  # incProgress(amount = 0.8,message = "Secret statistique")
  if(secret_stat_agate){
    df.zone.secret <- df.zone %>% 
      filter(type.indicateur %in% c("freq","n"))
    
    var.secret <- lstCategorie$nomVariable[lstCategorie$typeVar == "pct" & substr(lstCategorie$source,1,2) == "rp"]
    
    # Secret statistique
    indicateur.secret <- bind_rows(lapply(var.secret,secret_stat,df.zone.secret = df.zone.secret,seuil_secret_stat = seuil_secret_stat))
    
    df.zone <- df.zone.secret %>% 
      left_join(indicateur.secret,by = c("idZonage","nomVariable","nomIndicateur")) %>% # Ajout de la variable diffusable
      mutate(value = ifelse(is.na(diff.secret),"diffusable",diff.secret),
             type.indicateur = "secret_stat") %>% 
      select(-diff.secret) %>% 
      bind_rows(df.zone)
    
    df.zone <- df.zone %>% 
      filter(type.indicateur %in% c("freq_p","superficie","val.qualite","secret_stat","avg","Q_0.5")) %>% 
      spread(key = type.indicateur, value = value) %>% 
      left_join(lstCategorie %>% select(nomVariable,Arrondi_agate),by=c("nomVariable")) %>% 
      mutate(secret_stat = case_when(is.na(secret_stat) & nomVariable == "superficie" ~ "diffusable",
                                     is.na(secret_stat) & nomVariable != "superficie" ~ "n_diffusable",
                                     TRUE ~ secret_stat),
             valeur.diffusable = case_when(secret_stat == "diffusable" & !is.na(val.qualite) ~ val.qualite,
                                           secret_stat == "diffusable" & is.na(val.qualite) & nomVariable %in% c("log_tot") ~ freq_p,
                                           secret_stat == "diffusable" & is.na(val.qualite) & nomVariable %in% c("superficie") ~ superficie,
                                           secret_stat == "diffusable" & is.na(val.qualite) & nomVariable %in% c("dem_ageDistrib") ~ Q_0.5,
                                           secret_stat == "diffusable" & is.na(val.qualite) & nomVariable %in% c("res_nperslog") ~ avg,
                                           TRUE ~ "c"), # c : données confidencielles
             valeur.diffusable = ifelse(!is.na(as.numeric(valeur.diffusable)),
                                        round(as.numeric(valeur.diffusable),digits = Arrondi_agate),
                                        valeur.diffusable)) %>% 
      select(domaine,categorie,source,group_var,nomVariable,nomIndicateur,valeur.diffusable) %>% 
      gather("type.indicateur","value",-group_var,-nomVariable,-nomIndicateur,-domaine,-categorie,-source) %>% 
      bind_rows(df.zone) %>% 
      mutate(zone.pred = zone.pred)
    
  }else{
    
    print("Secret statistique non appliqué")
    df.zone <- df.zone %>% 
      filter(type.indicateur %in% c("freq_p","superficie","val.qualite","avg","Q_0.5")) %>% 
      spread(key = type.indicateur, value = value) %>% 
      left_join(lstCategorie %>% select(nomVariable,Arrondi_agate),by=c("nomVariable")) %>% 
      mutate(valeur.diffusable = case_when(!is.na(val.qualite) ~ val.qualite,
                                           is.na(val.qualite) & nomVariable %in% c("log_tot") ~ freq_p,
                                           is.na(val.qualite) & nomVariable %in% c("superficie") ~ superficie,
                                           is.na(val.qualite) & nomVariable %in% c("dem_ageDistrib") ~ Q_0.5,
                                           is.na(val.qualite) & nomVariable %in% c("res_nperslog") ~ avg,
                                           TRUE ~ "///"), # c : données confidencielles
             valeur.diffusable = ifelse(!is.na(as.numeric(valeur.diffusable)),
                                        round(as.numeric(valeur.diffusable),digits = Arrondi_agate),
                                        valeur.diffusable)) %>%  
      select(domaine,categorie,source,group_var,nomVariable,nomIndicateur,valeur.diffusable) %>% 
      gather("type.indicateur","value",-group_var,-nomVariable,-nomIndicateur,-domaine,-categorie,-source) %>% 
      bind_rows(df.zone) %>% 
      mutate(zone.pred = zone.pred)
  }

  return(list(df.zone = df.zone,
              pyramide = statZone$pyramide_tr %>% 
                mutate(zone.pred = zone.pred),
              zonage.com = zonage.com))
  
}


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

# statistics_zone <- function(group_var,zone,rpi,rpl,lstCategorie,sourceRp,rpi.weight,rpl.weight){
#   
#   #----------------------------------------------------------------------------------------------------------------------------------------------#
#   #                                             A. Recensement de la population                                                                  #
#   #----------------------------------------------------------------------------------------------------------------------------------------------#
#   
#   # I. Territoire
#   #-----------------------------------------------------------------------------------------------------------------------------------------------
#   # domaine <- 1
# 
#   # I.1. Superficie et densité de population
#   #-----------------------------------------
#   
#   # Superficie en km²
#   zone@data$value <- round(gArea(zone,byid = T)/1000000,1)
#   zone <- zone@data %>% 
#     mutate(type.indicateur = "superficie",
#            nomVariable = "superficie",
#            nomIndicateur = "a_superficie")
# 
#   # II. Individu
#   #-----------------------------------------------------------------------------------------------------------------------------------------------
#   
#   # II.1. population totale
#   #------------------------
#   rpi.popTot <- rpi %>%
#     group_by(!!! syms(group_var)) %>% 
#     summarise(freq = n(),
#               freq_p = round(sum(!!! syms(rpi.weight),na.rm = TRUE),0)) %>% 
#     gather("type.indicateur","value",-group_var) %>% 
#     mutate(nomVariable = "population",
#            nomIndicateur = "a_population") %>% 
#     bind_rows(zone)
#   
#   # II.2. Champ : population totale
#   #--------------------------------
#   
#   # II.2.1. Selection des variables
#   lst_var <- lstCategorie$nomVariable[lstCategorie$source == "rpi" & lstCategorie$typeVar == "pct" & lstCategorie$ssChamp == 0]
#   
#   # II.2.2. Calcul indicateurs
#   rpi.popTot.champ <- bind_rows(lapply(lst_var, agate_qualitative,df = rpi,group_var = group_var, poids = rpi.weight))
#   
#   # II.3. Sous-champs
#   #------------------
#   
#   # II.3.1. Informations sur les champs
#   df_ssChamp <- lstCategorie %>% 
#     filter(source == "rpi" & typeVar == "pct" & ssChamp == 1) %>% 
#     select(nomVariable,variableChamp,modaliteChamp) %>% 
#     mutate(varmod.champ = paste0(variableChamp,modaliteChamp))
#   
#   # II.3.2. Liste des sous-champs à calculer
#   lst_champ <- unique(df_ssChamp$varmod.champ)
#   
#   # II.3.3. Calcul des indicateurs
#   rpi.sschamp <- bind_rows(lapply(lst_champ, agate_qualitative.ssChamp,group_var = group_var,df_ssChamp = df_ssChamp,rp = rpi, poids = rpi.weight))
#   
#   # III. Logements
#   #-----------------------------------------------------------------------------------------------------------------------------------------------
#   
#   # III.1. population totale
#   #------------------------
#   rpl.popTot <- rpl %>%
#     group_by(!!! syms(group_var)) %>% 
#     summarise(freq = n(),
#               freq_p = round(sum(!!! syms(rpl.weight),na.rm = TRUE),0)) %>% 
#     gather("type.indicateur","value",-group_var) %>% 
#     mutate(nomVariable = "log_tot",
#            nomIndicateur = "a_log_tot")
#   
#   # III.2. Champ : Logements
#   #-------------------------
#   # III.2.1. Selection des variables
#   lst_var <- lstCategorie$nomVariable[lstCategorie$variableChamp %in% "X" & lstCategorie$typeVar == "pct" & lstCategorie$ssChamp == 0]
#   
#   # III.2.2. Calcul indicateurs
#   rpl.log.champ <- bind_rows(lapply(lst_var, agate_qualitative,df = rpl,group_var = group_var, poids = rpl.weight))
#   
#   # III.3. Sous-champs
#   #------------------
#   
#   # III.3.1. Informations sur les champs
#   df_ssChamp <- lstCategorie %>% 
#     filter(source == "rpl" & !is.na(modaliteChamp)) %>% 
#     select(nomVariable,variableChamp,modaliteChamp) %>% 
#     mutate(varmod.champ = paste0(variableChamp,modaliteChamp))
#   
#   # III.3.2. Liste des sous-champs à calculer
#   lst_champ <- unique(df_ssChamp$varmod.champ)
#   
#   # III.3.3. Calcul des indicateurs
#   rpl.sschamp <- bind_rows(lapply(lst_champ, agate_qualitative.ssChamp,group_var = group_var,df_ssChamp = df_ssChamp,rp = rpl, poids = rpl.weight))
#   
#   # IV. Ajout à la table finale
#   #-----------------------------------------------------------------------------------------------------------------------------------------------
#   indicateur_stat <- bind_rows(rpi.popTot,rpi.popTot.champ,rpi.sschamp,rpl.popTot,rpl.log.champ,rpl.sschamp) %>% 
#     left_join(lstCategorie %>% select(nomVariable,domaine,categorie,source),by="nomVariable") %>% 
#     mutate(source = paste0(source,sourceRp),
#            value = as.character(value))
#     
#   #----------------------------------------------------------------------------------------------------------------------------------------------#
#   #                                             B.  Objets speciaux                                                                              #
#   #----------------------------------------------------------------------------------------------------------------------------------------------#  
# 
#   # 1. Pyramide par sexe, zonage et tranche d'age (MAJ : 27.03.2019) 
#   #----------------------------------------------------------------
#   pyramide_tr <- bind_rows(lapply(c("a_homme","b_femme"),function(mod.sexe,group_var){
#     rpi %>% 
#       filter(dem_sexe == mod.sexe) %>% 
#       agate_qualitative(df = .,indicateur = "dem_agerevTr",group_var = group_var,poids = rpi.weight) %>% 
#       filter(type.indicateur == "part_p") %>% 
#       mutate(dem_sexe = mod.sexe)
#   },group_var = group_var)) %>% 
#     mutate(value = ifelse(dem_sexe == "a_homme",-value,value),
#            source = paste0("rpi",sourceRp)) %>% 
#     rename(age = nomIndicateur,
#            sexe = dem_sexe,
#            pop = value)
#   
#   #----------------------------------------------------------------------------------------------------------------------------------------------#
#   #                                             VII. Listes de tableaux finaux                                                                   #
#   #----------------------------------------------------------------------------------------------------------------------------------------------#
#   list_tab <- list(indicateur_stat = indicateur_stat,pyramide_tr = pyramide_tr)
#   
#   return(list_tab)  
# } 

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
agate_statRp.shiny <- function(session,rp.an,zonage,zone.pred,zoneType = "",group_var,com.dom,rpi.weight,rpl.weight,
                         seuil_qualite_appariement = 30,seuil_diffusion_qualite = 5,seuil_secret_stat=11,secret_stat=FALSE){
  
  
  # Selection des bases du RP
  rpi.path.string <- paste0("Data/Rp/rpi",rp.an,".fst")
  rpl.path.string <- paste0("Data/Rp/rpl",rp.an,".fst")
  rpa.path.string <- paste0("Data/Rp/rpa",rp.an,".fst")
  rpiPath <- ifelse(file.exists(rpi.path.string),rpi.path.string,"Data/Rp/FakeRpi.fst")
  rplPath <- ifelse(file.exists(rpl.path.string),rpl.path.string,"Data/Rp/FakeRpl.fst")
  rpaPath <- ifelse(file.exists(rpa.path.string),rpa.path.string,NA)
  
  #--------------------------------------------------------------------------------------#
  #                                   Cartographie                                       #
  #--------------------------------------------------------------------------------------#
  
  zonage <- spTransform(zonage, "+init=epsg:3857")
  
  # 1. Intersection zone + communes
  zoneInter <- gIntersects(zonage,com.dom,byid = TRUE)
  test <- apply(zoneInter, 1, function(x){
    test <- sum(x)
    return(ifelse(test>0,TRUE,FALSE))}) 
  com.dom.select <- com.dom@data$idZonage[test]
  
  # 2. adresses
  rpa <- read_fst(rpaPath) %>%
    filter(com %in% com.dom.select)
  
  rpa.geo <- rpa %>% 
    filter(!is.na(ril.millesime)) %>% 
    mutate(idx = C_IMM) %>% 
    select(idx,C_IMM,com,x,y,nb_logn.ril)
  
  coordinates(rpa.geo) <- ~x+y
  rpa.geo@proj4string <- CRS("+init=epsg:3857")
  
  incProgress(amount = 0.01,message = "Zone dans laquelle chaque logement se situe")
  
  # 3. Intersection entre le recensement de la population géolocalisé et les zones d'études
  pts.sp <- zonaPts(pts.sp = rpa.geo,zonage = zonage)
  pts.df <- pts.sp@data %>% 
    mutate(idZonage = ifelse(is.na(idZonage) | idZonage == "Hors zonage",paste0("horsZon",com,zoneType), idZonage)) %>%   
    select(-idx,-com)
  
  incProgress(amount = 0.05,message = "Ajout de la zone aux données du RP")
  
  rpi <- read_fst(rpiPath) %>% 
    select(-com.lib) %>% 
    filter(C_IMM %in% unique(pts.df$C_IMM)) %>% 
    left_join(pts.df, by = c("C_IMM")) %>% 
    left_join(com.dom@data %>% 
                rename(Codgeo = idZonage,
                       Libgeo = idZonage.name) %>% 
                select(Codgeo,Libgeo), by = c("com"="Codgeo")) %>% 
    mutate(com.lib = Libgeo,
           idZonage = ifelse(is.na(idZonage) | idZonage == "Hors zonage",paste0("horsZon",com,zoneType), idZonage),
           idZonage.name = ifelse(is.na(idZonage.name) & substr(idZonage,1,7) == "horsZon",
                                  paste0(com.lib," (hors zone ",zoneType,")"),idZonage.name))
  
  rpl <- read_fst(rplPath) %>% 
    select(-com.lib) %>% 
    filter(C_IMM %in% unique(pts.df$C_IMM)) %>% 
    left_join(pts.df, by = c("C_IMM")) %>% 
    left_join(com.dom@data %>% 
                rename(Codgeo = idZonage,
                       Libgeo = idZonage.name) %>% 
                select(Codgeo,Libgeo), by = c("com"="Codgeo")) %>% 
    mutate(com.lib = Libgeo,
           idZonage = ifelse(is.na(idZonage) | idZonage == "Hors zonage",paste0("horsZon",com,zoneType), idZonage),
           idZonage.name = ifelse(is.na(idZonage.name) & substr(idZonage,1,7) == "horsZon",
                                  paste0(com.lib," (hors zone ",zoneType,")"),idZonage.name))
  
  # 4. Qualite de l'appariement
  qualiteAppariement <- read_fst("Data/Rp/appariementRil_Rp.fst") %>% 
    filter(com %in% com.dom.select & an == rp.an) %>% 
    mutate(appariement_diff = ifelse(nonApparie.pct < seuil_qualite_appariement,TRUE,FALSE))
  
  zonage.com <- rpl %>% 
    group_by(dep,com,com.lib,idZonage,idZonage.name) %>% 
    summarise(freq=n()) %>% 
    ungroup() %>% 
    left_join(qualiteAppariement,by="com")
  
  #--------------------------------------------------------------------------------------#
  #                                   Statistiques                                       #
  #--------------------------------------------------------------------------------------#
  
  incProgress(amount = 0.1,message = "Calcul des statistiques")
  
  # 1. Indicateurs statistiques dans chaque zone et hors des zones
  #---------------------------------------------------------------
  statZone <- statistics_zone(group_var = group_var,zone = zonage,rpi = rpi,rpl = rpl, 
                              lstCategorie = lstCategorie,
                              sourceRp = rp.an,
                              rpi.weight = rpi.weight,
                              rpl.weight = rpl.weight)
  
  df.zone <- statZone$indicateur_stat
  
  # 2. Qualité des données du RP (Travaux Baptiste Raimbaud)
  #---------------------------------------------------------
  incProgress(amount = 0.2,message = "Qualité des données du rp")
  
  ril <- read_fst("Data/Ril/ril_leger.fst") %>% 
    select(idx,x,y,nb_logn) %>% 
    mutate(com = substr(idx,1,5)) %>%
    filter(com %in% com.dom.select & C_ANNEE_COL %in% unique(rpi$C_ANNEE_COL)) %>% 
    select(-C_ANNEE_COL)
  coordinates(ril) <- ~x+y
  ril@proj4string <- CRS("+init=epsg:3857")
  ril.geo <- ril[!duplicated(ril@data$idx),]
  ril.geo <- zonaPts(pts.sp = ril.geo,zonage = zonage)
  ril <- ril@data %>% 
    left_join(ril.geo@data %>% select(idx,idZonage),by="idx") %>% 
    mutate(idZonage = ifelse(is.na(idZonage) | idZonage == "Hors zonage",paste0("horsZon",com,zoneType), idZonage))
  
  rpa.qualite <- rpa %>% 
    left_join(rpl %>% 
                filter(!duplicated(C_IMM)) %>%
                select(C_IMM,idZonage,idZonage.name), by = c("C_IMM")) %>% 
    filter(ril.millesime == 1) %>% 
    mutate(IPOND = IPOND.cal)
  
  # Indicateurs de qualité (variance, coefficient de variation, intervalle de confiance, etc.)
  group_var.qualite <- c("INPER",lstIndicateur$qualiteIndicateur[paste0(lstIndicateur$nomVariable,lstIndicateur$nomIndicateur) %in% colnames(rpa.qualite) & 
                                                                   lstIndicateur$calculQualite == 1])
  qualityZone <- Qlfinal(rpa.qualite,group_var.qualite,ril = ril) %>% 
    mutate(val.qualite = ifelse(CoefVariation <= seuil_diffusion_qualite & !is.nan(CoefVariation),EstVariable,IntervalConf.)) 
  
  df.zone <- qualityZone %>% 
    rename(idZonage = zonage,
           qualiteIndicateur = Variable) %>% 
    left_join(rpl %>% filter(!duplicated(idZonage)) %>% 
                select(idZonage,idZonage.name),by="idZonage") %>% 
    left_join(lstIndicateur %>% 
                filter(lstIndicateur$calculQualite == 1) %>% 
                select(domaine,categorie,nomVariable,nomIndicateur,qualiteIndicateur,source),
              by = "qualiteIndicateur") %>% # Ajout de variables
    mutate(source = paste0(source,rp.an)) %>% 
    select(-qualiteIndicateur) %>% 
    gather("type.indicateur","value",-group_var,-nomVariable,-nomIndicateur,-domaine,-categorie,-source) %>% # Transformation de la base
    bind_rows(df.zone %>% mutate(value = as.character(value))) # Ajout des données sur la qualité dans un format simple a exploiter
  
  # 3 Secret statistique
  #---------------------
  incProgress(amount = 0.5,message = "Secret statistique")
  
  if(secret_stat){
    df.zone.secret <- df.zone %>% 
      filter(type.indicateur %in% c("freq","n"))
    
    var.secret <- lstCategorie$nomVariable[lstCategorie$typeVar == "pct" & substr(lstCategorie$source,1,2) == "rp"]
    
    # Secret statistique
    indicateur.secret <- bind_rows(lapply(var.secret,secret_stat,df.zone.secret = df.zone.secret,seuil_secret_stat = seuil_secret_stat))
    
    df.zone <- df.zone.secret %>% 
      left_join(indicateur.secret,by = c("idZonage","nomVariable","nomIndicateur")) %>% # Ajout de la variable diffusable
      mutate(value = ifelse(is.na(diff.secret),"diffusable",diff.secret),
             type.indicateur = "secret_stat") %>% 
      select(-diff.secret) %>% 
      bind_rows(df.zone)
    
    df.zone <- df.zone %>% 
      filter(type.indicateur %in% c("freq_p","superficie","val.qualite","secret_stat","avg","Q_0.5")) %>% 
      spread(key = type.indicateur, value = value) %>% 
      left_join(lstCategorie %>% select(nomVariable,Arrondi_agate),by=c("nomVariable")) %>% 
      mutate(secret_stat = case_when(is.na(secret_stat) & nomVariable == "superficie" ~ "diffusable",
                                     is.na(secret_stat) & nomVariable != "superficie" ~ "n_diffusable",
                                     TRUE ~ secret_stat),
             valeur.diffusable = case_when(secret_stat == "diffusable" & !is.na(val.qualite) ~ val.qualite,
                                           secret_stat == "diffusable" & is.na(val.qualite) & nomVariable %in% c("log_tot") ~ freq_p,
                                           secret_stat == "diffusable" & is.na(val.qualite) & nomVariable %in% c("superficie") ~ superficie,
                                           secret_stat == "diffusable" & is.na(val.qualite) & nomVariable %in% c("dem_ageDistrib") ~ Q_0.5,
                                           secret_stat == "diffusable" & is.na(val.qualite) & nomVariable %in% c("res_nperslog") ~ avg,
                                           TRUE ~ "c"), # c : données confidencielles
             valeur.diffusable = ifelse(!is.na(as.numeric(valeur.diffusable)),
                                        round(as.numeric(valeur.diffusable),digits = Arrondi_agate),
                                        valeur.diffusable)) %>% 
      select(domaine,categorie,source,group_var,nomVariable,nomIndicateur,valeur.diffusable) %>% 
      gather("type.indicateur","value",-group_var,-nomVariable,-nomIndicateur,-domaine,-categorie,-source) %>% 
      bind_rows(df.zone) %>% 
      mutate(zone.pred = zone.pred)
  }else{
    
    print("Secret statistique non appliqué")
    df.zone <- df.zone %>% 
      filter(type.indicateur %in% c("freq_p","superficie","val.qualite","avg","Q_0.5")) %>% 
      spread(key = type.indicateur, value = value) %>% 
      left_join(lstCategorie %>% select(nomVariable,Arrondi_agate),by=c("nomVariable")) %>% 
      mutate(valeur.diffusable = case_when(!is.na(val.qualite) ~ val.qualite,
                                           is.na(val.qualite) & nomVariable %in% c("log_tot") ~ freq_p,
                                           is.na(val.qualite) & nomVariable %in% c("superficie") ~ superficie,
                                           is.na(val.qualite) & nomVariable %in% c("dem_ageDistrib") ~ Q_0.5,
                                           is.na(val.qualite) & nomVariable %in% c("res_nperslog") ~ avg,
                                           TRUE ~ "///"), # c : données confidencielles
             valeur.diffusable = ifelse(!is.na(as.numeric(valeur.diffusable)),
                                        round(as.numeric(valeur.diffusable),digits = Arrondi_agate),
                                        valeur.diffusable)) %>%  
      select(domaine,categorie,source,group_var,nomVariable,nomIndicateur,valeur.diffusable) %>% 
      gather("type.indicateur","value",-group_var,-nomVariable,-nomIndicateur,-domaine,-categorie,-source) %>% 
      bind_rows(df.zone) %>% 
      mutate(zone.pred = zone.pred)
  }
  
  
  return(list(df.zone = df.zone,
              pyramide = statZone$pyramide_tr %>% 
                mutate(zone.pred = zone.pred),
              zonage.com = zonage.com))
  
}


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

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
  # Distribution de l'age
  rpi.popTot <- rpi %>%
    group_by(!!! syms(group_var)) %>% 
    agate_distrib_weight(x = "AGEREV",weight = rpi.weight) %>% 
    gather("type.indicateur","value",-group_var) %>% 
    mutate(nomVariable = "dem_ageDistrib",
           nomIndicateur = "a_ageMed") %>% 
    bind_rows(rpi.popTot)
  
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
  rpi.sschamp <- bind_rows(lapply(lst_champ, agate_qualitative.ssChamp,group_var = group_var,df_ssChamp = df_ssChamp,rp = rpi, poids = rpi.weight))
  
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
    filter(source == "rpl" & !is.na(modaliteChamp) & !VarCreation %in% c("Agate")) %>% 
    select(nomVariable,variableChamp,modaliteChamp) %>% 
    mutate(varmod.champ = paste0(variableChamp,modaliteChamp))
  
  # III.3.2. Liste des sous-champs à calculer
  lst_champ <- unique(df_ssChamp$varmod.champ)
  
  # III.3.3. Calcul des indicateurs
  rpl.sschamp <- bind_rows(lapply(lst_champ, agate_qualitative.ssChamp,group_var = group_var,df_ssChamp = df_ssChamp,rp = rpl, poids = rpl.weight))
  
  # III.3.4. Distribution de nombre de personnes par logement
  
  rpl.sschamp <- rpl %>%
    filter(log_cat == "a_res_princ") %>% 
    group_by(!!! syms(group_var)) %>% 
    agate_distrib_weight(x = "NPERR",weight = rpl.weight) %>% 
    gather("type.indicateur","value",-group_var) %>% 
    mutate(nomVariable = "res_nperslog",
           nomIndicateur = "a_nbpers_mean") %>% 
    bind_rows(rpl.sschamp)
    
  
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
  pyramide_tr <- bind_rows(lapply(c("a_homme","b_femme"),function(mod.sexe,group_var){
    rpi %>% 
      filter(dem_sexe == mod.sexe) %>% 
      agate_qualitative(df = .,indicateur = "dem_agerevTr",group_var = group_var,poids = rpi.weight) %>% 
      filter(type.indicateur == "part_p") %>% 
      mutate(dem_sexe = mod.sexe)
  },group_var = group_var)) %>% 
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
agate_qualitative.ssChamp <- function(champ,group_var,df_ssChamp,rp, poids){
  
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

agate_distrib_weight <- function(df,x,weight,
                                 proba = c(0.01,0.05,0.1,0.2,0.25,0.5,0.75,0.80,0.90,0.95,0.99)){
  # Programming tips about do function : https://www.r-bloggers.com/dplyr-do-some-tips-for-using-and-programming/
  df <- df %>%
    mutate(x = as.numeric(!!! syms(x)),
           weight = as.numeric(!!! syms(weight))) %>% 
    do(data.frame(Q=proba, n = length(.$x),avg = round(wtd.mean(.$x,weights = .$weight),2), stat=wtd.quantile(.$x,weights = .$weight, probs=proba))) %>% 
    spread(Q, stat,sep = "_") %>% 
    mutate(d9_d1 = round(Q_0.9 / Q_0.1,2),
           q4_q1 = round(Q_0.8 / Q_0.2,2),
           d5_d1 = round(Q_0.5 / Q_0.1,2),
           d9_d5 = round(Q_0.9 / Q_0.5,2))
  
  # Arrondi
  df.select <- colnames(df)[substr(colnames(df),1,2) == "Q_"]
  df[,df.select] <- round(df[,df.select],0)
  
  return(df)
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







