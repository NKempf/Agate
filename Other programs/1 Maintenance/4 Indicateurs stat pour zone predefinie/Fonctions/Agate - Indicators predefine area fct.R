#----------------------------------------------------------------------------------------------------------------------#
#                                   Agate - Statistical calculation for predefine area fonction                        #
#----------------------------------------------------------------------------------------------------------------------#

source("Other programs/2 Fonctions Agate/3 Indicateurs statistiques/Agate - Statistics Zonage_v10.R",encoding = "UTF-8")

# 02.04.2019

indStat_RegCity <- function(rp.an,var.pred,zonage,lstCategorie,lstIndicateur,pred.zone,group_var){
  
  rpi.path.string <- paste0("Data/Rp/rpi",rp.an,".fst")
  rpl.path.string <- paste0("Data/Rp/rpl",rp.an,".fst")
  rpa.path.string <- paste0("Data/Rp/rpa",rp.an,".fst")
  rpiPath <- ifelse(file.exists(rpi.path.string),rpi.path.string,"Data/Rp/FakeRpi.fst")
  rplPath <- ifelse(file.exists(rpl.path.string),rpl.path.string,"Data/Rp/FakeRpl.fst")
  rpaPath <- ifelse(file.exists(rpa.path.string),rpa.path.string,NA)
  
  rpi <- read_fst(rpiPath) %>% 
    mutate(idZonage = !! sym(var.pred)) %>% 
    left_join(zonage@data,by="idZonage")
  
  rpl <- read_fst(rplPath) %>%
    mutate(idZonage =  !! sym(var.pred)) %>% 
    left_join(zonage@data,by="idZonage")

  statZone <- statistics_zone(group_var = group_var ,zone = zonage,rpi = rpi,rpl = rpl, 
                              lstCategorie = lstCategorie,
                              sourceRp = rp.an,
                              rpi.weight = "IPONDI",
                              rpl.weight = "IPONDL")

  df.zone <- statZone$indicateur_stat %>% 
    mutate(zone.pred = pred.zone) %>% 
    filter(type.indicateur %in% c("freq_p","superficie","part_p","Q_0.5","avg")) %>% 
    spread(key = type.indicateur, value = value) %>% 
    left_join(lstIndicateur %>% select(nomVariable,nomIndicateur,typeIndicateurDiffusable),by=c("nomVariable","nomIndicateur")) %>%
    left_join(lstCategorie %>% select(nomVariable,Arrondi_RegCity),by=c("nomVariable")) %>% 
    mutate(valeur.diffusable = case_when(typeIndicateurDiffusable == "freq_p" ~ freq_p,
                                         typeIndicateurDiffusable == "superficie" ~ superficie,
                                         typeIndicateurDiffusable == "Q_0.5" ~ Q_0.5,
                                         typeIndicateurDiffusable == "avg" ~ avg,
                                         TRUE ~ part_p),
           valeur.diffusable = as.character(round(as.numeric(valeur.diffusable),digits = Arrondi_RegCity))) %>% 
    select(zone.pred,domaine,categorie,source,group_var,nomVariable,nomIndicateur,valeur.diffusable) %>% 
    mutate(type.indicateur = "valeur.diffusable") %>% 
    rename(value = valeur.diffusable)

  
  pyramide <- statZone$pyramide_tr %>% 
    mutate(zone.pred = pred.zone)
  
  return(list(df.zone = df.zone,pyramide = pyramide))
  
}



