#--------------------------------------------------------------------------------------------------------------------------------#
#                      Agate - Plotly Graphes fonctions                                                                          #
#--------------------------------------------------------------------------------------------------------------------------------#

# MAJ : 31.05.2018

# Nicolas Kempf

# Fournit la table contenant les données communales et départementales pour une commune
agePyramidDep_Com <- function(df.dep,df.com,commune){
  
  # Pyramide communale
  pyramidTmp <- df.com %>% 
    select(-freq,-part_np) %>%
    filter(!is.na(SEXE) & com == commune) %>% 
    rename(age = AGEREV,
           part_com = part_p,
           pop_com = freq_p) %>% 
    mutate(age = as.numeric(age),
           abs_pop_com = paste("Commune : ",abs(pop_com)," ",SEXE,"s de ",age," ans, soit ",part_com," % de la population.",sep="")
    )
  
  # Valeur des Hommes négatives
  pyramidTmp$part_com[pyramidTmp$SEXE=="homme"] <- - pyramidTmp$part_com[pyramidTmp$SEXE=="homme"]
  
  # Pyramide départementale
  agePyramid <- statReg_rp14_filo14$tRp.II.4 %>% 
    select(-freq,-part_np) %>%
    # filter(idZonage == "QP971001") %>% 
    filter(!is.na(SEXE) & dep == "971") %>% 
    rename(age = AGEREV,
           part_dep = part_p,
           pop_dep = freq_p) %>% 
    mutate(age = as.numeric(age),
           abs_pop = paste("Département : ",abs(pop_dep)," ",SEXE,"s de ",age," ans, soit ",part_dep," %.",sep="")
    )
  
  # Valeur des Hommes négatives
  agePyramid$part_dep[agePyramid$SEXE=="homme"] <- - agePyramid$part_dep[agePyramid$SEXE=="homme"]
  
  # Jointure
  agePyramid.final <- merge(agePyramid,pyramidTmp[,c("SEXE","age","part_com","pop_com","abs_pop_com")],by=c("SEXE","age"),all = T)
  
  test <- is.na(agePyramid.final$pop_com)
  agePyramid.final$part_com[test] <- 0
  agePyramid.final$pop_com[test] <- 0
  agePyramid.final$abs_pop_com[test] <- paste(abs(agePyramid.final$pop_com)[test]," ",agePyramid.final$SEXE[test],"s de ",
                                              agePyramid.final$age[test]," ans",sep="")
  
  return(agePyramid.final)
}


# Pyramide des ages finale
agePyramidZonage <- function(df.zone,zone,df.dep){

  # Selection de la commune
  comtmp <- df.zone$com[df.zone$idZonage == zone][1]
  
  # Pyramide communale
  pyramidTmp <- df.zone %>% 
    select(-freq,-part_np) %>%
    filter(!is.na(SEXE) & idZonage %in% c(zone,"Hors zonage") & com == comtmp)  %>% 
    rename(age = AGEREV) %>% 
    mutate(age = as.numeric(age))
  
  # Pyramide des hors zone de la commune
    pyramidTmp1 <- pyramidTmp %>% 
      filter(idZonage == "Hors zonage") %>% 
      rename(pop_hz = freq_p,
             part_hz = part_p) %>% 
      mutate(abs_pop_hz = paste("Hors zone : ",abs(pop_hz)," ",SEXE,"s de ",age," ans, soit ",part_hz," % de la population.",sep="")) %>% 
      ungroup() %>% 
      select(-com,-com.lib,-idZonage)

  # Pyramide finale
    pyramid <- pyramidTmp %>% 
      filter(idZonage == zone) %>% 
      rename(pop_z = freq_p,
             part_z = part_p) %>% 
      mutate(abs_pop_z = paste(zone," : ",abs(pop_z)," ",SEXE,"s de ",age," ans, soit ",part_z," % de la population.",sep="")) %>% 
      full_join(pyramidTmp1,by=c("SEXE","age")) %>% 
      full_join(df.dep,by=c("SEXE","age"))
    
  test <- is.na(pyramid$part_z)
  pyramid[test,c("pop_z","pop_hz","part_z","part_hz")] <- 0
  pyramid$abs_pop_z[test] <- paste(zone," : ",abs(pyramid$pop_z)[test]," ",pyramid$SEXE[test],"s de ",
                                     pyramid$age[test]," ans",sep="")
  pyramid$abs_pop_hz[test] <- paste("Hors zone : ",abs(pyramid$pop_z)[test]," ",pyramid$SEXE[test],"s de ",
                                   pyramid$age[test]," ans",sep="")
  
  test <- pyramid$SEXE == "homme"
  pyramid$part_z[test] <- - pyramid$part_z[test]
  pyramid$part_hz[test] <- - pyramid$part_hz[test]
  
  return(pyramid)
}




