#--------------------------------------------------------------------------------------------------------------------------------#
#                      Agate - Plotly Graphes fonctions                                                                          #
#--------------------------------------------------------------------------------------------------------------------------------#

# MAJ : 21.06.2018

# Nicolas Kempf

# I. Age pyramid
#-------------------------------------------------------------------------------------------------------------------------------------------------------
agePyramid_zone <- function(df.zone,idZonage.p,agemax = 120){
  
  # df complete
  df <- rbind(data.frame(sexe = "femme", age = seq(0,agemax,1)), data.frame(sexe = "homme", age = seq(0,agemax,1)))
  
  # Ajout des données sur la zone
  df.zone2 <- df.zone %>% 
    ungroup() %>% 
    select(idZonage,SEXE,AGEREV,freq_p,part_p) %>% 
    filter(idZonage == idZonage.p) %>% 
    rename(sexe = SEXE,
           age = AGEREV) %>%
    mutate(age = as.numeric(age)) 
  
  df <- left_join(df,df.zone2,by=c("sexe","age")) %>% 
    mutate(idZonage = idZonage[1],
           freq_p = ifelse(is.na(freq_p),0,freq_p),
           part_p = ifelse(is.na(part_p),0,part_p),
           abs_pop = paste(idZonage.p," : ",abs(freq_p)," ",sexe,"s de ",age," ans, soit ",part_p," % de la population.",sep=""))
  
  # Modification du signe de la part des hommes
  test <- df$sexe == "homme"
  df$part_p[test] <- - df$part_p[test]
  
  return(df)
}

agePyramid_Hzone <- function(df.zone,com.p,idZonage.p,agemax = 120){
  
  # df complete
  df <- rbind(data.frame(sexe = "femme", age = seq(0,agemax,1)), data.frame(sexe = "homme", age = seq(0,agemax,1)))
  
  # Ajout des données sur la zone
  df.zone2 <- df.zone %>% 
    ungroup() %>% 
    select(com,idZonage,SEXE,AGEREV,freq_p,part_p) %>% 
    filter(com == com.p & idZonage == idZonage.p) %>% 
    rename(sexe = SEXE,
           age = AGEREV) %>%
    mutate(age = as.numeric(age)) 
  
  df <- left_join(df,df.zone2,by=c("sexe","age")) %>% 
    mutate(com = com[1],
           idZonage = idZonage[1],
           freq_p = ifelse(is.na(freq_p),0,freq_p),
           part_p = ifelse(is.na(part_p),0,part_p),
           abs_pop = paste(idZonage.p," : ",abs(freq_p)," ",sexe,"s de ",age," ans, soit ",part_p," % de la population.",sep=""))
  
  # Modification du signe de la part des hommes
  test <- df$sexe == "homme"
  df$part_p[test] <- - df$part_p[test]
  
  return(df)
}



agePyramid_com <- function(df.zone,com.p,agemax = 120){
  # df complete
  df <- rbind(data.frame(sexe = "femme", age = seq(0,agemax,1)), data.frame(sexe = "homme", age = seq(0,agemax,1)))
  
  # Ajout des données sur la zone
  df.zone2 <- df.zone %>% 
    ungroup() %>% 
    select(com,SEXE,AGEREV,freq_p,part_p) %>% 
    filter(com == com.p) %>% 
    rename(sexe = SEXE,
           age = AGEREV) %>%
    mutate(age = as.numeric(age)) 
  
  df <- left_join(df,df.zone2,by=c("sexe","age")) %>% 
    mutate(com = com[1],
           freq_p = ifelse(is.na(freq_p),0,freq_p),
           part_p = ifelse(is.na(part_p),0,part_p),
           abs_pop = paste(com.p," : ",abs(freq_p)," ",sexe,"s de ",age," ans, soit ",part_p," % de la population.",sep=""))
  
  # Modification du signe de la part des hommes
  test <- df$sexe == "homme"
  df$part_p[test] <- - df$part_p[test]
  
  return(df)
}


agePyramid_dep <- function(df.zone,dep.p,agemax = 120){
  # df complete
  df <- rbind(data.frame(sexe = "femme", age = seq(0,agemax,1)), data.frame(sexe = "homme", age = seq(0,agemax,1)))
  
  # Ajout des données sur la zone
  df.zone2 <- df.zone %>% 
    ungroup() %>% 
    select(dep,SEXE,AGEREV,freq_p,part_p) %>% 
    filter(dep == dep.p) %>% 
    rename(sexe = SEXE,
           age = AGEREV) %>%
    mutate(age = as.numeric(age)) 
  
  df <- left_join(df,df.zone2,by=c("sexe","age")) %>% 
    mutate(dep = dep[1],
           freq_p = ifelse(is.na(freq_p),0,freq_p),
           part_p = ifelse(is.na(part_p),0,part_p),
           abs_pop = paste(dep.p," : ",abs(freq_p)," ",sexe,"s de ",age," ans, soit ",part_p," % de la population.",sep=""))
  
  # Modification du signe de la part des hommes
  test <- df$sexe == "homme"
  df$part_p[test] <- - df$part_p[test]
  
  return(df)
}


# II. Boxplot niveau de vie
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
df_boxplot <- function(df,varKeep){
  df <- as.data.frame(df)
  df$Q_0.25_2 <- df$Q_0.25
  df$Q_0.75_2 <-  df$Q_0.75
  varQuantile <- c("Q_0.1","Q_0.25","Q_0.25_2","Q_0.5","Q_0.75","Q_0.75_2","Q_0.9")
  df[,varQuantile] <- round(df[,varQuantile],digits = -1)
  return(df[,as.character(c(varKeep,varQuantile))])
}


