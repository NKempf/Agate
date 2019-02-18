#----------------------------------------------------------------------------------------------------------------------------------------#
#                     Agate - qualité de l'appariement RIL _ RP fonction                                                                 #
#----------------------------------------------------------------------------------------------------------------------------------------#

# Nicolas Kempf

# Derniere MAJ : 18.07.2018

# Fonction de calage de l'appariement avec le RP
#-----------------------------------------------
calage.agate <- function(commune,df,list_var_cal,poids,totaux.cal){
  # Totaux à caler
  tot.cal <- totaux.cal %>% 
    filter(com == commune)
  tot.cal <- as.data.frame(tot.cal[,list_var_cal])

  # Vérification qu'un total n'est pas à 0
  list_var <- names(tot.cal)[tot.cal!=0]
  
  # Selection des logements de la commune
  df <- df %>% 
    filter(com == commune)
  
  if(nrow(df) == 0) {return(NULL)}
  
  # P1 : Données de calage à la commune
  #-----------------------------------------------------------------------------
  # Table de calage : on ne conserve que les indicatrices 
  tab.cal <-  df[,list_var]  
  
  # Pour une raison inconnue, le calage ne fonctionne pas pour Saint-Elie (97358)
  if(commune == "97358"){
    list_var2 <- colnames(tab.cal)[1:4]
  }else{
    list_var2 <- list_var
  }
  
  # Selection des variables comportant au moins une observation à 1  
  tab.cal <-  df[,list_var2] 
  
  tot.cal <- tot.cal[,list_var2]
  # Poids à caler
  poids.cal <- df[,poids]
  
  # P3 : Calage
  #-----------------------------------------------------------------------------
  pcal <- calib(tab.cal, poids.cal, as.numeric(tot.cal), method="logit", description=TRUE, bounds=c(0.2,1.8))
  
  if(!is.null(pcal)){
    df <- df %>% 
      mutate(IPOND.agate = pcal * poids.cal,
             calage = 1)
  }else{
    df <- df %>% 
      mutate(IPOND.agate = poids.cal,
             calage = 0)
  }
  # df <- df %>% 
  #   select(idx,C_ANNEE_COL,C_GEO,C_IMM,C_LOG,com,!!! syms(poids),IPOND.agate,calage)
  
  return(df)
}
