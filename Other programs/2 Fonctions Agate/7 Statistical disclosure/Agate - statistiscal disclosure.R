#--------------------------------------------------------------------------------------------------------------------------------#
#                 Agate - Fonction Secret statistique                                                                            #
#--------------------------------------------------------------------------------------------------------------------------------#

# MAJ : 26.03.2019

# Nicolas Kempf

# Applique le secret statistique aux indicateurs calculés par Agate
# Utilisation du package "easySdcTable" et plus particulièrement la fonction protectTable dans le cas de stacked data
secret_stat <- function(cat,df.zone.secret,seuil_secret_stat){
  # Secret statistique
  secret <- df.zone.secret %>% 
    filter(nomVariable %in% cat) %>% 
    select(idZonage,nomIndicateur,value) %>% 
    as.data.frame() %>% 
    ProtectTable(dimVar = c("idZonage","nomIndicateur"),freqVar = "value",maxN = seuil_secret_stat,addName = TRUE) 

  # Filtre et transforme  
  secret2 <- secret$data %>% 
    filter(idZonage != "Total") %>%
    filter(nomIndicateur != "Total") %>% 
    filter(freq != 0) %>% 
    mutate(diff.secret = ifelse(sdcStatus %in% c("u","x"),"n_diffusable","diffusable"),
           nomVariable = cat) %>% 
    select(idZonage,nomVariable,nomIndicateur,diff.secret)
  return(secret2)
}

