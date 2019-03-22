#--------------------------------------------------------------------------------------------------------------------------------#
#                 Agate - Fonction Secret statistique                                                                            #
#--------------------------------------------------------------------------------------------------------------------------------#

# MAJ : 21.03.2019

# Nicolas Kempf

# Applique le secret statistique aux indicateurs calculés par Agate
# Utilisation du package "easySdcTable" et plus particulièrement la fonction protectTable dans le cas de stacked data

secret_stat <- function(cat,df.zone.secret,seuil_secret_stat){
  # Secret statistique
  secret <- df.zone.secret %>% 
    filter(cat.secret %in% cat) %>% 
    select(idZonage,indicateur,value) %>% 
    as.data.frame() %>% 
    ProtectTable(dimVar = c("idZonage","indicateur"),freqVar = "value",maxN = seuil_secret_stat,addName = TRUE) 

  # Filtre et transforme  
  secret2 <- secret$data %>% 
    filter(idZonage != "Total") %>%
    filter(indicateur != "Total") %>% 
    filter(freq != 0) %>% 
    mutate(diff.secret = ifelse(sdcStatus %in% c("u","x"),"Non diffusable","Diffusable"),
           domaine = as.numeric(substr(cat,1,1)),
           categorie = as.numeric(substr(cat,3,3))) %>% 
    select(domaine,categorie,idZonage,indicateur,diff.secret)
  return(secret2)
}

