table(test$data$sdcStatus)
View(test$data %>% 
  filter(indicateur %in% c("etudi","n_etudi")))


df.zone2 <- df.zone %>% 
  mutate(cat.secret = paste(domaine,categorie,sep = "_"))

# Toutes les categories d'indicateurs sur lesquels appliquer le secret statistique
df.zone.secret <- df.zone2 %>% 
  filter(type.indicateur %in% c("freq","n"))
unique(df.zone.secret$cat.secret)



secret_stat <- function(cat,df.zone.secret){
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
           domaine = substr(cat,1,1),
           categorie = substr(cat,3,3)) %>% 
    select(domaine,categorie,idZonage,indicateur,diff.secret)
  return(secret2)
}

test <- bind_rows(lapply(unique(df.zone.secret$cat.secret)[1:3],secret_stat,df.zone.secret = df.zone.secret))






