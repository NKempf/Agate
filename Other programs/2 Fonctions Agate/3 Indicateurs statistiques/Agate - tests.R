
df <- rpi[1:10000,]
group_var <- c("idZonage","idZonage.name")
poids = "IPONDI.cal"
indicateur <- "dem_sexe"



agate_qualitative <- function(indicateur,df,group_var,poids){
  df %>% 
    rename(nomIndicateur = indicateur) %>% 
    group_by(!!! syms(group_var),nomIndicateur) %>%
    weighted_frequency(poids) %>% 
    gather("type.indicateur","value",-group_var,-nomIndicateur) %>%
    mutate(nomVariable = indicateur) %>%
    ungroup()
}

lst_indicateur <- c("dem_sexe","dem_agerevTr")



lst_var <- unique(lstIndicateur$nomVariable[lstIndicateur$qualiteIndicateurDenom1 %in% "INPER"])
test <- bind_rows(lapply(lst_var, agate_qualitative,df = rpi[1:10000,],group_var = group_var, poids = rpi.weight))


table(df$idZonage)

# Automatisation du calcul d'indicateur sur des sous-champs

df_ssChamp <- lstCategorie %>% 
  filter(source == "rpi" & !is.na(modaliteChamp)) %>% 
  select(nomVariable,variableChamp,modaliteChamp) %>% 
  mutate(varmod.champ = paste0(variableChamp,modaliteChamp))





lst_champ <- unique(df_ssChamp$varmod.champ)[1]



agate.qualitative.ssChamp <- function(champ,df_ssChamp){
  
  lst_var <- df_ssChamp$nomVariable[df_ssChamp$varmod.champ == champ]
  var.filtre <- df_ssChamp$variableChamp[df_ssChamp$varmod.champ == champ]
  modalite.filtre <- df_ssChamp$modaliteChamp[df_ssChamp$varmod.champ == champ]
  
  bind_rows(lapply(lst_var, agate_qualitative,
                   df = rpi[1:10000,] %>% filter_at(vars(var.filtre), all_vars(. %in% modalite.filtre)),
                   group_var = group_var, poids = rpi.weight))
}



lst_var <- df_ssChamp$nomVariable[df_ssChamp$varmod.champ == lst_champ]
var.filtre <- df_ssChamp$variableChamp[df_ssChamp$varmod.champ == lst_champ]
modalite.filtre <- df_ssChamp$modaliteChamp[df_ssChamp$varmod.champ == lst_champ]


bind_rows(lapply(lst_var, agate_qualitative,
                 df = rpi[1:10000,] %>% filter_at(vars(var.filtre), all_vars(. %in% modalite.filtre)),
                 group_var = group_var, poids = rpi.weight))



#--------------------------------------------------------------------------------------------------------------------------
df %>%
  group_by(!!! syms(group_var)) %>% 
  summarise(freq = n(),
            freq_p = round(sum(!!! syms(rpi.weight),na.rm = TRUE),0)) %>% 
  gather("type.indicateur","value",-group_var) %>% 
  mutate(nomVariable = "population",
         nomIndicateur = "a_population")









