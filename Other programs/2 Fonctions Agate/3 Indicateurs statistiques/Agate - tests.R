
df <- rpi[1:1000,]
group_var <- c("idZonage","idZonage.name")
poids = "IPONDI.cal"
indicateur <- "dem_sexe"



agate_qualitative <- function(indicateur,df,group_var,poids){
  df %>% 
    group_by(!!! syms(c(group_var,indicateur))) %>%
    weighted_frequency(poids) %>% 
    gather("type.indicateur","value",-group_var,-indicateur) %>%
    mutate(nomIndicateur = indicateur) %>% 
    ungroup()
}

lst_indicateur <- c("dem_sexe","dem_agerevTr")

test <- bind_rows(lapply(lst_indicateur, agate_qualitative,df = rpi[1:1000,],group_var = group_var, poids = rpi.weight))


