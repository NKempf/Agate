df <- read_fst("Data/Stats/Prefine aera/Real/fst/indicateur_stat.fst")

df %>% 
  mutate(value2 = case_when(nomVariable %in% c("population","superficie","log_tot") & type.indicateur %in% c("valeur.diffusable") & 
                              !is.na(as.numeric(value)) ~ round(as.numeric(value)),
                            TRUE  ~ value
    
    
  ))
as.numeric("0.1")
is.numeric(as.numeric("test"))
!is.na(as.numeric("0.1"))
