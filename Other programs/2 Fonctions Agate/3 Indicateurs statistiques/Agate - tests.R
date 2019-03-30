# VI.2. Pyramide par sexe, zonage et tranche d'age 
#------------------------------------------------
pyramide_tr <- rpi %>%
  mutate(age = cut(as.numeric(rpi$AGEREV),
                   breaks = c(seq(0,90,4),120),
                   include.lowest = TRUE,
                   right = FALSE)) %>%
  group_by(!!! syms(group_var),SEXE,age) %>%
  weighted_frequency(rpi.weight) %>% 
  mutate(source = paste0("rpi",sourceRp))


pyramide_tr <- bind_rows(lapply(c("a_homme","b_femme"),function(mod.sexe){
  rpi %>% 
    filter(dem_sexe == mod.sexe) %>% 
    agate_qualitative(df = .,indicateur = "dem_agerevTr",group_var = group_var,poids = rpi.weight) %>% 
    filter(type.indicateur == "part_p") %>% 
    mutate(dem_sexe = mod.sexe)
})
) %>% 
  mutate(value = ifelse(dem_sexe == "a_homme",-value,value))





