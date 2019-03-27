#----------------------------------------------------------------------------------------------------------------------#
#                                   Agate - Dashboard fonction                                                         #
#----------------------------------------------------------------------------------------------------------------------#

# MAJ : 27.03.2019

# Nicolas Kempf


#----------------------------------------------------------------------------------------------------------------------------------------
# Pyramide des ages
pyramide_Agate <- function(pyramide,zone.etude,zone.compare,lstIndicateur){
  
  df <- pyramide %>% 
    filter(idZonage %in% c(zone.etude,zone.compare))
  
  g <- ggplot(df %>% filter(sexe == "b_femme" & idZonage == zone.compare), 
              aes(x = age, y = pop)) +
    geom_bar(data = df %>% filter(idZonage == zone.etude) %>% mutate(sexe = ifelse(sexe=="a_homme","a_homme.etude","b_femme.etude")), 
             aes(fill = sexe), stat = "identity") +
    geom_point(aes(colour = sexe)) +
    geom_line(aes(colour = sexe, group=1)) +
    geom_point(data=df %>% filter(sexe == "a_homme" & idZonage == zone.compare) %>% mutate(sexe = "a_homme"),
               aes(colour = sexe)) +
    geom_line(data=df %>% filter(sexe == "a_homme" & idZonage == zone.compare),aes(colour = sexe, group=2)) +
    
    scale_fill_manual(values = c(a_homme.etude = "#CEBC81", b_femme.etude = "#A16E83"), name=zone.etude,
                      labels=c("Hommes", "Femmes")) +
    scale_colour_manual(values = c(a_homme = "#479761",b_femme = "#479761" ), name=zone.compare,
                        labels=c("Hommes", "Femmes")) +
    coord_flip() +
    scale_y_continuous(breaks = c(c(-40,-30, -20, -10, 0), c(0, 10, 20, 30,40)),
                       labels = c("40","30", "20", "10", "0", "0", "10", "20", "30","40")) +
    scale_x_discrete(labels=lstIndicateur$labelIndicateur[lstIndicateur$nomVariable %in% "dem_agerevTr"]) +
    labs(x="",y = "Part des individus en %")  # Libellé des axes  
  return(g)
  
}
