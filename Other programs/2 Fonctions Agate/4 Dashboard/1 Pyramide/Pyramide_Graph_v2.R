#---------------------------------------------------------------------------------------------------------------------#
#                                    Pyramide des ages - Transformation des donnees V2                                #
#---------------------------------------------------------------------------------------------------------------------#

library(readxl) # lecture des fichiers excel
library(tidyverse) # Transformation des données
library(plotly) # Interactive graphs
# library(openxlsx) # Export en excel

# Fonction pyramide avec ggplot2
source("Other programs/2 Fonctions Agate/4 Dashboard/1 Pyramide/Pyramide_fct_v3.R",encoding = "utf-8")

# Paramètres
zone.etude <- "QP971002"
zone.compare <- "QP971001"

# I. import des données
#----------------------------------------------------------------------------------------------------------------------
load("Data/Liste indicateurs statistiques/lstIndicateur.RData")

load("Data/Tmp/qpv_stat_tmp.RData") # Indicateurs statistiques sur deux QPV
pyramide <- statZone$pyramide_tr %>% 
  rename(age = nomIndicateur,
         sexe = dem_sexe,
         pop = value) %>% 
  select(-type.indicateur,-nomVariable)



# II. Pyramide des ages
#-----------------------------------------------------------------------------------------------------------------------
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
    
    scale_fill_manual(values = c(a_homme.etude = "#CEBC81", b_femme.etude = "#A16E83"), name=zone.etude,labels=c("Hommes", "Femmes")) +
    scale_colour_manual(values = c(a_homme = "#479761",b_femme = "#479761" ), name=zone.compare,labels=c("Hommes", "Femmes")) +
    coord_flip() +
    scale_y_continuous(breaks = c(c(-40,-30, -20, -10, 0), c(0, 10, 20, 30,40)),
                       labels = c("40","30", "20", "10", "0", "0", "10", "20", "30","40")) +
    scale_x_discrete(labels=lstIndicateur$labelIndicateur[lstIndicateur$nomVariable %in% "dem_agerevTr"]) +
    labs(x="",y = "Part des individus en %")  # Libellé des axes  
  return(g)
 
}

pyr <- pyramide_Agate(pyramide = pyramide, zone.etude = zone.etude, zone.compare = zone.compare, lstIndicateur = lstIndicateur)

ggplotly(pyr)


