#---------------------------------------------------------------------------------------------------------------------#
#                                    Agate - Barchart dashboard                                                       #
#---------------------------------------------------------------------------------------------------------------------#

library(tidyverse) # Transformation des données
library(plotly) # Interactive graphs
# library(openxlsx) # Export en excel

# Paramètres
zone.etude <- "QP971002"
zone.compare <- "QP971001"

# I. import des données
#----------------------------------------------------------------------------------------------------------------------
load("Data/Liste indicateurs statistiques/lstIndicateur.RData")

load("Data/Tmp/qpv_stat_tmp.RData") # Indicateurs statistiques sur deux QPV

# II. Bar chart
#-----------------------------------------------------------------------------------------------------------------------
barChart_agate <- function(df,var.barChart,zone.etude,zone.compare,lstIndicateur){
  
  tab.label <- lstIndicateur$labelIndicateur[lstIndicateur$nomVariable %in% var.barChart & 
                                               !lstIndicateur$nomIndicateur %in% c("g_moins14")]
  
  df <- df %>% 
    filter(idZonage %in% c(zone.etude,zone.compare) & nomVariable == var.barChart & type.indicateur == "part_p") %>% 
    mutate(value = as.numeric(value),
           nomIndicateur = factor(nomIndicateur,labels = tab.label))
  
  g <- ggplot(df,aes(x = reorder(nomIndicateur, value), y = value,fill=idZonage.name)) +
    geom_bar(stat = "identity",position = "dodge") +
    coord_flip() +
    scale_fill_manual(values=c("#CEBC81","#A16E83"),name=" ") +
    labs(x=" ",y = "Part des individus en %")  # Libellé des axes  
  return(g)
}

# Emploi : type d'activité
g.emp.typeAct <- barChart_agate(df = df.zone,var.barChart = "emp_typeActivite",
                                   zone.etude = zone.etude,zone.compare = zone.compare,lstIndicateur = lstIndicateur)

ggplotly(g.emp.typeAct)

# Scolarisation : population scoalrisé
g.sco.pop <- barChart_agate(df = df.zone,var.barChart = "sco_popSco2",
                            zone.etude = zone.etude,zone.compare = zone.compare,lstIndicateur = lstIndicateur)
# Affichage
ggplotly(g.sco.pop)

# Scolarisation : Diplome
g.sco.diplome <- barChart_agate(df = df.zone,var.barChart = "sco_diplome",
                                   zone.etude = zone.etude,zone.compare = zone.compare,lstIndicateur = lstIndicateur)
# Affichage
ggplotly(g.sco.diplome)








