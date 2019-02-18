#--------------------------------------------------------------------------------------------------------------------------------#
#                      Agate - Plotly Graphes                                                                                    #
#--------------------------------------------------------------------------------------------------------------------------------#

# MAJ : 29.05.2018

# Nicolas Kempf

# Developpement des graphes avec plotly avant integration dans Agate 

# For more details : https://plotly-book.cpsievert.me/get-started.html

### Working directory 
# Paramètre automatiquement le dossier dans lequel se trouve le programme comme espace de travail
# Ne fonction que pour une version de R supérieure à 3.3
setwd(dir=dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Packages nécessaires
#---------------------
library(tidyverse) # transform data
library(plotly) # interactive graphs

source("Agate - plotly graphes fonctions.R",encoding = "UTF-8")

# Statistiques issues d'Agate
#----------------------------
load("../../Bdd/RData/Temp/StatZonav2.RData")

# Statistiques régionales
#------------------------
load("../../Bdd/RData/Statistiques Zonage/StatRegCom_rp14_filo14.RData")

# I. Pyramide des ages
#------------------------------------------------------------------------------------------------------------------------------------------

# Données régionales et communales
agepyramid.dep <- agePyramidDep_Com(df.dep = statReg_rp14_filo14$tRp.II.4,df.com = statCom_rp14_filo14$tRp.II.4,commune = "97101")

# Données sur zonage
pyramid <- agePyramidZonage(df.zone = StatZona$tRp.II.4,df.dep = agepyramid.dep,zone = "QP971001") %>% 
  arrange(SEXE,age)

# Affichage de la pyramide
plot_ly(data = pyramid,x= ~part_z, y=~age,color=~SEXE,colors = c('#fb9a99','#a6cee3')) %>%
  add_bars(orientation = 'h', hoverinfo = 'text', text = ~abs_pop_z,alpha = 0.8) %>%
  add_trace(x = ~part_hz, y = ~age - 0.5, type = 'scatter', mode = 'lines', name = 'Hors zone',
            line = list(shape="vh",color = "#74c476"),
            hoverinfo = "text",text = ~ abs_pop_hz) %>%
  # add_trace(x = ~part_dep, y = ~age - 0.5, type = 'scatter', mode = 'lines', name = 'Region',
  #           line = list(shape="vh",color = "#969696"),
  #           hoverinfo = "text",text = ~ abs_pop_com) %>%
  layout(bargap = 0.1, barmode = 'overlay',
         xaxis = list(title = "Population",tickmode = "array"),
         yaxis = list(title = "Age")) 

# II. Boxplot du niveau de vie
#--------------------------------------------------------------------------------------------------------------------------------------------

# Paramètres généraux
idZonage <- "QP973010"
typeVar.switch <- "dep"


# Trace zonage
trace.name.zonage <- idZonage
varKeep.zone <- c("com","idZonage")
zone.boxplot <- df_boxplot(df = StatZona$tFilo.I.1,varKeep = varKeep.zone) 
y <- as.character(zone.boxplot[zone.boxplot$idZonage == idZonage,!colnames(zone.boxplot) %in% varKeep.zone])

switch(typeVar.switch,
       dep = {
         varKeep <- c("dep")
         trace.name <- substr(zone.boxplot$com[zone.boxplot$idZonage == idZonage],1,3)
         reg.boxplot <- df_boxplot(df = statReg_rp14_filo14$tFilo.I.1,varKeep = varKeep)
         z <-  as.character(reg.boxplot[reg.boxplot$dep == trace.name,!colnames(reg.boxplot) %in% varKeep])
         
       },
       com = {
         print("todo")
       },
       hzone = {
         print("todo")
       }
       ,
       {
         z <- as.character(zone.boxplot[zone.boxplot$idZonage == typeVar,!colnames(zone.boxplot) %in% varKeep.zone])
       }
)

  # Graphique plotly
  plot_ly() %>% 
    add_trace(y=y, name=trace.name.zonage, type="box") %>% 
    add_trace(y=z, name= trace.name, type="box") %>% 
    layout(autosize=TRUE, boxmode="group", hovermode="closest", 
           showlegend=TRUE,
           xaxis = list(
             autorange = TRUE, 
             showticklabels = FALSE, 
             type = "category"),
           yaxis = list(
             autorange = TRUE, 
             type = "linear")
           )

  
  
 