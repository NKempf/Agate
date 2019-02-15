#--------------------------------------------------------------------------------------------------------------------------------#
#                      Agate - Plotly Graphes                                                                                    #
#--------------------------------------------------------------------------------------------------------------------------------#

# MAJ : 31.10.2018

# Nicolas Kempf

# Developpement des graphes avec plotly avant integration dans Agate 

# For more details : https://plotly-book.cpsievert.me/get-started.html

### Working directory 
# Paramètre automatiquement le dossier dans lequel se trouve le programme comme espace de travail
# Ne fonction que pour une version de R supérieure à 3.3
# setwd(dir=dirname(rstudioapi::getActiveDocumentContext()$path))
# getwd()

# Packages nécessaires
#---------------------
library(tidyverse) # transform data
library(plotly) # interactive graphs

source("Other programs/Plotly/Agate - plotly graphes fonctions_v3.R",encoding = "UTF-8")

# Statistiques issues d'Agate
#----------------------------
load("../../Bdd/RData/Temp/StatZonav2.RData")

# Statistiques régionales
#------------------------
load("Data/Statistiques Zonage/StatRegCom_rp14_filo14.RData")


# Paramètres généraux
#--------------------
idZonage <- "QP973010"
typeVar.switch <- "com"

unique(StatZona$tFilo.I.1$idZonage)

# Paramètres utiles
#------------------
idZonage.com <- StatZona$tRp.II.4$com[StatZona$tRp.II.4$idZonage == idZonage][1]
idZonage.dep <- substr(StatZona$tRp.II.4$com[StatZona$tRp.II.4$idZonage == idZonage][1],1,3)


# I. Pyramide des ages
#------------------------------------------------------------------------------------------------------------------------------------------

# Données sur zonage
pyramid.zone <- agePyramid_zone(df.zone = StatZona$tRp.II.4,idZonage.p = idZonage)

# Affichage de la pyramide
p <- plot_ly(data = pyramid.zone,x= ~ part_p, y=~age,color=~sexe,colors = c('#fb9a99','#a6cee3')) %>%
  add_bars(orientation = 'h', hoverinfo = 'text', text = ~abs_pop,alpha = 0.8) %>%
  layout(bargap = 0.1, barmode = 'overlay',
         xaxis = list(title = "Population",tickmode = "array"),
         yaxis = list(title = "Age")) 

# Affichage de la zone de comparaison
switch(typeVar.switch,
       dep = {
         pyramid.dep <- agePyramid_dep(df.zone = statReg_rp14_filo14$tRp.II.4,dep.p = idZonage.dep)
         p <- add_trace(p = p,data = pyramid.dep,x = ~part_p, y = ~age - 0.5, type = 'scatter', mode = 'lines', name = idZonage.dep,
                        line = list(shape="vh",color = "#74c476"),
                        hoverinfo = "text",text = ~abs_pop)
       },
       com = {
         pyramid.com <- agePyramid_com(df.zone = statCom_rp14_filo14$tRp.II.4,com.p = idZonage.com)
         p <- add_trace(p = p,data = pyramid.com,x = ~part_p, y = ~age - 0.5, type = 'scatter', mode = 'lines', name = idZonage.com,
                        line = list(shape="vh",color = "#74c476"),
                        hoverinfo = "text",text = ~ abs_pop)
       },
       hzone = {
         print("todo")
       }
       ,
       {
         pyramid.zone2 <- agePyramid_zone(df.zone = StatZona$tRp.II.4,idZonage.p = typeVar.switch)
         p <- add_trace(p = p,data = pyramid.zone2,x = ~part_p, y = ~age - 0.5, type = 'scatter', mode = 'lines', name = typeVar.switch,
                        line = list(shape="vh",color = "#74c476"),
                        hoverinfo = "text",text = ~ abs_pop)
       }
)
p

# II. Boxplot du niveau de vie
#--------------------------------------------------------------------------------------------------------------------------------------------

# Zone d'étude
varKeep.zone <- c("com","idZonage")
zone.boxplot <- df_boxplot(df = StatZona$tFilo.I.1,varKeep = varKeep.zone) 
y <- as.character(zone.boxplot[zone.boxplot$idZonage == idZonage,!colnames(zone.boxplot) %in% varKeep.zone])

# Affichage de la zone de comparaison
switch(typeVar.switch,
       dep = {
         varKeep <- c("dep")
         reg.boxplot <- df_boxplot(df = statReg_rp14_filo14$tFilo.I.1,varKeep = varKeep)
         z <-  as.character(reg.boxplot[reg.boxplot$dep == idZonage.dep,!colnames(reg.boxplot) %in% varKeep])
         name.comp <- idZonage.dep
       },
       com = {
         print("todo")
       },
       hzone = {
         print("todo")
       }
       ,
       {
         z <- as.character(zone.boxplot[zone.boxplot$idZonage == typeVar.switch,!colnames(zone.boxplot) %in% varKeep.zone])
         name.comp <- typeVar.switch
       }
)

# Graphique de la zone
plot_ly() %>% 
  add_trace(y=y, name=idZonage, type="box") %>% 
  add_trace(y=z, name= name.comp, type="box") %>% 
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

# III. Statistiques infracommunales niveau individu
#-------------------------------------------------------------------------------------------------------------------------------------------------------------
  
# Synthèse des données
#---------------------
stat_synthese <- function(df){
  # var <- enquo(var)
  # df %>% 
  #   ungroup() %>% 
  #   select(com,idZonage, !! var,part_p) %>% 
  #   spread(key = !! var,value = part_p)
  df <- df[,colnames(df)[c(1,3,4,8)]]
  spread(df,key = colnames(df)[3],value = colnames(df)[4])
}
StatZona.reduit <- StatZona[c("tRp.II.1","tRp.II.3","tRp.III.2","tRp.III.3","tRp.III.4","tRp.IV.1","tRp.IV.2","tRp.IV.4","tRp.V.1","tRp.V.2")]
StatZona.synthese <- lapply(StatZona.reduit, stat_synthese)
StatZona.synthese <- Reduce(function(...) merge(..., by = c('com', 'idZonage')), StatZona.synthese) 

var_lst <- c("[0,20)","[65,120]","femme","decrocheur","inactif","cadre_prof_inter","etranger")

df.zone <- StatZona.synthese[StatZona.synthese$com == idZonage.com & StatZona.synthese$idZonage==idZonage,var_lst]

# Plotly graphs
p <- plot_ly(type = 'scatterpolar',fill = 'toself') %>%
  add_trace(
    r = as.numeric(df.zone),
    theta = var_lst,
    name = idZonage) %>% 
  layout(polar = list(radialaxis = list(visible = T
                                        # ,range = c(0,100)
                                        ))
  )

# Switch à faire
df.zone2 <- StatZona.synthese[StatZona.synthese$idZonage=="QP972007",var_lst]

p %>%
  add_trace(
    r = as.numeric(df.zone2),
    theta = var_lst,
    name = "QP972007")
p

 