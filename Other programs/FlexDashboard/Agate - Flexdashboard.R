#------------------------------------------------------------------------------------------------------------------------------------#
#                       Agate - Flexdashboard                                                                                        #
#------------------------------------------------------------------------------------------------------------------------------------#

# Nicolas Kempf

# 28.11.2018

# Objectif : Code pour afficher un flexdashboard statique

# Packages necessaires
library(flexdashboard) # Tableaux de bord statiques
library(plotly) # Graphiques interactifs
library(tidyverse) # transformation des données

# Fonctions supplémentaires
source("Other programs/Plotly/Agate - plotly graphes fonctions_v3.R",encoding = "utf-8")

# I. Import des bases
#-------------------------------------------------------------------------------------------------------------------------------------
load("Data/Statistiques Zonage/StatRegCom_rp14_filo14.RData")
communes <- statCom_rp14_filo14
region <- statReg_rp14_filo14

# II. Graphiques
#-------------------------------------------------------------------------------------------------------------------------------------

# II.1. BoxPlot
#--------------
idZonage.com <- "97209" # Fort de france
idZonage.dep <- "972"

# Commune
varKeep <- c("com")
reg.boxplot <- df_boxplot(df = communes$tFilo.I.1,varKeep = varKeep) 
y <- as.character(reg.boxplot[reg.boxplot$com == idZonage.com,!colnames(reg.boxplot) %in% varKeep])
# Region
varKeep <- c("dep")
reg.boxplot <- df_boxplot(df = region$tFilo.I.1,varKeep = varKeep)
z <-  as.character(reg.boxplot[reg.boxplot$dep == idZonage.dep,!colnames(reg.boxplot) %in% varKeep])
name.comp <- idZonage.dep

# Boxplot
plot_ly() %>% 
  add_trace(y=y, name=idZonage.com , type="box") %>% 
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


# II.2. Aged pyramid
#-------------------

# Communes
pyramid.com <- agePyramid_com(df.zone = communes$tRp.II.4,com.p = idZonage.com)
pyramid.dep <- agePyramid_dep(df.zone = region$tRp.II.4,dep.p = idZonage.dep)

# Affichage de la pyramide
p <- plot_ly(data = pyramid.com,x= ~ part_p, y=~age,color=~sexe,colors = c('#fb9a99','#a6cee3')) %>%
  add_bars(orientation = 'h', hoverinfo = 'text', text = ~abs_pop,alpha = 0.8) %>%
  layout(bargap = 0.1, barmode = 'overlay',
         xaxis = list(title = "Population",tickmode = "array"),
         yaxis = list(title = "Age")) 


p <- add_trace(p = p,data = pyramid.dep,x = ~part_p, y = ~age - 0.5, type = 'scatter', mode = 'lines', name = idZonage.dep,
               line = list(shape="vh",color = "#74c476"),
               hoverinfo = "text",text = ~abs_pop)
p

# II.3 Informations about household
#---------------------------------

plot_ly(
  type = 'scatterpolar',
  fill = 'toself'
) %>%
  add_trace(
    r = c(39, 28, 8, 7, 28, 39),
    theta = c('A','B','C', 'D', 'E', 'A'),
    name = 'Group A'
  ) %>%
  add_trace(
    r = c(1.5, 10, 39, 31, 15, 1.5),
    theta = c('A','B','C', 'D', 'E', 'A'),
    name = 'Group B'
  ) %>%
  layout(
    polar = list(
      radialaxis = list(
        visible = T,
        range = c(0,50)
      )
    )
  )

















