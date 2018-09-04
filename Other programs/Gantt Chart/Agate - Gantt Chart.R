#--------------------------------------------------------------------------------------------------------------------------------#
#                      Agate - Gantt chart prototype                                                                             #
#--------------------------------------------------------------------------------------------------------------------------------#

# MAJ : 30.08.2018

# Nicolas Kempf

# Creation et affichage du diagramme de gantt du projet Agate

# Packages nécessaires
#---------------------
library(timevis) # Interactive Gantt chart
library(tidyverse) # transformation des données et calcul de statistiques descriptives simples.

# I. Saisie des données
#-------------------------------------------------------------------------------------------------------------------------------

# I.1. Mise en forme du contenu de chaque item
#---------------------------------------------
templateNK <- function(tache, equipe, pourcent) {
  sprintf(    
    '<table><tbody>
      <tr><td colspan="3"><b>%s</b></td></tr>
      <tr><td><i>%s</i></td></tr>
      <tr><td>%s</td></tr>
    </tbody></table>',
    tache, equipe,pourcent 
  )
}

# I.2. Grandes familles de tâches
#--------------------------------
groups <- data.frame(
  id = c("prot","app"),
  content = c("Prototype","Agate")
)

# I.3. Liste des tâches
#----------------------
df <- data.frame(id=1:3,
                 tache = c("Prototype Agate","Déploiement application","Qualité du RP"),
                 team = c("Nico et Baptiste","Baptiste","Nico et Baptiste"),
                 pct = c(90,24,45),
                 start = c("2018-08-29","2018-08-30","2018-09-06"),
                 end = c("2018-08-30","2018-09-01","2018-09-10"),
                 group = c("prot","prot","prot"),
                 type = c("range","range","range")) %>% 
  mutate(content = templateNK(tache = tache,equipe = team,pourcent = paste0(pct," %")),
         style = case_when(pct >=75 ~ "color:#001f3f;border-style:solid;background-color:#2ECC40;",
                           pct < 25 ~ "color:#001f3f;border-style:solid;background-color:#FF4136;",
                           TRUE ~ "color:#001f3f;border-style:solid;background-color:#FFDC00;"))

# II. Diagramme de Gantt
#---------------------------------------------------------------------------------------------------------------------------------
timevis(data = df, groups = groups, options = list(editable = TRUE))

# III. Enregistrement de la base
#---------------------------------------------------------------------------------------------------------------------------------
save(groups,df,file="Data/Gantt/gantt.RData")

