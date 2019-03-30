#----------------------------------------------------------------------------------------------------------------------#
#                                   Agate - Dashboard mini app                                                         #
#----------------------------------------------------------------------------------------------------------------------#

# 30.03.2019

# Amélioration du tableau de bord en trois parties 
# 1) Affichage de chaque widget indépendemment 
# 2) Construction d'une petite application qui affiche le tableau de bord interactif
# 3) intégration dans l'application principale

# Packages nécessaires
#---------------------
library(tidyverse)
library(plotly)
library(DT)
library(shiny) # Graphic web interface
library(shinyBS) # Pop-up windows
library(shinyjs) # Mask buttons/elements on graphic interface
library(shinyWidgets) # Widgets supplementaires
library(shinydashboard) # Tools like infoBox

# Fonctions  particulières
source("Other programs/2 Fonctions Agate/4 Dashboard/Agate - Dashboard fct.R",encoding = "UTF-8")

# Label et choix
load("data/Liste indicateurs statistiques/lstIndicateur.RData")

#-------------------------------------------------------------------------------------------------------------------


#------------------------------------------------------------------------------------------------------------------------------------


shinyApp(
  ui <- navbarPage(textOutput("to_titleDash"),
                   
                   # II. Thème Démographie
                   #-----------------------------------------------------------------------------------------------------------------
                   tabPanel("Démographie",
                            includeCSS(path = "www/AdminLTE.css"),
                            includeCSS(path = "www/shinydashboard.css"),
                            
                            
                            # II.2. Selection de la zone de comparaison
                            #------------------------------------------
                            fluidRow(
                              column(6,
                                     # II.2.1. Type de zone
                                     selectInput("si_typeZone", "Type de zone",
                                                 choices = pred.choice,
                                                 selected = c(4))
                              ),
                              column(6,
                                     # II.2.2. Zone
                                     selectInput("si_zone", "Zone",
                                                 choices = c("QP971001"),
                                                 selected = c("QP971001"))
                              ),
                            
                            # II.1. Infobox
                            #--------------
                            fluidRow(
                              infoBoxOutput(outputId = "ib_feminite"),
                              infoBoxOutput(outputId = "ib_population"),
                              infoBoxOutput(outputId = "ib_superficie")
                            )
                            
                            
                            )
                            
                            
                   ),
                   # III. Thème  Emploi
                   #-----------------------------------------------------------------------------------------------------
                   tabPanel("Emploi")
  ),
  
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
  
  server = function(input, output, session) {

    load("Data/Tmp/dashboard_tmp.RData")
    load("Data/Tmp/qpv_stat_tmp.RData")
    
    # 0. Reactive Values
    #----------------------------------------------------------------------------------------------------------------------------
    rv <- reactiveValues(zone.etude = "QP971002",
                         zone.compare = NULL,
                         df.zone=df.zone,
                         dash.indicateur = NULL,
                         statZone = statZone)
    
    # Indicateur dashboard
    #-----------------------------------------------------------------------------------------------------------------------------
    observeEvent(c(input$si_typeZone,input$si_zone),{
      rv$zone.compare <- input$si_zone
      rv$dash.indicateur <- stat.dashboard_agate(df = rv$df.zone,zone.etude = rv$zone.etude,
                                                 zone.compare = rv$zone.compare, lstIndicateur = lstIndicateur,
                                                 pyramide_tr = rv$statZone$pyramide_tr)
    })
    
    observeEvent(rv$zone.etude,{
      
      
      
    })

    observeEvent(rv$dash.indicateur,{
      
      # Titre dashboard
      output$to_titleDash = renderText({
        rv$dash.indicateur$titreDash
      })
      
      # Theme Démographie
      #------------------
      output$ib_feminite <- renderInfoBox({
        infoBox(title = "Féminité", value = rv$dash.indicateur$vb.dem.fem,
                icon = icon("female"),fill=TRUE)
      })
      
      output$ib_population <- renderInfoBox({
        infoBox(title = "Population", value = rv$dash.indicateur$vb.dem.pop,
                icon = icon("users"),fill=TRUE)
      })
      
      output$ib_superficie <- renderInfoBox({
        infoBox(title = "Superficie", value = "TODO",
                icon = icon("tree"),fill=TRUE)
      })
      
    })
    
  
    
    
  } # End server
)







