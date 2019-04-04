#----------------------------------------------------------------------------------------------------------------------#
#                                   Agate - Dashboard Improvement Mini app                                             #
#----------------------------------------------------------------------------------------------------------------------#

# 19.02.2019

# 2) Construction d'une petite application qui affiche le tableau de bord interactif

library(shiny) # Graphic web interface
library(shinyBS) # Pop-up windows
library(shinyjs) # Mask buttons/elements on graphic interface
library(shinyWidgets) # Widgets supplementaires
library(shinydashboard) # Tools like infoBox
library(DT)


# Paramètre temporaire
zone.selection <- 3

# 0. Label tableau
#-----------------
load("Data/Liste indicateurs statistiques/lstIndicateur.RData")
dmn <- lstDomaine$idDomaine
names(dmn) <- lstDomaine$labelDomaine

# I.1. Zone d'étude
#------------------
load("Data/Tmp/qpv_stat_tmp.RData") # Indicateurs statistiques sur deux QPV

# Selection d'une zone
zone.etude <- "QP971002"
df.etude <- indStat$indicateur_stat %>% 
  filter(idZonage == zone.etude & type.indicateur != "part_np") %>% 
  select(source,domaine,categorie,idZonage,idZonage.name,indicateur,type.indicateur,value) %>% 
  mutate(dashboard = "zone.etude1")

# Source utilisée pour les indicateurs de l'étude
source.etude <- unique(df.etude$source)

# I.2. Zone de comparaison
#-------------------------
if(zone.selection == 4){ #Si la zone de comparaison fait partie des zones d'études
  zone.compare <- "QP971001"
  df.compare <- indStat$indicateur_stat %>% 
    filter(idZonage == zone.compare & type.indicateur != "part_np") %>% 
    select(source,domaine,categorie,idZonage,idZonage.name,indicateur,type.indicateur,value)
}else{ # Sinon, elle fait partie des zones predefinies
  zone.compare <- "971"
  df.compare <- read_fst("Data/Stats/Prefine aera/Real/fst/indicateur_stat.fst") %>%
    select(zone.predefine,source,domaine,categorie,idZonage,idZonage.name,indicateur,type.indicateur,value) %>%
    filter(type.indicateur != "part_np" & zone.predefine == 1 & idZonage == zone.compare & source %in% source.etude) %>% 
    select(-zone.predefine)
}

# Table de travail
df.dashboard <- bind_rows(df.etude,df.compare %>% mutate(dashboard = "zone.etude2"))

# Label des colonnes 
dash.label <- c("indicateur","zone.etude1","zone.etude2")
names(dash.label) <- c("Indicateurs",
                       unique(df.etude$idZonage.name[df.etude$idZonage == zone.etude]),
                       unique(df.compare$idZonage.name[df.compare$idZonage == zone.compare]))


print(df.etude)

unique(df.etude$idZonage.name)[1]













shinyApp(
  ui <- navbarPage(textOutput("to_titleDash"),
                   
                   # II. Thème territoire
                   #-----------------------------------------------------------------------------------------------------------------
                   tabPanel("Territoire",
                            
                            includeCSS(path = "www/AdminLTE.css"),
                            includeCSS(path = "www/shinydashboard.css"),
                            
                            # II.1. Infobox
                            #--------------
                            fluidRow(
                              column(4,
                                     infoBoxOutput(outputId = "Ib_pop")
                              ),
                              column(4,
                                     infoBoxOutput(outputId = "ib_densite")
                              ),
                              column(4,
                                     infoBoxOutput(outputId = "ib_superficie")
                              )
                            ),
                            
                            # II.2. Selection de la zone de comparaison
                            #------------------------------------------
                            fluidRow(
                              column(6,
                                     # II.2.1. Type de zone
                                     selectInput("si_typeZone", "Type de zone",
                                                 c("Choice" =""),
                                                 selected = c(""))
                              ),
                              column(6,
                                     # II.2.2. Zone
                                     selectInput("si_zone", "Zone",
                                                 choices = c("Choice" =""),
                                                 selected = c(""))
                              )
                            ),
                            
                            # II.3. Table Territoire
                            #-----------------------
                            fluidRow(
                              column(6,
                                     # II.2.1. Type de zone
                                     selectInput("si_typeZone", "Type de zone",
                                                 c("Choice" =""),
                                                 selected = c(""))
                              ),
                              column(6,
                                     # II.2.2. Territoire
                                     DT::dataTableOutput("dt_territoire")
                              )
                            ),
                            
                            
                            textOutput("TO_titleTab"),
                            tags$head(tags$style("#TO_titleTab{font-size: 30px;font-style: bold;}")),
                            DT::dataTableOutput("table")
                   ),
                   
                   # III. Thème Revenu - Pauvreté
                   #-----------------------------------------------------------------------------------------------------------------
                   tabPanel("Revenu - Pauvreté",
                            verbatimTextOutput("summary")
                            
                            
                            
                            
                            
                            
                   ),
                   
                   # IV. Thème Emploi
                   #-----------------------------------------------------------------------------------------------------------------
                   tabPanel("Emploi",
                            verbatimTextOutput("summary")
                            
                            
                            
                            
                            
                            
                   ),
                   
                   # V. Thème Scolarisation
                   #-----------------------------------------------------------------------------------------------------------------
                   tabPanel("Scolarisation",
                            verbatimTextOutput("summary")
                            
                            
                            
                            
                            
                            
                   ),
                   # VI. Thème Logement
                   #-----------------------------------------------------------------------------------------------------------------
                   tabPanel("Logement",
                            verbatimTextOutput("summary")
                            
                            
                            
                            
                            
                            
                   )
  ),
  
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
  
  server = function(input, output, session) {
  
    # Titre du dashboard
    #TODO
    output$to_titleDash <- renderText({ 
      paste0("Vieux - tableau de bord")
      })
    
    
    # II. Thème Territoire
    #------------------------------------------------------------------------------------------------------------------------------
    
    # II.1.InfoBox 1
    #---------------
    output$ib_pop <- renderInfoBox({
      infoBox(title = "Population", value = "100",
              icon = icon("child"),color = "green", fill = TRUE
      )
    })
    # 
    # # II.2. InfoBox 2
    # #----------------
    # output$ib_densite <- renderInfoBox({
    #   infoBox(title = "Densité de population", value = "100",
    #           icon = icon("child"),color = "green", fill = TRUE
    #   )
    # })
    # 
    # # InfoBox 3
    # output$ib_superficie <- renderInfoBox({
    #   infoBox(title = "Superficie", value = "100",
    #           icon = icon("child"),color = "green", fill = TRUE
    #   )
    # })
    
    
    # output$plot <- renderPlot({
    #   plot(cars, type=input$plotType)
    # })
    # 
    output$summary <- renderPrint({
      summary(cars)
    })
    
    # output$table <- DT::renderDataTable({
    #   DT::datatable(cars)
    # })
  } # End server
)


