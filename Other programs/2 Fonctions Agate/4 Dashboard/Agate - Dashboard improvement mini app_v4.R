#----------------------------------------------------------------------------------------------------------------------#
#                                   Agate - Dashboard mini app                                                         #
#----------------------------------------------------------------------------------------------------------------------#

# 01.04.2019

# Mini app pour améliorer le dashboard d'Agate avant intégration définitive

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

# NavBar Spéciale :)
navbarPageWithInputs <- function(..., inputs) {
  navbar <- navbarPage(...)
  form <- tags$form(class = "navbar-form", inputs)
  navbar[[3]][[1]]$children[[1]] <- htmltools::tagAppendChild(
    navbar[[3]][[1]]$children[[1]], form)
  navbar
}


ui <- shinyUI(
  fluidPage(
    navbarPage("title",
               tabPanel("OpenModal",
                        actionButton("ab_modal","Dashboard"))
    ),
    tags$head(tags$style("#bs_dashboard .modal-footer{ display:none}
                        #bs_dashboard .modal-header{ display:none}")), # Remove BS modal footer
    tags$head(tags$style(HTML('

                                                    .modal-lg {
                                                    width: 90%;
                                                    
                                                    }
                                                    '))), # Increase modal size
    bsModal('bs_dashboard', title = "",'test',size = "large",
            # Add CSS files : use infobox from shinydashboard package into a shinyApp
            includeCSS(path = "www/AdminLTE.css"),
            includeCSS(path = "www/shinydashboard.css"),
            includeCSS(path = "www/agateDashboard.css"),
            
            navbarPageWithInputs(id="nbwi_agate_dashboard",textOutput("to_titleDash"),
                                 
                                 # II. Thème Démographie
                                 #-----------------------------------------------------------------------------------------------------------------
                                 tabPanel("Démographie",
                                          includeCSS(path = "www/AdminLTE.css"),
                                          includeCSS(path = "www/shinydashboard.css"),
                                          
                                          fluidRow(
                                            infoBoxOutput(outputId = "ib_dem_feminite"),
                                            infoBoxOutput(outputId = "ib_dem_population"),
                                            infoBoxOutput(outputId = "ib_dem_superficie")
                                          ),
                                          fluidRow(
                                            box(title = "Individus", solidHeader = TRUE,
                                                collapsible = TRUE,
                                                DT::dataTableOutput("dt_dem_hg")
                                            ),
                                            box(title = "Ménages", solidHeader = TRUE,
                                                collapsible = TRUE,
                                                DT::dataTableOutput("dt_dem_hd")
                                            )
                                          ),
                                          fluidRow(
                                            box(title = "Pyramide des âges", solidHeader = TRUE,
                                                collapsible = TRUE,
                                                plotlyOutput("g_dem_bg")
                                            ),
                                            box(title = "Immigration", solidHeader = TRUE,
                                                collapsible = TRUE,
                                                DT::dataTableOutput("dt_dem_bd")
                                            )
                                          ),
                                          fluidRow(
                                            textOutput("to_source")
                                          )
                                 ),
                                 # III. Thème  Emploi
                                 #-----------------------------------------------------------------------------------------------------
                                 tabPanel("Emploi",
                                          
                                          fluidRow(
                                            infoBoxOutput(outputId = "ib_emp_pop_trav"),
                                            infoBoxOutput(outputId = "ib_emp_chomeur"),
                                            infoBoxOutput(outputId = "ib_emp_inactif")
                                          ),
                                          fluidRow(
                                            box(title = "Marché de l'emploi", solidHeader = TRUE,
                                                collapsible = TRUE,
                                                DT::dataTableOutput("dt_emp_hg")
                                            ),
                                            box(title = "Chômage", solidHeader = TRUE,
                                                collapsible = TRUE,
                                                DT::dataTableOutput("dt_emp_hd")
                                            )
                                          ),
                                          fluidRow(
                                            box(title = "Type d'activité", solidHeader = TRUE,
                                                collapsible = TRUE,
                                                plotlyOutput("g_emp_bg")
                                            ),
                                            box(title = "Travail", solidHeader = TRUE,
                                                collapsible = TRUE,
                                                DT::dataTableOutput("dt_emp_bd")
                                            )
                                          )
                                 ),
                                 
                                 # IV. Thème Scolarisation
                                 #-----------------------------------------------------------------------------------------------------
                                 tabPanel("Scolarisation",
                                          
                                          fluidRow(
                                            infoBoxOutput(outputId = "ib_sco_pop_sco"),
                                            infoBoxOutput(outputId = "ib_sco_etud"),
                                            infoBoxOutput(outputId = "ib_sco_decrocheur")
                                          ),
                                          fluidRow(
                                            box(title = "Jeunes scolarisés", solidHeader = TRUE,
                                                collapsible = TRUE,
                                                DT::dataTableOutput("dt_sco_hg")
                                            ),
                                            box(title = "Jeunes non scolarisés", solidHeader = TRUE,
                                                collapsible = TRUE,
                                                DT::dataTableOutput("dt_sco_bd")
                                            )
                                          ),
                                          fluidRow(
                                            box(title = "Niveau de diplôme", solidHeader = TRUE,
                                                collapsible = TRUE,
                                                plotlyOutput("g_sco_bg")
                                            ),
                                            
                                            box(title = "Jeunes", solidHeader = TRUE,
                                                collapsible = TRUE,
                                                plotlyOutput("g_sco_hd")
                                            )
                                          )
                                 ),
                                 
                                 # V. Thème Logement
                                 #-----------------------------------------------------------------------------------------------------
                                 tabPanel("Logement",
                                          
                                          fluidRow(
                                            infoBoxOutput(outputId = "ib_log_pop"),
                                            infoBoxOutput(outputId = "ib_log_hlm"),
                                            infoBoxOutput(outputId = "ib_log_maison")
                                          ),
                                          fluidRow(
                                            box(title = "Caractéristiques des logements", solidHeader = TRUE,
                                                collapsible = TRUE,
                                                DT::dataTableOutput("dt_log_hg")
                                            ),
                                            box(title = "Catégorie de logement", solidHeader = TRUE,
                                                collapsible = TRUE,
                                                plotlyOutput("g_log_hd")
                                            )
                                          ),
                                          fluidRow(
                                            box(title = "Année d'achevement", solidHeader = TRUE,
                                                collapsible = TRUE,
                                                plotlyOutput("g_log_bg")
                                            ),
                                            box(title = "Aspect du bâti", solidHeader = TRUE,
                                                collapsible = TRUE,
                                                plotlyOutput("g_log_bd")
                                            )
                                          )
                                 ),
                                 
                                 # V. Thème Résidences principales
                                 #-----------------------------------------------------------------------------------------------------
                                 tabPanel("Résidences principales",
                                          
                                          fluidRow(
                                            infoBoxOutput(outputId = "ib_res_pop"),
                                            infoBoxOutput(outputId = "ib_res_collectif"),
                                            infoBoxOutput(outputId = "ib_res_todo")
                                          ),
                                          fluidRow(
                                            box(title = "Caractéristiques des résidences principales", solidHeader = TRUE,
                                                collapsible = TRUE,
                                                DT::dataTableOutput("dt_res_hg")
                                            ),
                                            box(title = "Équipements", solidHeader = TRUE,
                                                collapsible = TRUE,
                                                DT::dataTableOutput("dt_res_bd")
                                            )
                                          ),
                                          fluidRow(
                                            box(title = "Surface", solidHeader = TRUE,
                                                collapsible = TRUE,
                                                plotlyOutput("g_res_bg")
                                            ),
                                            box(title = "Nombre de pièces", solidHeader = TRUE,
                                                collapsible = TRUE,
                                                plotlyOutput("g_res_hd")
                                            )
                                          )
                                 ),
                                 
                                 # VI. Zones de comparaisons
                                 #-----------------------------------------------------------------------------------------------------
                                 inputs = dropdownButton(inputId = "ddb_compare" ,
                                                         selectInput("si_typeZone", "Type de zone",
                                                                     choices = pred.choice,
                                                                     selected = c(4)),
                                                         # II.2.2. Zone
                                                         selectInput("si_zone", "Zone",
                                                                     choices = c("Choice" =""),
                                                                     selected = c("")),
                                                         circle = TRUE, status = "danger",
                                                         icon = icon("gear"),size = "sm",right = TRUE,
                                                         tooltip = tooltipOptions(title = "Zones de comparaison")
                                 )
            )
    )
  )
)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

server = function(input, output, session) {
  
  # load("Data/Tmp/dashboard_tmp.RData")
  load("Data/Tmp/qpv_stat_tmp.RData")
  
  observeEvent(input$ab_modal,{
    toggleModal(session, "bs_dashboard", toggle = "toggle")
  })
  
  # Reactive Values
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
    
    # Titre dashboard
    output$to_titleDash = renderText({
      rv$dash.indicateur$titreDash
    })
    
    output$to_source = renderText({
      paste0("Source : recensement de la population 20",rv$dash.indicateur$source.an,
             " niveau individu et logement - exploitation principale. \n Note : c = données confidentielles, intervalles de confiance 
             à 95 % calculés à partir d'une estimation de la variance du plan de sondage.")

    })
    
    # Theme Démographie
    #------------------
    output$ib_dem_feminite <- renderInfoBox({
      infoBox(title = "Féminité", value = rv$dash.indicateur$vb.dem.fem,
              icon = icon("female"),fill=TRUE)
    })
    
    output$ib_dem_population <- renderInfoBox({
      infoBox(title = "Population", value = rv$dash.indicateur$vb.dem.pop,
              icon = icon("users"),fill=TRUE)
    })
    
    output$ib_dem_superficie <- renderInfoBox({
      infoBox(title = "Superficie", value = "TODO",
              icon = icon("tree"),fill=TRUE)
    })
    
    output$dt_dem_hg = renderDT(
      datatable(rv$dash.indicateur$df.dem.tab.hg, colnames = rv$dash.indicateur$dash.label,
                rownames = FALSE, options = list(dom = 't'))
    )
    
    output$dt_dem_hd = renderDT(
      datatable(rv$dash.indicateur$df.dem.tab.hd, colnames = rv$dash.indicateur$dash.label,
                rownames = FALSE, options = list(dom = 't'))
    )
    
    output$g_dem_bg = renderPlotly({
      ggplotly(rv$dash.indicateur$g.dem.pyramide)
    })
    
    output$dt_dem_bd = renderDT(
      datatable(rv$dash.indicateur$df.dem.tab.bd, colnames = rv$dash.indicateur$dash.label,
                rownames = FALSE, options = list(dom = 't'))
    )
    
    # Theme Emploi
    #-------------
    output$ib_emp_pop_trav <- renderInfoBox({
      infoBox(title = "Population en âge de travailler", value = rv$dash.indicateur$vb.emp.popTrav,
              icon = icon("fa-user-cog"),fill=TRUE)
    })
    
    output$ib_emp_chomeur <- renderInfoBox({
      infoBox(title = "Part de chômeur (au sens du RP)", value = rv$dash.indicateur$vb.emp.chom,
              icon = icon("fa-people-carry"),fill=TRUE)
    })
    
    output$ib_emp_inactif <- renderInfoBox({
      infoBox(title = "Inactifs", value = rv$dash.indicateur$vb.emp.inact,
              icon = icon("user"),fill=TRUE)
    })
    
    output$dt_emp_hg = renderDT(
      datatable(rv$dash.indicateur$df.emp.tab.hg, colnames = rv$dash.indicateur$dash.label,
                rownames = FALSE, options = list(dom = 't'))
    )
    
    output$dt_emp_hd = renderDT(
      datatable(rv$dash.indicateur$df.emp.tab.hd, colnames = rv$dash.indicateur$dash.label,
                rownames = FALSE, options = list(dom = 't'))
    )
    
    output$g_emp_bg = renderPlotly({
      ggplotly(rv$dash.indicateur$g.emp.typeAct)
    })
    
    output$dt_emp_bd = renderDT(
      datatable(rv$dash.indicateur$df.emp.tab.bd, colnames = rv$dash.indicateur$dash.label,
                rownames = FALSE, options = list(dom = 't'))
    )
    
    # Theme Scolarisation
    #--------------------
    output$ib_sco_pop_sco <- renderInfoBox({
      infoBox(title = "Population en âge d'être scolarisée", value = rv$dash.indicateur$vb.sco.popSco,
              icon = icon("child"),fill=TRUE)
    })
    
    output$ib_sco_etud <- renderInfoBox({
      infoBox(title = "Jeunes inscrit dans un établissement scolaire", value = rv$dash.indicateur$vb.sco.etud,
              icon = icon("user-graduate"),fill=TRUE)
    })
    
    output$ib_sco_decrocheur <- renderInfoBox({
      infoBox(title = "Taux de décrocheur", value = rv$dash.indicateur$vb.sco.decrocheur,
              icon = icon("user-slash"),fill=TRUE)
    })
    
    output$dt_sco_hg = renderDT(
      datatable(rv$dash.indicateur$df.sco.tab.hg, colnames = rv$dash.indicateur$dash.label,
                rownames = FALSE, options = list(dom = 't'))
    )
    
    output$g_sco_hd = renderPlotly({
      ggplotly(rv$dash.indicateur$g.sco.pop)
    })
    
    output$g_sco_bg = renderPlotly({
      ggplotly(rv$dash.indicateur$g.sco.diplome)
    })
    
    output$dt_sco_bd = renderDT(
      datatable(rv$dash.indicateur$df.sco.tab.bd, colnames = rv$dash.indicateur$dash.label,
                rownames = FALSE, options = list(dom = 't'))
    )
    
    # Theme Logement
    #---------------
    output$ib_log_pop <- renderInfoBox({
      infoBox(title = "Nombre de logements", value = "TODO",
              icon = icon("child"),fill=TRUE)
    })
    
    output$ib_log_hlm <- renderInfoBox({
      infoBox(title = "HLM", value = rv$dash.indicateur$vb.log.hlm,
              icon = icon("user-graduate"),fill=TRUE)
    })
    
    output$ib_log_maison <- renderInfoBox({
      infoBox(title = "Taux de décrocheur", value = rv$dash.indicateur$vb.log.maison,
              icon = icon("user-slash"),fill=TRUE)
    })
    
    output$dt_log_hg = renderDT(
      datatable(rv$dash.indicateur$df.log.tab.hg, colnames = rv$dash.indicateur$dash.label,
                rownames = FALSE, options = list(dom = 't'))
    )
    
    output$g_log_hd = renderPlotly({
      ggplotly(rv$dash.indicateur$g.log.cat)
    })
    
    output$g_log_bg = renderPlotly({
      ggplotly(rv$dash.indicateur$g.log.ach)
    })
    
    output$g_log_bd = renderPlotly({
      ggplotly(rv$dash.indicateur$g.log.bati)
    })
    
    # Theme Résidences principales
    #-----------------------------
    output$ib_res_pop <- renderInfoBox({
      infoBox(title = "Nombre de logements", value = rv$dash.indicateur$vb.res.part,
              icon = icon("child"),fill=TRUE)
    })
    
    output$ib_res_collectif <- renderInfoBox({
      infoBox(title = "Logements collectifs", value = rv$dash.indicateur$vb.res.collectif,
              icon = icon("user-graduate"),fill=TRUE)
    })
    
    output$ib_res_todo <- renderInfoBox({
      infoBox(title = "TODO", value = "TODO",
              icon = icon("user-slash"),fill=TRUE)
    })
    
    output$dt_res_hg = renderDT(
      datatable(rv$dash.indicateur$df.res.tab.hg, colnames = rv$dash.indicateur$dash.label,
                rownames = FALSE, options = list(dom = 't'))
    )
    
    output$g_res_hd = renderPlotly({
      ggplotly(rv$dash.indicateur$g.res.nbp)
    })
    
    output$g_res_bg = renderPlotly({
      ggplotly(rv$dash.indicateur$g.res.surf)
    })
    
    output$dt_res_bd = renderDT(
      datatable(rv$dash.indicateur$df.res.tab.bd, colnames = rv$dash.indicateur$dash.label,
                rownames = FALSE, options = list(dom = 't'))
    )
    

    
    
  })
  
  observeEvent(c(input$si_typeZone),{
    test <- !rv$df.zone$idZonage %in% rv$zone.etude
    cat <- unique(rv$df.zone$idZonage[test])
    # names(cat) <- unique(rv$df.zone$idZonage.name[test])
    updateSelectInput(session, "si_zone",
                      choices = cat
    )
  })
  
} # End server



# Application
shinyApp(ui = ui, server = server)







