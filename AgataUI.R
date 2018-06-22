#---------------------------------------------------------------------------------------------------------------------------#
#                                             Agate - Graphic web interface                                                 #
#---------------------------------------------------------------------------------------------------------------------------#

ui <- navbarPage("Agate",theme = "cosmo",collapsible=TRUE,
                 
                 # I. Interactive web map
                 #-------------------------------------------------------------------------------------------------------------
                 tabPanel("Carte",value="vis",
                          
                          #-----------------#
                          # I.1. Map object #
                          #-----------------#
                          div(class="outer",
                              # NB : Leaflet map need agate.css to fit windows
                              tags$head(includeCSS("www/agate.css")),
                              # I.1.1. Leaflet map
                              #-----------------
                              leafletOutput("mymap", width = "100%", height = "100%"),
                              
                              # I.1.2. Statistical controls
                              #----------------------------
                              absolutePanel(top = 30, right = 20,height=200, width=400,
                                            # Affichage du logo Insee
                                            #img(src = "Logo_Insee.png", height = 72, width = 72,align="right"),
                                            
                                            wellPanel(
                                              # I.1.2.1. Import ShapeFile
                                              #--------------------------
                                              # Import button
                                              fileInput('file1', 'Import shapeFile',multiple = T),
                                              
                                              # I.1.2.2. Statistical calculation
                                              #---------------------------------
                                              actionButton("b_statInfra", "Calculer !"),
                                              
                                              # I.1.2.3. Sweet pop-up
                                              #----------------------
                                              useSweetAlert()
                                            ),
                                            style = "opacity: 0.75; z-index: 1000;" # IMPORTANT : Absolute panel not hidden by tiles
                              ),
                              # Import modal
                              tags$head(tags$style("#bs_importShp .modal-footer{ display:none}")), # Remove BS modal footer
                              bsModal(id = "bs_importShp",title =  "Parametres de la couche cartographique", trigger = "b_statInfra", size="large",
                                      fluidRow(
                                        column(6,
                                               selectInput(inputId = "SI_id", label = "Identifiant", choices = c("Defaut"),selected = c("Defaut"))
                                        ),
                                        column(6,
                                               selectInput(inputId = "SI_name", label = "Libelle", choices = c("Defaut"),selected = c("Defaut"))
                                        )
                                      )
                              )
                              
                              
                              
                          ), # end div
                          
                          #-------------------#
                          # I.2. Modal object #
                          #-------------------#
                          
                          # I.2.1 Advanced options
                          #-----------------------
                          tags$head(tags$style("#bs_optad .modal-footer{ display:none}")), # Remove BS modal footer
                          bsModal(id = "bs_optad",title =  "Options avancées", trigger = "b_statInfra", size="large",
                                  
                                  fluidRow(
                                    # III.4.1. Instructions
                                    HTML('
                                                             <p style="text-align:justify"> Select datasets'
                                    ),
                                    # III.4.2. Ril
                                    column(4,
                                           selectInput(inputId = "SI_ril", label = "Ril", choices = c("2015"),
                                                       selected = "2015" )
                                    ),
                                    # III.4.3. Census
                                    column(4,
                                           
                                           selectInput(inputId = "SI_Rp", label = "Recensement de la population", choices = c("2014","2013"),
                                                       selected = "2014" )
                                    ),
                                    # III.4.4. Filosofi
                                    column(4,
                                           selectInput(inputId = "SI_filo", label = "Fichiers fiscaux",choices = c("2014"),
                                                       selected="2014")
                                    ),
                                    # III.4.5. Compute
                                    column(12,
                                           actionButton("b_calcul", "Compute !")
                                    )
                                    
                                  )
                          ),
                          
                          # I.2.2 Dashboard
                          #----------------
                          # tags$head(tags$style("#boxPopUp1 .modal-dialog{ width:1000px}")),
                          # tags$head(tags$style("#boxPopUp1 .modal-body{ min-height:700px}")),
                          tags$head(tags$style("#boxPopUp1 .modal-footer{ display:none}")), # Remove BS modal footer
                          tags$head(tags$style(HTML('

                                                    .modal-lg {
                                                    width: 80%;
                                                    
                                                    }
                                                    '))), # Increase modal size
                          bsModal('boxPopUp1', textOutput("modalTitle"),'test',size = "large",
                                  # Add CSS files : use infobox from shinydashboard package into a shinyApp
                                  includeCSS(path = "www/AdminLTE.css"),
                                  includeCSS(path = "www/shinydashboard.css"),
                                  
                                  # I.2.2.1 Infobox
                                  #----------------
                                  fluidRow(
                                    infoBoxOutput(outputId = "IB_pop"),
                                    infoBoxOutput(outputId = "IB_rev"),
                                    infoBoxOutput(outputId = "IB_chom")
                                  ),
                                  fluidRow(
                                    # I.2.2.2 Comparison field
                                    #-------------------------
                                           selectInput(inputId = "SI_comp", label = "Zone de comparaison", choices = c("Commune","Departement","HorsZone"),selected = c("Commune"))
                                  ),
                                  
                                  fluidRow(
                                    # Distribution du niveau de vie
                                    box(
                                      title = "Niveau de vie annuel (en euros)", status = "primary", solidHeader = TRUE,
                                      collapsible = TRUE,
                                      plotlyOutput("plotly1")
                                    ),
                                    # Pyramide des ages
                                    box(
                                      title = "Pyramide des ages", status = "primary", solidHeader = TRUE,
                                      collapsible = TRUE,
                                      plotlyOutput("plotly2")
                                    )
                                  ),
                                  
                                  fluidRow(
                                    # Informations sur les individus
                                    box(
                                      title = "Informations sur les individus", status = "primary", solidHeader = TRUE,
                                      collapsible = TRUE,
                                      plotlyOutput("plotly3")
                                    ),
                                    # Informations sur les logements
                                    box(
                                      title = "Informations sur les logements", status = "primary", solidHeader = TRUE,
                                      collapsible = TRUE,
                                      plotlyOutput("plotly4")
                                    )
                                  ),
                                  
                                  
                                  fluidRow(
                                    # I.2.2.3 Download Dashboard
                                    #---------------------------
                                    column(3,downloadButton("dl_dash", "Get Dashboard", class="btn-block"))
                                  )
                          )
                         
                        ),
                 
                 # II. Statistics
                 #-------------------------------------------------------------------------------------------------------------                 
                 tabPanel("Statistiques",value="vis",
                          
                          # Sidebar layout with input and output definitions ----
                          sidebarLayout(
                            
                            # Sidebar panel for inputs ----
                            sidebarPanel(width = 3,
                              
                              # II.1. Zone selection
                              #---------------------
                              selectInput("SI_ZoneSelect", "Zone selection",
                                          choices = c("Commune","Departement","HorsZone","Zone"),
                                          selected = c("Zone")),
                              
                              # II.2. Tab selection
                              #--------------------
                              selectInput("SI_TabSelect", "Tab selection",
                                          choices = c(""),
                                          selected = c("")),
                              
                              # Button
                              downloadButton(outputId = "DL_StatReport","Download report")
                              
                            ),
                            
                            # Main panel for displaying outputs ----
                            mainPanel(width = 9,
                              DT::dataTableOutput("table")
                              #tableOutput("table")
                            )
                            
                          )
                          
                          
                          
                          ),
                 
                 # III. Qualité
                 #-------------------------------------------------------------------------------------------------------------                 
                 tabPanel("Qualité",value="vis",
                          titlePanel("To do")
                 ),                       
                          
                  # III. Documentation
                  #-------------------------------------------------------------------------------------------------------------
                  navbarMenu("Documentation",

                             tabPanel("Ameliorarion",
                                      includeHTML("Html pages/AmeliorationAppli/Amelioration.html")
                             ),
                             tabPanel("Maintenance Agate",
                                      includeHTML("Html pages/Maintenance/MaintenanceAgate.html")
                             ),
                             tabPanel("Maintenance des reseaux routiers",
                                      includeHTML("Html pages/Maintenance/MaintenanceResRoute.html")
                             ),
                             tabPanel("Remerciements",
                                      includeHTML("Html pages/Remerciements/Remerciements.html")
                             )
                             
                             
                  )
         )
