#---------------------------------------------------------------------------------------------------------------------------#
#                                             Agate - Graphic web interface                                                 #
#---------------------------------------------------------------------------------------------------------------------------#

ui <- navbarPage("Agate",theme = "cosmo",collapsible=TRUE,
                 
                 # I. Interactive web map
                 #-------------------------------------------------------------------------------------------------------------
                 tabPanel("Carte",value="vis",
                          
                          div(class="outer",
                              # NB : Leaflet map need agate.css to fit windows
                              tags$head(includeCSS("www/agate.css")),
                              # I.1. Leaflet map
                              #-----------------
                              leafletOutput("mymap", width = "100%", height = "100%"),
                              
                              # I.2. Statistical controls
                              #--------------------------
                              absolutePanel(top = 30, right = 20,height=200, width=400,
                                            # Affichage du logo Insee
                                            #img(src = "Logo_Insee.png", height = 72, width = 72,align="right"),
                                            
                                            wellPanel(
                                              # I.2.1. Import ShapeFile
                                              #------------------------
                                              fileInput('file1', 'Import shapeFile',multiple = T),
                                              
                                              
                                              # I.2.2. Statistical calculation
                                              #--------------------------------------------------
                                              actionButton("b_statInfra", "Calculer !"),
                                              
                                              # I.2.3. Sweet pop-up
                                              #--------------------
                                              useSweetAlert()
                                              
                                            ),
                                            style = "opacity: 0.75; z-index: 1000;" # IMPORTANT : Absolute panel not hidden by tiles
                              )
                              
                              
                          ), # end div
                          
                          # III.4. Advanced options
                          #------------------------
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
                                  
                          )
                          
                         
                        ),
                          
                          
                  # II. Documentation
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
