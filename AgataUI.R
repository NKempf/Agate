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
                              absolutePanel(top = 30, right = 20,
                                            # Affichage du logo Insee
                                            #img(src = "Logo_Insee.png", height = 72, width = 72,align="right"),
                                            
                                            # III.1. Import carte en geojson
                                            #------------------------
                                            fileInput('file1', 'Importer une carte au format geojson',multiple = T),
                                            
                                            # III.2. Lancement du calcul des statistiques infra
                                            #--------------------------------------------------
                                            actionButton("b_statInfra", "Statistiques infra communales")
                              )
                              
                              
                          ) # end div
                         
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
