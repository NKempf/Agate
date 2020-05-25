ui <- tagList(
  introjsUI(),
  useShinyjs(),
  navbarPageWithInputs(
    "Agate",
    
    # I. Carte
    #-------------------------------------------------------------------------------------------------------------------------------------------------
    tabPanel(
      "Carte",
      div(class="outer",
          tags$head(includeCSS("www/agate.css")),
          # I.1.1. Leaflet map
          #-------------------
          leafletOutput("llo_agateMap", width = "100%", height = "100%"),
          
          # I.1.2. Statistical controls
          #----------------------------
          absolutePanel(id = "ap_controls",
                        draggable = TRUE,top = 20, right = 10,width = 300,#height = 500,
                        useSweetAlert(),
                        # h3("Navigation"),
                        fluidRow(id="fr_typeZone",
                                 radioGroupButtons(
                                   inputId = "rg_typeZone",
                                   choices = c("Utilisateur" = 1,
                                               "Prédéfini" = 2),
                                   justified = TRUE
                                 )
                        ),
                        # I.1.2.1 UTILISATEUR
                        fluidRow(id = "fr_utilimportMap",
                                 column(12,
                                        fileInput('fi_userMap', 'Importer un shapeFile',multiple = T),
                                        selectInput(inputId = "si_userMap_id", label = "Identifiant", 
                                                    choices = c("Choice" =""),selected = c("")),
                                        selectInput(inputId = "si_userMap_name", label = "Libellé", 
                                                    choices = c("Choice" =""),
                                                    selected = c("")),
                                        fluidRow(
                                          column(6,
                                                 switchInput(
                                                   inputId = "swi_userMapEdit",
                                                   value = FALSE,label = "Editer",onStatus = "success"
                                                 )
                                          ),
                                          column(6,
                                                 
                                                 switchInput(
                                                   inputId = "swi_heatPoint",
                                                   value = FALSE,label = "Chaleur",onStatus = "warning"
                                                 )
                                          )
                                        )     
                                 )
                        ),
                        fluidRow(id = "fr_utilStat",
                                 column(12,
                                        
                                        selectInput(inputId = "si_rp", label = "Recensement de la population", 
                                                    choices = c("2015" = "15","2014" = "14","2013" = "13"),
                                                    selected = "15" ),
                                        pickerInput(inputId = "pi_userMapSelect",
                                                    label = "Selection des zones à calculer", 
                                                    choices = c("Choice" =""),
                                                    options = list(
                                                      `actions-box` = TRUE), 
                                                    multiple = TRUE),
                                        actionButton("ab_userStat","Indicateurs statistiques")
                                 ))
                        ,
                        # I.1.2.2 Zones prédéfinies
                        hidden(
                          fluidRow(id = "fr_predOption",
                                   column(12,
                                          selectInput("si_zonePred", "Selectionner une maille",
                                                      choices = c("",pred.choice),
                                                      selected =c(4)
                                          ),
                                          
                                          selectInput("si_zonePred_dep", "Selectionner une zone géographique",
                                                      choices = "",
                                                      selected = ""
                                          ) 
                                   )
                          )
                        ),
                        # Affichage / réduction du menu
                        prettyToggle(
                          inputId = "pt_hideMenu",
                          label_on = "Réduire", 
                          label_off = "Afficher",
                          outline = TRUE,
                          plain = TRUE,value = TRUE,status_on = "success",status_off = "success",
                          icon_on = icon("arrow-up"), 
                          icon_off = icon("arrow-down")
                        )
          ) # end absolutepanel 
      ) %>% withSpinner(type = 6,size = 2,proxy.height = 100) # end div
      
      
      
      # IV. Dashboard
      #-------------------------------------------------------------------------------------------------------------------------------------------------
      ,
      tags$head(tags$style("#bs_dashboard .modal-footer{ display:none}
                         #bs_dashboard .modal-header{ display:none}
                         #bs_dashboard { opacity:0.95}")), # Remove BS modal footer
      tags$head(tags$style(HTML('.modal-lg {width: 90%;}'))), # Increase modal size
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
                                              infoBoxOutput(outputId = "ib_dem_agemed")
                                            ) ,
                                            fluidRow(
                                              box(id="b_test",title = "Individus", solidHeader = TRUE,
                                                  collapsible = TRUE,
                                                  DT::dataTableOutput("dt_dem_hg") %>% withSpinner(type = 6) 
                                              ),
                                              box(title = "Ménages", solidHeader = TRUE,
                                                  collapsible = TRUE,
                                                  DT::dataTableOutput("dt_dem_hd") %>% withSpinner(type = 6) 
                                              )
                                            ),
                                            fluidRow(
                                              box(title = "Structure de la population", solidHeader = TRUE,
                                                  collapsible = TRUE,
                                                  plotlyOutput("g_dem_bg") %>% withSpinner(type = 6) 
                                              ),
                                              box(title = "Étrangers - immigrés", solidHeader = TRUE,
                                                  collapsible = TRUE,
                                                  DT::dataTableOutput("dt_dem_bd") %>% withSpinner(type = 6) 
                                              )
                                            ),
                                            fluidRow(
                                              textOutput("to_source_dem")
                                            )
                                            
                                   ),
                                   # III. Thème  Emploi
                                   #-----------------------------------------------------------------------------------------------------
                                   tabPanel("Emploi",
                                            
                                            fluidRow(
                                              infoBoxOutput(outputId = "ib_emp_pop_trav"),
                                              infoBoxOutput(outputId = "ib_emp_chomeur"),
                                              infoBoxOutput(outputId = "ib_emp_actif")
                                            ),
                                            fluidRow(
                                              box(title = "Marché de l'emploi", solidHeader = TRUE,
                                                  collapsible = TRUE,
                                                  DT::dataTableOutput("dt_emp_hg") %>% withSpinner(type = 6) 
                                              ),
                                              box(title = "Chômage", solidHeader = TRUE,
                                                  collapsible = TRUE,
                                                  DT::dataTableOutput("dt_emp_hd") %>% withSpinner(type = 6) 
                                              )
                                            ),
                                            fluidRow(
                                              box(title = "Type d'activité", solidHeader = TRUE,
                                                  collapsible = TRUE,
                                                  plotlyOutput("g_emp_bg") %>% withSpinner(type = 6) 
                                              ),
                                              box(title = "Conditions d'emploi", solidHeader = TRUE,
                                                  collapsible = TRUE,
                                                  DT::dataTableOutput("dt_emp_bd") %>% withSpinner(type = 6) 
                                              )
                                            ),
                                            fluidRow(
                                              textOutput("to_source_emp")
                                            )
                                   ),
                                   
                                   # IV. Thème Scolarisation
                                   #-----------------------------------------------------------------------------------------------------
                                   tabPanel("Education - formation",
                                            
                                            fluidRow(
                                              infoBoxOutput(outputId = "ib_sco_pop_sco"),
                                              infoBoxOutput(outputId = "ib_sco_etud"),
                                              infoBoxOutput(outputId = "ib_sco_decrocheur")
                                            ),
                                            fluidRow(
                                              box(title = "Jeunes scolarisés", solidHeader = TRUE,
                                                  collapsible = TRUE,
                                                  DT::dataTableOutput("dt_sco_hg") %>% withSpinner(type = 6) 
                                              ),
                                              box(title = "Jeunes non scolarisés", solidHeader = TRUE,
                                                  collapsible = TRUE,
                                                  DT::dataTableOutput("dt_sco_bd") %>% withSpinner(type = 6) 
                                              )
                                            ),
                                            fluidRow(
                                              box(title = "Niveau de diplôme", solidHeader = TRUE,
                                                  collapsible = TRUE,
                                                  plotlyOutput("g_sco_bg") %>% withSpinner(type = 6) 
                                              ),
                                              
                                              box(title = "Jeunes", solidHeader = TRUE,
                                                  collapsible = TRUE,
                                                  plotlyOutput("g_sco_hd") %>% withSpinner(type = 6) 
                                              )
                                            ),
                                            fluidRow(
                                              textOutput("to_source_sco")
                                            )
                                   ),
                                   
                                   # V. Thème Logement
                                   #-----------------------------------------------------------------------------------------------------
                                   tabPanel("Logement",
                                            
                                            fluidRow(
                                              infoBoxOutput(outputId = "ib_log_pop"),
                                              infoBoxOutput(outputId = "ib_log_vac"),
                                              infoBoxOutput(outputId = "ib_log_maison")
                                            ),
                                            fluidRow(
                                              box(title = "Caractéristiques des logements", solidHeader = TRUE,
                                                  collapsible = TRUE,
                                                  DT::dataTableOutput("dt_log_hg") %>% withSpinner(type = 6) 
                                              ),
                                              box(title = "Catégorie de logement", solidHeader = TRUE,
                                                  collapsible = TRUE,
                                                  plotlyOutput("g_log_hd") %>% withSpinner(type = 6) 
                                              )
                                            ),
                                            fluidRow(
                                              box(title = "Année d'achevement", solidHeader = TRUE,
                                                  collapsible = TRUE,
                                                  plotlyOutput("g_log_bg") %>% withSpinner(type = 6) 
                                              ),
                                              box(title = "Aspect du bâti", solidHeader = TRUE,
                                                  collapsible = TRUE,
                                                  plotlyOutput("g_log_bd") %>% withSpinner(type = 6) 
                                              )
                                            ),
                                            fluidRow(
                                              textOutput("to_source_log")
                                            )
                                   ),
                                   # V. Thème Résidences principales
                                   #-----------------------------------------------------------------------------------------------------
                                   tabPanel("Résidences principales",
                                            fluidRow(
                                              infoBoxOutput(outputId = "ib_res_pop"),
                                              infoBoxOutput(outputId = "ib_res_collectif"),
                                              infoBoxOutput(outputId = "ib_res_persmoy")
                                            ),
                                            fluidRow(
                                              box(title = "Caractéristiques des résidences principales", solidHeader = TRUE,
                                                  collapsible = TRUE,
                                                  DT::dataTableOutput("dt_res_hg") %>% withSpinner(type = 6) 
                                              ),
                                              box(title = "Équipements", solidHeader = TRUE,
                                                  collapsible = TRUE,
                                                  DT::dataTableOutput("dt_res_bd") %>% withSpinner(type = 6) 
                                              )
                                            ),
                                            fluidRow(
                                              box(title = "Surface", solidHeader = TRUE,
                                                  collapsible = TRUE,
                                                  plotlyOutput("g_res_bg") %>% withSpinner(type = 6) 
                                              ),
                                              box(title = "Nombre de pièces", solidHeader = TRUE,
                                                  collapsible = TRUE,
                                                  plotlyOutput("g_res_hd") %>% withSpinner(type = 6) 
                                              )
                                            ),
                                            fluidRow(
                                              textOutput("to_source_res")
                                            )
                                   ),
                                   
                                   # VI. Zones de comparaisons
                                   #-----------------------------------------------------------------------------------------------------
                                   inputs = dropdownButton(inputId = "ddb_compare" ,
                                                           selectInput("si_typeZone", "Type de zone",
                                                                       choices = pred.choice,
                                                                       selected = c(4)),
                                                           selectInput("si_zone", "Zone de comparaison",
                                                                       choices = c("Choice" =""),
                                                                       selected = c("")),
                                                           circle = TRUE, status = "danger",
                                                           icon = icon("gear"),size = "sm",right = TRUE,
                                                           tooltip = tooltipOptions(title = "Zones de comparaison")
                                   )
              ) 
      ) # end Dashboard
      
    ),# Tabset Carte
    
    # II. Statistiques et téléchargement
    #-------------------------------------------------------------------------------------------------------------------------------------------------
    tabPanel("Statistiques",
             # value="vis", # MAJ 24 juin 2019
             sidebarPanel(width=2,id = "sp_test",
                          # II.1. Maille géographique
                          #--------------------------
                          selectInput("si_stat_zoneSelect", "Maille géographique",
                                      choices = pred.choice,
                                      selected = 1),
                          
                          # II.2. Zones étudiéds
                          #---------------------
                          pickerInput(inputId = "pi_stat_zone_etude",
                                      label = "Zones étudiées",
                                      choices = c("Choice" =""),
                                      options = list(
                                        `live-search` = TRUE,
                                        `actions-box` = TRUE), 
                                      multiple = TRUE
                          ),
                          
                          # II.3. Domaine selection
                          #------------------------
                          selectInput("si_stat_domaine", "Thématique",
                                      choices = c("Choice" ="",dmn),
                                      selected = dmn[2]),
                          
                          # II.4. Categorie selection
                          #--------------------------
                          selectInput("si_stat_categorie", "Indicateurs",
                                      choices = c("Choice" =""),
                                      selected = c(""))
             ),
             
             mainPanel(
               
               # II.4. Table visualisation
               #--------------------------
               textOutput("to_stat_title"),
               tags$head(tags$style("#to_stat_title{font-size: 30px;font-style: bold;}")),
               DT::dataTableOutput("dt_stat_explore")
             )
             
             
    ),
    # III. Documentation
    #-------------------------------------------------------------------------------------------------------------------------------------------------
    navbarMenu("Documentation",
               # III.1. indicateurs
               #-------------------
               tabPanel("Indicateurs",
                        
                        sidebarPanel(width=2,
                                     selectInput("si_ind_domaine", "Thématique",
                                                 choices = c("Choice" ="",dmn),
                                                 selected = dmn[2]),
                                     
                                     selectInput("si_ind_categorie", "Indicateurs",
                                                 choices = c("Choice" =""),
                                                 selected = c(""))
                        ),
                        mainPanel(
                          DT::dataTableOutput("dt_indicateurs") 
                        )
               )
               # ,
               # # III.2. Qualité du RP
               # #---------------------
               # tabPanel("Qualité du RP",
               #          withMathJax(includeMarkdown(qualiteRpFile))
               # )
               
               
    ),# End documentation
    # IV. Aide Agate
    #-------------------------------------------------------------------------------------------------------------------------------------------------
    # Aide bouton
    inputs = actionBttn(
      inputId = "ab_aide",
      style = "stretch",
      color = "warning",
      icon = icon("question")
    )
    
  )
)