library(shiny)
library(DT)
library(readxl)

# Chargement des options
load("Data/Liste indicateurs statistiques/lstIndicateur.RData")
dmn <- lstDomaine$idDomaine
names(dmn) <- lstDomaine$labelDomaine


shinyApp(
  
  ui = fluidPage( fluidRow(
    column(4,
           # II.1. Zone selection
           #---------------------
           selectInput("SI_ZoneSelect", "Zone selection",
                       choices = c("Commune","Departement","HorsZone","Zone"),
                       selected = c("Zone"))
    ),
    column(4,
           
           # II.2. Domaine selection
           #------------------------
           selectInput("si_domaine", "Domaine",
                       choices = c("Choice" ="",dmn),
                       selected = c(""))
    ),
    column(4,
           # II.3. Categorie selection
           #--------------------------
           selectInput("si_categorie", "Catégorie",
                       choices = c("Choice" =""),
                       selected = c(""))
    )
  ),
  
  hr(), # Line between buttons and plot
  
  # II.4. Table visualisation
  #--------------------------
  textOutput("TO_titleTab"),
  tags$head(tags$style("#TO_titleTab{font-size: 30px;font-style: bold;}")),
  DT::dataTableOutput("table")
                 
                 ),
  

  
  server = function(session,input, output) {
    
    load("Data/Tmp/qpv_stat_tmp.RData")
    
    # Affichage de la table + titre
    observeEvent(c(input$si_categorie,input$si_domaine),{
      
      if(input$si_categorie != ""){
  
        # Construction de la table
        df <- indStat$indicateur_stat %>% 
          select(source,domaine,categorie,com,idZonage,indicateur,type.indicateur,value) %>% 
          filter(domaine == input$si_domaine & categorie == input$si_categorie) %>% 
          spread(key = type.indicateur, value = value) %>% 
          select(-domaine,-categorie)
        
        # Affichage du titre de la
        output$TO_titleTab <- renderText({lstCategorie$titreTab[lstCategorie$idDomaine == input$si_domaine & 
                                                                   lstCategorie$idCategorie == input$si_categorie]})
      
        # Affichage
        output$table = renderDT(
          datatable(df, extensions = 'Buttons',
                    options = list(
                      scrollX = TRUE,
                      # fixedColumns = TRUE,
                      # autoWidth = TRUE,
                      ordering = FALSE,
                      dom = 'lBfrtip',
                      buttons = c(I('colvis'),'excel', 'pdf')),
                    rownames= FALSE)
        )
      } # end if

    })
    
    # Update Si_categorie
    observeEvent(input$si_domaine,{
      
      print(input$si_domaine)
      cat <- lstCategorie$idCategorie[lstCategorie$idDomaine == input$si_domaine]
      names(cat) <- lstCategorie$labelCategorie[lstCategorie$idDomaine == input$si_domaine]
      updateSelectInput(session, "si_categorie",
                        choices = cat
      )
    })
  }
)

