library(shiny)
library(DT)
library(tidyverse)
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
                       choices = c("Commune","Departement","QPV","Zone"),
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
    
    # 0. Reactive Values
    #----------------------------------------------------------------------------------------------------------------------------
    rv <- reactiveValues(df.explore=NULL)
    

    
    # Affichage de la table + titre
    observeEvent(c(input$si_categorie,input$si_domaine),{
      
      if(input$si_categorie != ""){
  
        # Selection des données
        df <- indStat$indicateur_stat %>% 
          select(source,domaine,categorie,com,idZonage,indicateur,type.indicateur,value) %>% 
          filter(domaine == input$si_domaine & categorie == input$si_categorie) 
        
        # selection des libelles de colonnes
        type.ind <- typInd[typInd %in% unique(df$type.indicateur)]
        
        # Construction du tableau final
        df <- df %>% 
          spread(key = type.indicateur, value = value) %>% 
          select(-domaine,-categorie)
        
        # Affichage du titre de la
        output$TO_titleTab <- renderText({lstCategorie$titreTab[lstCategorie$idDomaine == input$si_domaine & 
                                                                   lstCategorie$idCategorie == input$si_categorie]})
        # Affichage
        output$table = renderDT(
          datatable(df,
                    colnames = type.ind,
                    extensions = 'Buttons',
                    options = list(
                      scrollX = TRUE,
                      # fixedColumns = TRUE,
                      # autoWidth = TRUE,
                      ordering = FALSE,
                      dom = 'lBfrtip',
                      buttons = c(I('colvis'),'excel')),
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

