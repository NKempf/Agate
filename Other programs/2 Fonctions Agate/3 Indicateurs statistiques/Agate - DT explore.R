library(shiny)
library(DT)
library(tidyverse)
library(fst)
library(fstplyr)
library(readxl)

# Chargement des options
load("Data/Liste indicateurs statistiques/lstIndicateur.RData")
dmn <- lstDomaine$domaine
names(dmn) <- lstDomaine$labelDomaine


shinyApp(
  
  ui = fluidPage( fluidRow(
    column(4,
           # II.1. Zone selection
           #---------------------
           selectInput("si_zoneSelect", "Zone",
                       choices = pred.choice,
                       selected = 4)
    ),
    column(4,
           
           # II.2. Domaine selection
           #------------------------
           selectInput("si_domaine", "Domaine",
                       choices = c("Choice" ="",dmn),
                       selected = dmn[2])
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
    rv <- reactiveValues(source = NULL,
                         df.zone=NULL)
    
    # I. Import des tables
    #----------------------------------------------------------------------------------------------------------------------------
    load("Data/Tmp/qpv_stat_tmp.RData")
    # typInd <- lstTypeIndicateur$typeIndicateur
    # names(typInd) <- lstTypeIndicateur$labelTypeIndicateur
    
    observe({
      rv$df.zone <- df.zone
      rv$source <- unique(df.zone$source)
    })
    
    
    # Update Si_categorie
    observeEvent(input$si_domaine,{
      cat <- lstCategorie$categorie[lstCategorie$domaine == input$si_domaine]
      names(cat) <- lstCategorie$labelCategorie[lstCategorie$domaine == input$si_domaine]
      
      updateSelectInput(session, "si_categorie",
                        choices = cat
      )
    })
    

    # Affichage de la table + titre
    observeEvent(c(input$si_categorie,input$si_domaine,input$si_zoneSelect),{

      if(input$si_categorie != ""){
        
        type.indicateur.filtre <- c("freq","part_np","CoefVariation","IntervalConf.","valeur.diffusable")
        # type.indicateur.filtre <- c("freq","part_np")
        
        if(input$si_zoneSelect == 4){
          # Selection des données
          df <- rv$df.zone %>%
            select(source,domaine,categorie,idZonage,idZonage.name,nomIndicateur,type.indicateur,value) %>%
            filter(type.indicateur %in% type.indicateur.filtre) %>%
            filter(domaine == input$si_domaine & categorie == input$si_categorie)
          
        }else{
          df <- read_fst("Data/Stats/Prefine aera/Real/fst/indicateur_stat.fst") %>%
            filter(zone.pred == input$si_zoneSelect & domaine == input$si_domaine &
                     categorie == input$si_categorie & source %in% rv$source) %>%
            filter(type.indicateur != "part_np") %>%
            select(source,domaine,categorie,idZonage,idZonage.name,nomIndicateur,type.indicateur,value)
        }

        # selection des libelles de colonnes
        typInd <- lstTypeIndicateur$typeIndicateur
        names(typInd) <- lstTypeIndicateur$labelTypeIndicateur
        type.ind <- typInd[typInd %in% c("idZonage","idZonage.name",unique(df$type.indicateur))]
        print(type.ind)

        df <- df %>%
          spread(key = type.indicateur, value = value) %>%
          left_join(lstIndicateur %>% select(nomIndicateur,labelIndicateur),"nomIndicateur") %>%
          mutate(nomIndicateur = labelIndicateur) %>%
          select(-labelIndicateur,-domaine,-categorie)
        
        # Affichage du titre de la
        output$TO_titleTab <- renderText({lstCategorie$titreTab[lstCategorie$domaine == input$si_domaine &
                                                                  lstCategorie$categorie == input$si_categorie]})
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


  }
)

