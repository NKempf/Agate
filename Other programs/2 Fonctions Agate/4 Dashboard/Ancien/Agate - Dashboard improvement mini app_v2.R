# Paramètre temporaire
zone.selection <- 3
zone.etude <- "QP971002"

# 0. Label tableau
#-----------------
load("Data/Liste indicateurs statistiques/lstIndicateur.RData")
dmn <- lstDomaine$idDomaine
names(dmn) <- lstDomaine$labelDomaine

# I.1. Zone d'étude
#------------------
load("Data/Tmp/qpv_stat_tmp.RData") # Indicateurs statistiques sur deux QPV

# Selection d'une zone

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
    filter(type.indicateur != "part_np" & zone.predefine == zone.selection & idZonage == zone.compare & source %in% source.etude) %>% 
    select(-zone.predefine)
}

# Table de travail
df.dashboard <- bind_rows(df.etude,df.compare %>% mutate(dashboard = "zone.etude2"))

# Label des colonnes 
dash.label <- c("indicateur","zone.etude1","zone.etude2")
names(dash.label) <- c("Indicateurs",
                       unique(df.etude$idZonage.name[df.etude$idZonage == zone.etude]),
                       unique(df.compare$idZonage.name[df.compare$idZonage == zone.compare]))



#------------------------------------------------------------------------------------------------------------------------------------


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
                                     infoBoxOutput(outputId = "ib_pop")
                            ),
                            
                            # II.2. Selection de la zone de comparaison
                            #------------------------------------------
                            fluidRow(
                              column(6,
                                     # II.2.1. Type de zone
                                     selectInput("si_typeZone", "Type de zone",
                                                 c(pred.choice),
                                                 selected = 1)
                              ),
                              column(6,
                                     # II.2.2. Zone
                                     selectInput("si_zone", "Zone",
                                                 choices = c("Choice" =""),
                                                 selected = c(""))
                              )
                            )
                   
                            
                   )
  ),
  
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
  
  server = function(input, output, session) {
    
    # 0. Reactive Values
    #----------------------------------------------------------------------------------------------------------------------------
    rv <- reactiveValues(zone.etude = NULL,
                         df.dashboard=NULL)
    
    
    
    
    # Titre du dashboard
    #TODO
    output$to_titleDash <- renderText({ 
      paste0(unique(df.etude$idZonage.name)[1]," - tableau de bord")
    })
   
    # I. Comparaison
    #---------------------------------------------------------------------------------------------------------------------------
    
    # MAJ Liste des zones
    observeEvent(input$si_typeZone,{
      
      if(input$si_typeZone == "4"){ #Si la zone de comparaison fait partie des zones d'études
        tmp <- indStat$indicateur_stat %>% 
          filter(!idZonage %in% c("Hors zonage",zone.etude)) %>% 
          select(idZonage,idZonage.name) %>% 
          filter(!duplicated(idZonage))
        cat <- tmp$idZonage
        names(cat) <- tmp$idZonage.name
        
      }else{
        switch (input$si_typeZone,
          "1" = {
            var.compare <- unique(zonage.com$dep[zonage.com$idZonage %in% zone.etude])
            tmp <- read_fst("Data/Stats/Prefine aera/Real/fst/indicateur_stat.fst") %>% 
              filter(zone.predefine == input$si_typeZone & idZonage %in% var.compare) %>% 
              select(idZonage,idZonage.name) %>% 
              filter(!duplicated(idZonage))
            cat <- tmp$idZonage
            names(cat) <- tmp$idZonage.name
          },
          "2" = {
            var.compare <- unique(zonage.com$com[zonage.com$idZonage %in% zone.etude])
            tmp <- read_fst("Data/Stats/Prefine aera/Real/fst/indicateur_stat.fst") %>% 
              filter(zone.predefine == input$si_typeZone & idZonage %in% var.compare) %>% 
              select(idZonage,idZonage.name) %>% 
              filter(!duplicated(idZonage))
            cat <- tmp$idZonage
            names(cat) <- tmp$idZonage.name
          },
          {
            tmp <- read_fst("Data/Stats/Prefine aera/Real/fst/indicateur_stat.fst") %>% 
              filter(zone.predefine == input$si_typeZone & idZonage != "Hors zonage") %>% 
              select(idZonage,idZonage.name) %>% 
              filter(!duplicated(idZonage))
            cat <- tmp$idZonage
            names(cat) <- tmp$idZonage.name
          }
        )
      }

      updateSelectInput(session, "si_zone",
                        choices = cat
      )
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
    
    
    
   
  } # End server
)
