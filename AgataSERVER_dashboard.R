#---------------------------------------------------------------------------------------------------------------------------#
#                                             Agate - Server Dashboard                                                      #
#---------------------------------------------------------------------------------------------------------------------------#

# Include server code for plotting dashboard graphs


# I. Modal dashboard
#---------------------------------------------------------------------------------------------------------------------------------------------------

# I.1. BoxPlot Income
#--------------------
plotlyBoxplotIncome <- function(input, output, session){
  
  # Exemple test
  p <- plot_ly(midwest, x = ~percollege, color = ~state, type = "box")
  p 
  
  
  # # Paramètres généraux
  # idZonage <- "QP973010"
  # typeVar.switch <- "dep"
  # 
  # 
  # df_boxplot <- function(df,varKeep){
  #   df <- as.data.frame(df)
  #   df$Q_0.25_2 <- df$Q_0.25
  #   df$Q_0.75_2 <-  df$Q_0.75
  #   varQuantile <- c("Q_0.1","Q_0.25","Q_0.25_2","Q_0.5","Q_0.75","Q_0.75_2","Q_0.9")
  #   df[,varQuantile] <- round(df[,varQuantile],digits = -1)
  #   return(df[,as.character(c(varKeep,varQuantile))])
  # }
  # 
  # # Trace zonage
  # trace.name.zonage <- idZonage
  # varKeep.zone <- c("com","idZonage")
  # zone.boxplot <- df_boxplot(df = StatZona$tFilo.I.1,varKeep = varKeep.zone) 
  # y <- as.character(zone.boxplot[zone.boxplot$idZonage == idZonage,!colnames(zone.boxplot) %in% varKeep.zone])
  # 
  # switch(typeVar.switch,
  #        dep = {
  #          varKeep <- c("dep")
  #          trace.name <- substr(zone.boxplot$com[zone.boxplot$idZonage == idZonage],1,3)
  #          reg.boxplot <- df_boxplot(df = statReg_rp14_filo14$tFilo.I.1,varKeep = varKeep)
  #          z <-  as.character(reg.boxplot[reg.boxplot$dep == trace.name,!colnames(reg.boxplot) %in% varKeep])
  #          
  #        },
  #        com = {
  #          print("todo")
  #        },
  #        hzone = {
  #          print("todo")
  #        }
  #        ,
  #        {
  #          z <- as.character(zone.boxplot[zone.boxplot$idZonage == typeVar,!colnames(zone.boxplot) %in% varKeep.zone])
  #        }
  # )
  # 
  # # Graphique plotly
  # plot_ly() %>% 
  #   add_trace(y=y, name=trace.name.zonage, type="box") %>% 
  #   add_trace(y=z, name= trace.name, type="box") %>% 
  #   layout(autosize=TRUE, boxmode="group", hovermode="closest", 
  #          showlegend=TRUE,
  #          xaxis = list(
  #            autorange = TRUE, 
  #            showticklabels = FALSE, 
  #            type = "category"),
  #          yaxis = list(
  #            autorange = TRUE, 
  #            type = "linear")
  #   )
  # 

}


# I.2 Aged pyramid
#-----------------
plotlyAgedPyramid <- function(input, output, session){

  # Exemple de test
    plot_ly(
      x = c("giraffes", "orangutans", "monkeys"),
      y = c(20, 14, 23),
      name = "SF Zoo",
      type = "bar"
    )
    
    # # Pyramide des ages
    # agePyramid <- statZona$t1d_pyramide[statZona$t1e_pyramide$idZonage %in% qpv_filtre()$idZonage,]
    # 
    # # Valeur des Hommes négatives
    # agePyramid$pop[agePyramid$SEXE=="homme"] <- - agePyramid$pop[agePyramid$SEXE=="homme"]
    # 
    # # Label pour plotly
    # agePyramid$abs_pop <- paste(abs(agePyramid$pop)," ",agePyramid$SEXE,"s de ",
    #                             agePyramid$age," ans",sep="")
    # 
    # # Affichage de la pyramide
    # plot_ly(data = agePyramid,x= ~pop, y=~age,color=~SEXE,colors = c('#fb9a99','#a6cee3')) %>%
    #   add_bars(orientation = 'h', hoverinfo = 'text', text = ~abs_pop) %>%
    #   layout(bargap = 0.1, barmode = 'overlay',
    #          xaxis = list(title = "Population",tickmode = "array"),
    #          yaxis = list(title = "Age"))
}

# I.3 Informations about household
#---------------------------------
plotlyInfoPopulation <- function(input, output, session){
  
  InfoPopulation <- renderPlotly({  
      plot_ly(alpha = 0.6) %>%
      add_histogram(x = ~rnorm(500)) %>%
      add_histogram(x = ~rnorm(500) + 1) %>%
      layout(barmode = "overlay")
    
    
    
  #   # Selection des variables
  #   list_var <- c("p_etranger","p_immigre","p_cadre_prof_inter","t_decrocheur","p_Nscola_6_14","p_65plus",
  #                 "p_moins20","p_femmes")
  #   xvar <- c("Etranger","Immigre","Cadre et profession intermediaire","Decrocheur","Non scolarise de 6 a 14 ans",
  #             "65 ans et plus","Moins de 20 ans","Femmes")
  #   # Selection de la commune de la zone
  #   comZona <- statZona$filo$depcom[statZona$filo$idZonage %in% qpv_filtre()$idZonage][1]
  #   # Zonage
  #   zonage.df <- as.matrix(statZona$tStatRP[statZona$tStatRP$id %in% qpv_filtre()$idZonage,list_var])
  #   # Hors zonage
  #   Hzonage.df <- as.matrix(statZona$statHZone[statZona$statHZone$id %in% comZona, list_var])
  #   # Commune
  #   comZona.df <- as.matrix(statZona$statCom[statZona$statCom$id %in% comZona, list_var])
  #   # Autres zonages
  #   # AutZon.df <- as.matrix(statZona$statAutZon[statZona$statAutZon$id %in% qpv_filtre()$idZonage, list_var])
  #   
  #   # Base synthetique
  #   df <- data.frame(xvar=xvar,zonage=zonage.df[1,],Hzonage=Hzonage.df[1,],comZona=comZona.df[1,]
  #                    # ,AutZon=AutZon.df[1,]
  #   )
  #   
  #   # Graphique
  #   p <- plot_ly(df,x = ~zonage, y = ~xvar , type = 'bar', name = 'Zonage',
  #                marker = list(color = 'rgb(255,69,0)'),line = list(color = 'rgb(255,69,0)')) %>%
  #     add_trace(x = ~Hzonage, name = 'Hors zonage',marker = list(color = 'rgb(113,113,198)'),
  #               line = list(color = 'rgb(113,113,198)')) %>%
  #     add_trace(x = ~comZona, name = 'Commune',marker = list(color = 'rgb(113,198,113)'),
  #               line = list(color = 'rgb(113,198,113)')) %>%
  #     # add_trace(x = ~AutZon, name = 'Autres zonages',marker = list(color = 'rgb(252,141,98)'),
  #     #             line = list(color = 'rgb(252,141,98)')) %>%
  #     
  #     layout(xaxis = list(title = '%'),yaxis = list(title = '',categoryorder = "array",
  #                                                   categoryarray = xvar), barmode = 'group')
  #   # Correction de la marge pour l'affichage des libelles de la colonne y
  #   p$x$layout$margin$l <- p$x$layout$margin$l + 30
  #   # p$x$layout$margin$b <- p$x$layout$margin$b + 30
  #   p
  })
  
  return(InfoPopulation)
  
}

# I.4 Informations about housing
#-------------------------------
plotlyInfoHousing <- function(input, output, session){
  
  InfoHousing <- renderPlotly({ 

    dens <- with(diamonds, tapply(price, INDEX = cut, density))
    df <- data.frame(
      x = unlist(lapply(dens, "[[", "x")),
      y = unlist(lapply(dens, "[[", "y")),
      cut = rep(names(dens), each = length(dens[[1]]$x))
    )
    
   plot_ly(df, x = ~x, y = ~y, color = ~cut) %>%
   add_lines()
   
   # # Selection des variables
   # list_var <- c("p_N_tout_egout","p_N_Bain_douche","p_N_eau_chaude","p_surf100p","p_residHlm",
   #               "p_log_vacants","p_resid_princ")
   # xvar <- c("Pas de tout a l'egout","Pas de douche","Pas d'eau chaude","Surface de 100m² et plus",
   #           "Residence HLM","Logements vacants","Residences principales")
   # # Selection de la commune de la zone
   # comZona <- statZona$filo$depcom[statZona$filo$idZonage %in% qpv_filtre()$idZonage][1]
   # # Zonage
   # zonage.df <- as.matrix(statZona$tStatRP[statZona$tStatRP$id %in% qpv_filtre()$idZonage,list_var])
   # # Hors zonage
   # Hzonage.df <- as.matrix(statZona$statHZone[statZona$statHZone$id %in% comZona, list_var])
   # # Commune
   # comZona.df <- as.matrix(statZona$statCom[statZona$statCom$id %in% comZona, list_var])
   # # Autres zonages
   # # AutZon.df <- as.matrix(statZona$statAutZon[statZona$statAutZon$id %in% qpv_filtre()$idZonage, list_var])
   # 
   # # Base synthetique
   # df <- data.frame(xvar=xvar,zonage=zonage.df[1,],Hzonage=Hzonage.df[1,],comZona=comZona.df[1,]
   #                  # ,AutZon=AutZon.df[1,]
   # )
   # 
   # # Graphique
   # p <- plot_ly(df,x = ~zonage, y = ~xvar , type = 'bar', name = 'Zonage',
   #              marker = list(color = 'rgb(255,69,0)'),line = list(color = 'rgb(255,69,0)')) %>%
   #   add_trace(x = ~Hzonage, name = 'Hors zonage',marker = list(color = 'rgb(113,113,198)'),
   #             line = list(color = 'rgb(113,113,198)')) %>%
   #   add_trace(x = ~comZona, name = 'Commune',marker = list(color = 'rgb(113,198,113)'),
   #             line = list(color = 'rgb(113,198,113)')) %>%
   #   # add_trace(x = ~AutZon, name = 'Autres zonages',marker = list(color = 'rgb(252,141,98)'),
   #   #           line = list(color = 'rgb(252,141,98)')) %>%
   #   layout(xaxis = list(title = '%'),yaxis = list(title = '',categoryorder = "array",
   #                                                 categoryarray = xvar), barmode = 'group')
   # # Correction de la marge pour l'affichage des libelles de la colonne y
   # p$x$layout$margin$l <- p$x$layout$margin$l + 60
   # # p$x$layout$margin$b <- p$x$layout$margin$b + 30
   # p
   # 
    
  })
  
  return(InfoHousing)
  
}



