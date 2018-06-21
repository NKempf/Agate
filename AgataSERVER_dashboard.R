#---------------------------------------------------------------------------------------------------------------------------#
#                                             Agate - Server Dashboard                                                      #
#---------------------------------------------------------------------------------------------------------------------------#

# Include server code for plotting dashboard graphs


# I. Modal dashboard
#---------------------------------------------------------------------------------------------------------------------------------------------------

# I.1. BoxPlot Income
#--------------------
plotlyBoxplotIncome <- function(input, output, session,statZone,statHZone,zone_filtre,compare){
  
  idZonage <- as.character(unique(zone_filtre$idZonage))
  idZonage.com <- as.character(statHZone$tFilo.I.1$com[statHZone$tFilo.I.1$idZonage == idZonage][1])
  idZonage.dep <- substr(idZonage.com,1,3)

  # Zone d'étude
  varKeep.zone <- c("idZonage")
  zone.boxplot <- df_boxplot(df = statZone$tFilo.I.1,varKeep = varKeep.zone) 
  y <- as.character(zone.boxplot[zone.boxplot$idZonage == idZonage,!colnames(zone.boxplot) %in% varKeep.zone])

  # Affichage de la zone de comparaison
  switch(compare,
         Departement = {
           varKeep <- c("dep")
           reg.boxplot <- df_boxplot(df = dep.stat$tFilo.I.1,varKeep = varKeep)
           z <-  as.character(reg.boxplot[reg.boxplot$dep == idZonage.dep,!colnames(reg.boxplot) %in% varKeep])
           name.comp <- idZonage.dep
         },
         Commune = {
           varKeep <- c("com")
           reg.boxplot <- df_boxplot(df = com.stat$tFilo.I.1,varKeep = varKeep)
           z <-  as.character(reg.boxplot[reg.boxplot$com == idZonage.com,!colnames(reg.boxplot) %in% varKeep])
           name.comp <- idZonage.com
         },
         HorsZone = {
           varKeep <- c("com","idZonage")
           reg.boxplot <- df_boxplot(df = statHZone$tFilo.I.1,varKeep = varKeep)
           z <-  as.character(reg.boxplot[reg.boxplot$com == idZonage.com & reg.boxplot$idZonage == "Hors zonage",!colnames(reg.boxplot) %in% varKeep])
           name.comp <- "Hors zone"
         }
         ,
         {
           z <- as.character(zone.boxplot[zone.boxplot$idZonage == compare,!colnames(zone.boxplot) %in% varKeep.zone])
           name.comp <- compare
         }
  )
  
  # Graphique de la zone
  plot_ly() %>% 
    add_trace(y=y, name=idZonage, type="box") %>% 
    add_trace(y=z, name= name.comp, type="box") %>%
    layout(autosize=TRUE, boxmode="group", hovermode="closest", 
           showlegend=TRUE,
           xaxis = list(
             autorange = TRUE, 
             showticklabels = FALSE, 
             type = "category"),
           yaxis = list(
             autorange = TRUE, 
             type = "linear")
    )
}


# I.2 Aged pyramid
#-----------------
plotlyAgedPyramid <- function(input, output, session,statZone,statHZone,zone_filtre,compare){

  idZonage <- as.character(unique(zone_filtre$idZonage))
  idZonage.com <- as.character(statHZone$tFilo.I.1$com[statHZone$tFilo.I.1$idZonage == idZonage][1])
  idZonage.dep <- substr(idZonage.com,1,3)
  
  # Données sur zonage
  pyramid.zone <- agePyramid_zone(df.zone = statZone$tRp.II.4,idZonage.p = idZonage)
  
  # Affichage de la pyramide
  p <- plot_ly(data = pyramid.zone,x= ~ part_p, y=~age,color=~sexe,colors = c('#fb9a99','#a6cee3')) %>%
    add_bars(orientation = 'h', hoverinfo = 'text', text = ~abs_pop,alpha = 0.8) %>%
    layout(bargap = 0.1, barmode = 'overlay',
           xaxis = list(title = "Population",tickmode = "array"),
           yaxis = list(title = "Age")) 
  
  # Affichage de la zone de comparaison
  switch(compare,
         Departement = {
           pyramid.dep <- agePyramid_dep(df.zone = dep.stat$tRp.II.4,dep.p = idZonage.dep)
           p <- add_trace(p = p,data = pyramid.dep,x = ~part_p, y = ~age - 0.5, type = 'scatter', mode = 'lines', name = idZonage.dep,
                          line = list(shape="vh",color = "#74c476"),
                          hoverinfo = "text",text = ~abs_pop)
         },
         Commune = {
           pyramid.com <- agePyramid_com(df.zone = com.stat$tRp.II.4,com.p = idZonage.com)
           p <- add_trace(p = p,data = pyramid.com,x = ~part_p, y = ~age - 0.5, type = 'scatter', mode = 'lines', name = idZonage.com,
                          line = list(shape="vh",color = "#74c476"),
                          hoverinfo = "text",text = ~ abs_pop)
         },
         HorsZone = {
           pyramid.hzone<- agePyramid_Hzone(df.zone = statHZone$tRp.II.4,com.p = idZonage.com,idZonage.p = "Hors zonage")
           p <- add_trace(p = p,data = pyramid.hzone,x = ~part_p, y = ~age - 0.5, type = 'scatter', mode = 'lines', name = "Hors zone",
                          line = list(shape="vh",color = "#74c476"),
                          hoverinfo = "text",text = ~ abs_pop)
         }
         ,
         {
           pyramid.zone2 <- agePyramid_zone(df.zone = statZone$tRp.II.4,idZonage.p = compare)
           p <- add_trace(p = p,data = pyramid.zone2,x = ~part_p, y = ~age - 0.5, type = 'scatter', mode = 'lines', name = compare,
                          line = list(shape="vh",color = "#74c476"),
                          hoverinfo = "text",text = ~ abs_pop)
         }
  )
  p
  
}

# I.3 Informations about household
#---------------------------------
plotlyInfoPopulation <- function(input, output, session){
  
  plot_ly(
    type = 'scatterpolar',
    fill = 'toself'
  ) %>%
    add_trace(
      r = c(39, 28, 8, 7, 28, 39),
      theta = c('A','B','C', 'D', 'E', 'A'),
      name = 'Group A'
    ) %>%
    add_trace(
      r = c(1.5, 10, 39, 31, 15, 1.5),
      theta = c('A','B','C', 'D', 'E', 'A'),
      name = 'Group B'
    ) %>%
    layout(
      polar = list(
        radialaxis = list(
          visible = T,
          range = c(0,50)
        )
      )
    )
    
    
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



