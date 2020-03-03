

df <- read_fst("Data/Stats/Prefine aera/Real/fst/indicateur_stat.fst") %>%
  filter(zone.pred == 1 & idZonage %in% c("971","972") & substr(source,4,5) == "15")
df2 <- df %>%
  filter(nomVariable %in% c("emp_typeActivite","sco_popSco2","sco_diplome","log_cat","log_ach_constru",
                            "log_bati","res_nbPiece","res_surface")) %>%
  mutate(type.indicateur = "part_p")

df.zone.etude <- bind_rows(df,df2)

pyramide.etude <- read_fst("Data/Stats/Prefine aera/Real/fst/pyramide.fst") %>%
  filter(zone.pred == 1 & idZonage %in% c("971","972") & substr(source,4,5) == "15")

dash.label <- c("labelIndicateur","zone1.etude","zone2.compare")


df <- df %>% 
  select(-zone.pred)

df.dashboard <- df %>% 
  filter(type.indicateur %in% c("valeur.diffusable")) %>% 
  mutate(dashboard = ifelse(idZonage == zone.etude,"zone1.etude","zone2.compare")) %>% 
  left_join(lstIndicateur %>% select(nomVariable,nomIndicateur,labelIndicateur),by = c("nomVariable","nomIndicateur"))



var.barChart <- "emp_typeActivite"
zone.etude <- "971"
zone.compare <- "972"
lstIndicateur <- lstIndicateur
pyramide <- pyramide.etude


#----------------------------------------------------------------------------------------------------#

barChart_agate_plotly <- function(df,var.barChart,zone.etude,zone.compare,lstIndicateur){
  test <- lstIndicateur$nomVariable %in% var.barChart & 
    !lstIndicateur$nomIndicateur %in% c("g_moins14","e_log_metro","g_sansObjet","d_sansObjet")
  # tab.label <- lstIndicateur$labelIndicateur[test]
  
  df2 <- df %>% 
    filter(idZonage %in% c(zone.etude,zone.compare) & nomVariable == var.barChart) %>% 
    mutate(value = as.numeric(value),
           nomIndicateur = factor(nomIndicateur,levels = lstIndicateur$nomIndicateur[test],labels = lstIndicateur$labelIndicateur[test]))
  
  df3 <- df2 %>% filter(idZonage == zone.etude)
  df4 <- df2 %>% filter(idZonage == zone.compare)
  
  p <- plot_ly(data = df3,
               y = reorder(df3$nomIndicateur,df3$value), 
               x = ~value, type = 'bar', orientation = 'h',
               name = unique(df3$idZonage.name),
               marker = list(color = "#CEBC81")) %>% 
    add_trace(data = df4,
              y = reorder(df4$nomIndicateur,df4$value),  
              y = ~value,
              name = unique(df4$idZonage.name),
              marker = list(color = "#A16E83")) %>%
    layout(xaxis = list(
      title = "Part des individus en %"
    ))  
  return(p)
}

#---------------------------------------------------------------------------------------------------#
# Pyramide #







pyramide_Agate <- function(pyramide,zone.etude,zone.compare,lstIndicateur){
  
  df <- pyramide %>% 
    filter(idZonage %in% c(zone.etude,zone.compare))
  
  g <- ggplot(df %>% filter(sexe == "b_femme" & idZonage == zone.compare), 
              aes(x = age, y = pop,label = idZonage.name, label2 = sexe, label3 = age, label4 = abs(pop))) +
    geom_bar(data = df %>% filter(idZonage == zone.etude) %>% mutate(sexe = ifelse(sexe=="a_homme","a_homme.etude","b_femme.etude")), 
             aes(fill = sexe), stat = "identity") +
    geom_point(aes(colour = sexe)) +
    geom_line(aes(colour = sexe, group=1)) +
    geom_point(data=df %>% filter(sexe == "a_homme" & idZonage == zone.compare) %>% mutate(sexe = "a_homme"),
               aes(colour = sexe)) +
    geom_line(data=df %>% filter(sexe == "a_homme" & idZonage == zone.compare),aes(colour = sexe, group=2)) +
    
    scale_fill_manual(values = c(a_homme.etude = "#CEBC81", b_femme.etude = "#A16E83"), name=zone.etude,
                      labels=c("Hommes", "Femmes")) +
    scale_colour_manual(values = c(a_homme = "#479761",b_femme = "#479761" ), name=zone.compare,
                        labels=c("Hommes", "Femmes")) +
    coord_flip() +
    scale_y_continuous(breaks = c(c(-40,-30, -20, -10, 0), c(0, 10, 20, 30,40)),
                       labels = c("40","30", "20", "10", "0", "0", "10", "20", "30","40")) +
    scale_x_discrete(labels=lstIndicateur$labelIndicateur[lstIndicateur$nomVariable %in% "dem_agerevTr"]) +
    labs(x="",y = "Part des individus en %")  # Libellé des axes  
  return(g)
  
}



# library(plotly)
# library(dplyr)

age <- rep(1:5, 2)

sex <- rep(c('Male', 'Female'), each = 5)

pop <- c(-1000, -944, -888, -762, -667, 1100, 999, 844, 789, 655)

df <- data.frame(age, sex, pop) %>%
  mutate(abs_pop = abs(pop))

df <- pyramide %>% 
  filter(idZonage %in% c(zone.etude,zone.compare)) %>% 
  mutate(abs_pop = abs(pop))

df2 <- df %>% filter(idZonage == zone.etude)

  plot_ly(data = df2 %>% filter(sexe == "a_homme"),x= ~pop, y=~age,orientation = 'h',
          marker = list(color = "#A16E83")) %>% 
  add_bars(data = df2 %>% filter(sexe == "b_femme"),x= ~pop, y=~age,orientation = 'h', hoverinfo = 'text', 
           text = ~abs_pop, marker = list(color = "#CEBC81")
           ) %>%
  layout(bargap = 0.1, barmode = 'overlay',
         xaxis = list(tickmode = 'array', tickvals = c(c(-40,-30, -20, -10, 0), c(0, 10, 20, 30,40)),
                      ticktext = c("40","30", "20", "10", "0", "0", "10", "20", "30","40")))

  
  
  library(plotly)
  library(dplyr)
  
  age <- rep(1:5, 2)
  
  sex <- rep(c('Male', 'Female'), each = 5)
  
  pop <- c(-1000, -944, -888, -762, -667, 1100, 999, 844, 789, 655)
  
  df <- data.frame(age, sex, pop) %>%
    mutate(abs_pop = abs(pop))
  
  
  df %>% 
    plot_ly(x= ~pop, y=~age,split =~sex,colors = c("red","green")) %>% 
    add_bars(orientation = 'h', hoverinfo = 'text', text = ~abs_pop) %>%
    layout(bargap = 0.1, barmode = 'overlay',
           xaxis = list(tickmode = 'array', tickvals = c(-1000, -500, 0, 500, 1000),
                        ticktext = c('1000', '500', '0', '500', '1000')))





