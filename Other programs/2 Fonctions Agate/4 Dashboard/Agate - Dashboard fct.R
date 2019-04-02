#----------------------------------------------------------------------------------------------------------------------#
#                                   Agate - Dashboard fonction                                                         #
#----------------------------------------------------------------------------------------------------------------------#

# MAJ : 27.03.2019

# Nicolas Kempf


#----------------------------------------------------------------------------------------------------------------------------------------
# Pyramide des ages
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

#----------------------------------------------------------------------------------------------------------------------------------------
# barChart dashboard

barChart_agate <- function(df,var.barChart,zone.etude,zone.compare,lstIndicateur){
  
  tab.label <- lstIndicateur$labelIndicateur[lstIndicateur$nomVariable %in% var.barChart & 
                                               !lstIndicateur$nomIndicateur %in% c("g_moins14","e_log_metro","g_sansObjet","d_sansObjet")]
  
  df <- df %>% 
    filter(idZonage %in% c(zone.etude,zone.compare) & nomVariable == var.barChart & type.indicateur == "part_p") %>% 
    mutate(value = as.numeric(value),
           nomIndicateur = factor(nomIndicateur,labels = tab.label))
  
  g <- ggplot(df,aes(x = reorder(nomIndicateur, value), y = value,fill=idZonage.name)) +
    geom_bar(stat = "identity",position = "dodge") +
    coord_flip() +
    scale_fill_manual(values=c("#CEBC81","#A16E83"),name=" ") +
    labs(x=" ",y = "Part des individus en %")  # Libellé des axes  
  return(g)
}

#----------------------------------------------------------------------------------------------------------------------------------------
# Statistiques pour le dashboard d'Agate

stat.dashboard_agate <- function(df,zone.etude,zone.compare,lstIndicateur,pyramide_tr){
  
  # Label des colonnes 
  dash.label <- c("labelIndicateur","zone1.etude","zone2.compare")
  names(dash.label) <- c(" ",
                         unique(df$idZonage.name[df$idZonage == zone.etude]),
                         unique(df$idZonage.name[df$idZonage == zone.compare]))
  
  df.dashboard <- df %>% 
    filter(idZonage %in% c(zone.etude,zone.compare) & type.indicateur %in% c("valeur.diffusable")) %>% 
    mutate(dashboard = ifelse(idZonage == zone.etude,"zone1.etude","zone2.compare")) %>% 
    left_join(lstIndicateur %>% select(nomVariable,nomIndicateur,labelIndicateur),by = c("nomVariable","nomIndicateur"))
  
  # Nom de la zone d'étude
  #-----------------------
  titreDash <- unique(df$idZonage.name[df$idZonage == zone.etude])
  source.an <- unique(substr(df$source,4,5))
  
  # II. Thème Synthèse
  #-------------------------------------------------------------------------------------------------------------------------
  
  
  
  # III. Thème Démographie
  #-------------------------------------------------------------------------------------------------------------------------
  
  # II.1. Part des femmes
  #----------------------
  vb.dem.fem <- df.dashboard %>% 
    filter(idZonage %in% zone.etude & nomIndicateur == "b_femme") %>% 
    select(value)
  vb.dem.fem
  
  # II.2. Population
  #-----------------
  vb.dem.pop <- df.dashboard %>% 
    filter(idZonage %in% zone.etude & nomIndicateur == "a_population") %>% 
    select(value)
  vb.dem.pop <- format(round(as.numeric(vb.dem.pop),digits = 0),digits = 9,decimal.mark=",", big.mark=" ")
  vb.dem.pop
  
  # II.3. Superficie 
  #-----------------
  vb.dem.super <- df.dashboard %>% 
    filter(idZonage %in% zone.etude & nomIndicateur == "a_superficie") %>% 
    select(value) %>% 
    mutate(value = paste0(as.character(value)," km²") )
  vb.dem.super
  
  # II.4. Tableau haut gauche : Démographie
  #----------------------------------------
  df.dem.tab.hg <- df.dashboard %>% 
    filter(nomVariable %in% c("population","dem_sexe","men_npers","dem_lieuNais") & 
             nomIndicateur %in% c("a_population","b_femme","b_1pers","a_depResid")) %>% 
    select(-domaine,-categorie,-nomVariable,-type.indicateur,-source,-idZonage,-idZonage.name,-nomIndicateur) %>% 
    spread(key = dashboard,value = value) 
  df.dem.tab.hg <- df.dem.tab.hg[c(4,1,2,3),]
  
  # II.5. Tableau haut droit : Ménages
  #-----------------------------------
  df.dem.tab.hd <- bind_rows(df.dashboard %>% 
                               filter(nomVariable %in% c("men_statOcc","dem_residenceAnterieur","men_voiture") & 
                                        nomIndicateur %in% c("b_proprio","b_mmLog","b_n_voit")),
                             df.dashboard %>% 
                               filter(nomVariable %in% c("men_statOcc_detail") & 
                                        nomIndicateur %in% c("d_locataireVide_hlm"))) %>% 
    select(-domaine,-categorie,-nomVariable,-type.indicateur,-source,-idZonage,-idZonage.name,-nomIndicateur) %>% 
    spread(key = dashboard,value = value)
  df.dem.tab.hd <- df.dem.tab.hd[c(3,1,4,2),]
  
  # II.6. Tableau bas droit : Immigration
  #--------------------------------------
  df.dem.tab.bd <- df.dashboard %>% 
    filter(nomVariable %in% c("dem_immigration","dem_etranger","dem_lieuNais","dem_ancienArrive") & 
             nomIndicateur %in% c("a_immigre","b_etranger","c_etranger","a_moins5","d_20plus")) %>% 
    select(-domaine,-categorie,-nomVariable,-type.indicateur,-source,-idZonage,-idZonage.name,-nomIndicateur) %>% 
    spread(key = dashboard,value = value)
  df.dem.tab.bd <- df.dem.tab.bd[c(1,5,4,3,2),]
  
  # II.7. Graphique bas gauche : pyramide des âges
  #-----------------------------------------------
  pyramide <- pyramide_tr %>% 
    select(-type.indicateur,-nomVariable)
  g.dem.pyramide <- pyramide_Agate(pyramide = pyramide, zone.etude = zone.etude, zone.compare = zone.compare, lstIndicateur = lstIndicateur)
  
  # III. Thème Emploi
  #-------------------------------------------------------------------------------------------------------------------------
  
  # III.1. Population en âge de travailler
  #---------------------------------------
  vb.emp.popTrav <- df.dashboard %>% 
    filter(idZonage %in% zone.etude & nomIndicateur == "a_de15a64") %>% 
    select(value)
  
  # III.2. Taux de chômage
  #-----------------------
  vb.emp.chom <- df.dashboard %>% 
    filter(idZonage %in% zone.etude & nomVariable == "emp_chomage" & nomIndicateur == "b_chomeur") %>% 
    select(value)
  vb.emp.chom
  
  # III.3. Taux d'inactif 
  #----------------------
  vb.emp.inact <- df.dashboard %>% 
    filter(idZonage %in% zone.etude & nomIndicateur == "b_autreInactif") %>% 
    select(value)
  
  # III.4. tableau haut gauche : Marché de l'emploi
  #------------------------------------------------
  df.emp.tab.hg <- df.dashboard %>% 
    filter(nomVariable %in% c("emp_popTrav","emp_popActive","emp_positionActuelle") & 
             nomIndicateur %in% c("a_de15a64","b_autreInactif","c_professionInter","d_cadreEmplo")) %>% 
    select(-domaine,-categorie,-nomVariable,-type.indicateur,-source,-idZonage,-idZonage.name,-nomIndicateur) %>% 
    spread(key = dashboard,value = value) 
  df.emp.tab.hg <- df.emp.tab.hg[c(3,4,2,1),]
  
  # III.5. tableau haut droit : Chômage
  #------------------------------------
  df.emp.tab.hd <- df.dashboard %>% 
    filter(nomVariable %in% c("emp_typeActivite","emp_ancienRech","emp_tempsPartiel") & 
             nomIndicateur %in% c("a_actifocc","b_chomeur","c_plus1","b_tempsPartiel")) %>% 
    bind_rows(df.dashboard %>% 
                filter(nomVariable %in% c("emp_chomage") & 
                         nomIndicateur %in% c("b_chomeur"))) %>% 
    select(-domaine,-categorie,-nomVariable,-type.indicateur,-source,-idZonage,-idZonage.name,-nomIndicateur) %>% 
    spread(key = dashboard,value = value) 
  df.emp.tab.hd
  
  # III.6. tableau bas droit : Travail
  #-----------------------------------
  df.emp.tab.bd <- df.dashboard %>% 
    filter(nomVariable %in% c("emp_statPro","emp_lieuTravail","emp_modeTransport") & 
             nomIndicateur %in% c("a_salarie","b_autreComDep","d_vehicule","e_transportCommun")) %>% 
    select(-domaine,-categorie,-nomVariable,-type.indicateur,-source,-idZonage,-idZonage.name,-nomIndicateur) %>% 
    spread(key = dashboard,value = value) 
  df.emp.tab.bd <- df.emp.tab.bd[c(2,1,4,3),]
  
  # III.7. graphique bas gauche : Type d'activité
  #----------------------------------------------
  g.emp.typeAct <- barChart_agate(df = df,var.barChart = "emp_typeActivite",
                                  zone.etude = zone.etude,zone.compare = zone.compare,lstIndicateur = lstIndicateur)
  
  # IV. Thème Scolarisation
  #-------------------------------------------------------------------------------------------------------------------------
  
  # IV.1. Population en âge d'être scolarisée
  #------------------------------------------
  vb.sco.popSco <- df.dashboard %>% 
    filter(idZonage %in% zone.etude & nomIndicateur == "a_popSco") %>% 
    select(value)
  
  # IV.2. Jeunes en études
  #-----------------------
  vb.sco.etud <- df.dashboard %>% 
    filter(idZonage %in% zone.etude & nomVariable == "sco_etude" & nomIndicateur == "a_etud") %>% 
    select(value)
  
  # IV.3. Taux de décrocheur
  #-------------------------
  vb.sco.decrocheur <- df.dashboard %>% 
    filter(idZonage %in% zone.etude & nomVariable == "sco_decrocheur" & nomIndicateur == "a_decrocheur") %>% 
    select(value)
  
  # IV.4. tableau haut gauche : Jeunes scolarisés
  #----------------------------------------------
  df.sco.tab.hg <- df.dashboard %>% 
    filter(nomVariable %in% c("sco_popSco","sco_etude","sco_lieuEtude") & 
             nomIndicateur %in% c("a_popSco","a_etud","a_communeResid","b_autreComDep")) %>% 
    select(-domaine,-categorie,-nomVariable,-type.indicateur,-source,-idZonage,-idZonage.name,-nomIndicateur) %>% 
    spread(key = dashboard,value = value) 
  df.sco.tab.hg <- df.sco.tab.hg[c(3,4,2,1),]
  
  # IV.5. graphique haut droit : Population scolarisée
  #---------------------------------------------------
  g.sco.pop <- barChart_agate(df = df,var.barChart = "sco_popSco2",
                              zone.etude = zone.etude,zone.compare = zone.compare,lstIndicateur = lstIndicateur)
  
  # IV.6. graphique bas gauche : Dîplome
  #-------------------------------------
  g.sco.diplome <- barChart_agate(df = df,var.barChart = "sco_diplome",
                                  zone.etude = zone.etude,zone.compare = zone.compare,lstIndicateur = lstIndicateur)
  
  # IV.7. tableau bas droit : Jeunes non scolarisés
  #------------------------------------------------
  df.sco.tab.bd <- df.dashboard %>% 
    filter(nomVariable %in% c("sco_decrocheur","sco_scolarisation2a5","sco_scolarisation6a15","sco_scolarisation16a24") & 
             nomIndicateur %in% c("a_decrocheur","b_n_scolarise2a5","b_n_scolarise6a15","b_n_scolarise16a24")) %>% 
    select(-domaine,-categorie,-nomVariable,-type.indicateur,-source,-idZonage,-idZonage.name,-nomIndicateur) %>% 
    spread(key = dashboard,value = value) 
  df.sco.tab.bd <- df.sco.tab.bd[c(1,3,4,2),]
  
  # V. Thème Logement
  #-------------------------------------------------------------------------------------------------------------------------
  
  # V.1. Nombre de logements
  #--------------------------
  vb.log.tot <- df.dashboard %>% 
    filter(idZonage %in% zone.etude & nomVariable == "log_tot") %>% 
    select(value)
  vb.log.tot <- format(round(as.numeric(vb.log.tot),digits = 0),digits = 9,decimal.mark=",", big.mark=" ")
  vb.log.tot

  # V.2. HLM
  #----------
  vb.log.hlm <- df.dashboard %>% 
    filter(idZonage %in% zone.etude & nomVariable == "log_hlm" & nomIndicateur == "a_hlm") %>% 
    select(value)
  
  # V.3. Maisons
  #--------------
  vb.log.maison <- df.dashboard %>% 
    filter(idZonage %in% zone.etude & nomVariable == "log_type" & nomIndicateur == "a_maison") %>% 
    select(value)
  
  # V.4. tableau haut gauche : 
  #----------------------------------------------
  df.log.tab.hg <- df.dashboard %>% 
    filter(nomVariable %in% c("log_tot","log_hlm","log_type","log_emm") & 
             nomIndicateur %in% c("a_log_tot","a_hlm","b_appart","b_av60","e_ap99")) %>% 
    select(-domaine,-categorie,-nomVariable,-type.indicateur,-source,-idZonage,-idZonage.name,-nomIndicateur) %>% 
    spread(key = dashboard,value = value) 
  df.log.tab.hg <- df.log.tab.hg[c(5,4,1,2,3),]
  
  # V.5. graphique haut droit : Categorie de logement
  #--------------------------------------------------
  g.log.cat <- barChart_agate(df = df,var.barChart = "log_cat",
                              zone.etude = zone.etude,zone.compare = zone.compare,lstIndicateur = lstIndicateur)
  
  # V.6. graphique bas gauche : Année d'achevement
  #-----------------------------------------------
  g.log.ach <- barChart_agate(df = df,var.barChart = "log_ach_constru",
                              zone.etude = zone.etude,zone.compare = zone.compare,lstIndicateur = lstIndicateur)
  
  # V.7. graphique bas droit : Aspect du bati
  #-------------------------------------------
  g.log.bati <- barChart_agate(df = df,var.barChart = "log_bati",
                               zone.etude = zone.etude,zone.compare = zone.compare,lstIndicateur = lstIndicateur)
  
  # VI. Thème Résidences principales
  #-------------------------------------------------------------------------------------------------------------------------
  
  # VI.1. Part des résidences principales
  #--------------------------------------
  vb.res.part <- df.dashboard %>% 
    filter(idZonage %in% zone.etude & nomVariable == "log_cat" & nomIndicateur == "a_res_princ") %>% 
    select(value)
  
  # VI.2. Logements collectifs
  #--------------------------
  vb.res.collectif <- df.dashboard %>% 
    filter(idZonage %in% zone.etude & nomVariable == "res_type" & nomIndicateur == "b_collectif") %>% 
    select(value)
  
  # VI.3. TODO
  #--------------
  #TODO (Revoir le calcul)
  
  # VI.4. tableau haut gauche : Caractéristiques des résidences
  #-----------------------------------------------------------
  df.res.tab.hg <- df.dashboard %>% 
    filter(nomVariable %in% c("log_cat","res_type","res_eau","res_wc") & 
             nomIndicateur %in% c("a_res_princ","b_collectif","a_eauFroide","a_wc")) %>% 
    select(-domaine,-categorie,-nomVariable,-type.indicateur,-source,-idZonage,-idZonage.name,-nomIndicateur) %>% 
    spread(key = dashboard,value = value) 
  df.res.tab.hg <- df.res.tab.hg[c(4,1,2,3),]
  
  # VI.5. graphique haut droit : Nombre de pièces
  #---------------------------------------------
  g.res.nbp <- barChart_agate(df = df,var.barChart = "res_nbPiece",
                              zone.etude = zone.etude,zone.compare = zone.compare,lstIndicateur = lstIndicateur)
  
  # VI.6. graphique bas gauche : Surface
  #------------------------------------
  g.res.surf <- barChart_agate(df = df,var.barChart = "res_surface",
                               zone.etude = zone.etude,zone.compare = zone.compare,lstIndicateur = lstIndicateur)
  
  # VI.7. tableau bas droit : Équipements
  #--------------------------------------
  df.res.tab.bd <- df.dashboard %>% 
    filter(nomVariable %in% c("res_bain","res_cuis","res_clim","res_garage") & 
             nomIndicateur %in% c("b_n_bain","b_n_cuis","a_clim","a_garage")) %>% 
    select(-domaine,-categorie,-nomVariable,-type.indicateur,-source,-idZonage,-idZonage.name,-nomIndicateur) %>% 
    spread(key = dashboard,value = value) 
  
  
  #-------------------------------------------------------------------------------------------------------------------------
  # Enregistrement
  save(df.dashboard,titreDash,dash.label,
       vb.dem.fem,vb.dem.pop,vb.dem.super,df.dem.tab.hg,df.dem.tab.hd,df.dem.tab.bd,g.dem.pyramide,
       vb.emp.popTrav,vb.emp.chom,vb.emp.inact,df.emp.tab.hg,df.emp.tab.hd,df.emp.tab.bd,g.emp.typeAct,
       vb.sco.popSco,vb.sco.etud,vb.sco.decrocheur,df.sco.tab.hg,df.sco.tab.bd,g.sco.pop,g.sco.diplome,
       vb.log.tot,vb.log.hlm,vb.log.maison,df.log.tab.hg,g.log.cat,g.log.ach,g.log.bati,
       vb.res.part,vb.res.collectif,df.res.tab.hg,g.res.nbp,g.res.surf,df.res.tab.bd,
       file = "Data/Tmp/dashboard_tmp.RData")
  
  
  lst_dash <- list(df.dashboard = df.dashboard,titreDash = titreDash,dash.label = dash.label,source.an = source.an,
                   # Demographie
                   vb.dem.fem = vb.dem.fem, vb.dem.pop = vb.dem.pop, vb.dem.super = vb.dem.super,
                   df.dem.tab.hg = df.dem.tab.hg, df.dem.tab.hd = df.dem.tab.hd,df.dem.tab.bd = df.dem.tab.bd,
                   g.dem.pyramide = g.dem.pyramide,
                   # Emploi
                   vb.emp.popTrav = vb.emp.popTrav,vb.emp.chom = vb.emp.chom,vb.emp.inact = vb.emp.inact,
                   df.emp.tab.hg = df.emp.tab.hg,df.emp.tab.hd = df.emp.tab.hd,df.emp.tab.bd = df.emp.tab.bd,
                   g.emp.typeAct = g.emp.typeAct,
                   # Scolarisation
                   vb.sco.popSco = vb.sco.popSco,vb.sco.etud = vb.sco.etud,vb.sco.decrocheur = vb.sco.decrocheur,
                   df.sco.tab.hg = df.sco.tab.hg,df.sco.tab.bd = df.sco.tab.bd,
                   g.sco.pop = g.sco.pop,g.sco.diplome = g.sco.diplome,
                   # Logement
                   vb.log.tot = vb.log.tot,vb.log.hlm = vb.log.hlm,vb.log.maison = vb.log.maison,
                   df.log.tab.hg = df.log.tab.hg,
                   g.log.cat = g.log.cat,g.log.ach = g.log.ach,g.log.bati = g.log.bati,
                   # Résidences principales
                   vb.res.part = vb.res.part,vb.res.collectif = vb.res.collectif,
                   df.res.tab.hg = df.res.tab.hg,df.res.tab.bd = df.res.tab.bd,
                   g.res.nbp = g.res.nbp,g.res.surf = g.res.surf)
  
  return(lst_dash)
  
}

