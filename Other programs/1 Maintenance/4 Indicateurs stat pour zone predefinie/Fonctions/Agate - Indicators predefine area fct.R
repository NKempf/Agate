#----------------------------------------------------------------------------------------------------------------------#
#                                   Agate - Statistical calculation for predefine area fonction                        #
#----------------------------------------------------------------------------------------------------------------------#

# 17.02.2019

indStat_AutZOn <- function(zonage,rp.an, filo.an, ril.an){
  
  # O. Selection des bases de travail (donnees reelles ou fausses)
  #---------------------------------------------------------------
  
  # Ril
  ril.path.string <- paste0("Data/Ril/ril",ril.an,".fst")
  rilPath <- ifelse(file.exists(ril.path.string),ril.path.string,"Data/Ril/FakeRil.fst")
  # RP
  rpi.path.string <- paste0("Data/Rp/rp",rp.an,"i.fst")
  rpl.path.string <- paste0("Data/Rp/rp",rp.an,"l.fst")
  rpiPath <- ifelse(file.exists(rpi.path.string),rpi.path.string,"Data/Rp/FakeRpi.fst")
  rplPath <- ifelse(file.exists(rpl.path.string),rpl.path.string,"Data/Rp/FakeRpl.fst")
  # Filosofi
  filo.path.string <- paste0("Data/Filosofi/filo",filo.an,".fst")
  filoPath <- ifelse(file.exists(filo.path.string),filo.path.string,"Data/Filosofi/FakeFilo.fst")

  # I. Logements géolocalisés du RIL 
  #---------------------------------
  load("Data/Maps/Cities/cities.RData") # Cities map
  # I.1 Communes dans lesquelles se trouvent une ou plusieurs zones
  zoneInter <- gIntersects(zonage,com.dom,byid = TRUE)
  test <- apply(zoneInter, 1, function(x){
    test <- sum(x)
    return(ifelse(test>0,TRUE,FALSE))}) 
  com.dom.select <- com.dom@data$Codgeo[test]
  
  # I.2. Chargement des logements du RIL dans les communes d'interets
  ril <- read_fst(rilPath) %>% 
    select(idx,x,y) %>% 
    mutate(com = substr(idx,1,5)) %>%
    filter(com %in% com.dom.select)
  
  # I.3. Transformation du ril en objet spatial
  coordinates(ril) <- ~x+y
  ril@proj4string <- CRS("+init=epsg:3857")
  
  # II. Ajout de la zone aux données du recensement
  #-------------------------------------------------
  
  # II.1. Zone dans laquelle chaque logement se situe
  pts.sp <- zonaPts(pts.sp = ril,zonage = zonage)
  
  # II.2. Ajout de la zone aux données du rp individu
  # Note : pour des raisons de performances, les données du RP sont préalablement filtrées selon les communes étudiées
  rpi <- read_fst(rpiPath) %>% 
    filter(idx %in% ril@data$idx) %>% 
    left_join(pts.sp@data[,c("idx","idZonage")], "idx") %>% 
    mutate(idZonage = ifelse(is.na(idZonage),"Hors zonage", idZonage))
  
  # II.3. Ajout de la zone aux données du rp logement
  rpl <- read_fst(rplPath) %>% 
    filter(idx %in% ril@data$idx) %>% 
    left_join(pts.sp@data[,c("idx","idZonage")], "idx") %>% 
    mutate(idZonage = ifelse(is.na(idZonage),"Hors zonage", idZonage))
  
  # III. Ajout de la zone aux données fiscales
  #-------------------------------------------
  
  # III.1. Chargement des données fiscales filtrées selon les communes d'intérêts
  filo <- read_fst(filoPath) %>% 
    filter(com %in% com.dom.select)
  
  # IV.2. Transformation des données en objet spatial
  filo.sp <- SpatialPointsDataFrame(coords = filo[,c("x","y")],data = filo,proj4string = CRS("+init=epsg:3857"))
  
  # IV.3. Zone dans laquelle chaque foyer fiscal se situe
  filo.sp <- zonaPts(pts.sp = filo.sp,zonage = zonage)
  
  # IV.4. Ajout de la zone aux données fiscales
  typmen.label <- c("famille monoparentale","couple sans enfant","couple avec enfant(s)","menage complexe",
                    "femme seule","homme seul")
  
  filo <- filo.sp@data %>% 
    left_join(data.frame(unique(rpl[,c("com","com.lib")])),"com") %>% 
    mutate(dep = substr(com,1,3),
           idZonage = ifelse(is.na(idZonage),"Hors zonage", idZonage),
           typmenR.lib = factor(typmenR,labels = typmen.label))
  rm(filo.sp)
  
  # IV. Calcul des statistiques
  #----------------------------
  indStat <- statistics_zone(group_var = c("idZonage"),zone = zonage,rpi = rpi,rpl = rpl, filo = filo,
                             sourceRpi = paste0("rpi",rp.an),
                             sourceRpl = paste0("rpl",rp.an),
                             sourceFilo = paste0("filo",filo.an),
                             rpi.weight = "IPONDI",rpl.weight = "IPONDL",filo.weight = "nbpersm")
  return(indStat)
}

indStat_RegCities <- function(zonage,var,rp.an, filo.an, ril.an){
  
  # O. Selection des bases de travail (donnees reelles ou fausses)
  #---------------------------------------------------------------
  
  # Ril
  ril.path.string <- paste0("Data/Ril/ril",ril.an,".fst")
  rilPath <- ifelse(file.exists(ril.path.string),ril.path.string,"Data/Ril/FakeRil.fst")
  # RP
  rpi.path.string <- paste0("Data/Rp/rp",rp.an,"i.fst")
  rpl.path.string <- paste0("Data/Rp/rp",rp.an,"l.fst")
  rpiPath <- ifelse(file.exists(rpi.path.string),rpi.path.string,"Data/Rp/FakeRpi.fst")
  rplPath <- ifelse(file.exists(rpl.path.string),rpl.path.string,"Data/Rp/FakeRpl.fst")
  # Filosofi
  filo.path.string <- paste0("Data/Filosofi/filo",filo.an,".fst")
  filoPath <- ifelse(file.exists(filo.path.string),filo.path.string,"Data/Filosofi/FakeFilo.fst")
  
  # I.1. Recensement de la population
  #--------------------------------
  rpi <- read_fst(rpiPath) %>% 
    mutate(idZonage = !!parse_quosure(var))
  
  rpl <- read_fst(rplPath) %>% 
    mutate(idZonage = !!parse_quosure(var))
  
  # I.2. Données fiscales
  #---------------------
  typmen.label <- c("famille monoparentale","couple sans enfant","couple avec enfant(s)","menage complexe",
                    "femme seule","homme seul")
  filo <- read_fst(filoPath) %>% 
    mutate(dep = substr(com,1,3),
           idZonage = !!parse_quosure(var),
           typmenR.lib = factor(typmenR,labels = typmen.label))
  
  # I.3. Indicateurs statistiques
  #------------------------------
  indStat <- statistics_zone(group_var = c("idZonage"),zone = zonage,rpi = rpi,rpl = rpl, filo = filo,
                             sourceRpi = paste0("rpi",rp.an),
                             sourceRpl = paste0("rpl",rp.an),
                             sourceFilo = paste0("filo",filo.an),
                             rpi.weight = "IPONDI",rpl.weight = "IPONDL",filo.weight = "nbpersm")
  return(indStat)
  
}


