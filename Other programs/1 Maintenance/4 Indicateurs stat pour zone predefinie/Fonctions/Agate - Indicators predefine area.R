
function(){
  
  # I.1. Recensement de la population
  #--------------------------------
  rpi <- read_fst(rpiPath) %>% 
    mutate(idZonage = com)
  
  rpl <- read_fst(rplPath) %>% 
    mutate(idZonage = com)
  
  # I.2. Données fiscales
  #---------------------
  typmen.label <- c("famille monoparentale","couple sans enfant","couple avec enfant(s)","menage complexe",
                    "femme seule","homme seul")
  filo <- read_fst(filoPath) %>% 
    mutate(dep = substr(com,1,3),
           idZonage = com,
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


