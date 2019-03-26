
df <- rpi[1:10000,]
group_var <- c("idZonage","idZonage.name")
poids = "IPONDI.cal"
indicateur <- "dem_sexe"



agate_qualitative <- function(indicateur,df,group_var,poids){
  df %>% 
    rename(nomIndicateur = indicateur) %>% 
    group_by(!!! syms(group_var),nomIndicateur) %>%
    weighted_frequency(poids) %>% 
    gather("type.indicateur","value",-group_var,-nomIndicateur) %>%
    mutate(nomVariable = indicateur) %>%
    ungroup()
}

lst_indicateur <- c("dem_sexe","dem_agerevTr")



lst_var <- unique(lstIndicateur$nomVariable[lstIndicateur$qualiteIndicateurDenom1 %in% "INPER"])
test <- bind_rows(lapply(lst_var, agate_qualitative,df = rpi[1:10000,],group_var = group_var, poids = rpi.weight))


table(df$idZonage)

# Automatisation du calcul d'indicateur sur des sous-champs

df_ssChamp <- lstCategorie %>% 
  filter(source == "rpi" & !is.na(modaliteChamp)) %>% 
  select(nomVariable,variableChamp,modaliteChamp) %>% 
  mutate(varmod.champ = paste0(variableChamp,modaliteChamp))





lst_champ <- unique(df_ssChamp$varmod.champ)[1]



agate.qualitative.ssChamp <- function(champ,df_ssChamp){
  
  lst_var <- df_ssChamp$nomVariable[df_ssChamp$varmod.champ == champ]
  var.filtre <- df_ssChamp$variableChamp[df_ssChamp$varmod.champ == champ]
  modalite.filtre <- df_ssChamp$modaliteChamp[df_ssChamp$varmod.champ == champ]
  
  bind_rows(lapply(lst_var, agate_qualitative,
                   df = rpi[1:10000,] %>% filter_at(vars(var.filtre), all_vars(. %in% modalite.filtre)),
                   group_var = group_var, poids = rpi.weight))
}



lst_var <- df_ssChamp$nomVariable[df_ssChamp$varmod.champ == lst_champ]
var.filtre <- df_ssChamp$variableChamp[df_ssChamp$varmod.champ == lst_champ]
modalite.filtre <- df_ssChamp$modaliteChamp[df_ssChamp$varmod.champ == lst_champ]


bind_rows(lapply(lst_var, agate_qualitative,
                 df = rpi[1:10000,] %>% filter_at(vars(var.filtre), all_vars(. %in% modalite.filtre)),
                 group_var = group_var, poids = rpi.weight))



#--------------------------------------------------------------------------------------------------------------------------
df %>%
  group_by(!!! syms(group_var)) %>% 
  summarise(freq = n(),
            freq_p = round(sum(!!! syms(rpi.weight),na.rm = TRUE),0)) %>% 
  gather("type.indicateur","value",-group_var) %>% 
  mutate(nomVariable = "population",
         nomIndicateur = "a_population")


#--------------------------------------------------------------------------------------------------------------------------
# Test de la qualité des données
group_var <- group_var.qualite
rpa <- rpa.qualite
ril = rpa.qualite %>% select(com,C_IMM,idZonage,nb_logn.ril) %>% rename(nb_logn = nb_logn.ril)






Qlfinal<-function(rpa,group_var,ril){
  zone <- rpa %>% filter(!duplicated(idZonage)) %>% select(idZonage)
  
  total_var<-group_var[!grepl(" / ", group_var, fixed=TRUE)]
  
  if(!is_empty(total_var)){
    totaux<-bind_rows(apply(zone, 1, Qlinter,total_var,rpa,ril))
    r<-totaux
  }
  ratio_var<-group_var[grepl(" / ", group_var, fixed=TRUE)]
  if(!is_empty(ratio_var)){
    ratios<-bind_rows(apply(zone, 1, Qlrintr,ratio_var,rpa))
    r<-ratios
    if(!is_empty(total_var)){
      r<-rbind(totaux,ratios)
    }
  }
  r$CoefVariation<-as.numeric(r$CoefVariation)
  r$`Ecart-Type`<-as.numeric(r$`Ecart-Type`)
  r$EstVariable<-as.numeric(r$EstVariable)
  return(r)
}

zone <- zone[1,]


Qlrintr<-function(zone,ratio_var,rpa){
  t1 <- Sys.time()
  
  rpa2 <- rpa %>% filter(com %in% unique(rpa$com[rpa$idZonage == zone]))
  
  a<-apply(as.array(ratio_var),1, Qlrtio, zone,rpa=rpa2)
  a<-as.data.frame(t(a))
  colnames(a)<-c("zonage","Variable","CoefVariation","Ecart-Type","EstVariable","IntervalConf.")
  print(paste0(zone," finis en ",round(Sys.time()-t1,3)))
  return(a)
}

Y <- ratio_var[1]


Qlrtio <- function (Y,zone,rpa,constante=0.4){
  ## R           : Ration (Y1/Y2)
  ## 'Z'         : variable linéariser pour les calcule de variance 
  ## Rz1 / Rz2   : variable du ratio calculé sur la zone
  ## CommuneZone : Liste des communes dans la zone
  ## rpaR        : Liste des adresse avec Rz1 et Rz2
  ## rpaZ        : Liste des adresse recensé avec Z, Id€Zone
  ## rpaZ_s      : Liste des adresse sondée (ou f=0.4 / w=2.5)  <- ## pour l'instant ou ZAP ##
  ## T_Xg        : Nombre total de logement (com/gr) d'apres le recensement
  ## T_Zg        : Nombre total de Z        (com/gr)
  ## T_Rg        : Nombre total de R        (com/gr)
  ## Tt          : Table des Total et Ratio (com/gr)
  #  Rz     : Nombre de Z par logement (com/gr) 
  ## rpaU        : table avec les U = Z - RX
  ## T_Ug        : Total de U = Z-RX        (com/gr)
  ## U_          : moyenne des U = Z-RX     (com/gr)
  ## rpaf        : rpa avec les Tf = Z - RX - U_ et Tf²
  Y<-strsplit(Y," / ",fixed = TRUE)
  r1<-Y[[1]][1]
  r2<-Y[[1]][2]
  oppose<-FALSE
  if(grepl("- ",r1,fixed=TRUE)){
    r1<-substr(r1,3,100)
    oppose<-TRUE
  }
  
  rpaR <- rpa %>% select(idZonage,com,C_ANNEE_COL,r1,r2,IPOND,X) %>% mutate(Id=(idZonage==zone),join=1) %>% mutate(Rz1=Id*!!parse_quosure(r1),Rz2=Id*!!parse_quosure(r2))
  
  T_R <- rpaR %>% group_by(join) %>% summarise(TR1=sum(Rz1*IPOND),TR2=sum(Rz2*IPOND))
  
  rpaZ <- rpaR %>% left_join(T_R,by = "join") %>% mutate(Z=(Rz1-(TR1/TR2)*Rz2)/(TR2)) %>% 
    filter(!is.nan(Z) & IPOND>1.75 & !com %in% c("97360","97357"))
  
  Tt <- rpaZ %>% group_by(C_ANNEE_COL,com) %>% summarise(TZg=sum(Z*IPOND),TXg=sum(X*IPOND))
  
  rpaU <- rpaZ %>% left_join(Tt, by = c("C_ANNEE_COL", "com")) %>% mutate(U2 = (Z-(TZg/TXg)*X)^2)
  
  Vf <- (1-constante)/(constante^2)*sum(rpaU$U2)
  ec <- Vf^0.5*100
  TtZ<- sum(T_R$TR1/T_R$TR2)
  if (oppose){
    TtZ <- 1 - TtZ
    r1<-paste0("- ",r1)
  }
  cv <- ec/(0.5-abs(TtZ-0.5))
  TtZ <- 100*TtZ
  if(ceiling(TtZ+1.96*ec)<=10 & floor(TtZ-1.96*ec)<3){
    Intconf <- paste0("inferieur à ",ceiling(TtZ+1.96*ec),"%")
  }else if(floor(TtZ-1.96*ec)>=90 & ceiling(TtZ+1.96*ec)>97){
    Intconf <- paste0("supérieur à ",floor(TtZ-1.96*ec),"%")
  }else{
    Intconf <- paste0("[ ",floor(TtZ-1.96*ec),"% , ",ceiling(TtZ+1.96*ec),"% ]")
  }
  return(c(zone,paste0(r1," / ",r2),round(cv,1),round(ec,1),round(TtZ,1),Intconf))
} 









