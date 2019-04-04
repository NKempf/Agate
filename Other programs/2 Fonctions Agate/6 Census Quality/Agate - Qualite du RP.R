#### Fonction de qualité ####
Qlbsrt <- function(Y,zone,rpa,constante=0.4,ril){
  ## Estimation par le ratio d'un total
  ## T_Xrec : totaux pour les addresse prit avec probabilité 1.
  
  EstAdr<-ril %>% group_by(com) %>% mutate(Id=(idZonage==zone)) %>% summarise(TX_=sum(Id*nb_logn),TtX_=sum(nb_logn))
  
  rpaR <- rpa %>% select(C_ANNEE_COL,com,idZonage,!!parse_quosure(Y),X,IPOND) %>% mutate(Id=(idZonage==zone)) %>% mutate(Z=Id*!!parse_quosure(Y))
  
  T_rec <- rpaR %>% group_by(com) %>% filter(IPOND<1.75) %>% summarise(TZrec=sum(Z*IPOND),TXrec=sum(Id*X*IPOND),TtXrec=sum(X*IPOND))
  T_R <- rpaR %>% group_by(com) %>% filter(IPOND>1.75) %>% summarise(TZ=sum(Z*IPOND),TX=sum(Id*X*IPOND),TtX=sum(X*IPOND)) %>% 
    full_join(T_rec, by="com") %>% left_join(EstAdr, by="com")
  T_R[is.na(T_R)]<-0
  T_R <- T_R %>% mutate(TXmod=(TX_-TXrec)*(TtX/(TtX_-TtXrec))) %>% filter(com!=0)
  # ,test=5*sum(abs(0.5-Id))
  rpaZ <- rpaR %>% left_join(T_R, by="com") %>% mutate(Z2=(Z-(TZ/TX)*Id*X),Ztest=Z) %>% filter(IPOND>1.75) 
  
  Tt <- rpaZ %>% group_by(C_ANNEE_COL,com) %>% summarise(TZg=sum(Z2*IPOND),TXg=sum(X*IPOND),TZgtest=sum(Ztest*IPOND))
  
  rpaU <- rpaZ %>% left_join(Tt, by = c("C_ANNEE_COL", "com")) %>% mutate(U2 = (Z2-(TZg/TXg)*X)^2,Utest = (Ztest-(TZgtest/TXg)*X)^2)
  
  Vf <- (1-constante)/(constante^2)*sum(rpaU$U2)
  a<-T_R %>% mutate(Z=TZ/TX*TXmod) %>% filter(!(is.na(Z)|is.nan(Z)))

  TtZ<- sum(a$Z)+sum(T_R$TZrec)
  return(c(zone,Y,round(TtZ),Vf))
}
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
  
  ####
  
  rpaR <- rpa %>% select(C_ANNEE_COL,com,idZonage,!!parse_quosure(r1),!!parse_quosure(r2),X,IPOND) %>% mutate(Id=(idZonage==zone),join=1,Sd=IPOND>1.75) %>% mutate(Rz1=Id*!!parse_quosure(r1),Rz2=Id*!!parse_quosure(r2)) %>%
                 group_by(C_ANNEE_COL,com,join,Id,Rz1,Rz2,X,Sd) %>% summarise(IPOND=sum(IPOND),n=sum(join)) %>% ungroup()
  
  #### 0.0155s
  
  T_Rg <- rpaR %>% group_by(C_ANNEE_COL,com,Sd) %>% summarise(Tg1=sum(Rz1*IPOND),Tg2=sum(Rz2*IPOND),TXg=sum(X*IPOND))
  
  #### 0.0038s
  
  T_R <- T_Rg %>% group_by(Sd) %>% summarise(TR1=sum(Tg1),TR2=sum(Tg2)) %>% right_join(T_Rg, by="Sd") %>% mutate(CfX=(Tg1-TR1/TR2*Tg2)/TR2/TXg,TX=TR1/TR2)
  
  #### 0.0035s
  
  rpaU <- rpaR %>% filter(Sd) %>% left_join(T_R,by = c("C_ANNEE_COL","com","Sd")) %>% mutate(U2 =((Rz1-TX*Rz2)/(TR2) - CfX*X)^2) %>% 
    filter(!is.nan(U2))
  
  #### 0.0043s

  Vf <- (1-constante)/(constante^2)*sum(rpaU$U2)
  TtZ<- sum(T_R$TR1)/sum(T_R$TR2)*100

  return(c(zone,paste0(r1," / ",r2),round(TtZ,1),Vf))
} 
Qlinter<-function(zone,total_var,rpa,ril){
  t1 <- Sys.time()
  
  rpa2 <- rpa %>% filter(com %in% unique(rpa$com[rpa$idZonage == zone]))
  
  a<-apply(as.array(total_var),1, Qlbsrt, zone,rpa=rpa2,ril = ril)
  a<-as.data.frame(t(a))
  colnames(a)<-c("zonage","Variable","EstVariable","Variance")
  print(paste0(zone," finis en ",round(Sys.time()-t1,3)))
  return(a)
}
Qlrintr<-function(zone,ratio_var,rpa){
  t1 <- Sys.time()
  
  
  rpa2 <- rpa %>% filter(com %in% unique(rpa$com[rpa$idZonage == zone]))
  
  a<-apply(as.array(ratio_var),1, Qlrtio, zone,rpa=rpa2)
  a<-as.data.frame(t(a))
  colnames(a)<-c("zonage","Variable","EstVariable","Variance")
  print(paste0(zone," finis en ",round(Sys.time()-t1,3)))
  return(a)
}

Qlfinal<-function(rpa,group_var,ril){
  zone <- rpa %>% filter(!duplicated(idZonage)) %>% select(idZonage)
  
  total_var<-group_var[!grepl(" / ", group_var, fixed=TRUE)]
  if(!is_empty(total_var)){
    totaux<-bind_rows(apply(zone, 1, Qlinter,total_var,rpa,ril))
    totaux$`Ecart-Type`<-as.numeric(totaux$`Variance`)^0.5
    totaux$EstVariable<-as.numeric(totaux$EstVariable)
    totaux$CoefVariation<-totaux$`Ecart-Type`/totaux$EstVariable*100
    totaux$`IntervalConf.`<-paste0("[ ",floor(totaux$EstVariable-1.96*totaux$`Ecart-Type`)," , ",ceiling(totaux$EstVariable+1.96*totaux$`Ecart-Type`)," ]")
    r<-totaux
  }
  ratio_var<-group_var[grepl(" / ", group_var, fixed=TRUE)]
  if(!is_empty(ratio_var)){
    ratios<-bind_rows(apply(zone, 1, Qlrintr,ratio_var,rpa))
    r<-ratios
    ratios$`Ecart-Type`<-as.numeric(ratios$`Variance`)^0.5*100
    ratios$EstVariable<-as.numeric(ratios$EstVariable)
    ratios$CoefVariation<-ratios$`Ecart-Type`/(50-abs(ratios$EstVariable-50))*100
    ratios$`IntervalConf.`<-paste0("[ ",floor(ratios$EstVariable-1.96*ratios$`Ecart-Type`),"% , ",ceiling(ratios$EstVariable+1.96*ratios$`Ecart-Type`),"% ]")
    
    if(!is_empty(total_var)){
      r<-rbind(totaux,ratios)
    }
  }
    return(r)
}
