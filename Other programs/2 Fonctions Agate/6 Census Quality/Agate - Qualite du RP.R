#### Fonction de qualité ####
Qlbsrt <- function(Y,zone,rpa,constante=0.4,ril){
  ## Estimation par le ratio d'un total
  ## T_Xrec : totaux pour les addresse prit avec probabilité 1.
  
  EstAdr<-ril %>% group_by(com) %>% mutate(Id=(idZonage==zone)) %>% summarise(TX_=sum(Id*nb_logn),TtX_=sum(nb_logn))
  
  rpaR <- rpa %>% mutate(Id=(idZonage==zone)) %>% mutate(Z=Id*!!parse_quosure(Y))
  
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
  ec <- Vf^0.5
  a<-T_R %>% mutate(Z=TZ/TX*TXmod)
  a$Z[is.na(a$Z)]<-0
  a$Z[is.nan(a$Z)]<-0
  TtZ<- sum(a$Z)+sum(T_R$TZrec) # estimation par le ration modifier pour que le total des adresse rpa = total des adresse ril
  cv <- ec/TtZ*100
  print(T_R)
  return(c(zone,Y,round(cv,1),round(ec),round(TtZ),paste0("[ ",floor(TtZ-1.96*ec)," ; ",ceiling(TtZ+1.96*ec)," ]")))
} # Necessite la base "pts.sp@data" (ril avec zonage + nombre de logement par point)
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
  
  rpaR <- rpa %>% mutate(Id=(idZonage==zone),join=1) %>% mutate(Rz1=Id*!!parse_quosure(r1),Rz2=Id*!!parse_quosure(r2))
  
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
    Intconf <- paste0("[  0% ,",ceiling(TtZ+1.96*ec),"% ]")
  }else if(floor(TtZ-1.96*ec)>=90 & ceiling(TtZ+1.96*ec)>97){
    Intconf <- paste0("[ ",floor(TtZ-1.96*ec),"% ,100% ]")
  }else{
    Intconf <- paste0("[ ",floor(TtZ-1.96*ec),"% , ",ceiling(TtZ+1.96*ec),"% ]")
  }
  return(c(zone,paste0(r1," / ",r2),round(cv,1),round(ec,1),round(TtZ,1),Intconf))
} 

Qlinter<-function(zone,total_var,rpa,ril){
  t1 <- Sys.time()
  
  rpa2 <- rpa %>% filter(com %in% unique(rpa$com[rpa$idZonage == zone]))
  
  a<-apply(as.array(total_var),1, Qlbsrt, zone,rpa=rpa2,ril = ril)
  a<-as.data.frame(t(a))
  colnames(a)<-c("zonage","Variable","CoefVariation","Ecart-Type","EstVariable","IntervalConf.")
  print(paste0(zone," finis en ",round(Sys.time()-t1,3)))
  return(a)
}
Qlrintr<-function(zone,ratio_var,rpa){
  t1 <- Sys.time()
  
  rpa2 <- rpa %>% filter(com %in% unique(rpa$com[rpa$idZonage == zone]))
  
  a<-apply(as.array(ratio_var),1, Qlrtio, zone,rpa=rpa2)
  a<-as.data.frame(t(a))
  colnames(a)<-c("zonage","Variable","CoefVariation","Ecart-Type","EstVariable","IntervalConf.")
  print(paste0(zone," finis en ",round(Sys.time()-t1,3)))
  return(a)
}

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
