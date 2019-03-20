library(sqldf)



load("Data/Rp/rpa13.RData")


#### on prend

RP<-sqldf("select idx,recensement as ind, com as com,gr as g, NLOG_recensement as X
      , NLOG_ril as x_ril, INPER as Y,IPONDL as w, idZonage as Zone from rpa")


X_<-sqldf("select com,g,sum(X)/sum(ind) as X_ from RP where w>1.75 group by com,g") #Nombre moyen de Logement par addresse recense
Y_<-sqldf("select com,g,sum(y)/sum(ind) as Y_ from RP where w>1.75 group by com,g") #Nombre Moyen d'individu par addresse recende

##### on calcule uniquement pour un QPV a la fois
zone<-("QP971005")


CommuneQ<- sqldf(paste0("select distinct com as ComQ from RP where Zone=='",zone,"'"))
Z <- sqldf(paste0("select *,Zone=='",zone,"' as I1 from RP LEFT join  X_ using(com,g) LEFT join Y_ using(com,g) join CommuneQ where com=comQ and ind=1"))
    #Nombre d'individu dans une adresse recense et 
Z <- sqldf("select *,I1*Y as Z from Z")
Z_<-sqldf("select com,g,sum(Z)/sum(ind) as Z_ from Z where w>1.75 group by com,g")
U <- sqldf("select *,Z-(Z_/X_*X) as U from Z join z_ using (com,g) where w>1.75")

Z_2<- sqldf("select sum(Z*w) as Z_2 from Z")
  
VFSS_U <- sqldf("select com,g,U*U as U2 from U")
VFS_SU <- sqldf("select com,g,sum(U2) as SU from VFSS_U group by com,g")
VF_SSU <- sqldf("select com,sum(SU) as SSU from VFS_SU group by com")
V_FSSU <- sqldf("select (1-0.4)*6.25*SSU as V from VF_SSU")
V <- sqldf("select Sum(V) as V from V_FSSU")
CV<-sqldf("select 100*sqrt(V)/Z_2 from Z_2 join V ")

# 21.4 / 18.9 / 15.9 /  7.4
# 32.4 / 29.2 / 27.9 / 13.9
# 10.9 /  9.7 /  9.0 /  6.5
# 38.4 / 33.6 / 27.9 /  9.9
# 18.2 / 16.2 / 13.6 /  5.9
#  0   /  6.3 /  6.1  #Balif est une petit communes donc vrai recensement donc pas de sondage donc pas de variance
                      # excepté la Variance liée a la non-réponce


a<-sqldf("select distinct com,IpondL from rpa")
### test sur le nombre de logement ###

Nb_log <- rpa %>% group_by(gr,com) %>% 
  summarise(nb_Ril=sum(NLOG_ril))

Nb_log2 <- rpa %>% group_by(gr,com) %>% filter(recensement==1) %>%
  summarise(nb_recense=sum(NLOG_recensement*IPONDL))

Nb_log$nb_recense <- Nb_log2$nb_recense 
####  Réécriture en tidyverse ####

library(tidyverse)

zone<-("QP971001")

Qualite<-function(zone){
  CommuneZone <- rpa %>% filter( idZonage==zone) %>% filter(!duplicated(com))
  
  Z_t<- rpa %>% filter(recensement==1 & com %in% CommuneZone$com) 
  Z_t$I<- Z_t$idZonage==zone #liste des Adresse dans la zone
  Z_t$Z<-Z_t$I*Z_t$INPER
  
  Z<- rpa %>% filter(recensement==1 & IPONDL>1.75 & com %in% CommuneZone$com)   # l'ensemble des Ligne dans une commune de la zone et recensé
  
  Z$I<- Z$idZonage==zone #liste des Adresse dans la zone
        
  Z$Z<-Z$I*Z$INPER      #Crée la Colonne Z (variable d'interais dans la zone d'interet)
  
  Z_g<-Z %>% 
    group_by(com,gr) %>%
    summarise(Z_ = sum(Z))
  
  X_g<-rpa %>% 
    group_by(com,gr) %>%
    summarise(X_ = sum(NLOG_recensement))
  
  U <- Z %>% left_join(X_g) %>% right_join(Z_g)
  U <- U%>% mutate(U=Z-Z_/X_*NLOG_recensement)  # Création de la table des 'U'      
  
  X_<- Z %>% filter(Z$I)#nombre de logement dans le QPV
  
  VFSS_U <- U %>% mutate(U2=U*U)
  VF_SSU <- VFSS_U %>% 
    ungroup() %>%
    summarise(VF_ = sum(U2))
  V <- VF_SSU %>% 
    mutate (V= 0.6*6.25*VF_) %>%
    select(V)

  Z_ <- Z_t %>%
    summarise(Z_ = sum(Z_t*IPONDL))
  Final <- V 
  Final$Z <-Z_$Z_
  Final <- Final %>% mutate (CV = V^0.5/Z*100, s = V^0.5)
  return(Final)
}

#### chargement des package

library(rgdal)
library(rgeos) 
library(tidyverse)
library(sampling)
library(fst)

######################################################################################################
#### code de nicola ####
########################
load("Data/Rp/rpa13.RData")

source("Other programs/2 Fonctions Agate/2 Cartographie/Agate - Cartographie fct.R",encoding = "UTF-8")
source("Other programs/2 Fonctions Agate/6 Census Quality/Agate - Census infra quality.R")

# I.2. RP niveau logement et individu
#------------------------------------
rp13l <- read_fst("Data/Rp/rp13l.fst")
# Ajout d'autres variables
rpl_rpa <- rp13l %>% 
  select(idrp,INPCM) %>% 
  group_by(idrp) %>% 
  summarise(INPCM = sum(INPCM)) %>% 
  ungroup()

# Ajout des nouvelles variables à la table adresse
rpa13 <- rpa13 %>% 
  left_join(rpl_rpa,c("idx"="idrp")) %>% 
  mutate(INPCM = ifelse(is.na(INPCM),0,INPCM))

# I.3. RIL
#---------
load("Data/Ril/ril15.RData")

# I.4. Zonage
#------------
load("Data/QPV/qpvFake.RData")

qpv_stat.fake <- spTransform(qpv_stat.fake, CRS("+init=epsg:3857"))

qpv_stat.fake@data <- qpv_stat.fake@data %>% 
  select(CODE_QP,NOM_QP) %>% 
  mutate(NOM_QP = iconv(NOM_QP,to="ASCII//TRANSLIT"))


# II. Qualité du RP dans la zone
#---------------------------------------------------------------------------------------------------------------------------------------------------------
# Parametres
zonage <- qpv_stat.fake
zonage@data$idZonage <- zonage@data$CODE_QP
ril <- rilhab15
rpa <- rpa13
Y <- "INPCM"
idZonage <- "idZonage"

# Ajout de la superficie à la couche Zonage
zonage <- superficieZon(zonage = zonage,idZonage = idZonage)

# Jointure spatiale
ril <- zonaPts(pts.sp = ril,zonage = zonage)

# Ajout de la variable CODE_QP a la base rpa
rpa <- merge(rpa,ril@data[,c("idx","idZonage")],by="idx",all.x=T)

### Toute adresse non affectée à un zonage QPV donné est considérée hors du périmètre des
### quartiers Politique de la Ville.
rpa[,idZonage][is.na(rpa[,idZonage]) | rpa$idZonage == "Hors zonage"] <- "horsZon"



##### test base de lionelle ######################################################################################################################
setwd(dir = "U:/3-Action régionale/8 Etudes non partenariales/0 outils statistiques/1 Agate/1 Qualite Lionel Delta/QPV")
if(! exists("rpl_qpv")){load("Extension/base_qpv.Rdata")}

  ###### Agrégation au niveau adresse des données du recensement ######
  Y="INPER"
  groupevars <- c("carto_ril", "IPONDL", "idx", "ilot", "idZonageV", "com", "gr", "C_ANNEE_COL")
  
  rpa_qpv <- aggregate( as.formula(paste(paste0("cbind(comptage_recensement, comptage_menages,", Y, ")"), paste(groupevars, collapse = "+")   ,sep = "~")) , data=rpl_qpv, sum)
  
  names(rpa_qpv)[match("comptage_recensement",names(rpa_qpv))] <- "NLOG"
  names(rpa_qpv)[match("comptage_menages",names(rpa_qpv))] <- "NB_menages"
  
  rpa_qpv$comptage <- 1
  
  rpa<- rpa_qpv
  rpa$recensement <- T
  rpa$NLOG_recensement <- rpa$NLOG
  rpa$idZonage <- rpa$idZonageV
##############################################################
  
############################
##### Fonction Qualité #####
############################
  
  
Ql <- function(zone){
  ## CommuneZone : Liste des communes dans la zone
  ## rpaZ        : Liste des adresse recensé avec Z, Id€Zone
  ## rpaZ_s      : Liste des adresse sondée (ou f=0.4 / w=2.5)  <- ## pour l'instant ou ZAP ##
  ## T_Xg        : Nombre total de logement (com/gr) d'apres le recensement
  ## T_Yg        : Nombre total de Y        (com/gr)
  ## T_Zg        : Nombre total de Z        (com/gr)
  ## Tt          : Table des Total et Ratio (com/gr)
        # Ry     : Nombre de Y par logement (com/gr) 
        # Rz     : Nombre de Z par logement (com/gr) 
  ## rpaU        : table avec les U = Z - RX
  ## T_Ug        : Total de U = Z-RX        (com/gr)
  ## U_          : moyenne des U = Z-RX     (com/gr)
  ## rpaf        : rpa avec les Tf = Z - RX - U_ et Tf²
  
  CommuneZone <- rpa %>% filter( idZonage==zone) %>% filter(!duplicated(com)) %>% select(com)
  
  rpaZ <- rpa %>% filter(com %in% CommuneZone$com,recensement)
  rpaZ$Id<- rpaZ$idZonage==zone #"QP971001"
  rpaZ$Z<-rpaZ$Id*rpaZ$INPER    #change INPER par variable Y au choix
  
  rpaZ_s <- rpaZ %>% filter(IPONDL>1.75 & !com %in% c("97360","97357"))
  
  T_Xg <- rpaZ %>% group_by(C_ANNEE_COL,com) %>% summarise(TXg=sum(NLOG_recensement*IPONDL))
  T_Yg <- rpaZ %>% group_by(C_ANNEE_COL,com) %>% summarise(TYg=sum(INPER *IPONDL))
  T_Zg <- rpaZ %>% group_by(C_ANNEE_COL,com) %>% summarise(TZg=sum(Z*IPONDL))
  
  Tt <- T_Xg %>% left_join(T_Yg) %>% left_join(T_Zg)
  Tt$Ry<- Tt$TYg/Tt$TXg
  Tt$Rz<- Tt$TZg/Tt$TXg
  
  rpaU <- rpaZ %>% left_join(Tt) %>% mutate(U = Z-Ry*NLOG_recensement)
  #rpaU <- rpaZ %>% left_join(Tt) %>% mutate(U = Z-Rz*NLOG_recensement)
  
  T_Ug <- rpaU %>% group_by(C_ANNEE_COL,com) %>% summarise(TUg=sum(U*IPONDL))
  U_ <- rpaU %>% group_by(C_ANNEE_COL,com) %>% summarise(u_=sum(U*IPONDL)/sum(IPONDL))
  
  rpaf <- rpaU %>% left_join(U_) %>% mutate(Tf = U -u_, Tf2 = (U -u_)^2)
  
  Vf <- 3.75*sum(rpaf$Tf2)
  ec <- Vf^0.5
  cv <- ec/sum(Tt$TZg)
  
  
  return(c(cv,ec,sum(Tt$TZg)))
}


rpatest<-rpa
rpatest[!rpatest$idZonage %in%c("QP971001","QP971002","QP971003","QP971004","QP971005","QP971006"),]$idZonage<-"HorsZone"
  
Qlf <- function(zone,rpa,constante=3.75){
    ## CommuneZone : Liste des communes dans la zone
    ## rpaZ        : Liste des adresse recensé avec Z, Id€Zone
    ## rpaZ_s      : Liste des adresse sondée (ou f=0.4 / w=2.5)  <- ## pour l'instant ou ZAP ##
    ## T_Xg        : Nombre total de logement (com/gr) d'apres le recensement
    ## T_Yg        : Nombre total de Y        (com/gr)
    ## T_Zg        : Nombre total de Z        (com/gr)
    ## Tt          : Table des Total et Ratio (com/gr)
    # Rz     : Nombre de Z par logement (com/gr) 
    ## rpaU        : table avec les U = Z - RX
    ## T_Ug        : Total de U = Z-RX        (com/gr)
    ## U_          : moyenne des U = Z-RX     (com/gr)
    ## rpaf        : rpa avec les Tf = Z - RX - U_ et Tf²
    
    CommuneZone <- rpa %>% filter( idZonage==zone) %>% filter(!duplicated(com)) %>% select(com)

    rpaZ_t <- rpa %>% filter(com %in% CommuneZone$com,recensement) %>% mutate(Id=(idZonage==zone)) %>% mutate(Z=Id*Y)
    
    rpaZ <- rpaZ_t %>% filter(IPONDL>1.75 & !com %in% c("97360","97357"))
    T_Xg <- rpaZ %>% group_by(C_ANNEE_COL,com) %>% summarise(TXg=sum(NLOG_recensement*IPONDL))
    T_Zg <- rpaZ %>% group_by(C_ANNEE_COL,com) %>% summarise(TZg=sum(Z*IPONDL))
    
    Tt <- T_Xg %>% left_join(T_Zg) %>% mutate(Rz=TZg/TXg)
    
    rpaU <- rpaZ %>% left_join(Tt) %>% mutate(U = Z-Rz*NLOG_recensement)
    
    T_Ug <- rpaU %>% group_by(C_ANNEE_COL,com) %>% summarise(TUg=sum(U*IPONDL))
    U_ <- rpaU %>% group_by(C_ANNEE_COL,com) %>% summarise(u_=sum(U*IPONDL)/sum(IPONDL))
    
    rpaf <- rpaU %>% left_join(U_) %>% mutate(Tf = U -u_, Tf2 = (U -u_)^2)
    
    
    T_Zg_t <- rpaZ_t %>% group_by(C_ANNEE_COL,com) %>% summarise(TZg=sum(Z*IPONDL))
    
    Vf <- constante*sum(rpaf$Tf2)
    ec <- Vf^0.5
    cv <- ec/sum(T_Zg_t$TZg)
    TtZ<- sum(T_Zg_t$TZg)
    return(c(cv,ec,TtZ,TtZ-1.96*ec,TtZ+1.96*ec))
}

QlfTotal <- function (rpa,y){
  y <- substitute(y)
  rpa2 <- rpa %>% mutate(Y=eval(y))
  zone <- rpa %>% filter(idZonage!="horsZon") %>% filter(!duplicated(idZonage)) %>% select(idZonage)
  r<-apply(zone,1,Qlf,rpa2)
  r<-t(r)
  colnames(r)<-c("CV","Ecart-Type","Tz","TzMin","TzMax")
  row.names(r)<-zone$idZonage
  return(r)
} 
  
  

#### ratio , sans effet de la stratification ####
Qlfration <- function (zone,rpa){
  ## R           : Ration (Y1/Y2)
  ## 'Z'     : variable linéariser pour les calcule de variance 
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
  
  CommuneZone <- rpa %>% filter( idZonage==zone) %>% filter(!duplicated(com)) %>% select(com)
  
  rpaR <- rpa %>% filter(com %in% CommuneZone$com,recensement) %>% mutate(Id=(idZonage==zone),join=1) %>% mutate(Rz1=Id*r1,Rz2=Id*r2)
  T_Rg <- rpaR %>% group_by(join) %>% summarise(TR1=sum(Rz1*IPONDL),TR2=sum(Rz2*IPONDL)) %>% mutate(TR= TR1/TR2)
  
  rpaZ_t <- rpaR %>% left_join(T_Rg) %>% mutate(Z=(Rz1-TR*Rz2)/TR2) %>% filter(!is.nan(Z))
  
  rpaZ <- rpaZ_t %>% filter(IPONDL>1.75 & !com %in% c("97360","97357"))
  T_Xg <- rpaZ %>% group_by(join) %>% summarise(TXg=sum(NLOG_recensement*IPONDL))
  T_Zg <- rpaZ %>% group_by(join) %>% summarise(TZg=sum(Z*IPONDL))
  
  Tt <- T_Xg %>% left_join(T_Zg) %>% mutate(Rz=TZg/TXg)
  
  rpaU <- rpaZ %>% left_join(Tt) %>% mutate(U = Z-Rz*NLOG_recensement)
  
  T_Ug <- rpaU %>% group_by(join) %>% summarise(TUg=sum(U*IPONDL))
  U_ <- rpaU %>% group_by(join) %>% summarise(u_=sum(U*IPONDL)/sum(IPONDL))
  
  rpaf <- rpaU %>% left_join(U_) %>% mutate(Tf = U -u_, Tf2 = (U -u_)^2)
  

  T_R <- T_Rg %>% ungroup() %>% summarise(Tt1=sum(TR1),Tt2=sum(TR2)) %>% mutate(TR=Tt1/Tt2)
  
  
  Vf <- 3.75*sum(rpaf$Tf2)
  ec <- Vf^0.5
  cv <- ec/sum(T_R$TR)
  TtZ<- sum(T_R$TR)
  TR1<- sum(T_R$Tt1)
  TR2<- sum(T_R$Tt2)
  return(c(TR1,TR2,T_R$TR2[1],cv,ec,TtZ,TtZ-1.96*ec,TtZ+1.96*ec))
} 
QlfrationTotal <- function (rpa,r1,r2){
  r1 <- substitute(r1)
  r2 <- substitute(r2)
  rpa2 <- rpa %>% mutate(r1=eval(r1),r2=eval(r2))
  zone <- rpa %>% filter(idZonage!="HorsZone") %>% filter(!duplicated(idZonage)) %>% select(idZonage)
  r<-apply(zone,1,Qlfration,rpa2)
  r<-t(r)
  colnames(r)<-c("R1","R2","CV","Ecart-Type","Tz","TzMin","TzMax")
  row.names(r)<-zone$idZonage
  return(r)
} 


#### avec la stratification ####
Qlfration <- function (zone,rpa){
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
  
  CommuneZone <- rpa %>% filter( idZonage==zone) %>% filter(!duplicated(com)) %>% select(com)
  
  rpaR <- rpa %>% filter(com %in% CommuneZone$com,recensement) %>% mutate(Id=(idZonage==zone),join=1) %>% mutate(Rz1=Id*r1,Rz2=Id*r2)
  T_R <- rpaR %>% group_by(join) %>% summarise(TR1=sum(Rz1*IPONDL),TR2=sum(Rz2*IPONDL),n=sum(join)) %>% mutate(TR= TR1/TR2)
  
  rpaZ_t <- rpaR %>% left_join(T_R) %>% mutate(Z=(Rz1-TR*Rz2)/(TR2)) %>% filter(!is.nan(Z))
  
  rpaZ <- rpaZ_t %>% filter(IPONDL>1.75 & !com %in% c("97360","97357"))
  T_Xg <- rpaZ %>% group_by(C_ANNEE_COL,com) %>% summarise(TXg=sum(NLOG_recensement*IPONDL))
  T_Zg <- rpaZ %>% group_by(C_ANNEE_COL,com) %>% summarise(TZg=sum(Z*IPONDL))
  
  Tt <- T_Xg %>% left_join(T_Zg) %>% mutate(Rz=TZg/TXg)
  
  rpaU <- rpaZ %>% left_join(Tt) %>% mutate(U = Z-Rz*NLOG_recensement)
  
  T_Ug <- rpaU %>% group_by(C_ANNEE_COL,com) %>% summarise(TUg=sum(U*IPONDL))
  U_ <- rpaU %>% group_by(C_ANNEE_COL,com) %>% summarise(u_=sum(U*IPONDL)/sum(IPONDL))
  
  rpaf <- rpaU %>% left_join(U_) %>% mutate(Tf = U -u_, Tf2 = (U -u_)^2)
  

  Vf <- 3.75*sum(rpaf$Tf2)
  ec <- Vf^0.5
  cv <- ec/sum(T_R$TR)
  TtZ<- sum(T_R$TR)
  TR1<- sum(T_R$TR1)
  TR2<- sum(T_R$TR2)
  return(c(TR1,TR2,cv,ec,TtZ,TtZ-1.96*ec,TtZ+1.96*ec))
} 

################################################### Calé test ###################################################

  setwd(dir = "U:/Mes documents/QPV")
  if(! exists("rpl_qpv")){load("Extension/base_qpv.Rdata")}

  groupevars <- c("carto_ril", "IPONDL", "idx", "ilot", "idZonageV", "com", "gr", "C_ANNEE_COL")
  ifelse("INPER" %in% Y, 
         rpa_qpv <- aggregate( as.formula(paste(paste0("cbind(comptage_recensement, comptage_menages,", paste(Y, collapse = ","), ")"), paste(groupevars, collapse = "+")   ,sep = "~")) , data=rpl_qpv, sum),
         rpa_qpv <- aggregate( as.formula(paste(paste0("cbind(comptage_recensement, comptage_menages, INPER,", paste(Y, collapse = ","), ")"), paste(groupevars, collapse = "+")   ,sep = "~")) , data=rpl_qpv, sum))
  names(rpa_qpv)[match("comptage_recensement",names(rpa_qpv))] <- "NLOG_recensement"
  names(rpa_qpv)[match("comptage_menages",names(rpa_qpv))] <- "NB_menages"
  names(rpa_qpv)[match("carto_ril",names(rpa_qpv))] <- "NLOG_ril"
  rpa_qpv$comptage <- 1

  rpa<-rpa_qpv

  rpa$recensement <- T
  rpa$idZonage <- rpa$idZonageV

  rpa_bis <- rpl_qpv %>% filter(!duplicated(idx))
  


sum_log_ril <- rpa_bis %>% group_by(gr,com) %>% filter(!is.na(NLOG_ril))%>% summarise(log_ril=sum(NLOG_ril))
sum_log_rc <- rpa      %>% group_by(C_ANNEE_COL,com) %>% filter(!is.na(IPONDL))  %>% summarise(log_ril=sum(NLOG_recensement*IPONDL))

  
library(icarus)
  

calage_sur_marges <- function(zonage, rpa_calage, rpa_nc,list_com,idZonage,effectifs_Zon){
  
  # Problème avec les objets du type tibble
  rpa_calage <- as.data.frame(rpa_calage) 
  rpa_nc <- rpa_nc %>% 
    as.data.frame() 
  effectifs_Zon <- effectifs_Zon %>% 
    as.data.frame()
  
  # Pour chaque commune
  rpax <- rpa_calage[rpa_calage$com == zonage, ] %>% 
    as.data.frame()
  # Recupération de la liste des zonages de la commune
  list_zonage <- unique(rpax$idZonage[rpax$com == zonage])
  list_zonage <- list_zonage[order(list_zonage)]
  
  # Pour chaque zonage de la commune, 
  for (i in 1:length(list_zonage)){
    rpax$newvar <- rpax$NLOG*(rpax[,idZonage] == list_zonage[i])
    colnames(rpax)[length(rpax)] <- paste("NLOG", zonage, list_zonage[i], sep = "_")
  }
  
  rpax$NPER_commune <- rpax$INPER*(rpax$com == zonage)
  
  # Si il n'y a aucun logement du RIL dans la commune (gestion du cas particulier de la commune 97306 qui comporte un logement dans le QPV)
  if(effectifs_Zon$NLOG_ril[effectifs_Zon$com == zonage][1] == 0 | zonage == "97306"){
    rpax$poids_cales <- rpax$IPONDL
    rpax$calage <- 0
    
  }else{
    # Sinon, on effectue le calage
    # On effectue le calage sur marge
    g <- calib(Xs = rpax[,(length(rpa_calage)+1):length(rpax)], 
               d = rpax$IPONDL, 
               total = c(effectifs_Zon[effectifs_Zon$com == zonage, c("NLOG_Zon") ], unique(effectifs_Zon[effectifs_Zon$com ==zonage, c("estimRP_totalCOM_pop") ])), 
               # total = c(effectifs_Zon[effectifs_Zon$com == zonage, c("NLOG_Zon") ], unique(effectifs_Zon[effectifs_Zon$com ==zonage, c("estimRP_totalCOM_pop") ])), 
               method = "logit", 
               bounds = c(0.4, 10))
    
    rpax <- rpa_nc[rpa_nc$com == zonage, ]
    rpax <- rpax[rpax$com %in% list_com, ]
    rpax <- rpax[!(rpax$com == "97101" & rpax$idZonage == "QP971013"),]
    rpax$poids_cales <- g*rpax$IPONDL
    rpax$calage <- 1
  }
  
  return(rpax)
}


calage_RB <- function(rpa,zone){}
  
zone<-c("QP971001","QP971002","QP971003")
CommuneQ <- rpa %>% filter(idZonage %in% zone) %>% filter(!duplicated(com)) %>% select(com)
rpaQ <- rpa %>% filter(com %in% CommuneQ$com)  
#### rectification des groupe de rotation en fonction de l'année de collect , les groupe 0 et année 2009 sont ignoré.
#An:g / 2011:3 / 2012:4 / 2013:5 / 2014:1 / 2015:2  
rpaQ[is.na(rpaQ$C_ANNEE_COL),]$C_ANNEE_COL<-0
rpaQ[rpaQ$C_ANNEE_COL=="2011",]$gr<-3
rpaQ[rpaQ$C_ANNEE_COL=="2012",]$gr<-4
rpaQ[rpaQ$C_ANNEE_COL=="2013",]$gr<-5
rpaQ[rpaQ$C_ANNEE_COL=="2014",]$gr<-1
rpaQ[rpaQ$C_ANNEE_COL=="2015",]$gr<-2

a<- rpa %>% group_by(com) %>% filter(!is.na(IPONDL)) %>% summarise(log_rc=sum(IPONDL*NLOG_recensement))
b<- rpa %>% group_by(com) %>% summarise(log_ril=sum(NLOG_ril))



load("Bdd/RData/rp13.Rdata")

load("Bdd/RData/rilhab15.Rdata")

rildata<-rilhab15@data
rpl<- rp13l
rpl$id0 <- paste0(substr(rpl$C_IMM,1,5)," ",substr(rpl$C_IMM,9,12)," ",substr(rpl$C_IMM,14,16))

test <- rildata %>% left_join(rpl)





t1 <- Sys.time()
QlfrationTotal(rpa,INPCM,INPER)
Sys.time() - t1
  
  
t1 <- Sys.time()
QlfTotal(rpa,INPCM)
QlfTotal(rpa,INPER)
Sys.time() - t1
#################################################################################################################
###################################### Derniere version Qualité avant opti ######################################
#################################################################################################################

library(rlang)
library(tidyverse)

Qlbase <- function(Y,zone,rpa,constante=0.4){
  ## CommuneZone : Liste des communes dans la zone
  ## rpaZ        : Liste des adresse recensé avec Z, Id€Zone
  ## rpaZ_s      : Liste des adresse sondée (ou f=0.4 / w=2.5)  <- ## pour l'instant ou ZAP ##
  ## T_Xg        : Nombre total de logement (com/gr) d'apres le recensement
  ## T_Yg        : Nombre total de Y        (com/gr)
  ## T_Zg        : Nombre total de Z        (com/gr)
  ## Tt          : Table des Total et Ratio (com/gr)
  # Rz     : Nombre de Z par logement (com/gr) 
  ## rpaU        : table avec les U = Z - RX
  ## T_Ug        : Total de U = Z-RX        (com/gr)
  ## U_          : moyenne des U = Z-RX     (com/gr)
  ## rpaf        : rpa avec les Tf = Z - RX - U_ et Tf²
  
  rpaZ_t <- rpa %>% mutate(Id=(idZonage==zone)) %>% mutate(Z=Id*!!parse_quosure(Y))
  
  rpaZ <- rpaZ_t %>% filter(IPOND>1.75 & !com %in% c("97360","97357"))
  T_Xg <- rpaZ %>% group_by(C_ANNEE_COL,com) %>% summarise(TXg=sum(X*IPOND))
  T_Zg <- rpaZ %>% group_by(C_ANNEE_COL,com) %>% summarise(TZg=sum(Z*IPOND))
  Tt <- T_Xg %>% left_join(T_Zg,by = c("C_ANNEE_COL", "com")) %>% mutate(Rz=TZg/TXg)
  
  rpaU <- rpaZ %>% left_join(Tt,by = c("C_ANNEE_COL", "com")) %>% mutate(U = (Z-Rz*X))
  
  T_Ug <- rpaU %>% group_by(C_ANNEE_COL,com) %>% summarise(TUg=sum(U*IPOND))
  U_ <- rpaU %>% group_by(C_ANNEE_COL,com) %>% summarise(u_=sum(U*IPOND)/sum(IPOND))
  rpaf <- rpaU %>% left_join(U_,by = c("C_ANNEE_COL", "com")) %>% mutate(Tf = U -u_, Tf2 = (U -u_)^2)
  
  
  T_Zg_t <- rpaZ_t %>% group_by(C_ANNEE_COL,com) %>% summarise(TZg=sum(Z*IPOND))
  
  Vf <- (1-constante)/(constante^2)*sum(rpaU$Tf2)
  ec <- Vf^0.5
  cv <- ec/sum(T_Zg_t$TZg)
  TtZ<- sum(T_Zg_t$TZg)
  return(c(zone,Y,round(cv,3),round(ec),round(TtZ),round(TtZ-1.96*ec),round(TtZ+1.96*ec)))
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
  
  rpaR <- rpa %>% mutate(Id=(idZonage==zone),join=1) %>% mutate(Rz1=Id*!!parse_quosure(r1),Rz2=Id*!!parse_quosure(r2))
  T_R <- rpaR %>% group_by(join) %>% summarise(TR1=sum(Rz1*IPOND),TR2=sum(Rz2*IPOND),n=sum(join)) %>% mutate(TR= TR1/TR2)
  
  rpaZ_t <- rpaR %>% left_join(T_R,by = "join") %>% mutate(Z=(Rz1-TR*Rz2)/(TR2)) %>% filter(!is.nan(Z))
  
  rpaZ <- rpaZ_t %>% filter(IPOND>1.75 & !com %in% c("97360","97357"))
  T_Xg <- rpaZ %>% group_by(C_ANNEE_COL,com) %>% summarise(TXg=sum(X*IPOND))
  T_Zg <- rpaZ %>% group_by(C_ANNEE_COL,com) %>% summarise(TZg=sum(Z*IPOND))
  
  Tt <- T_Xg %>% left_join(T_Zg, by = c("C_ANNEE_COL", "com")) %>% mutate(Rz=TZg/TXg)
  
  rpaU <- rpaZ %>% left_join(Tt, by = c("C_ANNEE_COL", "com")) %>% mutate(U = Z-Rz*X)
  
  T_Ug <- rpaU %>% group_by(C_ANNEE_COL,com) %>% summarise(TUg=sum(U*IPOND))
  U_ <- rpaU %>% group_by(C_ANNEE_COL,com) %>% summarise(u_=sum(U*IPOND)/sum(IPOND))
  
  rpaf <- rpaU %>% left_join(U_, by = c("C_ANNEE_COL", "com")) %>% mutate(Tf = U -u_, Tf2 = (U -u_)^2)
  
  
  Vf <- (1-constante)/(constante^2)*sum(rpaf$Tf2)
  ec <- Vf^0.5
  cv <- ec/sum(T_R$TR)
  TtZ<- sum(T_R$TR)
  return(c(zone,paste0(r1," / ",r2),round(cv,3),round(ec,3),round(TtZ,3),round(TtZ-1.96*ec,3),round(TtZ+1.96*ec,3)))
} 
Qlfinal<-function(rpa,group_var){
  zone <- rpa %>% filter(idZonage!="Hors zonage") %>% filter(!duplicated(idZonage)) %>% select(idZonage)
  
  total_var<-group_var[!grepl(" / ", group_var, fixed=TRUE)]
  if(!is_empty(total_var)){
    totaux<-bind_rows(apply(zone, 1, Qlinter,total_var,rpa))
    r<-totaux
  }
  ratio_var<-group_var[grepl(" / ", group_var, fixed=TRUE)]
  if(!is_empty(ratio_var)){
    ratios<-bind_rows(apply(zone, 1, Qlrintr,ratio_var,rpa))
    r<-ratio
    if(!is_empty(total_var)){
      r<-rbind(totaux,ratios)
    }
  }
  return(r)
}

Qlinter<-function(zone,total_var,rpa){
  a<-apply(as.array(total_var),1, Qlbase, zone,rpa=rpa)
  a<-as.data.frame(t(a))
  colnames(a)<-c("zonage","Variable","CoefVariation","Ecart-Type","EstVariable","minIntervalConf.","maxIntervalConf.")
  return(a)
}
Qlrintr<-function(zone,ratio_var,rpa){
  a<-apply(as.array(ratio_var),1, Qlrtio, zone,rpa=rpa)
  a<-as.data.frame(t(a))
  colnames(a)<-c("zonage","Variable","CoefVariation","Ecart-Type","EstVariable","minIntervalConf.","maxIntervalConf.")
  return(a)
}



######################################################################################################
###################################### Derniere version Qualité ######################################
######################################################################################################

library(rlang)
library(tidyverse)

Qlbase <- function(Y,zone,rpa,constante=0.4){
  ## rpaZ_t      : Liste des adresse avec Z, Id€Zone
  ## rpaZ        : Liste des adresse sondée avec Z, Id€Zone (ou f=0.4 / w=2.5)
  ## Tt          : Table des Total et Ratio (com/gr)
  ## rpaU        : table avec les U2 = (Z - RX)^2
  ## T_Zg_t      : 

  
  rpaZ_t <- rpa %>% mutate(Id=(idZonage==zone)) %>% mutate(Z=Id*!!parse_quosure(Y))
  
  rpaZ <- rpaZ_t %>% filter(IPOND>1.75 & !com %in% c("97360","97357"))
  
  Tt <- rpaZ %>% group_by(C_ANNEE_COL,com) %>% summarise(TXg=sum(X*IPOND),TZg=sum(Z*IPOND))
  
  rpaU <- rpaZ %>% left_join(Tt,by = c("C_ANNEE_COL", "com")) %>% mutate(U2 = (Z-TZg/TXg*X)^2) %>% filter(TZg!=0)
  
  T_Zg_t <- rpaZ_t %>% group_by(C_ANNEE_COL,com) %>% summarise(TZg=sum(Z*IPOND))
  
  Vf <- (1-constante)/(constante^2)*sum(rpaU$U2)
  ec <- Vf^0.5
  TtZ<- sum(T_Zg_t$TZg)
  cv <- ec/TtZ*100
  return(c(zone,Y,round(cv,1),round(ec),round(TtZ),paste0("[ ",floor(TtZ-1.96*ec)," ; ",ceiling(TtZ+1.96*ec)," ]")))
}
Qlbsrt <- function(Y,zone,rpa,constante=0.4){
  ## Estimation par le ratio d'un total
  ## T_Xrec : totaux pour les addresse prit avec probabilité 1.
  
  EstAdr<-pts.sp@data %>% group_by(com) %>% mutate(Id=(idZonage==zone)) %>% summarise(TX_=sum(Id*nb_logn),TtX_=sum(nb_logn))

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
    Intconf <- paste0("inferieur à ",ceiling(TtZ+1.96*ec),"%")
  }else if(floor(TtZ-1.96*ec)>=90 & ceiling(TtZ+1.96*ec)>97){
    Intconf <- paste0("supérieur à ",floor(TtZ-1.96*ec),"%")
  }else{
    Intconf <- paste0("[ ",floor(TtZ-1.96*ec),"% , ",ceiling(TtZ+1.96*ec),"% ]")
  }
  return(c(zone,paste0(r1," / ",r2),round(cv,1),round(ec,1),round(TtZ,1),Intconf))
} 

Qlinter<-function(zone,total_var,rpa){
  t1 <- Sys.time()
  
  rpa2 <- rpa %>% filter(com %in% unique(rpa$com[rpa$idZonage == zone]))
  
  a<-apply(as.array(total_var),1, Qlbsrt, zone,rpa=rpa2)
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

Qlfinal<-function(rpa,group_var){
  zone <- rpa %>% filter(!duplicated(idZonage)) %>% select(idZonage)

  total_var<-group_var[!grepl(" / ", group_var, fixed=TRUE)]
  if(!is_empty(total_var)){
    totaux<-bind_rows(apply(zone, 1, Qlinter,total_var,rpa))
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


rpi<-rp14i
rpl<-rp14l

t1 <- Sys.time()

rpa <- rpa %>% right_join(rpa2)
rpa[is.na(rpa)]<-0
Sys.time() - t1
#rpai   167 850
#rpal   204 828

group_var=c("INPER","NbFemme / INPER","NbHomme / INPER","NbJeune / INPER","NbMoyen / INPER","NbVieux / INPER", #Territoire
            "NbVieux2 / INPER","NbMoyen2 / INPER","NbVieux3 / INPER",
            "ACTIF / NbAgeTravaille","- ACTIF / NbAgeTravaille","INPCM / ACTIF","- INPCM / ACTIF","HommeActif / NbHomme", # Emploi
            "- HommeActif / NbHomme","FemmeActif / NbFemme","- FemmeActif / NbFemme","NbCadre / NbAgeTravaille","- NbCadre / NbAgeTravaille",
            "NbEtudian1825 / INPER","NbEtudian0206 / INPER","NbEtudian0614 / Nb0614","NbDecrocheur / Nb1625","NbScole_15plus / NbScole", #Scolarité
            "- NbEtudian1825 / INPER","- NbEtudian0206 / INPER","- NbEtudian0614 / Nb0614","- NbDecrocheur / Nb1625","- NbScole_15plus / NbScole",
            "NbImmigre / INPER","- NbImmigre / INPER","NbEtranger / INPER","- NbEtranger / INPER", # Immigration
            "CATL1 / X","CATL2 / X","CATL3 / X","CATL4 / X","NbLocataire / X","- NbLocataire / X","NblocHLM / X", #Logement
            "- NblocHLM / X","NbAppartement / X","- NbAppartement / X",
            "NbHLM / CATL1","Surface / CATL1","- Surface / CATL1","NbBain / CATL1","NbEAU / CATL1","NbEGOUL / CATL1", # Residence principal
            "- NbHLM / CATL1","- NbBain / CATL1","- NbEAU / CATL1","- NbEGOUL / CATL1"
            )
TablePassage <- data.frame(group_var)
TablePassage$LibVar <- c("population","femme","homme","[0,20)" ,"[20,65)","[65,120]","[75,120]","[20,60)","[60,75)", #Territoire
                         "actif","inactif","chomeur","actifocc","actif homme","inactif homme","actif femme","inactif femme","cadre_prof_inter","autre", #Emploi
                         "etudi[18,25)","etudi[2,6)","etudi","decrocheur","nScola_15plus","n_etudi[18,25)","n_etudi[2,6)","n_etudi","n_decrocheur","autre", #Scolarité
                         "immigre","non_immigre","etranger","francais", #Immigration
                         "CATL_1","CATL_2","CATL_3","CATL_4","locataire","autre","locataireHlm","autre","appartement","autre", #Logement
                         "hlm","surf100etplus","surf100moins","bain_douche","Eau_chaude","tout_egout","n_hlm","N_bain_douche","N_eau_chaude","N_tout_egout") # Residence principal
TablePassage$Domaine <-   c(1,1,1,1,1,1,1,1,1,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,5,5,5,5,6,6,6,6,6,6,6,6,6,6,7,7,7,7,7,7,7,7,7,7)
TablePassage$Categorie <- c(1,2,2,3,3,3,3,3,3,1,1,1,1,2,2,2,2,3,3,1,1,2,3,4,1,1,2,3,4,1,1,2,2,1,1,1,1,2,2,3,3,4,4,1,2,2,3,4,5,1,3,4,5)
group_var=c("INPER")

t1 <- Sys.time()
qualite<-Qlfinal(rpa,group_var)
Sys.time() - t1

QuatiteRatio<- qualite7 %>% left_join(qualite6, by= c("zonage")) %>% mutate(`Ecart-Type.y`=`Ecart-Type.y`/100) %>% mutate(ecartEstimation=EstVariable.x-EstVariable.y,ecartVariance=`Ecart-Type.x`-`Ecart-Type.y`)

sum(rpa$INPER * rpa$IPOND) -
sum(qualite7$EstVariable)

save(QuatiteRatio,file="Data/Tmp/QualiteRatiofinal.RData")

#####################################################################################################
#################################### Création du RPA ################################################
#####################################################################################################
library(fst)

rp.an <- "14"
rpi.path.string <- paste0("Data/Rp/rp",rp.an,"i.fst")
rpl.path.string <- paste0("Data/Rp/rp",rp.an,"l.fst")
rpiPath <- ifelse(file.exists(rpi.path.string),rpi.path.string,"Data/Rp/FakeRpi.fst")
rplPath <- ifelse(file.exists(rpl.path.string),rpl.path.string,"Data/Rp/FakeRpl.fst")

rpi <- read_fst(rpiPath)
rpl <- read_fst(rplPath) 
 
t1 <- Sys.time()
rpa1 <- rpi %>% group_by(idx,C_IMM) %>% 
  summarise(NbHomme=sum(SEXE=="homme"),NbFemme=sum(SEXE=="femme"),ACTIF=sum(inactif=="actif"),INPCM=sum(TACT=="chomeur"),
            NbJeune=sum(as.numeric(AGEREV)<20),NbVieux=sum(as.numeric(AGEREV)>=65),
            NbVieux2=sum(as.numeric(AGEREV)>=75),NbMoyen2=sum(as.numeric(AGEREV)>=20 & as.numeric(AGEREV)<60),
            NbVieux3=sum(as.numeric(AGEREV)>=60 & as.numeric(AGEREV)<75),
            NbMoyen=sum(as.numeric(AGEREV)>=20 & as.numeric(AGEREV)<65),
            FemmeActif=sum(SEXE=="femme" & inactif=="actif"),HommeActif=sum(SEXE=="homme" & inactif=="actif"),
            FemmeChomeur=sum(SEXE=="femme" & TACT=="chomeur"),HommeChomeur=sum(SEXE=="homme" & TACT=="chomeur"),
            NbEtranger=sum(etranger=="etranger"),NbImmigre=sum(immigre=="immigre"),Nb1825=sum(as.numeric(AGEREV)>=18 & as.numeric(AGEREV)<=25),
            NbEtudian1825=sum(as.numeric(AGEREV)>=18 & as.numeric(AGEREV)<25 & ETUD=="etudi"),
            NbAgeTravaille=sum(as.numeric(AGEREV)>=15 & as.numeric(AGEREV)<65),Nb0614=sum(as.numeric(AGEREV)>=06 & as.numeric(AGEREV)<14),
            NbCadre=sum(POSP %in% c("1J","1I","1H","1G","1F")),NbEtudian0206=sum(as.numeric(AGEREV)>=2 & as.numeric(AGEREV)<6 & ETUD=="etudi"),
            NbEtudian0614=sum(as.numeric(AGEREV)>=6 & as.numeric(AGEREV)<14 & ETUD=="etudi"),
            NbDecrocheur=sum(DIPL == "A" & ETUD == "n_etudi" & as.numeric(AGEREV) %in% c(16:25)),Nb1625=sum(as.numeric(AGEREV) %in% c(16:25)),
            NbScole_15plus=sum(ETUD == "n_etudi" & as.numeric(AGEREV) >= 15 & DIPL == "A"),NbScole=sum(as.numeric(AGEREV) >= 15 & DIPL == "A")
  ) %>% mutate(INPER=NbFemme+NbHomme) %>% ungroup()

Sys.time() - t1

rpa2 <- rpl %>% mutate(X=as.numeric(C_LOG)) %>% group_by(idx,C_IMM,C_ANNEE_COL,com) %>% 
  summarise(X=max(1,X),IPOND=mean(IPONDL),CATL1=sum(CATL==1),CATL2=sum(CATL==2),CATL3=sum(CATL==3),CATL4=sum(CATL==4),
            NbLocataire=sum(locataire=="locataire"),NblocHLM=sum(locataireHlm=="locataireHlm"),
            NbAppartement=sum(appartement=="appartement"),NbHLM=sum(hlm=="hlm"& CATL==1),Surface=sum(surf100=="surf100etplus"& CATL==1),NbBain=sum(BAIN=="1" & CATL==1),
            NbEAU=sum(EAU=="2"& CATL==1),NbEGOUL=sum(EGOUL =="1"& CATL==1)
  ) %>% ungroup()
Sys.time() - t1
rpa <- rpa1 %>% right_join(rpa2,by="idx")
rpa[is.na(rpa)]<-0
Sys.time() - t1

write.fst(rpa, path = "Data/Tmp/rpa.fst")


  ########################################## Test #############################

  ril.an <- "15"
  ril.path.string <- paste0("Data/Ril/ril",ril.an,".fst")
  # ril.path.string <- "Data/Ril/ril_leger.fst"
  rilPath <- ifelse(file.exists(ril.path.string),ril.path.string,"Data/Ril/FakeRil.fst")
  
  
  zonage <- readOGR(dsn = "Data/QPV/qpv.shp",encoding = "UTF-8",stringsAsFactors = FALSE)[1:6,]
  zonage@data <- zonage@data %>% 
    rename(idZonage = CODE_QP,
           idZonage.name = NOM_QP) %>% 
    select(idZonage,idZonage.name)
  zonage <- spTransform(zonage, "+init=epsg:3857")
  # II.2. Chargement des logements du RIL dans les communes d'interets
  ril <- read_fst(rilPath) %>% 
    select(idx,nb_logn,x,y) %>% 
    mutate(com = substr(idx,1,5)) %>%
    filter(com %in% c("97101","97120","97102"))
  
  # II.3. Transformation du ril en objet spatial
  coordinates(ril) <- ~x+y
  ril@proj4string <- CRS("+init=epsg:3857")
  
  # III. Ajout de la zone aux données du recensement
  #-------------------------------------------------
  
  # III.1. Zone dans laquelle chaque logement se situe
  incProgress(amount = 0.1,message = "Zone dans laquelle chaque logement se situe")
  pts.sp <- zonaPts(pts.sp = ril,zonage = zonage)
  
  # III.2. Ajout de la zone aux données du rp individu
  # Note : pour des raisons de performances, les données du RP sont préalablement filtrées selon les communes étudiées
  incProgress(amount = 0.2,message = "Ajout de la zone aux données du RP")
  


