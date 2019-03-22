#-------------------------------------------------------------------------------------#
#                Agate - rpa tmp Codir                                                #
#-------------------------------------------------------------------------------------#

# Nicolas Kempf & Baptiste Raimbaud

# Derniere MAJ : 18.03.2019

library(tidyverse)
library(fst)


# I. Import des bases
#----------------------------------------------------------------------------------------------------------------------------------

# Ril
ril.an <- "15"
ril.path.string <- paste0("Data/Ril/ril",ril.an,".fst")
# ril.path.string <- "Data/Ril/ril_leger.fst"
rilPath <- ifelse(file.exists(ril.path.string),ril.path.string,"Data/Ril/FakeRil.fst")
# RP
rp.an <- "14"
rpi.path.string <- paste0("Data/Rp/rp",rp.an,"i.fst")
rpl.path.string <- paste0("Data/Rp/rp",rp.an,"l.fst")
rpiPath <- ifelse(file.exists(rpi.path.string),rpi.path.string,"Data/Rp/FakeRpi.fst")
rplPath <- ifelse(file.exists(rpl.path.string),rpl.path.string,"Data/Rp/FakeRpl.fst")
# Filosofi
filo.an <- "14"
filo.path.string <- paste0("Data/Filosofi/filo",filo.an,".fst")
filoPath <- ifelse(file.exists(filo.path.string),filo.path.string,"Data/Filosofi/FakeFilo.fst")

# RP individu
rpi <- read_fst(rpiPath) 

# III.3. Ajout de la zone aux données du rp logement
rpl <- read_fst(rplPath) 


# II. RP niveau adresse
#----------------------------------------------------------------------------------------------------------------------------------
# Données logements
rpa.t <- rpl %>% mutate(X=as.numeric(C_LOG)) %>% group_by(C_IMM,C_ANNEE_COL,com) %>% 
  summarise(X=max(1,X),IPOND=mean(IPONDL),INPER=sum(INPER),INPCM=sum(INPCM)) %>% ungroup()

# Données individus
rpa2.t <- rpi %>% mutate(X=as.numeric(C_LOG)) %>% group_by(C_IMM,C_ANNEE_COL,com) %>% 
  summarise(X=max(1,X),IPOND=mean(IPONDI),NbHomme=sum(SEXE=="homme"),NbFemme=sum(SEXE=="femme"),ACTIF=sum(inactif=="actif"),INPCM=sum(TACT=="chomeur"),
            NbJeune=sum(as.numeric(AGEREV)<20),NbVieux=sum(as.numeric(AGEREV)>=65),NbMoyen=sum(as.numeric(AGEREV)>=20 & as.numeric(AGEREV)<65),
            FemmeActif=sum(SEXE=="femme" & inactif=="actif"),HommeActif=sum(SEXE=="homme" & inactif=="actif"),
            FemmeChomeur=sum(SEXE=="femme" & TACT=="chomeur"),HommeChomeur=sum(SEXE=="homme" & TACT=="chomeur"),
            NbEtranger=sum(etranger=="etranger"),NbImmigre=sum(immigre=="immigre"),Nb1825=sum(as.numeric(AGEREV)>=18 & as.numeric(AGEREV)<=25),
            NbEtudian1825=sum(as.numeric(AGEREV)>=18 & as.numeric(AGEREV)<25 & ETUD=="etudi")) %>% mutate(INPER=NbFemme+NbHomme) %>% ungroup()

# Table finale
rpa3 <- left_join(rpa.t,rpa2.t %>% select(-X,-IPOND,-INPER,-INPCM),c("C_IMM","C_ANNEE_COL","com")) %>% 
  as.data.frame() %>% 
  mutate(idx = paste(substr(C_IMM,1,5),"___",substr(C_IMM,9,12),"_",substr(C_IMM,14,16),sep = ""))
colnames(rpa)
colnames(rpa2)
colnames(rpa3)

class(rpa3)
# Gestion des valeurs manquantes
rpa3[is.na(rpa3)] <- 0

rpa <- rpa3




# III. enregistrement
#-------------------------------------------------------------------------------------------------------------------------------------
write.fst(rpa,"Data/Tmp/rpa.fst",compress = 100)






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
  
  
  
  # rpa2 <- rpl %>% mutate(X=as.numeric(C_LOG)) %>% group_by(C_IMM,C_ANNEE_COL,com) %>% 
  summarise(X=max(1,X),IPOND=mean(IPONDL),CATL1=sum(CATL==1),CATL2=sum(CATL==2),CATL3=sum(CATL==3),CATL4=sum(CATL==4),
            NbLocataire=sum(locataire=="locataire"),NblocHLM=sum(locataireHlm=="locataireHlm"),
            NbAppartement=sum(appartement=="appartement"),NbHLM=sum(hlm=="hlm"& CATL==1),Surface=sum(surf100=="surf100etplus"& CATL==1),NbBain=sum(BAIN=="1" & CATL==1),
            NbEAU=sum(EAU=="2"& CATL==1),NbEGOUL=sum(EGOUL =="1"& CATL==1)
  ) %>% ungroup()
Sys.time() - t1
rpa <- rpa1 %>% right_join(rpa2,by="idx")
rpa[is.na(rpa)]<-0
Sys.time() - t1


rpa2 %>% 
  filter(is.na(C_IMM))

rpl %>% 
  filter(is.na(C_IMM))



