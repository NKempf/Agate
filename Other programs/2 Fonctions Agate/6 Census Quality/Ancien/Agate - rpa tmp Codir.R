#-------------------------------------------------------------------------------------#
#                Agate - rpa tmp Codir                                                #
#-------------------------------------------------------------------------------------#

# Nicolas Kempf & Baptiste Raimbaud

# Derniere MAJ : 20.02.2019

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
rpa <- rpl %>% mutate(X=as.numeric(C_LOG)) %>% group_by(C_IMM,C_ANNEE_COL,com) %>% 
  summarise(X=max(1,X),IPOND=mean(IPONDL),INPER=sum(INPER),INPCM=sum(INPCM)) %>% ungroup()

# Données individus
rpa2 <- rpi %>% mutate(X=as.numeric(C_LOG)) %>% group_by(C_IMM,C_ANNEE_COL,com) %>% 
  summarise(X=max(1,X),IPOND=mean(IPONDI),NbHomme=sum(SEXE=="homme"),NbFemme=sum(SEXE=="femme"),ACTIF=sum(inactif=="actif"),INPCM=sum(TACT=="chomeur"),
            NbJeune=sum(as.numeric(AGEREV)<20),NbVieux=sum(as.numeric(AGEREV)>=65),NbMoyen=sum(as.numeric(AGEREV)>=20 & as.numeric(AGEREV)<65),
            FemmeActif=sum(SEXE=="femme" & inactif=="actif"),HommeActif=sum(SEXE=="homme" & inactif=="actif"),
            FemmeChomeur=sum(SEXE=="femme" & TACT=="chomeur"),HommeChomeur=sum(SEXE=="homme" & TACT=="chomeur"),
            NbEtranger=sum(etranger=="etranger"),NbImmigre=sum(immigre=="immigre"),Nb1825=sum(as.numeric(AGEREV)>=18 & as.numeric(AGEREV)<=25),
            NbEtudian1825=sum(as.numeric(AGEREV)>=18 & as.numeric(AGEREV)<25 & ETUD=="etudi")) %>% mutate(INPER=NbFemme+NbHomme) %>% ungroup()

# Table finale
rpa3 <- left_join(rpa,rpa2 %>% select(-X,-IPOND,-INPER,-INPCM),c("C_IMM","C_ANNEE_COL","com")) %>% 
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



