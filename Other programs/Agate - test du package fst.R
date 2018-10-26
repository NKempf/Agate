#--------------------------------------------------------------------------------------------------------------------------------#
#                 Agate - Tests du package fst                                                                                   #
#--------------------------------------------------------------------------------------------------------------------------------#

# MAJ : 26.10.2018

# Nicolas Kempf

# Problématique : le chargement des bases du RP, de filosofi et autres prend trop de temps dans l'application.

# Objectif : réduire les temps de chargement et gagner en efficacité

# Solutions : 
# 1) Lecture partielle des bases de données (package fst de JLL)
# 2) Chargement des bases en arrière plan : chargement asynchrone des objets (packages future et promises)

# I. Analyse des temps de calcul
#-----------------------------------------------------------------------------------------------------------------------------------

# RP2013
t1 <- Sys.time()
load("Data/Rp/rp13.RData")
Sys.time() - t1
# 12 secondes de chargement
# rm(list = ls())

# RP2014
t1 <- Sys.time()
load("Data/Rp/rp14.RData")
Sys.time() - t1
# 10 secondes de chargement
rm(list = ls())

# filo14
t1 <- Sys.time()
load("Data/Filosofi/filo14.Rdata")
Sys.time() - t1
# 5 secondes de chargement
rm(list = ls())

# filo15
t1 <- Sys.time()
load("Data/Filosofi/filo15.Rdata")
Sys.time() - t1
# 5 secondes de chargement
rm(list = ls())


# II. Solution 1 : Lecture partielle des bases de données
#----------------------------------------------------------------------------------------------------------------------------------
library(fst)
library(fstplyr)
write_fst(rp13i,"Data/fst/rp13i.fst",compress = 100) %>% 
  
write_fst(rp13l,"Data/fst/rp13l.fst",compress = 100)

t1 <- Sys.time()
a <- read_fst("Data/fst/rp13i.fst") %>% 
  filter(com == "97101")
Sys.time() - t1

b <- read_fst("Data/fst/rp13l.fst")
Sys.time() - t1


metadata_fst("Data/fst/rp13i.fst")


