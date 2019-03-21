#----------------------------------------------------------------------------------------------------------------------#
#                                   Agate - Statistical disclosure                                                     #
#----------------------------------------------------------------------------------------------------------------------#

# 14.03.2019

# Test du package sdc Table qui gère le secret statistique

# On retient ce package simple d'utilisation pour appliquer le secret dans Agate. On utilisera particulierement la fonction protectTable
# dans le cas de stacked data.

# Le package sdcTable est plus complet mais a les defauts de ses qualités. Il est complexe d'utilisation. Dans Agate, nous allons appliquer
# le secret statistique de manière classique c'est-à-dire nous utiliserons la méthode de la suppression de case à partir de seuils. Le secret
# secondaire sera appliqué avec la méthode la plus simple (car la plus rapide en temps de calcul) : simple heuristique
# Les seuils utilisés correspondent aux règles de diffusion en matière de secret statistique des fichiers filosofi. 


# respect du secret fiscal (uniquement pour les dispositifs construits à partir des sources fiscales, en raison d’une convention passée 
# entre l’Insee et la DGFiP) : tout résultat diffusé doit représenter au moins 11 ménages fiscaux.

library(tidyverse)


# I. Test du package easySdcTable
#-----------------------------------------------------------------------------------------------------------------------
# Ce package offre une interface graphique simple au package sdcTable

library(easySdcTable)

# I.1. Données de l'exemple
#--------------------------
z2w <- EasyData("z2w")
print(z2w, row.names = FALSE)

# Protect table
t1 <- Sys.time()
ex2w <- ProtectTable(z2w, 1, 4:7)
Sys.time() - t1

# Computed totals
print(ex2w$freq, row.names = FALSE)

# SdcStatus
# In the output element sdcStatus the cells are coded as “u” (primary suppressed), “x” (secondary suppression), and “s” (can be published).
print(ex2w$sdcStatus, row.names = FALSE)

# Suppressed data
print(ex2w$suppressed, row.names = FALSE)


# Autres exemples
t1 <- Sys.time()
ex2wHITAS <- ProtectTable(z2w, dimVar = c("region"), freqVar = c("annet", "arbeid", 
                                                                 "soshjelp", "trygd"), method = "HITAS")
Sys.time() - t1

t1 <- Sys.time()
ex2wAdvanced <- ProtectTable(z2w, dimVar = c("region", "fylke", "kostragr"), 
                             freqVar = c("annet", "arbeid", "soshjelp", "trygd"), maxN = 2, protectZeros = FALSE, 
                             addName = TRUE)
Sys.time() - t1

print(ex2wAdvanced$suppressed, row.names = FALSE)

prmatrix(ex2wAdvanced$info, rowlab = rep("", 99), collab = "", quote = FALSE)

# Stacked data
z2 <- EasyData("z2")
print(z2)

t1 <- Sys.time()
ex2 <- ProtectTable(z2, dimVar = c("region", "hovedint", "kostragr"), freqVar = "ant",maxN = 11)
Sys.time() - t1
print(ex2$data)

# Graphical user interface
out <- PTgui(data = EasyData("z1w"))

# II. Tests sur données issues d'Agate
#-----------------------------------------------------------------------------------------------------------------------------------------------

# Utiliser la règle des 11 observations minimum par case.
seuil_secret_stat <- 11

# II.1. Statistiques issues d'Agate sur deux QPV
#-----------------------------------------------
load("Data/Tmp/qpv_stat_tmp.RData")

stat <- indStat$indicateur_stat

# Selection d'un tableau
tab <- stat %>% 
  filter(source == "rpi14" & domaine == 3 & categorie == 1 & type.indicateur == "freq") %>% 
  select(idZonage,indicateur,value) %>% 
  as.data.frame()

# Secret statistique
ProtectTable(tab,dimVar = c("idZonage","indicateur"),freqVar = "value",maxN = seuil_secret_stat)



ProtectTable(z2, dimVar = c("region", "hovedint", "kostragr"), freqVar = "ant",maxN = 11)


str(tab)



# 


