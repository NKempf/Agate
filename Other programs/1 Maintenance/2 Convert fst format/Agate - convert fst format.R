#--------------------------------------------------------------------------------------------------------------------------------#
#                 Agate - Convert fst format                                                                                     #
#--------------------------------------------------------------------------------------------------------------------------------#

# MAJ : 30.10.2018

# Nicolas Kempf

# Transformation des bases de données (RData) au format fst compressé

library(fst)
library(tidyverse)

# I. Recensement de la population
#------------------------------------------------------------------------------------------------------------------------------------

# I.1. RP 2013
#-------------
load("Data/Rp/rp13.RData")

# Enregistrement au format fst
write_fst(rp13i,"Data/Rp/rp13i.fst",compress = 100)
write_fst(rp13l,"Data/Rp/rp13l.fst",compress = 100)

# I.2. RP 2014
#-------------
load("Data/Rp/rp14.RData")

# Enregistrement au format fst
write_fst(rp14i,"Data/Rp/rp14i.fst",compress = 100)
write_fst(rp14l,"Data/Rp/rp14l.fst",compress = 100)


# II. Fichiers fiscaux : filosofi
#------------------------------------------------------------------------------------------------------------------------------------

# II.1. filo 2014
#----------------
load("Data/Filosofi/filo14.Rdata")

# Enregistrement au format fst
write_fst(filo14.disp,"Data/Filosofi/filo14.fst",compress = 100)

# II.2. filo 2015
#----------------
load("Data/Filosofi/filo15.Rdata")

# Enregistrement au format fst
write_fst(filo15.disp,"Data/Filosofi/filo15.fst",compress = 100)

# III. RIL
#-------------------------------------------------------------------------------------------------------------------------------------

# III.1. ril 2015
#----------------
load("Data/Ril/ril15.RData")

# Transformation du ril en data.frame
rilhab15@data %>% 
  mutate(x = rilhab15@coords[,1],
         y = rilhab15@coords[,2]) %>% 
  as.data.frame() -> ril

# Enregistrement au format fst
write_fst(ril,"Data/Ril/ril15.fst",compress = 100)




