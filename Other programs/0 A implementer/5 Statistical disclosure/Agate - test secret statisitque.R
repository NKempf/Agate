library(sdcTable)
library(tidyverse)

# https://cran.r-project.org/web/packages/sdcTable/vignettes/sdcTable.html#ex1:microDat

# MicroData
completeData <- read.csv("Data/Tmp/test.csv",sep = ";") %>% 
  select(-X..)

# Agregated Data
print(tail(completeData))



dimV1 <- matrix(nrow=0, ncol=2)
dimV1 <- rbind(dimV1, c('@','Tot'))
print(dimV1)


