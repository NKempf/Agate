#---------------------------------------------------------------------------------------------------------------------------#
#                                             Agate - Fake data                                                             #
#---------------------------------------------------------------------------------------------------------------------------#

# I. QPV
#-----------------------------------------------------------------------------------------------------------------------------------
library(rgdal)

# Official map loading
qpv_stat <- readOGR(dsn = "../Data/Maps/QPV/qpv.shp")
# Change projection system to fit leaflet
qpv_stat <- spTransform(qpv_stat, "+init=epsg:4326")

# Statistical data to fake
load("../Data/RData/QPVstatTmp.RData")

# Faking data
tmp <- merge(statZona$tStatRP[!statZona$tStatRP$id %in% c("sum","Sum"),],
             statZona$filoStat.nivvie[!is.na(statZona$filoStat.nivvie$idZonage),],by.x = "id",by.y="idZonage",all.x=TRUE)

data.fake <- lapply(colnames(tmp)[2:length(colnames(tmp))], FUN = function(x) {
  return(runif(n = nrow(tmp),min = min(tmp[,x],na.rm = T),max = max(tmp[,x],na.rm = T)))
})
df.fake <- data.frame(matrix(unlist(data.fake), nrow=nrow(tmp), byrow = F))
colnames(df.fake) <- colnames(tmp)[2:length(colnames(tmp))]
df.fake$CODE_QP <- tmp$id

# Add fake data to qpv_stat
qpv_stat.fake <- merge(qpv_stat,df.fake, by = "CODE_QP",all.x=TRUE)

# Save as RData
save(qpv_stat.fake,file = "Data/QPV/qpvFake.RData")


