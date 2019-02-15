#--------------------------------------------------------------------------------------------------------------------------------#
#                 Agate - Drawings function                                                                                      #
#--------------------------------------------------------------------------------------------------------------------------------#

# MAJ : 14.11.2018

# Drawings tools to convert leaflet.extras objects into spatial object

# Nicolas Kempf


# I. Convert Features list into spatialpolygonsDataFrames
#--------------------------------------------------------------------------------------------------------------------------------

# See : https://gis.stackexchange.com/questions/171124/data-frame-to-spatialpolygonsdataframe-with-multiple-polygons

# feature : liste de feature issue du package leaflet.extras
featureToSpatialPolygonDF <- function(featureList){
  
  # 1) FeatureList to data.frame list of coordinates (note : each data.frame contain polygon coordinates)
  ldf <- lapply(featureList$features, function(x){
    lapply(x$geometry[[2]][[1]], function(l){
      data.frame(matrix(unlist(l), nrow=1, byrow=T))
    }) %>% 
      bind_rows() %>% 
      rename(x = X1, y = X2)
  })
  
  # Polygon names
  names(ldf) <- paste0("pol",seq(1:length(featureList$features)))
  
  # 2) Convert data.frame to polygon objects 
  ps <- lapply(ldf, Polygon)
  
  # 3) Polygon object to Polygon list with correct IDs
  p1 <- lapply(seq_along(ps), function(i) Polygons(list(ps[[i]]), 
                                                   ID = names(ldf)[i]  ))
  # 4) Convert into SpatialPolygon object (note : Leaflet object use projection system -> epsg4326)
  poly_drawn <- SpatialPolygons(p1, proj4string = CRS("+init=epsg:4326")) 
  
  # 5) Attribut data with default value
  table_attribut <- data.frame(idZonage = paste0("pol",seq(1:length(names(ldf)))),name = NA,stringsAsFactors = FALSE)
  rownames(table_attribut) <- table_attribut$id # Important to create SpatialPolygonDataFrame
  
  # 6) Convert into spatialPolygonDataFrame object
  zone <- SpatialPolygonsDataFrame(poly_drawn,data = table_attribut)
  
  return(zone)
}


