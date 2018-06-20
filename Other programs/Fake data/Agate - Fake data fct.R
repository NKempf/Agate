#--------------------------------------------------------------------------------------------------------------------------------#
#                     Agate - fake data function                                                                            #
#--------------------------------------------------------------------------------------------------------------------------------#

# MAJ : 3.06.2018

# Nicolas Kempf

fake_zonaRil <- function(zone,pts.alea){
  
  # # Points aléatoire au sein des communes des DOM
  # # Rq : pour des questions de rapidité, il faut travailler département par département
  # pts.alea <- spsample(zone , n = nbObs,type = "random")
  
  df <- data.frame(idx = paste0("fk",seq(1,length(pts.alea),1)),x = pts.alea@coords[,1], y = pts.alea@coords[,2])
  pts.alea <- SpatialPointsDataFrame(pts.alea,data = df)
  
  ### Jointure spatiale entre le ril et les communes de guyane ###
  zonageDB <- over(pts.alea,zone)
  # Ajout des identifiants des points
  df <- df %>% 
    mutate(com = zonageDB$insee,
           com.lib = zonageDB$nom,
           dep = substr(com,1,3))
  pts.alea@data <- df
  
  return(pts.alea)
  
}


spatial_fakedata <- function(zone,nbObs){
  
  # Points aléatoire au sein des communes des DOM
  # Rq : pour des questions de rapidité, il faut travailler département par département
  pts.alea <- spsample(zone , n = nbObs,type = "random")
  
  df <- data.frame(idx = paste0("fk",seq(1,length(pts.alea),1)),x = pts.alea@coords[,1], y = pts.alea@coords[,2])
  pts.alea <- SpatialPointsDataFrame(pts.alea,data = df)
  
  ### Jointure spatiale entre le ril et les communes de guyane ###
  zonageDB <- over(pts.alea,zone)
  # Ajout des identifiants des points
  df <- df %>% 
    mutate(com = zonageDB$insee,
           com.lib = zonageDB$nom,
           dep = substr(com,1,3))
  pts.alea@data <- df
  
  return(pts.alea)
  
}

# Simule des fausses données ayant la structure de vrai données
faking_data <- function(df,nObs){
  L <- lapply(colnames(df), nObs = nObs,function(x,nObs){
    return(sample(df[,x],replace = TRUE,size = nObs))
  })
  df2 <-  data.frame(do.call(data.frame, L))
  colnames(df2) <-  colnames(df)
  return(df2)
}





