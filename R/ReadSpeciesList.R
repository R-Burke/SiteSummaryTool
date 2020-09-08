ReadSpeciesList <- function(TerrADat_Path, Internal){
  
  if(!Internal){
    SpeciesList<- sf::st_read(dsn = TerrADat_Path ,  
                              layer = "tblStateSpecies")}
  if(Internal){
    SpeciesList <- SpeciesList
  }
  
  SpeciesList <- SpeciesList %>% dplyr::rename(Species = SpeciesCode) 
  return(SpeciesList)
  
}

