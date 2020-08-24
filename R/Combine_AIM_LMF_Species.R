Combine_AIM_LMF_Species <- function(TerrADat_Path, Internal){
  if(!Internal){
    TerrADatSpecies <- sf::st_read(dsn = TerrADat_Path , layer = "TerrADatSpeciesIndicators")
    LMFSpecies <- sf::st_read(dsn = TerrADat_Path , layer = "LMFSpeciesIndicators")
    TerrADatSpecies <- as.data.frame(TerrADatSpecies)
    TerrADatSpecies <- dplyr::select(TerrADatSpecies, -Shape)
    LMFSpecies <- as.data.frame(LMFSpecies)
    LMFSpecies <- dplyr::select(LMFSpecies, -Shape)}

  if(Internal){
    TerrADatSpecies <- AIMPub_TerrADat_Species()
    LMFSpecies <- AIMPub_LMF_Species()
    SpeciesList <- AIMPub_SpeciesList()
  }

  LMFSpecies[setdiff(names(TerrADatSpecies) , names(LMFSpecies))] <- NA
  TerrADatSpecies[setdiff(names(LMFSpecies), names(TerrADatSpecies))] <- NA
  output <- rbind(TerrADatSpecies , LMFSpecies)

  return(output)
}


