ReadSpeciesList <- function(TerrADat_Path, Internal){
  
  if(!Internal){
    SpeciesList<- sf::st_read(dsn = TerrADat_Path ,  
                              layer = "tblStateSpecies")}
  if(Internal){
    conn <- RODBC::odbcConnect("ilmocAIMPub")
    SpeciesList <- sqlQuery(conn, 'SELECT * FROM ilmocAIMPub.ilmocAIMPubDBO.tblStateSpecies;')}
  
  SpeciesList <- SpeciesList %>% dplyr::rename(Species = SpeciesCode) 
  return(SpeciesList)

  }