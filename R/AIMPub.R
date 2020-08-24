AIMPub_conn <- function(){
  conn <- RODBC::odbcConnect("ilmocAIMPub")
  return(conn)
}

AIMPub_TerrADat <- function(){
TerrADat <- sqlQuery(conn, 'SELECT * FROM ilmocAIMPub.ilmocAIMPubDBO.TerrADat;')

return(TerrADat)
}

AIMPub_LMF <- function(){
  sqlQuery(conn, 'SELECT * FROM ilmocAIMPub.ilmocAIMPubDBO.LMF;')

return(LMF)
}

AIMPub_TerrADat_Species <- function(){
TerrADat_Species <- sqlQuery(conn, 'SELECT * FROM ilmocAIMPub.ilmocAIMPubDBO.TerrADatSpeciesIndicators;')
return(TerrADat_Species)
}

AIMPub_LMF_Species <- function(){
LMF_Species <- sqlQuery(conn, 'SELECT * FROM ilmocAIMPub.ilmocAIMPubDBO.LMFSpeciesIndicators;')
return(LMF_Species)
}

AIMPub_SpeciesList <- function(){

SpeciesList <- sqlQuery(conn, 'SELECT * FROM ilmocAIMPub.ilmocAIMPubDBO.tblStateSpecies;')
return(SpeciesList)
}

