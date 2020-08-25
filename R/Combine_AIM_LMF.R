Combine_AIM_LMF <- function(TerrADat_Path, EDIT_List_Path, Internal){
 
   if(!Internal){
  TerrADat <- sf::st_read(dsn = TerrADat_Path , layer = "TerrADat")
  LMF <- sf::st_read(dsn = TerrADat_Path , layer = "LMF")
  TerrADat <- as.data.frame(TerrADat)
  TerrADat <- dplyr::select(TerrADat, -Shape)
  LMF <- as.data.frame(LMF)
  LMF <- dplyr::select(LMF, -Shape)}
  
  if(Internal){
  TerrADat <- TerrADat
  LMF <- LMF
  
  #Read in full csv of ecological site ids from EDIT
  
  EDIT <- read.csv(file = paste0(EDIT_List_Path, "/", "EDIT_public_ecological_site_list.csv"))
  
  
  #Add a new column with R or F dropped
  
  EDIT[["EcoSiteId_Stripped"]] <- gsub(EDIT[["new_es_symbol"]],
                                       pattern = "^[RF]", replacement = "")
  
  #Check to see if unique
  ecosite_lut <- unique(EDIT[,c("new_es_symbol" , "EcoSiteId_Stripped")])
  
  any(table(ecosite_lut[["EcoSiteId_Stripped"]]) > 1)
  
  #Pull out the repeat ids (fortunatley there are only 15)
  
  trouble_ids <- names(table(ecosite_lut[["EcoSiteId_Stripped"]]))[table(ecosite_lut[["EcoSiteId_Stripped"]]) > 1]
  
  #Drop the repeat ids
  ecosite_lut_drop_bad <- dplyr::filter(ecosite_lut, !EcoSiteId_Stripped == trouble_ids)
  
  #Add a new field called EcologicalSiteId that has the dropped R and F
  EcoSites_Update <- dplyr::mutate(ecosite_lut_drop_bad, EcologicalSiteId = EcoSiteId_Stripped)
  
  #Merge the dataframe with the full EcologicalSiteId and dropped R/F Id with the LMF
  LMF_EcoSite <- merge(LMF , EcoSites_Update, by = "EcologicalSiteId")
  
  #Drop the EcologicalSiteId value that we added earlier
  LMF_EcoSite <- dplyr::select(LMF_EcoSite, -EcologicalSiteId)
  
  #Rename Ecological SIte Id to the full Ecological Site Id code (= new_es_symbol)
  LMF_EcoSite <-  dplyr::rename(LMF_EcoSite, EcologicalSiteId = new_es_symbol)
  
  LMF_EcoSite <- dplyr::select(LMF_EcoSite, -EcoSiteId_Stripped)
  
  #Bind LMF and TerrADat
  #Place NAs in non-matching columns
  TerrADat[setdiff(names(LMF_EcoSite) , names(TerrADat))] <- NA
  LMF_EcoSite[setdiff(names(TerrADat), names(LMF_EcoSite))] <- NA
  
  #Bind LMF and TerrADat
  #Place NAs in non-matching columns
  TerrADat[setdiff(names(LMF_EcoSite) , names(TerrADat))] <- NA
  LMF_EcoSite[setdiff(names(TerrADat), names(LMF_EcoSite))] <- NA
  
  TDat_LMF <- rbind(TerrADat , LMF_EcoSite)
  
  #Pull out year for futur
  TDat_LMF$DateVisited <- sub("^(\\d{4}).*$", "\\1", TDat_LMF$DateVisited)
  TDat_LMF <- TDat_LMF %>% dplyr::rename(Year = DateVisited)
  
  EDIT_rename <- EDIT %>% rename(EcologicalSiteId = new_es_symbol)
  
  #Get with full esd names
  
  output <- merge(TDat_LMF , EDIT_rename , by = "EcologicalSiteId", all = TRUE)
  

  
  output <- output %>% dplyr::select(-EcoSiteId_Stripped)
  
  return(output)
  
}

