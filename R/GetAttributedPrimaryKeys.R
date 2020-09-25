GetAttributedPrimaryKeys <- function(TDat_LMF, 
                           shapefile_name, shapefile_path, 
                           attribute_title, attribute_name){
  
TDat_LMF_Attributed <- AttributePlots(TDat_LMF = TDat_LMF, shapefile_name = shapefile_name,
               shapefile_path = shapefile_path, attribute_title = attribute_title)

Attributed_Pks <- TDat_LMF_Attributed %>% dplyr::select(PrimaryKey, all_of(attribute_title))

return(Attributed_Pks)
  
}
