SubsetEcologicalSite <- function(TDat_LMF, EcologicalSiteId){
  
  EcoSitePlots <- TDat_LMF[TDat_LMF[["EcologicalSiteId"]] %in% EcologicalSiteId, ]
# Add sagebrush indicator
  EcoSitePlots <- EcoSitePlots %>%
                  dplyr::mutate(AH_SagebrushCover_Dead = AH_SagebrushCover - AH_SagebrushCover_Live)
   
  return(EcoSitePlots)
}

