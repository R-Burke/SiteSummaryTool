SubsetEcologicalSite <- function(TDat_LMF, EcologicalSiteId){
  
  EcoSitePlots <- TDat_LMF[TDat_LMF[["EcologicalSiteId"]] %in% EcologicalSiteId, ]
# Add sagebrush indicator
  EcoSitePlots <- EcoSitePlots %>%
                  mutate(AH_SagebrushCover_Dead = AH_SagebrushCover - AH_SagebrushCover_Live)
   
  return(EcoSitePlots)
}

SubsetEcologicalSite_Species <- function(EcoSitePlots, Species_Indicator){
  
  EcoSite_PKs <- EcoSitePlots$PrimaryKey
  Species_plots_ecosite <- Species_Indicator[(Species_Indicator[["PrimaryKey"]] %in% EcoSite_PKs), ]
  
  return(Species_plots_ecosite)
  
}