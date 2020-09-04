
SubsetEcologicalSite_Species <- function(EcoSitePlots, Species_Indicator){}
  
  EcoSite_PKs <- EcoSitePlots$PrimaryKey
  Species_plots_ecosite <- Species_Indicator[(Species_Indicator[["PrimaryKey"]] %in% EcoSite_PKs), ]
  
  return(Species_plots_ecosite)
  
}
