## Prep species

SummaryFigures <- function(SpeciesList, Species_plots_ecosite, EcologicalSiteId, 
                                  SummaryVar, Interactive){

#Prep
  SpeciesList <- SpeciesList %>% dplyr::select(Species, ScientificName, CommonName,
                                               Family, SpeciesState,
                                               SynonymOf, UpdatedSpeciesCode) %>% 
                 dplyr::mutate(link = paste("https://plants.sc.egov.usda.gov/core/profile?symbol=", Species, sep = ""))

#Merge with species list so we can hover for scientific name
  
Species_plots_ecosite <- merge(Species_plots_ecosite , SpeciesList , by = c("Species" , "SpeciesState")) %>% 
                         dplyr::select(Species, ScientificName, CommonName, PrimaryKey, 
                                PlotID,  AH_SpeciesCover, 
                                AH_SpeciesCover_n, Hgt_Species_Avg, 
                                Hgt_Species_Avg_n, GrowthHabit, GrowthHabitSub, Duration, 
                                Noxious, SG_Group, link) %>%
                         dplyr::mutate_if(is.numeric, round , digits = 2) 

#Get Noxious versus Non in Standard Format

Species_plots_ecosite$Noxious <- gsub("YES" , "Yes", Species_plots_ecosite$Noxious)
Species_plots_ecosite$Noxious <- gsub("NO", "No", Species_plots_ecosite$Noxious)

#Ignoring NAs- make disclosure as this may overestimate cover
#For summarizing across all plots
Species_cover_summary <- Species_plots_ecosite %>% filter(!is.na(AH_SpeciesCover)) %>% 
                         mutate(Tally = 1) %>%
                         group_by(Species , GrowthHabit , GrowthHabitSub , 
                                  Duration , Noxious , ScientificName , 
                                  CommonName , SG_Group, link) %>% 
                         summarize(AveragePercentCover = mean(AH_SpeciesCover) ,
                         StandardDeviation = sd(AH_SpeciesCover),
                         MinCover = min(AH_SpeciesCover) ,
                         MaxCover = max(AH_SpeciesCover) , n = sum(Tally)) %>%
                         mutate_if(is.numeric, round , digits = 2) %>%
                         dplyr::select(Species, ScientificName, 
                                       AveragePercentCover, StandardDeviation,
                                       MinCover, MaxCover, n, GrowthHabit, 
                                       GrowthHabitSub, Duration, 
                                       Noxious, CommonName, SG_Group, link)

##Setting color palette for plot
NoxNonPal_Fill <- c("grey75"  , "#D55E00")
NoxNonPal_Dot <- c("grey33" , "#993300")

  #Remove NAs for plotting
Species_plots_ecosite <- Species_plots_ecosite %>% filter(!is.na(AH_SpeciesCover))
  
if(SummaryVar == "GrowthHabitSub"){
if(Interactive){
  Plots <-  lapply(X = split(Species_plots_ecosite, Species_plots_ecosite[["GrowthHabitSub"]] , 
                                           drop = TRUE),
                                 
                                 FUN = function(Species_plots_ecosite){
                                 
                                   current_plot <- ggplot2::ggplot(Species_plots_ecosite , 
                                                          aes(x = GrowthHabitSub, 
                                                              y = AH_SpeciesCover, 
                                                              text = paste("Primary Key: " , PrimaryKey , 
                                                                           "Plot ID: " , PlotID , "Species: " , 
                                                                            ScientificName , "Code: " , Species , 
                                                                            "Percent Cover: " , AH_SpeciesCover , 
                                                                            "Noxious: " , Noxious , sep = "<br>"))) +
                                     geom_boxplot(width = .6 , outlier.shape = NA) +
                                     geom_jitter(width = .2 , size = 1.25 , shape = 21) +
                                     theme_light() +
                                     scale_y_continuous(limits = c(0 , 100))  +
                                     theme(axis.text.y = element_blank() , axis.ticks.y = element_blank() ,
                                           axis.title.y = element_blank() , axis.title.x = element_blank() ,  
                                           axis.line.y = element_blank()) + theme(panel.grid.major.y = element_blank() ,
                                                                                  axis.title.y = element_blank()) +
                                     ggtitle(paste0("Percent Cover by Functional Group: " , 
                                                    Species_plots_ecosite$GrowthHabitSub,
                                                    toString(EcologicalSiteId), sep = " ")) +
                                     coord_flip() + facet_grid(cols = vars(GrowthHabitSub) ,
                                                               rows = vars(Duration) ,
                                                               switch = "y" ,
                                                               scales = "free" , drop = TRUE)
                                   
                                   return(current_plot)
                                 }
                  )
}

if(!Interactive){
   Plots <- lapply(X = split(Species_plots_ecosite, Species_plots_ecosite[["GrowthHabitSub"]] , drop = TRUE), 
                             FUN = function(Species_plots_ecosite){
                             
                                 current_plot <- ggplot2::ggplot(Species_plots_ecosite , aes(x = GrowthHabitSub , y = AH_SpeciesCover)) +
                                 geom_boxplot(width = .6 , outlier.shape = NA) +
                                 geom_jitter(width = .2 , size = 1.25, aes(color = Noxious)) +
                                 scale_color_manual(values = NoxNonPal_Dot) +
                                 labs(y = "Percent Cover", 
                                 caption = paste("Percent cover in: ", toString(EcologicalSiteId))) +
                                 scale_y_continuous(limits = c(0 , 100))  +
                                 theme_light() + 
                                 theme(axis.text.y = element_blank() , axis.ticks.y = element_blank() ,
                                       axis.line.y = element_blank()) + theme(panel.grid.major.y = element_blank() ,
                                                                              axis.title.y = element_blank()) +
                                 ggtitle(paste("Percent Cover by Functional Group:", 
                                               Species_plots_ecosite$GrowthHabitSub,
                                               toString(EcologicalSiteId), sep = " ")) +
                                 coord_flip() + facet_grid(cols = vars(GrowthHabitSub) ,
                                                           rows = vars(Duration) , switch = "y" ,
                                                           scales = "free" , drop = TRUE)
                               
    
                               return(current_plot)
                             })
}
}

if(SummaryVar == "Noxious"){
  if(Interactive){
     Plots <- Species_plots_ecosite %>% group_by(Noxious) %>% 
     filter(!is.na(Noxious)) %>% filter(!is.na(AH_SpeciesCover)) %>%
     ggplot2::ggplot((aes(x = Noxious , y = AH_SpeciesCover , 
                                             text = paste("Primary Key : " , PrimaryKey, 
                                                    "Plot ID: " , PlotID , 
                                                    "Species: " ,  ScientificName, 
                                                    "Code: " , Species, 
                                                    "Percent Cover: " , AH_SpeciesCover, 
                                                    "Noxious: " , Noxious , 
                                                    sep = "<br>")))) +
           geom_boxplot(width = .6 , outlier.shape = NA) +
           geom_jitter(width = .15 , size = 1.25 , shape = 21) +
           theme_light() +
           scale_y_continuous(limits = c(0 , 100)) +
           labs(y = "Percent Cover") + 
           ggtitle(paste("Percent Cover, Noxious vs. Non-Noxious Species: " , 
                         toString(EcologicalSiteId))) +
            theme(axis.title.y = element_blank() , axis.text.y = element_blank() ,
                  axis.ticks.y = element_blank() , axis.line.y = element_blank() , 
                  axis.title.x = element_blank()) +
           theme(panel.grid.major.y = element_blank() , legend.position = "none") +
           coord_flip() + 
           facet_grid(rows = vars(Noxious) , switch = "y" , scales = "free" , 
                      drop = TRUE) 
           return(Plots)
  }
  
   if(!Interactive){
     Plots <- Species_plots_ecosite %>% group_by(Noxious) %>% 
       filter(!is.na(Noxious)) %>% filter(!is.na(AH_SpeciesCover)) %>%
               ggplot2::ggplot((aes(x = Noxious , y = AH_SpeciesCover))) +
               geom_boxplot(width = .6 , outlier.shape = NA) +
               geom_jitter(width = .15 , size = 1.25 , aes(color = Noxious)) +
               scale_color_manual(values = NoxNonPal_Dot) +
               theme_light() +
               scale_y_continuous(limits = c(0 , 100)) +
               labs(y = "Percent Cover") + 
               ggtitle(paste("Percent Cover, Noxious vs. Non-Noxious Species: " , 
                             toString(EcologicalSiteId))) +
                 theme(axis.title.y = element_blank() , axis.text.y = element_blank() , 
                       axis.ticks.y = element_blank() ,
                       axis.line.y = element_blank() , 
                       panel.grid.major.y = element_blank() , 
                       legend.position = "none") +
                 coord_flip() + 
                facet_grid(rows = vars(Noxious) ,
                           switch = "y" , scales = "free" , drop = TRUE)
    
   }}

if(SummaryVar == "Species"){
  PercentCover <- Species_plots_ecosite %>% subset(AH_SpeciesCover > 0.000000)
  if(Interactive){
  Plots <-lapply(X = split(PercentCover, list(PercentCover$GrowthHabitSub , PercentCover$Duration) , drop = TRUE),
                                     FUN = function(PercentCover){
                                       current_plot <- ggplot2::ggplot(PercentCover , aes(x = Species , y = AH_SpeciesCover,
                                                                                             text = paste("PrimaryKey: ", PrimaryKey , 
                                                                                                          "Plot ID: " , PlotID , "Species: " , 
                                                                                                          ScientificName , "Code: " , Species , 
                                                                                                          "Percent Cover: " , AH_SpeciesCover , "Noxious: " , 
                                                                                                          Noxious , sep = "<br>"))) +
                                         geom_boxplot(width = .6 , outlier.shape = NA) +
                                         geom_jitter(width = .15 , size = 1, shape = 21) +
                                         scale_y_continuous(limits = c(0 , 100)) +
                                         theme_light() +
                                         labs(y = "Percent Cover") + 
                                         ggtitle(paste("Percent Cover by Species, " , 
                                                       PercentCover$GrowthHabitSub, 
                                                       PercentCover$Duration ,
                                                       toString(EcologicalSiteId))) + 
                                         theme(axis.title.y = element_blank() ,
                                               axis.text.y = element_blank(),
                                               axis.ticks.y = element_blank(), 
                                               axis.title.x = element_blank())+   
                                         theme(panel.grid.major.y = element_blank() ,
                                               axis.title.y = element_blank()) +
                                         coord_flip() +  facet_grid(rows = vars(Species), 
                                                                    scales = "free" , switch = "y",  drop = TRUE) 
                                       return(current_plot)
                                     })
  }
  
  if(!Interactive){
    Plots <- lapply(X = split(PercentCover, list(PercentCover$GrowthHabitSub , 
                                                                            PercentCover$Duration) , 
                                             drop = TRUE),
                                   FUN = function(PercentCover){
                                     current_plot <- ggplot2::ggplot(PercentCover , aes(x = Species , y = AH_SpeciesCover)) +
                                       geom_boxplot(width = .6 , outlier.shape = NA) +
                                       geom_jitter(width = .15 , size = 1 , aes(color = Noxious)) +
                                       scale_color_manual(values = NoxNonPal_Dot) + scale_y_continuous(limits = c(0 , 100)) +
                                       theme_light() +
                                       labs(y = "Percent Cover") + 
                                       ggtitle(paste("Percent Cover by Species, " , 
                                                     PercentCover$GrowthHabitSub , 
                                                     PercentCover$Duration , 
                                                     toString(EcologicalSiteId))) + 
                                       theme(axis.title.y = element_blank()) +
                                       coord_flip() +  facet_grid(cols = vars(GrowthHabitSub) , rows = vars(Duration) ,
                                                                  switch = "y" , scales = "free" , drop = TRUE) 
                                     return(current_plot)
                                   })
    
}
}

if(SummaryVar == "GroundCover"){
  
  #Prep
    #BareSoilCover
    #TotalFoliarCover
    #FH_TotalLitterCover
    #FH_RockCover
    
    Ground_Cover_Tall <- EcoSitePlots %>% 
                        dplyr::select(PlotID, PrimaryKey, BareSoilCover , 
                                      TotalFoliarCover , FH_TotalLitterCover , 
                                      FH_RockCover) %>%
                        gather(key = Indicator , value = Percent, 
                               BareSoilCover:FH_RockCover) %>% mutate(Tally = 1) 
    if(Interactive){
    
    Plots <- Ground_Cover_Tall %>% mutate_if(is.numeric , round , digits = 2) %>% 
              ggplot2::ggplot((aes(x = Indicator , y = Percent , 
                           text = paste("PlotID: " , PlotID , 
                           "PrimaryKey: " , PrimaryKey , 
                           "Indicator: " , Indicator ,
                           "Percent Cover: " , Percent , 
                           sep = "<br>" )))) +
               geom_boxplot(width = .6 , outlier.shape = NA) +
               geom_jitter(width = .15 , shape = 21) +
               theme_light() +
               scale_y_continuous(limits = c(0 , 100)) +
               labs(y = "Ground Cover (%)" , x = "Indicator") +
               theme(axis.text.y = element_blank() , 
                     axis.ticks.y = element_blank() ,
                     axis.line.y = element_blank() ,  
                     axis.title.x = element_blank() , 
                     axis.title.y = element_blank()) +
              coord_flip() + facet_grid(rows = vars(Indicator) ,
                                switch = "y" ,
                                scales = "free_y" , drop = TRUE)
    
    }
    
    if(!Interactive){
       Plots <- Ground_Cover_Tall %>% mutate_if(is.numeric , round , digits = 2) %>% 
                   ggplot2::ggplot((aes(x = Indicator , y = Percent))) +
                   geom_boxplot(width = .6 , outlier.shape = NA) +
                   geom_jitter(width = .15) +
                   theme_light() +
                   scale_y_continuous(limits = c(0 , 100)) +
                   labs(y = "Ground Cover (%)" , x = "Indicator") +
                   theme(axis.text.y = element_blank() , 
                         axis.ticks.y = element_blank() ,
                         axis.line.y = element_blank()) + 
                   coord_flip() + facet_grid(rows = vars(Indicator) ,
                                  switch = "y" ,
                                  scales = "free_y" , drop = TRUE)
      
       
    }
  
  
}

if(SummaryVar == "Gap"){
  
  Gap <- EcoSitePlots %>% dplyr::select(PlotID , PrimaryKey , 
                                        GapCover_25_50 , GapCover_51_100 , 
                                        GapCover_101_200 , GapCover_200_plus , 
                                        GapCover_25_plus) %>% 
                                gather(key = Gap_Class_cm , 
                                value = Percent , GapCover_25_50:GapCover_25_plus) %>%
                                mutate_if(is.numeric , round, digits = 2)
  
  #Plot prep
  if(Interactive){
 
     Plots <- ggplot2::ggplot(data = Gap , aes(x = Gap_Class_cm , y = Percent , 
                                      text = paste("PlotID: " , PlotID , 
                                            "PrimaryKey: ", PrimaryKey , 
                                            "Gap Class (cm): " , Gap_Class_cm, 
                                            "Percent Cover: " , Percent , 
                                             sep = "<br>"))) +
                       geom_boxplot() + coord_flip() + 
                       geom_jitter(width = 0.1 , shape = 21) +
                       theme_light() + 
                       theme(axis.title.x = element_blank() ,
                          axis.text.y = element_blank() , 
                          axis.ticks.y = element_blank() , 
                          axis.title.y = element_blank() , 
                          axis.line.y = element_blank(), 
                          panel.grid.major.y = element_blank()) +
                      facet_grid(rows = vars(Gap_Class_cm) , switch = "y" ,
                      scales = "free_y" , drop = TRUE)
  
  }
  
  if(!Interactive){
      Plots <- ggplot2::ggplot(data = Gap , aes(x = Gap_Class_cm , y = Percent)) +
               labs(y = "Percent Cover" , x = "Gap Size Class (cm)", 
                    caption = paste("Percent cover of canopy gap in: ", 
                                    toString(EcologicalSiteId))) +
                    geom_boxplot() + 
                    coord_flip() + 
                    geom_jitter(width = 0.1) +
                    theme_light() + 
                    theme(axis.text.y = element_blank() , 
                          axis.ticks.y = element_blank() ,
                          axis.line.y = element_blank(),
                          panel.grid.major.y = element_blank()) +
                   facet_grid(rows = vars(Gap_Class_cm) , switch = "y" ,
                   scales = "free_y" , drop = TRUE)
  }
  
}

if(SummaryVar == "SoilStability"){
  
  soil_labels <- c("SoilStability_All" = "All" , "SoilStability_Protected" = "Protected" , 
                   "SoilStability_Unprotected" = "Unprotected")
  
  SoilStability <- EcoSitePlots %>% dplyr::select(PlotID , PrimaryKey , 
                                                           SoilStability_All , 
                                             SoilStability_Protected , 
                                             SoilStability_Unprotected) %>%
                          gather(key = Veg , value = Rating , 
                          SoilStability_All:SoilStability_Unprotected) %>%
                          mutate_if(is.numeric, round, digits = 2) 
    
  if(Interactive){

    Plots <- ggplot2::ggplot(data = SoilStability , 
                    aes(x = Veg , y = Rating , 
                        text = paste("Primary Key: " , PrimaryKey,
                        "Plot ID: " , PlotID , 
                        "Rating: " , Rating , 
                        sep = "<br>"))) +
                        geom_boxplot() + 
                        coord_flip() + 
                        geom_jitter(width = 0.1 , shape = 21) +
                        theme_light() + 
                        theme(axis.text.y = element_blank() , 
                             axis.ticks.y = element_blank() ,
                             axis.line.y = element_blank(),
                             panel.grid.major.y = element_blank(),
                             axis.title = element_blank()) +
                       facet_grid(rows = vars(Veg) , switch = "y" ,
                       scales = "free_y" , drop = TRUE , 
                       labeller = as_labeller(soil_labels))
  }
  
  
  if(!Interactive){
    
            Plots <- ggplot2::ggplot(data = SoilStability , 
                            aes(x = Veg , y = Rating)) +
                     labs(x = "Vegetation cover class" , 
                          y = "Soil Stability Rating",
                          caption = paste("Soil stability ratings in: ", 
                                          toString(EcologicalSiteId))) +
                          geom_boxplot() + coord_flip() + geom_jitter(width = 0.1) +
                          theme_light() + 
                          theme(axis.text.y = element_blank() , 
                          axis.ticks.y = element_blank() ,
                          axis.line.y = element_blank(), 
                          panel.grid.major.y = element_blank()) +
                          facet_grid(rows = vars(Veg) , 
                                     switch = "y" ,
                                     scales = "free_y" , 
                                     drop = TRUE , 
                                     labeller = as_labeller(soil_labels))
    
  }

}

return(Plots)
  
}
