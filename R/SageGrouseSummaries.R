SageGrouseSummaries <- function(EcoSitePlots, Species_plots_ecosite,
                                Interactive, SummaryVar){

  SageGrouseStates <- c("CO", "UT", "NV", "CA", "WY", "MT", "ID", "OR", "WA", "ND", "SD")

  #Prep
  SpeciesList <- SpeciesList %>% dplyr::select(Species, ScientificName, CommonName,
                                               Family, SpeciesState,
                                               SynonymOf, UpdatedSpeciesCode) %>%
    dplyr::mutate(link = paste("https://plants.sc.egov.usda.gov/core/profile?symbol=", Species, sep = ""))

  Species_plots_ecosite <- merge(Species_plots_ecosite , SpeciesList , by = c("Species" , "SpeciesState")) %>%
    dplyr::select(Species, ScientificName, CommonName, PrimaryKey,
                  PlotID,  AH_SpeciesCover,
                  AH_SpeciesCover_n, Hgt_Species_Avg,
                  Hgt_Species_Avg_n, GrowthHabit, GrowthHabitSub, Duration,
                  Noxious, SG_Group, link) %>%
                  dplyr::mutate_if(is.numeric, round , digits = 2) %>%
                  filter(!is.na(SG_Group)) # only filter this for the sage-grouse plots

if(!EcoSitePlots$State %in% SageGrouseStates){SG_Plots <- NULL}

  else{

     if(SummaryVar == "PreferredForb"){
     PrefForb <- Species_plots_ecosite %>%
                                  filter(!is.na(SG_Group)) %>%
                                  mutate(PreferredForb = (SG_Group == "PreferredForb")) %>%
                                  subset(PreferredForb == TRUE) %>%
                                  subset(AH_SpeciesCover > 0.0000)

  if(Interactive){
      SG_Plots <-  if (nrow(PrefForb) < 1){SG_Plots <- NULL}
      else{
      ggplot(PrefForb , aes(x = Species , y = AH_SpeciesCover,
                                              text = paste("Primary Key: " , PrimaryKey ,
                                                           "Plot ID: " , PlotID ,
                                                           "Species: " , ScientificName ,
                                                           "Code: " , Species ,
                                                           "Sage-Grouse Group: " , SG_Group ,
                                                           "Duration: " , Duration ,
                                                           "Percent Cover: " , AH_SpeciesCover ,
                                                           "Noxious: " , Noxious , sep = "<br>"))) +
                                            geom_boxplot(width = .6 , outlier.shape = NA) +
                                            geom_jitter(width = .15 , size = 1 , shape = 21) +
                                            scale_y_continuous(limits = c(0 , 100)) +
                                            theme_light() +
                                            labs(y = "Percent Cover") +
                                            ggtitle(paste("Percent Cover, Preferred Forbs: " , toString(EcologicalSiteId))) +
                                            theme(axis.title.y = element_blank() ,
                                            axis.title.x = element_blank(),
                                            axis.text.y = element_blank(),
                                            axis.ticks.y = element_blank(),
                                            panel.grid.major.y = element_blank()) +
                                            facet_grid(rows = vars(Species), drop = TRUE,
                                                       scales = "free") +
                                            coord_flip()
    }}

    if(!Interactive){

    #add this if statement to every plot. it will return NULL if the dataframe is empty

    SG_Plots <- if (nrow(PrefForb) < 1) {SG_Plots <- NULL} else{
      ggplot(PrefForb , aes(x = Species , y = AH_SpeciesCover)) +
                        geom_boxplot(width = .6 , outlier.shape = NA) +
                        geom_jitter(width = .15 , size = 1) +
                        scale_y_continuous(limits = c(0 , 100)) +
                        theme_light() +
                        labs(y = "Percent Cover") +
                        ggtitle(paste("Percent Cover, Preferred Forbs: " ,
                                      toString(EcologicalSiteId))) +
                        theme(panel.grid.major.y = element_blank()) +
                        coord_flip()
}}

return(SG_Plots)

}


if(SummaryVar == "SageGrouseGroup"){

##SG GROUP

    if(Interactive){
    SG_Plots <- ggplot(Species_plots_ecosite, aes(x = SG_Group , y = AH_SpeciesCover,
                                               text = paste("Primary Key: " , PrimaryKey ,
                                               "Plot ID: " , PlotID , "Species: " ,
                                                ScientificName , "Code: " , Species ,
                                                "Sage-Grouse Group: " , SG_Group ,
                                                "Percent Cover: " , AH_SpeciesCover ,
                                                "Noxious: " , Noxious , sep = "<br>"))) +
                    geom_boxplot(width = .6 , outlier.shape = NA) +
                    geom_jitter(width = .15 , size = 1 , shape = 21) +
                    scale_y_continuous(limits = c(0 , 100)) +
                    theme_light() +
                    ggtitle(paste("Percent Cover, Sage-Grouse Group: " ,
                                  toString(EcologicalSiteId))) +
                    theme(axis.title.y = element_blank() , axis.title.x = element_blank(),
                          axis.text.y = element_blank(), axis.ticks.y = element_blank(),
                          panel.grid.major.y = element_blank()) +
                          facet_grid(rows = vars(SG_Group), drop = TRUE, scales = "free")+
                    coord_flip() }

  if(!Interactive){

    SG_Plots <- ggplot(Species_plots_ecosite , aes(x = SG_Group , y = AH_SpeciesCover)) +
                            geom_boxplot(width = .6 , outlier.shape = NA) +
                            geom_jitter(width = .15 , size = 1) +
                            scale_y_continuous(limits = c(0 , 100)) +
                            theme_light() +
                            labs(y = "Percent Cover") +
                            ggtitle(paste("Percent Cover, Sage-Grouse Group: " ,
                                          toString(EcologicalSiteId))) +
                            theme(panel.grid.major.y = element_blank(),
                            axis.text.y = element_blank(), axis.title.y = element_blank()) +
                            facet_grid(rows = vars(SG_Group) ,
                            switch = "y" ,
                            scales = "free" ,
                            drop = TRUE) +
                            coord_flip()}

    return(SG_Plots)

}}

if(SummaryVar == "Sagebrush"){

SageBrushCover <- EcoSitePlots %>% dplyr::select(PlotID , PrimaryKey ,
                                                 AH_SagebrushCover ,
                                                 AH_SagebrushCover_Live ,
                                                 AH_SagebrushCover_Dead) %>%
                                    mutate_if(is.numeric, round , digits = 2) %>%
                gather(key = SageBrushCover ,
                       value = Percent , AH_SagebrushCover:AH_SagebrushCover_Dead)

if (nrow(SageBrushCover) < 1) {SG_Plots <- NULL} else{
    if(Interactive){
    SG_Plots <-
      ggplot(SageBrushCover, aes(x = SageBrushCover , y = Percent ,
                                       text = paste("Primary Key: " , PrimaryKey ,
                                                    "Plot ID: " , PlotID ,
                                                    "Percent Cover: " , Percent ,
                                                    sep = "<br>"))) +
                             geom_boxplot(width = .6 , outlier.shape = NA) +
                             geom_jitter(width = .15 , size = 1 , shape = 21) +
                             scale_y_continuous(limits = c(0 , 100)) +
                             theme_light() +
                             labs(y = "Percent Cover") +
                             ggtitle(paste("Percent Cover, Sagebrush: " ,
                                           toString(EcologicalSiteId))) +
                             theme(axis.title.y = element_blank() ,
                             axis.text.y = element_blank(),
                             axis.title.x = element_blank(),
                             axis.ticks.y = element_blank(),
                             panel.grid.major.y = element_blank()) +
                             facet_grid(rows = vars(SageBrushCover),
                                        scales = "free",  drop = TRUE) +
                            coord_flip()
    }

  if(!Interactive){
        SG_Plots <- ggplot(SageBrushCover , aes(x = SageBrushCover , y = Percent)) +
                    geom_boxplot(width = .6 , outlier.shape = NA) +
                    geom_jitter(width = .15 , size = 1) +
                    scale_y_continuous(limits = c(0 , 100)) +
                    theme_light() +
                    labs(y = "Percent Cover", x = "Sagebrush cover") +
                    ggtitle(paste("Percent Cover, Sagebrush: " ,
                                   toString(EcologicalSiteId))) +
                    theme(panel.grid.major.y = element_blank(),
                          axis.text.y = element_blank()) +
                     facet_grid(rows = vars(SageBrushCover),
                                drop = TRUE, scales = "free") +
                     coord_flip()}

return(SG_Plots)

}}
}




