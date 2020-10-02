SummaryTables_WithAttributes <- function(EcoSitePlots, Species_plots_ecosite, 
                          SummaryVar, SummarizeBy, Attributed_Pks){

#Prep
SpeciesList <- SpeciesList %>% dplyr::select(Species, ScientificName, CommonName,
                                             Family, SpeciesState, 
                                             SynonymOf, UpdatedSpeciesCode, SpeciesState) %>% 
  dplyr::mutate(link = paste("https://plants.sc.egov.usda.gov/core/profile?symbol=", Species, sep = ""))

#Merge with species list so we can hover for scientific name

Species_plots_ecosite <- merge(Species_plots_ecosite , SpeciesList , by = c("Species" , "SpeciesState")) %>% 
      dplyr::select(Species, ScientificName, CommonName, Family, PrimaryKey, 
                PlotID,  AH_SpeciesCover, 
                AH_SpeciesCover_n, Hgt_Species_Avg, 
                Hgt_Species_Avg_n, GrowthHabit, GrowthHabitSub, Duration, 
                Noxious, SG_Group, link, SpeciesState, SynonymOf,
                UpdatedSpeciesCode) %>% 
                dplyr::mutate_if(is.numeric, round , digits = 2) 

#Merge with Attributed_Pks
Species_plots_ecosite_attributed <- merge(Species_plots_ecosite, Attributed_Pks, by = "PrimaryKey", all = TRUE) %>% 
                                    unique() 
  
 # For some reason there are plots in here with no data. Filtering them out. 
  
Species_plots_ecosite_attributed <- Species_plots_ecosite_attributed %>% filter(!is.na(Species)) %>% filter(!is.na(GrowthHabit))

 #Attributing EcoSitePlots
EcoSitePlots_Attributed <- merge(EcoSitePlots, Attributed_Pks, by = "PrimaryKey", all = TRUE)

# Filtering empty plots
  
EcoSitePlots_Attributed <- EcoSitePlots_Attributed %>% filter(!is.na(TotalFoliarCover))
#Get Noxious versus Non in Standard Format

Species_plots_ecosite_attributed$Noxious <- gsub("YES" , "Yes", Species_plots_ecosite_attributed$Noxious)
Species_plots_ecosite_attributed$Noxious <- gsub("NO", "No", Species_plots_ecosite_attributed$Noxious)

# Prep species richness for trace species

#Prep

#Detected in richness only on plot
RichnessPresent <- Species_plots_ecosite_attributed %>% filter(is.na(AH_SpeciesCover))
#Detected in LPI on plot
LPI_Present <- Species_plots_ecosite_attributed %>% filter(AH_SpeciesCover > 0.000000)
#Removes duplicates
LPI_Present_String <- unique(LPI_Present$Species)
#Removes values from richness that also occurred in LPI
RichnessSpecies_Only <- RichnessPresent[!(RichnessPresent[["Species"]] %in% LPI_Present_String),]
#Removes duplicates
TraceCover_List <- unique(RichnessSpecies_Only$Species)
#Get into dataframe (just select state species that were trace)
TraceSpeciesCover <- Species_plots_ecosite_attributed[Species_plots_ecosite_attributed[["Species"]] %in% TraceCover_List,]

TraceCover_Table_SpList <- TraceSpeciesCover %>%
                dplyr::select(Species, ScientificName , Family , GrowthHabit ,
                GrowthHabitSub , Duration, Noxious , SG_Group ,
                SynonymOf , CommonName ,
                UpdatedSpeciesCode, link, ALLOT_NAME, ALLOT_NO, PAST_NAME) %>% unique() %>% filter(!is.na(Species))

if(SummaryVar == "Species" & SummarizeBy == "Plot"){
  #hyperlink species
  Species_plots_ecosite_attributed$Species <- paste0("<a href='",Species_plots_ecosite_attributed$link,"'>",Species_plots_ecosite_attributed$Species,"</a>")
  
  table <- Species_plots_ecosite_attributed %>% select(-link) %>% filter(!is.na(AH_SpeciesCover)) %>% 
           DT::datatable(escape = FALSE, extensions = 'Buttons', filter = "top" , 
                  options = list(scrollX = TRUE ,
                            dom = 'Bfrtip',
                            buttons =
                            list(list(extend = 'collection', buttons = c('csv', 'excel'),
                            text = 'Download Table'))) , 
                  caption = (paste("Percent Cover by Species by Plot within " , toString(EcologicalSiteId))) , 
                  rownames = FALSE)
}

if(SummaryVar == "Species" & SummarizeBy == "EcologicalSite"){
  #For summarizing across all plots
  Species_cover_summary <- Species_plots_ecosite_attributed %>% filter(!is.na(AH_SpeciesCover)) %>% 
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
 
  #hyperlink species
  Species_cover_summary$Species <- paste0("<a href='",Species_cover_summary$link,"'>", Species_cover_summary$Species,"</a>")
  
  table <- Species_cover_summary %>% select(-link) %>% DT::datatable(escape = FALSE, 
                                                            extensions = 'Buttons', 
                                                            filter = "top" , 
                                                            options = list(scrollX = TRUE ,
                                                            dom = 'Bfrtip',
                                                            buttons =
                                                            list(list(
                                                            extend = 'collection',
                                                            buttons = c('csv', 'excel'),
                                                            text = 'Download Table'))) , 
                                                            caption = (paste("Average Percent Cover Values Across" , toString(EcologicalSiteId))) , 
                                                            rownames = FALSE)
}


if(SummaryVar== "GrowthHabitSub" & SummarizeBy == "Plot"){
  
       table <-  Species_plots_ecosite_attributed %>% 
                 group_by(PrimaryKey , PlotID , GrowthHabitSub , Duration) %>% 
                 filter(!is.na(AH_SpeciesCover)) %>% 
                 summarize(PercentCover = sum(AH_SpeciesCover)) %>%
                 mutate_if(is.numeric, round , digits = 2) %>% 
                 filter(!is.na(GrowthHabitSub)) %>%
                 DT::datatable(extensions = 'Buttons', filter = "top" ,  
                               options = list(scrollX = TRUE ,
                               dom = 'Bfrtip',
                               buttons =
                               list(list(
                               extend = 'collection',
                               buttons = c('csv', 'excel'),
                               text = 'Download Table'))) ,
                  caption = (paste("Percent Cover by Structure and Functional Group by Plot  within " , 
                                   toString(EcologicalSiteId))), 
                  rownames = FALSE)
}

if(SummaryVar == "GrowthHabitSub" & SummarizeBy == "EcologicalSite"){
    #removes trace species
    table <- Species_plots_ecosite_attributed %>% filter(!is.na(AH_SpeciesCover)) %>% 
                              mutate(Tally = 1) %>%
                              group_by(GrowthHabitSub, Duration, Tally) %>%
              summarize(AveragePercentCover = mean(AH_SpeciesCover) ,
              StandardDeviation = sd(AH_SpeciesCover),
              MinCover = min(AH_SpeciesCover) ,
              MaxCover = max(AH_SpeciesCover) , n = sum(Tally)) %>%
              mutate_if(is.numeric, round , digits = 2) %>%
              select(-Tally) %>%
              filter(!is.na(GrowthHabitSub)) %>%
              DT::datatable(extensions = 'Buttons', filter = "top" ,  
                            options = list(scrollX = TRUE ,
                            dom = 'Bfrtip',
                            buttons =
                            list(list(
                            extend = 'collection',
                            buttons = c('csv', 'excel'),
                            text = 'Download Table'))) ,
                  caption = (paste("Percent Cover by Structure and Functional Group in " , 
                                   toString(EcologicalSiteId))), 
                  rownames = FALSE)
    
}

if(SummaryVar== "Noxious" & SummarizeBy == "Plot"){
  
  table <- EcoSitePlots_Attributed %>% dplyr::select(PlotID, PrimaryKey, AH_NoxCover, AH_NonNoxCover, ALLOT_NAME, ALLOT_NO, PAST_NAME) %>%
                   filter(!is.na(AH_SpeciesCover)) %>%
                   rename(NonNoxious = AH_NonNoxCover, Noxious = AH_NoxCover) %>%
                   dplyr::mutate_if(is.numeric, round , digits = 2) %>% 
                   DT::datatable(extensions = 'Buttons', filter = "top" , 
                                 options = list(scrollX = TRUE ,
                                 dom = 'Bfrtip',
                                 buttons =
                                 list(list(
                                 extend = 'collection',
                                 buttons = c('csv', 'excel'),
                                 text = 'Download Table'))) , 
                                 caption = (paste("Percent Cover Noxious Versus Non by Plot within " , 
                                                  toString(EcologicalSiteId))) , 
                                 rownames = FALSE)
}

if(SummaryVar== "Noxious" & SummarizeBy == "EcologicalSite"){
 
 prep <-  EcoSitePlots_Attributed %>% dplyr::select(PlotID, PrimaryKey, AH_NoxCover, AH_NonNoxCover) %>% 
                            filter(!is.na(AH_SpeciesCover)) %>%
                            dplyr::rename(NonNoxious = AH_NonNoxCover, Noxious = AH_NoxCover) %>%
                            gather(key = "Noxious", value = Percent,
                                   NonNoxious:Noxious) %>%
                            dplyr::mutate(Tally = 1) 
 
 prep$Noxious <- gsub("NonNoxious" , "No", prep$Noxious)
 prep$Noxious <- gsub("Noxious", "Yes", prep$Noxious)
  
   table <-   prep %>% group_by(Noxious) %>% 
              summarize(AveragePercentCover = mean(Percent) ,
              StandardDeviation = sd(Percent),
              MinCover = min(Percent) ,
              MaxCover = max(Percent) , n = sum(Tally)) %>%
              mutate_if(is.numeric, round , digits = 2) %>%
              DT::datatable(extensions = 'Buttons', filter = "top" , 
                             options = list(scrollX = TRUE ,
                             dom = 'Bfrtip',
                             buttons =
                             list(list(
                             extend = 'collection',
                             buttons = c('csv', 'excel'),
                             text = 'Download Table'))) , 
                   caption = (paste("Percent Cover Noxious Versus Non in " , 
                                    toString(EcologicalSiteId))) , 
                   rownames = FALSE)
  
}

if(SummaryVar == "Woody" & SummarizeBy == "Plot"){
  
   table <-  Species_plots_ecosite_attributed %>% filter(!is.na(AH_SpeciesCover)) %>%
                  group_by(GrowthHabit , PrimaryKey , PlotID) %>%
                  summarize(PercentCover = sum(AH_SpeciesCover)) %>%
                  mutate_if(is.numeric, round , digits = 2) %>%
                  DT::datatable(extensions = 'Buttons', filter = "top" , 
                                options = list(scrollX = TRUE ,
                                dom = 'Bfrtip',
                                buttons =
                                list(list(
                                extend = 'collection',
                                buttons = c('csv', 'excel'),
                                text = 'Download Table'))) , 
                      caption = (paste("Percent Cover Woody vs. Non by Plot within: " , toString(EcologicalSiteId))) , 
                      rownames = FALSE)
  
}

if(SummaryVar == "Woody" & SummarizeBy == "EcologicalSite"){

    table <- Species_plots_ecosite_attributed %>% filter(!is.na(AH_SpeciesCover)) %>%
              mutate(Tally = 1) %>%
              group_by(GrowthHabit , Tally) %>%
              summarize(AveragePercentCover = mean(AH_SpeciesCover) ,
              StandardDeviation = sd(AH_SpeciesCover),
              MinCover = min(AH_SpeciesCover) ,
              MaxCover = max(AH_SpeciesCover) , n = sum(Tally)) %>%
              subset(AveragePercentCover > 0.0000) %>%
              mutate_if(is.numeric, round , digits = 2) %>% 
              dplyr::select(-Tally) %>%
              filter(!is.na(GrowthHabit)) %>% 
              DT::datatable(extensions = 'Buttons', 
                            filter = "top" , options = list(scrollX = TRUE ,
                            dom = 'Bfrtip',
                            buttons =
                            list(list(
                            extend = 'collection',
                            buttons = c('csv', 'excel'),
                            text = 'Download Table'))) , 
                            caption = (paste("Percent Cover Woody vs. Non in: " , 
                                             toString(EcologicalSiteId))) , 
                                       rownames = FALSE)
    
}


if(SummaryVar == "SageGrouseGroup" & SummarizeBy == "Plot"){
   table <-     Species_plots_ecosite_attributed %>% filter(!is.na(SG_Group)) %>% 
                filter(!is.na(AH_SpeciesCover)) %>%
                group_by(SG_Group, PrimaryKey , PlotID) %>%
                summarize(PercentCover = sum(AH_SpeciesCover)) %>%
                mutate_if(is.numeric, round , digits = 2) %>% 
                DT::datatable(extensions = 'Buttons', 
                              filter = "top" ,  options = list(scrollX = TRUE ,
                              dom = 'Bfrtip',
                              buttons =
                              list(list(
                              extend = 'collection',
                              buttons = c('csv', 'excel'),
                              text = 'Download Table'))) , 
                  caption = (paste("Percent Cover by Sage-Grouse Group by Plot within: " , 
                                   toString(EcologicalSiteId))), 
                  rownames = FALSE)
}

if(SummaryVar == "SageGrouseGroup" & SummarizeBy == "EcologicalSite"){
  
 table <-  Species_plots_ecosite_attributed %>% filter(!is.na(SG_Group)) %>% 
    filter(!is.na(AH_SpeciesCover)) %>% mutate(Tally = 1) %>%
    group_by(SG_Group, Tally) %>%
    summarize(AveragePercentCover = mean(AH_SpeciesCover) ,
              StandardDeviation = sd(AH_SpeciesCover),
              MinCover = min(AH_SpeciesCover) ,
              MaxCover = max(AH_SpeciesCover) , n = sum(Tally)) %>%
              mutate_if(is.numeric, round , digits = 2) %>%
              dplyr::select(-Tally) %>%
              DT::datatable(extensions = 'Buttons', filter = "top" ,  
                            options = list(scrollX = TRUE ,
                            dom = 'Bfrtip',
                            buttons =
                            list(list(
                            extend = 'collection',
                            buttons = c('csv', 'excel'),
                            text = 'Download Table'))) ,
                 caption = (paste("Percent Cover by Sage-Grouse Group in " , 
                                  toString(EcologicalSiteId))) , 
                 rownames = FALSE)
  
}

if(SummaryVar == "PreferredForb" & SummarizeBy == "Plot"){
  
   table <- Species_plots_ecosite_attributed %>% 
            mutate(PreferredForb = (SG_Group == "PreferredForb")) %>% 
            subset(PreferredForb == TRUE) %>% 
            subset(AH_SpeciesCover > 0.0000) %>%
            filter(!is.na(AH_SpeciesCover)) %>%
            group_by(Species, PrimaryKey , PlotID) %>%
            summarize(PercentCover = sum(AH_SpeciesCover)) %>%
            mutate_if(is.numeric, round , digits = 2) %>%
            DT::datatable(extensions = 'Buttons', filter = "top" ,  
                          options = list(scrollX = TRUE ,
                          dom = 'Bfrtip',
                          buttons =
                          list(list(
                          extend = 'collection',
                          buttons = c('csv', 'excel'),
                          text = 'Download Table'))) , 
                  caption = (paste("Percent Cover by Preferred Forb By Plot within " , 
                                   toString(EcologicalSiteId))) , 
                  rownames = FALSE)
  
  
}

if(SummaryVar == "PreferredForb" & SummarizeBy == "EcologicalSite"){
  
  table <- Species_plots_ecosite_attributed %>% 
           mutate(PreferredForb = (SG_Group == "PreferredForb")) %>% 
           subset(PreferredForb == TRUE) %>% 
           subset(AH_SpeciesCover > 0.0000) %>%
           filter(!is.na(AH_SpeciesCover)) %>%
           filter(!is.na(AH_SpeciesCover)) %>%
           mutate(Tally = 1) %>%
           group_by(Species, Tally) %>%
           summarize(AveragePercentCover = mean(AH_SpeciesCover) ,
                     StandardDeviation = sd(AH_SpeciesCover),
                     MinCover = min(AH_SpeciesCover) ,
                     MaxCover = max(AH_SpeciesCover) , n = sum(Tally)) %>%
           mutate_if(is.numeric, round , digits = 2) %>% dplyr::select(-Tally) %>%
           DT::datatable(extensions = 'Buttons', filter = "top" ,  
                        options = list(scrollX = TRUE ,
                        dom = 'Bfrtip',
                        buttons =
                        list(list(
                        extend = 'collection',
                        buttons = c('csv', 'excel'),
                        text = 'Download Table'))) , 
                        caption = (paste("Percent Cover by Preferred Forb in " , 
                                         toString(EcologicalSiteId))) , 
                        rownames = FALSE)
  
}

if(SummaryVar == "TraceSpecies" & SummarizeBy == "Plot"){
  
          RichnessPresent <-  RichnessPresent %>% 
          dplyr::select(Species, ScientificName , GrowthHabit ,
          GrowthHabitSub , Duration, Noxious , SG_Group, 
          PrimaryKey, PlotID, link, ALLOT_NAME, ALLOT_NO, PAST_NAME) %>% filter(!is.na(Species))

          RichnessPresent$Species <- paste0("<a href='",RichnessPresent$link,"'>", RichnessPresent$Species,"</a>")
  
          table <- RichnessPresent %>% select(-link) %>%  
                   DT::datatable(escape = FALSE, extensions = 'Buttons', 
                         filter = "top" , options = list(scrollX = TRUE ,
                         dom = 'Bfrtip',
                         buttons =
                         list(list(
                         extend = 'collection',
                         buttons = c('csv', 'excel'),
                         text = 'Download Table'))) , 
                         caption = (paste("Trace Species by Plot within " , 
                                         toString(EcologicalSiteId))) , 
                         rownames = FALSE)
  
}

if(SummaryVar == "TraceSpecies" & SummarizeBy == "EcologicalSite"){
  
  
  TraceCover_Table_SpList$Species <- paste0("<a href='",TraceCover_Table_SpList$link,"'>", TraceCover_Table_SpList$Species,"</a>")
 
   table <- TraceCover_Table_SpList %>% arrange(Species) %>% 
                              select(-link) %>% 
                              DT::datatable(escape = FALSE, 
                                            extensions = 'Buttons', 
                                            filter = "top" , 
                                            options = list(scrollX = TRUE ,
                                            dom = 'Bfrtip',
                                            buttons =
                                            list(list(
                                            extend = 'collection',
                                            buttons = c('csv', 'excel'),
                                            text = 'Download Table'))) , 
                                            caption = (paste("Trace species in " , 
                                                       toString(EcologicalSiteId))) , 
                                                       rownames = FALSE)
  
   
}

if(SummaryVar == "GroundCover" & SummarizeBy == "Plot"){

            table <- EcoSitePlots_Attributed %>% dplyr::select(PlotID, PrimaryKey, BareSoilCover , 
                     TotalFoliarCover , FH_TotalLitterCover , 
                     FH_RockCover, ALLOT_NAME, ALLOT_NO, PAST_NAME) %>% 
                     gather(key = Indicator , value = Percent, 
                     BareSoilCover:FH_RockCover) %>%
                     filter(!is.na(Percent)) %>% mutate(Tally = 1) %>% 
                     group_by(PlotID, PrimaryKey, Indicator) %>% 
                     mutate_if(is.numeric, round , digits = 2) %>% select(-Tally) %>% 
                     rename(PercentCover = Percent) %>%
                     DT::datatable(extensions = 'Buttons', filter = "top" , 
                                   options = list(scrollX = TRUE ,
                                   dom = 'Bfrtip',
                                   buttons =
                                   list(list(
                                   extend = 'collection',
                                   buttons = c('csv', 'excel'),
                                   text = 'Download Table'))) , 
                                   caption = (paste("Percent cover by plot within " , 
                                                    toString(EcologicalSiteId))) , 
                                             rownames = FALSE)
}

if(SummaryVar == "GroundCover" & SummarizeBy == "EcologicalSite"){
 
        table <- EcoSitePlots_Attributed %>% dplyr::select(PlotID, PrimaryKey, BareSoilCover , 
                                                TotalFoliarCover , FH_TotalLitterCover , 
                                                FH_RockCover) %>%
                 gather(key = Indicator , value = Percent, 
                 BareSoilCover:FH_RockCover) %>% 
                 filter(!is.na(Percent)) %>% mutate(Tally = 1) %>%
                 group_by(Indicator) %>%
                 summarize(AveragePercentCover = mean(Percent) ,
                 Standard_Deviation = sd(Percent) ,
                 Low = min(Percent) ,
                 High = max(Percent), n = sum(Tally)) %>% 
                 mutate_if(is.numeric, round , digits = 2) %>%
                 DT::datatable(extensions = 'Buttons', filter = "top" , 
                               options = list(scrollX = TRUE ,
                               dom = 'Bfrtip',
                               buttons =
                               list(list(
                               extend = 'collection',
                               buttons = c('csv', 'excel'),
                               text = 'Download Table'))) , 
                        caption = (paste("Average percent cover in " , 
                                         toString(EcologicalSiteId))) , 
                        rownames = FALSE)
}

if(SummaryVar == "Gap" & SummarizeBy == "Plot"){

  table  <- EcoSitePlots_Attributed %>% dplyr::select(PlotID , PrimaryKey , 
                             GapCover_25_50 , GapCover_51_100 , 
                             GapCover_101_200 , GapCover_200_plus , 
                             GapCover_25_plus, ALLOT_NAME, ALLOT_NO, PAST_NAME) %>% 
                             gather(key = Gap_Class_cm , 
                             value = Percent , GapCover_25_50:GapCover_25_plus) %>%
                             filter(!is.na(Percent)) %>% 
                             mutate_if(is.numeric , round, digits = 2) %>% 
                             group_by(PlotID , PrimaryKey) %>%  
                             mutate_if(is.numeric, round , digits = 2) %>% 
                             rename(Percent_Cover = Percent) %>% 
                             DT::datatable(extensions = 'Buttons', filter = "top" , 
                                           options = list(scrollX = TRUE ,
                                           dom = 'Bfrtip',
                                           buttons =
                                           list(list(
                                           extend = 'collection',
                                           buttons = c('csv', 'excel'),
                                           text = 'Download Table'))) , 
                                           caption = (paste("Percent cover by canopy gap class by plot within " , 
                                                           toString(EcologicalSiteId))) , 
                                           rownames = FALSE)
}

if(SummaryVar == "Gap" & SummarizeBy == "EcologicalSite"){
  
  table <- EcoSitePlots_Attributed %>% dplyr::select(PlotID , PrimaryKey , 
                                          GapCover_25_50 , GapCover_51_100 , 
                                          GapCover_101_200 , GapCover_200_plus , 
                                          GapCover_25_plus) %>% 
                            gather(key = Gap_Class_cm , 
                            value = Percent , GapCover_25_50:GapCover_25_plus) %>%
                            filter(!is.na(Percent)) %>%
                            mutate_if(is.numeric , round, digits = 2) %>%
                            group_by(Gap_Class_cm) %>%
                            summarize(AveragePercentCover = mean(Percent) ,
                            StandardDeviation = sd(Percent),
                            MinPercentCover = min(Percent) ,
                            MaxPercentCover = max(Percent)) %>%
                            mutate_if(is.numeric, round , digits = 2) %>% 
                            DT::datatable(extensions = 'Buttons', 
                                          filter = "top" , options = list(scrollX = TRUE ,
                                          dom = 'Bfrtip',
                                          buttons =
                                          list(list(
                                          extend = 'collection',
                                          buttons = c('csv', 'excel'),
                                          text = 'Download Table'))) , 
                            caption = (paste("Percent cover by canopy gap class in: " , 
                                             toString(EcologicalSiteId))) , 
                                      rownames = FALSE)
  
}
  
if(SummaryVar == "SoilStability" & SummarizeBy == "Plot"){
  
        table <-  EcoSitePlots_Attributed %>% dplyr::select(PlotID , PrimaryKey , 
                                   SoilStability_All , 
                                   SoilStability_Protected , 
                                   SoilStability_Unprotected, ALLOT_NAME, ALLOT_NO, PAST_NAME) %>%
                  gather(key = Veg , value = Rating , 
                  SoilStability_All:SoilStability_Unprotected) %>%
                  filter(!is.na(Rating)) %>% 
                  mutate_if(is.numeric, round, digits = 2)  %>% 
                  group_by(PrimaryKey , PlotID) %>% 
                  mutate_if(is.numeric, round , digits = 2) %>%
                  DT::datatable(extensions = 'Buttons', 
                                filter = "top" , 
                                options = list(scrollX = TRUE ,
                                dom = 'Bfrtip',
                                buttons =
                                list(list(
                                extend = 'collection',
                                buttons = c('csv', 'excel'),
                                text = 'Download Table'))) , 
                        caption = (paste("Soil stability ratings by plot in: " , 
                                         toString(EcologicalSiteId))) , 
                                rownames = FALSE)

}

if(SummaryVar == "SoilStability" & SummarizeBy == "EcologicalSite"){
  
           table <-  EcoSitePlots_Attributed %>% dplyr::select(PlotID , PrimaryKey , 
                                           SoilStability_All , 
                                           SoilStability_Protected , 
                                           SoilStability_Unprotected) %>%
                                      gather(key = Veg , value = Rating , 
                                      SoilStability_All:SoilStability_Unprotected) %>%
                                      filter(!is.na(Rating)) %>% 
                                      mutate_if(is.numeric, round, digits = 2)  %>% 
                                      group_by(Veg) %>% 
                                      summarize(AverageSoilStability = mean(Rating , na.rm = TRUE) ,
                                      StandardDeviation = sd(Rating , na.rm = TRUE) ,
                                      MinSoilStability = min(Rating , na.rm = TRUE) ,
                                      MaxSoilStability = max(Rating, na.rm = TRUE)) %>%
                                      mutate_if(is.numeric, round , digits = 2) %>%
                      DT::datatable(extensions = 'Buttons', filter = "top" , 
                                    options = list(scrollX = TRUE ,
                                    dom = 'Bfrtip',
                                    buttons =
                                    list(list(
                                    extend = 'collection',
                                    buttons = c('csv', 'excel'),
                                    text = 'Download Table'))) , 
                           caption = (paste("Average soil stability ratings in: " , 
                                            toString(EcologicalSiteId))) , 
                           rownames = FALSE)

}

return(table)

}
