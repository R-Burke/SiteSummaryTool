
# Summary tables for vegetation height
SummaryTables_Height <- function(Species_plots_ecosite, 
                          SummaryVar, SummarizeBy, GroupBy){

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
    
#Get Noxious versus Non in Standard Format
    
Species_plots_ecosite$Noxious <- gsub("YES" , "Yes", Species_plots_ecosite$Noxious)
Species_plots_ecosite$Noxious <- gsub("NO", "No", Species_plots_ecosite$Noxious)  

HgtPrep <- Species_plots_ecosite %>% filter(!is.na(Hgt_Species_Avg)) %>% 
           filter(!is.na(GrowthHabitSub))  

if(SummaryVar == "Height" & SummarizeBy == "Plot" & GroupBy == "GrowthHabit"){
  
 table  <- HgtPrep %>% group_by(GrowthHabitSub , Duration , PrimaryKey , PlotID) %>%
              summarize(AverageHeight_cm = mean(Hgt_Species_Avg) ,
              Standard_Deviation_cm = sd(Hgt_Species_Avg),
              MinHeight_cm = min(Hgt_Species_Avg) ,
              MaxHeight_cm = max(Hgt_Species_Avg), 
              n= sum(Hgt_Species_Avg_n))%>%
              mutate_if(is.numeric, round , digits = 2) %>%
              DT::datatable(extensions = 'Buttons', filter = "top" , 
                            options = list(scrollX = TRUE ,
                            dom = 'Bfrtip',
                            buttons =
                            list(list(
                            extend = 'collection',
                            buttons = c('csv', 'excel'),
                            text = 'Download Table'))) , 
                 caption = paste("Heights by functional group in: " , 
                                 toString(EcologicalSiteId)) , 
                 rownames = FALSE)
  
}

if(SummaryVar == "Height" & SummarizeBy == "EcologicalSite" & GroupBy == "GrowthHabit"){
  
  table <- HgtPrep %>%  group_by(GrowthHabitSub , Duration) %>%
                     summarize(AverageHeight_cm = mean(Hgt_Species_Avg) ,
                     Standard_Deviation_cm = sd(Hgt_Species_Avg),
                     MinHeight_cm = min(Hgt_Species_Avg) ,
                     MaxHeight_cm = max(Hgt_Species_Avg), 
                     n= sum(Hgt_Species_Avg_n)) %>% 
                     mutate_if(is.numeric, round , digits = 2) %>%
                     DT::datatable(extensions = 'Buttons', filter = "top" , 
                       options = list(scrollX = TRUE ,
                                 dom = 'Bfrtip',
                                 buttons =
                                 list(list(
                                 extend = 'collection',
                                 buttons = c('csv', 'excel'),
                                 text = 'Download Table'))) , 
                  caption = paste("Heights by functional group in: " , 
                                  toString(EcologicalSiteId)) , 
                  rownames = FALSE)
}


if(SummaryVar == "Height" & SummarizeBy == "Plot" & GroupBy == "Species"){
  # Figure out why this isn't working
  #HgtPrep$Species <- paste0("<a href='",HgtPrep$link,"'>", HgtPrep$Species,"</a>")
              
  table <- HgtPrep %>% dplyr::select(-link) %>% group_by(PlotID, PrimaryKey, Species, 
                                           ScientificName , CommonName , 
                                           GrowthHabitSub, Duration) %>%
                                   dplyr::select(GrowthHabitSub , Duration , PlotID , 
                                   PrimaryKey , Hgt_Species_Avg , 
                                   Hgt_Species_Avg_n) %>%
                                   DT::datatable(extensions = 'Buttons', 
                                                 filter = "top" , 
                                                 options = list(scrollX = TRUE ,
                                                 dom = 'Bfrtip',
                                                 buttons =
                                                 list(list(
                                                 extend = 'collection',
                                                 buttons = c('csv', 'excel'),
                                                 text = 'Download Table'))) , 
                                   caption = (paste("Species heights in: " , 
                                                    toString(EcologicalSiteId))) , 
                                   rownames = FALSE)

}

if(SummaryVar == "Height" & SummarizeBy == "EcologicalSite" & GroupBy == "Species"){
  
  table <- HgtPrep %>% group_by(Species, ScientificName , 
                                CommonName , GrowthHabitSub, Duration) %>%
                                summarize(AverageHeight_cm = mean(Hgt_Species_Avg) ,
                                Standard_Deviation_cm = sd(Hgt_Species_Avg),
                                MinHeight_cm = min(Hgt_Species_Avg) ,
                                MaxHeight_cm = max(Hgt_Species_Avg),
                                n = sum(Hgt_Species_Avg_n)) %>%
                                mutate_if(is.numeric, round , digits = 2) %>%
                                DT::datatable(extensions = 'Buttons', filter = "top" , options = list(scrollX = TRUE ,
                                              dom = 'Bfrtip',
                                              buttons =
                                              list(list(
                                              extend = 'collection',
                                              buttons = c('csv', 'excel'),
                                              text = 'Download Table'))) , 
                               caption = (paste("Heights by species in " , 
                                                toString(EcologicalSiteId))) ,
                                          rownames = FALSE)
  
}

if(SummaryVar == "Height" & SummarizeBy == "Plot" & GroupBy == "Sagebrush"){
    
        table <- HgtPrep %>% filter(!is.na(SG_Group)) %>% 
                 subset(SG_Group == "Sagebrush") %>%
                  group_by(PlotID, PrimaryKey, Species, ScientificName , 
                           CommonName , SG_Group) %>%
                  dplyr::select(Species, PlotID , 
                  PrimaryKey , Hgt_Species_Avg , 
                  Hgt_Species_Avg_n) %>% 
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
                caption = (paste("Heights by sagebrush species by plots within " , 
                                 toString(EcologicalSiteId))) ,
                           rownames = FALSE)
  
  
}

if(SummaryVar == "Height" & SummarizeBy == "EcologicalSite" & GroupBy == "Sagebrush"){

                 table <- HgtPrep %>% filter(!is.na(SG_Group)) %>% 
                          subset(SG_Group == "Sagebrush") %>%
                          group_by(Species, ScientificName , 
                          CommonName , SG_Group) %>%
                          summarize(AverageHeight_cm = mean(Hgt_Species_Avg) ,
                          Standard_Deviation_cm = sd(Hgt_Species_Avg),
                          MinHeight_cm = min(Hgt_Species_Avg) ,
                          MaxHeight_cm = max(Hgt_Species_Avg),
                          n = sum(Hgt_Species_Avg_n)) %>%
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
                  caption = (paste("Heights by sagebrush species in " , 
                                   toString(EcologicalSiteId))) ,
                  rownames = FALSE)
  
}

return(table)

}

