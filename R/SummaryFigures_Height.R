SummaryFigures_Height <- function(Species_plots_ecosite, EcologicalSiteId, 
                           SummaryVar, GroupBy, Interactive){

##Setting color palette for plot
NoxNonPal_Fill <- c("grey75"  , "#D55E00")
NoxNonPal_Dot <- c("grey33" , "#993300")
HgtPrep <- Species_plots_ecosite %>% filter(!is.na(Hgt_Species_Avg)) %>% 
             filter(!is.na(GrowthHabitSub))  

if(SummaryVar == "Height" & GroupBy == "Species"){
  #Prep
  #Summarizing by species across an EcologicalSiteId

  if(Interactive){
    
    #Species
    
    Plots <- lapply(X = split(HgtPrep, list(HgtPrep$GrowthHabitSub , HgtPrep$Duration) , 
                               drop = TRUE),
                     FUN = function(HgtPrep){
                           current_plot <- ggplot(HgtPrep , 
                                                  aes(x = Species , 
                                                      y = Hgt_Species_Avg, 
                                                      text = paste("Plot Id: " ,  PlotID , 
                                                            "PrimaryKey: " , PrimaryKey ,
                                                             "Species: " , Species , 
                                                             "Average Height: "  , Hgt_Species_Avg , 
                                                             "Average Height , n: " , Hgt_Species_Avg_n , 
                                                              sep = "<br>"))) +
                                          geom_boxplot(width = .6 , outlier.shape = NA) +
                                          geom_jitter(width = .15 , shape = 21) +
                                          scale_y_continuous(limits = c(0 , 100)) +
                                          theme_light() +
                                          coord_flip() + 
                                          theme(axis.text.y = element_blank() , axis.ticks.y = element_blank() ,
                                                axis.line.y = element_blank())  +
                                                facet_grid(rows = vars(Species) ,
                                                     switch = "y" ,
                                                     scales = "free_y" , drop = TRUE)
                                      return(current_plot)
                                      })
                
    
  }
  
  if(!Interactive){
    
    Plots <- lapply(X = split(HgtPrep, list(HgtPrep$GrowthHabitSub , HgtPrep$Duration) , 
                              drop = TRUE),
                    FUN = function(HgtPrep){
                    current_plot <- ggplot(HgtPrep , aes(x = Species , y = Hgt_Species_Avg)) +
                                           geom_boxplot(width = .6 , outlier.shape = NA) +
                                           geom_jitter(width = .15 , size = 1 , aes(color = Noxious)) +
                                           scale_color_manual(values = NoxNonPal_Dot) + 
                                           scale_y_continuous(limits = c(0 , 100)) +
                                           theme_light() +
                                           labs(x = "Species" , y = "Average Height, cm",
                                                caption = paste("Species height in: ", toString(EcologicalSiteId), sep = "")) + 
                                           theme(axis.text.y = element_blank() , 
                                                 axis.ticks.y = element_blank() ,
                                                 axis.line.y = element_blank()) + 
                                                 coord_flip() +
                                                 facet_grid(rows = vars(Species) ,
                                                            switch = "y" ,
                                                            scales = "free_y" , drop = TRUE)
                                           return(current_plot) 
                                           })
    
  }
}
  

if(SummaryVar == "Height" & GroupBy == "GrowthHabit"){
  
if(Interactive){
  
  Plots <- ggplot(HgtPrep, aes(x = GrowthHabit , y = Hgt_Species_Avg , 
                               text = paste("Plot Id: " , PlotID , 
                                      "PrimaryKey: " , PrimaryKey ,
                                       "Species: " , Species , 
                                       "Average Height (cm): "  , Hgt_Species_Avg , 
                                       "Average Height , n: " , Hgt_Species_Avg_n ,
                                        sep = "<br>"))) +
                             geom_boxplot() +
                             geom_jitter(width = .1 , shape = 21) + 
                             theme_light() +
                             theme(axis.ticks.y = element_blank() ,
                             axis.line.y = element_blank(), 
                             axis.title.y = element_blank() ,
                             axis.title.x = element_blank() , 
                             axis.text.y = element_blank()) +
                             coord_flip() +
                             facet_grid(rows = vars(GrowthHabitSub) ,
                             switch = "y" ,
                             scales = "free_y" , drop = TRUE)
  
}
  
  if(!Interactive){
          Plots <- ggplot(HgtPrep, aes(x = GrowthHabit , y = Hgt_Species_Avg)) +
                         geom_boxplot() +
                         geom_jitter(width = .2 , 
                                     aes(text = PlotID)) +
                         labs(x = "Growth Habit" , 
                              y =  "Average Height, cm",
                              caption = paste("Species height in: ", 
                                              toString(EcologicalSiteId), sep = "")) + 
                         theme_light() +
                         theme(axis.text.y = element_blank() , 
                               axis.ticks.y = element_blank() ,
                               axis.line.y = element_blank()) + 
                         coord_flip() +
                         facet_grid(rows = vars(GrowthHabitSub) ,
                                    switch = "y" ,
                                    scales = "free_y" , drop = TRUE)
    
  }
  
} 
  

if(SummaryVar == "Height" & GroupBy == "Sagebrush"){
  
  if(Interactive){
    
    Plots <- HgtPrep %>% subset(SG_Group == "Sagebrush") %>%
            ggplot(aes(x = Species , y = Hgt_Species_Avg , 
                       text = paste("Plot Id: " , PlotID , 
                       "PrimaryKey: " , PrimaryKey ,
                       "Species: " , Species , 
                       "Average Height (cm): "  , Hgt_Species_Avg , 
                       "Average Height , n: " , Hgt_Species_Avg_n ,
                       sep = "<br>"))) +
              geom_boxplot() +
              geom_jitter(width = .1 , shape = 21) + 
              theme_light() +
              theme(axis.ticks.y = element_blank() ,
              axis.line.y = element_blank(), axis.title.y = element_blank() ,
              axis.title.x = element_blank(), axis.text.y = element_blank()) +
              facet_grid(rows = vars(Species), scales = "free_y", drop = TRUE) + 
              coord_flip() 
    
  }
  
}

if(!Interactive){
  
  Plots <- HgtPrep %>% subset(SG_Group == "Sagebrush") %>%
           ggplot(aes(x = Species , y = Hgt_Species_Avg)) +
           geom_boxplot() +
           geom_jitter(width = .1 , shape = 21) + theme_light() +
           theme(axis.ticks.y = element_blank() ,
           axis.line.y = element_blank()) +  
           labs(x = "Species" , 
                y = "Average Height, cm",
                caption = paste("Species height in: ", 
                                toString(EcologicalSiteId), sep = "")) + 
           coord_flip() 
  
}

  return(Plots)
  
}
  
