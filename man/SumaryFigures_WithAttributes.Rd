\name{SumaryFigures_WithAttributes}
\alias{SumaryFigures_WithAttributes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
SummaryFigures_WithAttributes}
\description{
Same as SummaryFigures(), but colors by pasture name (could modify for allotment name). Currently hard-coded for BLM Allotment naming convention- ALLOT_NAME, ALLOT_NO, PAST_NAME. Will be updating to generalize.}
\usage{
SumaryFigures_WithAttributes(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{SpeciesList}{State species list as dataframe
%%     ~~Describe \code{x} here~~
}
  \item{Species_plots_ecosite}{Combined AIM and LMF species indicator, subset to your ecological site(s). Returned output from Combine_AIM_LMF_Species and SubsetEcologicalSite_Species.}
  \item{EcologicalSiteId}{Full ecological site id, list of ecological sites, or object}
  \item{SummaryVar}{c("GrowthHabitSub", "Noxious", "Species", "GroundCover", "Gap", "SoilStability")}
  \item{Interactive}{logical. If TRUE, boxplot aesthetics will be set up for interactive plotly::ggplotly object. If FALSE, will return static box plot.}
  \item{EcoSitePlots}{Combined TerrADat and LMF dataframe, subset to your ecological site. Returned object from consecutive Combine_AIM_LMF and SubsetEcologicalSite functions.}
  \item{Attributed_Pks}{simplified dataframe with PrimaryKeys associated with each shapefile attribute}
}

\details{To get Attributed_Pks-

attribute_title <- c("ALLOT_NAME", "ALLOT_NO", "PAST_NAME")
TDat_LMF_Attributed <- AttributePlots(TDat_LMF = TDat_LMF, shapefile_name = shapefile_name,
               shapefile_path = shapefile_path, attribute_title = attribute_title)
Attributed_Pks <- dplyr::select(TDat_LMF_Attributed, PrimaryKey, all_of(attribute_title))

}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (SpeciesList, Species_plots_ecosite, EcologicalSiteId, 
                                  SummaryVar, Interactive, Attributed_Pks, EcoSitePlots){

#Prep
  SpeciesList <- SpeciesList %>% dplyr::select(Species, ScientificName, CommonName,
                                               Family, SpeciesState,
                                               SynonymOf, UpdatedSpeciesCode) %>% 
                 dplyr::mutate(link = paste("https://plants.sc.egov.usda.gov/core/profile?symbol=", Species, sep = ""))

#Merge with species list so we can hover for scientific name
Species_plots_ecosite_attributed <- merge(Species_plots_ecosite, Attributed_Pks, by = "PrimaryKey", all = TRUE) %>% 
                                    unique() 

EcoSitePlots_Attributed <- merge(EcoSitePlots, Attributed_Pks, by = "PrimaryKey", all = TRUE)



#Unhardcode last 3 columns in selection
Species_plots_ecosite_attributed <- merge(Species_plots_ecosite_attributed , SpeciesList , by = c("Species" , "SpeciesState")) %>% 
                         dplyr::select(Species, ScientificName, CommonName, PrimaryKey, 
                                PlotID,  AH_SpeciesCover, 
                                AH_SpeciesCover_n, Hgt_Species_Avg, 
                                Hgt_Species_Avg_n, GrowthHabit, GrowthHabitSub, Duration, 
                                Noxious, SG_Group, link, 
                                ALLOT_NAME, ALLOT_NO, PAST_NAME) %>%
                         dplyr::mutate_if(is.numeric, round , digits = 2) 

#Get Noxious versus Non in Standard Format

Species_plots_ecosite_attributed$Noxious <- gsub("YES" , "Yes", Species_plots_ecosite_attributed$Noxious)
Species_plots_ecosite_attributed$Noxious <- gsub("NO", "No", Species_plots_ecosite_attributed$Noxious)

#Ignoring NAs- make disclosure as this may overestimate cover
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

##Setting color palette for plot
NoxNonPal_Fill <- c("grey75"  , "#D55E00")
NoxNonPal_Dot <- c("grey33" , "#993300")
## Setting color for attribute title
## FIgure out how to not hardcode ALLOT_NAME and instead use attribute_title
Attribute_Fill <- scales::seq_gradient_pal("#009966", "#E69F00", "Lab")(seq(0,1, length.out = length(unique(Species_plots_ecosite_attributed$PAST_NAME))))

#Remove NAs for plotting
Species_plots_ecosite_attributed <- Species_plots_ecosite_attributed %>% filter(!is.na(AH_SpeciesCover))
  
if(SummaryVar == "GrowthHabitSub"){
if(Interactive){
  Plots <-  lapply(X = split(Species_plots_ecosite_attributed, Species_plots_ecosite_attributed[["GrowthHabitSub"]] , 
                                           drop = TRUE),
                                 
                                 FUN = function(Species_plots_ecosite_attributed){
                                 
                                   current_plot <- ggplot2::ggplot(Species_plots_ecosite_attributed , 
                                                          aes(x = GrowthHabitSub, 
                                                              y = AH_SpeciesCover, 
                                                              text = paste("Primary Key: " , PrimaryKey , 
                                                                           "Plot ID: " , PlotID , "Species: " , 
                                                                            ScientificName , "Code: " , Species , 
                                                                            "Percent Cover: " , AH_SpeciesCover , 
                                                                            "Noxious: " , Noxious , 
                                                                            "Allotment: ", ALLOT_NAME, 
                                                                            "Pasture: ", PAST_NAME,
                                                                            sep = "<br>"))) +
                                     geom_boxplot(width = .6 , outlier.shape = NA) +
                                     geom_jitter(width = .2 , size = 2 , shape = 21) +
                                     theme_light() +
                                     scale_y_continuous(limits = c(0 , 100))  +
                                     theme(axis.text.y = element_blank() , axis.ticks.y = element_blank() ,
                                           axis.title.y = element_blank() , axis.title.x = element_blank() ,  
                                           axis.line.y = element_blank()) + theme(panel.grid.major.y = element_blank() ,
                                                                                  axis.title.y = element_blank()) +
                                     ggtitle(paste0("Percent Cover by Functional Group: " , 
                                                    Species_plots_ecosite_attributed$GrowthHabitSub,
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
   Plots <- lapply(X = split(Species_plots_ecosite_attributed, Species_plots_ecosite_attributed[["GrowthHabitSub"]] , drop = TRUE), 
                             FUN = function(Species_plots_ecosite_attributed){
                             
                                 current_plot <- ggplot2::ggplot(Species_plots_ecosite_attributed , aes(x = GrowthHabitSub , y = AH_SpeciesCover)) +
                                 geom_boxplot(width = .6 , outlier.shape = NA) +
                                 geom_jitter(width = .2 , size = 2, aes(color = PAST_NAME, shape = Noxious)) +
                                 scale_color_manual(values = Attribute_Fill, na.value="#000000") +
                                 labs(y = "Percent Cover", 
                                 caption = paste("Percent cover in: ", toString(EcologicalSiteId))) +
                                 scale_y_continuous(limits = c(0 , 100))  +
                                 theme_light() + 
                                 theme(axis.text.y = element_blank() , axis.ticks.y = element_blank() ,
                                       axis.line.y = element_blank()) + theme(panel.grid.major.y = element_blank() ,
                                                                              axis.title.y = element_blank()) +
                                 ggtitle(paste("Percent Cover by Functional Group:", 
                                               Species_plots_ecosite_attributed$GrowthHabitSub,
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
     Plots <- Species_plots_ecosite_attributed %>% group_by(Noxious) %>% 
     filter(!is.na(Noxious)) %>% filter(!is.na(AH_SpeciesCover)) %>%
     ggplot2::ggplot((aes(x = Noxious , y = AH_SpeciesCover , 
                                             text = paste("Primary Key : " , PrimaryKey, 
                                                    "Plot ID: " , PlotID , 
                                                    "Species: " ,  ScientificName, 
                                                    "Code: " , Species, 
                                                    "Percent Cover: " , AH_SpeciesCover, 
                                                    "Noxious: " , Noxious , 
                                                    "Allotment: ", ALLOT_NAME, 
                                                     "Pasture: ", PAST_NAME,
                                                    sep = "<br>")))) +
           geom_boxplot(width = .6 , outlier.shape = NA) +
           geom_jitter(width = .15 , size = 2 , shape = 21) +
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
     Plots <- Species_plots_ecosite_attributed %>% group_by(Noxious) %>% 
       filter(!is.na(Noxious)) %>% filter(!is.na(AH_SpeciesCover)) %>%
               ggplot2::ggplot((aes(x = Noxious , y = AH_SpeciesCover))) +
               geom_boxplot(width = .6 , outlier.shape = NA) +
               geom_jitter(width = .15 , size = 2 , aes(color = PAST_NAME, shape = Noxious)) +
               scale_color_manual(values = Attribute_Fill, na.value="#000000") +
               theme_light() +
               scale_y_continuous(limits = c(0 , 100)) +
               labs(y = "Percent Cover") + 
               ggtitle(paste("Percent Cover, Noxious vs. Non-Noxious Species: " , 
                             toString(EcologicalSiteId))) +
                 theme(axis.title.y = element_blank() , axis.text.y = element_blank() , 
                       axis.ticks.y = element_blank() ,
                       axis.line.y = element_blank() , 
                       panel.grid.major.y = element_blank()) +
                 coord_flip() + 
                facet_grid(rows = vars(Noxious) ,
                           switch = "y" , scales = "free" , drop = TRUE)
    
   }}

if(SummaryVar == "Species"){
  PercentCover <- Species_plots_ecosite_attributed %>% subset(AH_SpeciesCover > 0.000000)
  if(Interactive){
  Plots <-lapply(X = split(PercentCover, list(PercentCover$GrowthHabitSub , PercentCover$Duration) , drop = TRUE),
                                     FUN = function(PercentCover){
                                       current_plot <- ggplot2::ggplot(PercentCover , aes(x = Species , y = AH_SpeciesCover,
                                                                                             text = paste("PrimaryKey: ", PrimaryKey , 
                                                                                                          "Plot ID: " , PlotID , "Species: " , 
                                                                                                          ScientificName , "Code: " , Species , 
                                                                                                          "Percent Cover: " , AH_SpeciesCover , "Noxious: " , 
                                                                                                          Noxious , 
                                                                                                          "Allotment: ", ALLOT_NAME, 
                                                                                                          "Pasture: ", PAST_NAME,
                                                                                                          sep = "<br>"))) +
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
                                       geom_jitter(width = .15 , size = 1 , aes(color = PAST_NAME, shape = Noxious)) +
                                       scale_color_manual(values = Attribute_Fill, na.value="#000000") + 
                                       scale_y_continuous(limits = c(0 , 100)) +
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
    
    Ground_Cover_Tall <- EcoSitePlots_Attributed %>% 
                        dplyr::select(PlotID, PrimaryKey, BareSoilCover , 
                                      TotalFoliarCover , FH_TotalLitterCover , 
                                      FH_RockCover, ALLOT_NAME, ALLOT_NO, PAST_NAME) %>%
                        gather(key = Indicator , value = Percent, 
                               BareSoilCover:PAST_NAME) %>% mutate(Tally = 1) 
    if(Interactive){
    
    Plots <- Ground_Cover_Tall %>% mutate_if(is.numeric , round , digits = 2) %>% 
              ggplot2::ggplot((aes(x = Indicator , y = Percent , 
                           text = paste("PlotID: " , PlotID , 
                           "PrimaryKey: " , PrimaryKey , 
                           "Indicator: " , Indicator ,
                           "Percent Cover: " , Percent , 
                           "Allotment: ", ALLOT_NAME, 
                            "Pasture: ", PAST_NAME,
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
                   geom_jitter(width = .15, aes(color = PAST_NAME, shape = Noxious)) +
                   theme_light() +
                   scale_color_manual(values = Attribute_Fill, na.value="#000000") +
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
  
  Gap <- EcoSitePlots_Attributed %>% dplyr::select(PlotID , PrimaryKey , 
                                        GapCover_25_50 , GapCover_51_100 , 
                                        GapCover_101_200 , GapCover_200_plus , 
                                        GapCover_25_plus, ALLOT_NAME, ALLOT_NO, PAST_NAME) %>% 
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
                                             "Allotment: ", ALLOT_NAME, 
                                            "Pasture: ", PAST_NAME,
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
                    geom_jitter(width = 0.1, aes(color = PAST_NAME)) +
                    theme_light() + 
                    scale_color_manual(values = Attribute_Fill, na.value="#000000") + 
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
  
  SoilStability <- EcoSitePlots_Attributed %>% dplyr::select(PlotID , PrimaryKey , 
                                                           SoilStability_All , 
                                             SoilStability_Protected , 
                                             SoilStability_Unprotected,
                                             ALLOT_NAME, 
                                             ALLOT_NO,
                                             PAST_NAME) %>%
                          gather(key = Veg , value = Rating , 
                          SoilStability_All:SoilStability_Unprotected) %>%
                          mutate_if(is.numeric, round, digits = 2) 
    
  if(Interactive){

    Plots <- ggplot2::ggplot(data = SoilStability , 
                    aes(x = Veg , y = Rating , 
                        text = paste("Primary Key: " , PrimaryKey,
                        "Plot ID: " , PlotID , 
                        "Rating: " , Rating , 
                        "Allotment: ", ALLOT_NAME, 
                        "Pasture: ", PAST_NAME,
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
                          geom_boxplot() + coord_flip() + geom_jitter(width = 0.1, aes(color = PAST_NAME)) +
                          theme_light() + 
                          scale_color_manual(values = Attribute_Fill, na.value="#000000") + 
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
