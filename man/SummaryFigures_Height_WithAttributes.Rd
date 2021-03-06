\name{SummaryFigures_Height_WithAttributes}
\alias{SummaryFigures_Height_WithAttributes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
SummaryFigures_Height_WithAttributes
}
\description{
Returns a static or interactive (plotly-friendly) ggplot boxplot summarizing species heights across your ecological site(s) based on summary variables (Growth habit, individual species, or sagebrush), colored by attributed. Hard-coded for BLM grazing allotment naming convention. Will update to generalize for spatial attributes.
}
\usage{
SummaryFigures_Height(Species_plots_ecosite, EcologicalSiteId, SummaryVar, GroupBy, Interactive, Attributed_Pks)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Species_plots_ecosite}{
Combined AIM and LMF species indicator, subset to your ecological site(s). Returned output from Combine_AIM_LMF_Species and SubsetEcologicalSite_Species.}
  \item{EcologicalSiteId}{
Full ecological site id, list of ecological sites, or object
}
  \item{SummaryVar}{
"Height""}
  \item{GroupBy}{
c("Species", "GrowthHabit", "Sagebrush")
}
  \item{Interactive}{
logical. If TRUE, boxplot aesthetics will be set up for interactive plotly::ggplotly object. If FALSE, will return static box plot.
}
\item{Attributed_Pks}{simplified dataframe with PrimaryKeys associated with each shapefile attribute}
}
\details{
If GroupBy == "Species", returns a list of plots that must be called on- boxplots are broken up by GrowthHabitSub, with individual species within each group as faceting row. Y values are height in cm. See example for how to call on plot.
If GroupBy == "GrowthHabit", returns a boxplot. X values are growth habit, Y values are height in cm.
If GroupBy == "Sagebrush", returns a boxplot. X values are sagebrush species, Y values are height in cm.
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
Rachel Burke, ecologist/analyst @ Jornada
}
\note{
%%  ~~further notes~~
}
%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# If SummaryVar == "Height", GroupBy == "GrowthHabit", Interactive == TRUE:
plotly::ggplotly(SummaryFigures_Height(Species_plots_ecosite,
                  EcologicalSiteId, SummaryVar = "Height",
                  GroupBy = "GrowthHabit", Interactive = TRUE),
                  tooltip = "text")
# If SummaryVar == "Height", GroupBy == "GrowthHabit", Interactive == FALSE:
SummaryFigures_Height(Species_plots_ecosite, EcologicalSiteId,
                      SummaryVar = "Height", GroupBy = "GrowthHabit",
                      Interactive = FALSE)
# If SummaryVar == "Height", GroupBy == "Species", Interactive == TRUE:
Plots <- SummaryFigures_Height(Species_plots_ecosite,
                               EcologicalSiteId,
                               SummaryVar = "Height",
                               GroupBy = "Species",
                               Interactive = TRUE, 
                               Attributed_Pks = Attributed_Pks)
plotly::ggplotly((Plots[["Graminoid.Annual"]]), tooltip = "text")
# If SummaryVar == "Height", GroupBy == "Species", Interactive == FALSE:
Plots <- SummaryFigures_Height(Species_plots_ecosite,
                               EcologicalSiteId,
                               SummaryVar = "Height",
                               GroupBy = "Species",
                               Interactive = FALSE,
                                Attributed_Pks = Attributed_Pks)
Plots[["Graminoid.Annual"]]
## The function is currently defined as
function(Species_plots_ecosite, EcologicalSiteId, 
                           SummaryVar, GroupBy, Interactive, Attributed_Pks){


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


##Setting color palette for plot
  

NoxNonPal_Fill <- c("grey75"  , "#D55E00")
NoxNonPal_Dot <- c("grey33" , "#993300")
## FIgure out how to not hardcode ALLOT_NAME and instead use attribute_title
Attribute_Fill <- scales::seq_gradient_pal("#009966", "#E69F00", "Lab")(seq(0,1, length.out = length(unique(Species_plots_ecosite_attributed$PAST_NAME))))


# Prep for height
HgtPrep <- Species_plots_ecosite_attributed %>% filter(!is.na(Hgt_Species_Avg)) %>% 
             filter(!is.na(GrowthHabitSub)) %>% filter(Hgt_Species_Avg > 0.0000) 
  
 
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
                                                             "Average Height (cm): "  , Hgt_Species_Avg , 
                                                             "Average Height, n: " , Hgt_Species_Avg_n ,
                                                             "Allotment: ", ALLOT_NAME,
                                                             "Pasture: ", PAST_NAME, 
                                                              sep = "<br>"))) +
                                          geom_boxplot(width = .6 , outlier.shape = NA) +
                                          geom_jitter(width = .15 , shape = 21) +
                                          # scale_y_continuous(limits = c(0 , 100)) +
                                          theme_light() +
                                          coord_flip() + 
                                          theme(axis.text.y = element_blank() , axis.ticks.y = element_blank() ,
                                                axis.line.y = element_blank(), axis.title.x = element_blank())  +
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
                                           geom_jitter(width = .15 , size = 2 , aes(color = PAST_NAME, shape = Noxious)) +
                                           scale_color_manual(values = Attribute_Fill, na.value="#000000") + 
                                         # scale_y_continuous(limits = c(0 , 100)) +
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
                                       "Allotment: ", ALLOT_NAME, 
                                       'Pasture: ', PAST_NAME,
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
                                    (aes(color = PAST_NAME, shape = Noxious))) +
                         labs(x = "Growth Habit" , 
                              y =  "Average Height, cm",
                              caption = paste("Species height in: ", 
                                              toString(EcologicalSiteId), sep = "")) + 
                         theme_light() +
                         scale_color_manual(values = Attribute_Fill, na.value="#000000") +
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
    Sagebrush <- HgtPrep %>% subset(SG_Group == "Sagebrush") 
    if(nrow(Sagebrush) < 1){Plots <- NULL}
  else{
  if(Interactive){
        Plots <- ggplot(Sagebrush, aes(x = Species , y = Hgt_Species_Avg , 
                       text = paste("Plot Id: " , PlotID , 
                       "PrimaryKey: " , PrimaryKey ,
                       "Species: " , Species , 
                       "Average Height (cm): "  , Hgt_Species_Avg , 
                       "Average Height , n: " , Hgt_Species_Avg_n ,
                       "Allotment: ", ALLOT_NAME,
                       "Pasture: ", PAST_NAME,
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

if(!Interactive){
  
  Plots <-  ggplot(Sagebrush, aes(x = Species , y = Hgt_Species_Avg)) +
           geom_boxplot() +
           geom_jitter(width = .1 , aes(color = PAST_NAME, shape = Noxious)) + theme_light() +
           theme(axis.ticks.y = element_blank() ,
           axis.line.y = element_blank()) +  
           scale_color_manual(values = Attribute_Fill, na.value="#000000") +
           labs(x = "Species" , 
                y = "Average Height, cm",
                caption = paste("Species height in: ", 
                                toString(EcologicalSiteId), sep = "")) + 
           coord_flip() 
}
}
}
  return(Plots)
  
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
