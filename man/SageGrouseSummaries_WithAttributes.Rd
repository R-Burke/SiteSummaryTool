\name{SageGrouseSummaries_WithAttributes}
\alias{SageGrouseSummaries_WithAttribues}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{SageGrouseSummaries_WithAttributes
%%  ~~function to do ... ~~
}
\description{
Returns boxplot of Sage-Grouse indicator percent cover within your specified ecological site(s), colored by allotment information. Select one Sage-Grouse indicator in the inputs. Will return NULL if your specified state does not contain Type I Sage-Grouse habitat. Currently hard-coded with BLM naming convention for allotments, will update to generalize.}
\usage{
SageGrouseSummaries(EcoSitePlots, Species_plots_ecosite, Interactive, SummaryVar)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{EcoSitePlots}{
Combined TerrADat and LMF dataframe, subset to your ecological site. Returned object from consecutive Combine_AIM_LMF() and SubsetEcologicalSite().
}
  \item{Species_plots_ecosite}{
Combined TerrADat and LMF species indicators, subset to your ecological site. Returned object from consecutive Combine_AIM_LMF_Species() and SubsetEcologicalSite_Species().
}
  \item{Interactive}{logical. If TRUE, boxplot aesthetics will be set up for interactive plotly::ggplotly object. If FALSE, will return static box plot.
}
  \item{SummaryVar}{
c("PreferredForb", "SageGrouseGroup", "Sagebrush")}
  \item{Attributed_Pks}{simplified dataframe with PrimaryKeys associated with each shapefile attribute}
 }
 
\details{
SummaryVar == "PreferredForb"" returns cover values grouped by individual species on preferred forb list.
SummaryVar == "SageGrouseGroup" returns cover values grouped by NonSagebrushShrub, PreferredForb, Sagebrush, ShortStaturePerennialGrass, and TallStaturePerennialGrass.
SummaryVar == "Sagebrush"" returns cover values group by individual sagebrush species.

To get Attributed_Pks-

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
# For interactive plot:
plotly::ggplotly(SageGrouseSummaries_WithAttributes(EcoSitePlots, Species_plots_ecosite,
                              SummaryVar = "SageGrouseGroup",
                              Interactive = TRUE, Attributed_Pks = Attributed_Pks), tooltip = "text")
# For static plot:
SageGrouseSummaries_WithAttributes(EcoSitePlots, Species_plots_ecosite,
                    SummaryVar = "SageGrouseGroup", Interactive = FALSE, Attributed_Pks = Attributed_Pks)


## The function is currently defined as
function(EcoSitePlots_Attributed, Species_plots_ecosite,
          Interactive, SummaryVar, Attributed_Pks){

  SageGrouseStates <- c("CO", "UT", "NV", "CA", "WY", "MT", "ID", "OR", "WA", "ND", "SD")

  #Prep
  SpeciesList <- SpeciesList %>% dplyr::select(Species, ScientificName, CommonName,
                                               Family, SpeciesState,
                                               SynonymOf, UpdatedSpeciesCode) %>%
    dplyr::mutate(link = paste("https://plants.sc.egov.usda.gov/core/profile?symbol=", Species, sep = ""))

#Merge with species list so we can hover for scientific name
Species_plots_ecosite_attributed <- merge(Species_plots_ecosite, Attributed_Pks, by = "PrimaryKey", all = TRUE) %>% 
                                    unique() 

EcoSitePlots_Attributed <- merge(EcoSitePlots, Attributed_Pks, by = "PrimaryKey", all = TRUE)

Attribute_Fill <- scales::seq_gradient_pal("#009966", "#E69F00", "Lab")(seq(0,1, length.out = length(unique(Species_plots_ecosite_attributed$PAST_NAME))))


  Species_plots_ecosite_attributed <- merge(Species_plots_ecosite_attributed , SpeciesList , by = c("Species" , "SpeciesState")) %>%
    dplyr::select(Species, ScientificName, CommonName, PrimaryKey,
                  PlotID,  AH_SpeciesCover,
                  AH_SpeciesCover_n, Hgt_Species_Avg,
                  Hgt_Species_Avg_n, GrowthHabit, GrowthHabitSub, Duration,
                  Noxious, SG_Group, link, ALLOT_NAME, ALLOT_NO, PAST_NAME) %>%
                  dplyr::mutate_if(is.numeric, round , digits = 2) %>%
                  filter(!is.na(SG_Group)) # only filter this for the sage-grouse plots

if(!EcoSitePlots_Attributed$State %in% SageGrouseStates){SG_Plots <- paste0("No type I Sage-Grouse Habitat in ", State)}

  else{

     if(SummaryVar == "PreferredForb"){
     PrefForb <- Species_plots_ecosite_attributed %>%
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
                                                           "Noxious: " , Noxious , 
                                                           "Allotment: ", ALLOT_NAME, 
                                                           "Pasture: ", PAST_NAME,
                                                            sep = "<br>"))) +
                                            geom_boxplot(width = .6 , outlier.shape = NA) +
                                            geom_jitter(width = .15 , size = 2 , shape = 21) +
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
                        geom_jitter(width = .15 , size = 2, aes(color = PAST_NAME, shape = Noxious)) +
                        scale_color_manual(values = Attribute_Fill, na.value="#000000") +
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
    SG_Plots <- ggplot(Species_plots_ecosite_attributed, aes(x = SG_Group , y = AH_SpeciesCover,
                                               text = paste("Primary Key: " , PrimaryKey ,
                                               "Plot ID: " , PlotID , "Species: " ,
                                                ScientificName , "Code: " , Species ,
                                                "Sage-Grouse Group: " , SG_Group ,
                                                "Percent Cover: " , AH_SpeciesCover ,
                                                "Noxious: " , Noxious , 
                                                "Allotment: ", ALLOT_NAME, 
                                                "Pasture: ", PAST_NAME,
                                                 sep = "<br>"))) +
                    geom_boxplot(width = .6 , outlier.shape = NA) +
                    geom_jitter(width = .15 , size = 2 , shape = 21) +
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

    SG_Plots <- ggplot(Species_plots_ecosite_attributed , aes(x = SG_Group , y = AH_SpeciesCover)) +
                            geom_boxplot(width = .6 , outlier.shape = NA) +
                            geom_jitter(width = .15 , size = 2, aes(color = PAST_NAME, shape = Noxious)) +
                            scale_y_continuous(limits = c(0 , 100)) +
                            theme_light() +
                            labs(y = "Percent Cover") +
                            scale_color_manual(values = Attribute_Fill, na.value="#000000") +
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

SageBrushCover <- EcoSitePlots_Attributed %>% dplyr::select(PlotID , PrimaryKey ,
                                                 AH_SagebrushCover ,
                                                 AH_SagebrushCover_Live ,
                                                 AH_SagebrushCover_Dead, 
                                                 ALLOT_NAME,
                                                 ALLOT_NO, 
                                                 PAST_NAME) %>%
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
                                                    "Allotment: ", ALLOT_NAME, 
                                                    "Pasture: ", PAST_NAME,
                                                    sep = "<br>"))) +
                             geom_boxplot(width = .6 , outlier.shape = NA) +
                             geom_jitter(width = .15 , size = 2 , shape = 21) +
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
                    geom_jitter(width = .15 , size = 2, aes(color = PAST_NAME)) +
                    scale_y_continuous(limits = c(0 , 100)) +
                    theme_light() +
                    scale_color_manual(values = Attribute_Fill, na.value="#000000") +
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
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
