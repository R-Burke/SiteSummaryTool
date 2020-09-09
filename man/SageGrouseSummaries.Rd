\name{SageGrouseSummaries}
\alias{SageGrouseSummaries}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{SageGrouseSummaries
%%  ~~function to do ... ~~
}
\description{
Returns boxplot of Sage-Grouse indicator percent cover within your specified ecological site(s). Select one Sage-Grouse indicator in the inputs. Will return NULL if your specified state does not contain Type I Sage-Grouse habitat}
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
c("PreferredForb", "SageGrouseGroup", "Sagebrush")

}

}
\details{
SummaryVar == "PreferredForb"" returns cover values grouped by individual species on preferred forb list.
SummaryVar == "SageGrouseGroup" returns cover values grouped by NonSagebrushShrub, PreferredForb, Sagebrush, ShortStaturePerennialGrass, and TallStaturePerennialGrass.
SummaryVar == "Sagebrush"" returns cover values group by individual sagebrush species.
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
plotly::ggplotly(SageGrouseSummaries(EcoSitePlots, Species_plots_ecosite,
                              SummaryVar = "SageGrouseGroup",
                              Interactive = TRUE), tooltip = "text")
# For static plot:
SageGrouseSummaries(EcoSitePlots, Species_plots_ecosite,
                    SummaryVar = "SageGrouseGroup", Interactive = FALSE)


## The function is currently defined as
function (EcoSitePlots, Species_plots_ecosite, Interactive, SummaryVar)
{
    SageGrouseStates <- c("CO", "UT", "NV", "CA", "WY", "MT",
        "ID", "OR", "WA", "ND", "SD")
    SpeciesList <- SpeciesList \%>\% dplyr::select(Species, ScientificName,
        CommonName, Family, SpeciesState, SynonymOf, UpdatedSpeciesCode) \%>\%
        dplyr::mutate(link = paste("https://plants.sc.egov.usda.gov/core/profile?symbol=",
            Species, sep = ""))
    Species_plots_ecosite <- merge(Species_plots_ecosite, SpeciesList,
        by = c("Species", "SpeciesState")) \%>\% dplyr::select(Species,
        ScientificName, CommonName, PrimaryKey, PlotID, AH_SpeciesCover,
        AH_SpeciesCover_n, Hgt_Species_Avg, Hgt_Species_Avg_n,
        GrowthHabit, GrowthHabitSub, Duration, Noxious, SG_Group,
        link) \%>\% dplyr::mutate_if(is.numeric, round, digits = 2) \%>\%
        filter(!is.na(SG_Group))
    if (!EcoSitePlots$State \%in\% SageGrouseStates) {
        SG_Plots <- paste0("No type I Sage-Grouse Habitat in ",
            State)
    }
    else {
        if (SummaryVar == "PreferredForb") {
            PrefForb <- Species_plots_ecosite \%>\% filter(!is.na(SG_Group)) \%>\%
                mutate(PreferredForb = (SG_Group == "PreferredForb")) \%>\%
                subset(PreferredForb == TRUE) \%>\% subset(AH_SpeciesCover >
                0)
            if (Interactive) {
                SG_Plots <- if (nrow(PrefForb) < 1) {
                  SG_Plots <- NULL
                }
                else {
                  ggplot(PrefForb, aes(x = Species, y = AH_SpeciesCover,
                    text = paste("Primary Key: ", PrimaryKey,
                      "Plot ID: ", PlotID, "Species: ", ScientificName,
                      "Code: ", Species, "Sage-Grouse Group: ",
                      SG_Group, "Duration: ", Duration, "Percent Cover: ",
                      AH_SpeciesCover, "Noxious: ", Noxious,
                      sep = "<br>"))) + geom_boxplot(width = 0.6,
                    outlier.shape = NA) + geom_jitter(width = 0.15,
                    size = 1, shape = 21) + scale_y_continuous(limits = c(0,
                    100)) + theme_light() + labs(y = "Percent Cover") +
                    ggtitle(paste("Percent Cover, Preferred Forbs: ",
                      toString(EcologicalSiteId))) + theme(axis.title.y = element_blank(),
                    axis.title.x = element_blank(), axis.text.y = element_blank(),
                    axis.ticks.y = element_blank(), panel.grid.major.y = element_blank()) +
                    facet_grid(rows = vars(Species), drop = TRUE,
                      scales = "free") + coord_flip()
                }
            }
            if (!Interactive) {
                SG_Plots <- if (nrow(PrefForb) < 1) {
                  SG_Plots <- NULL
                }
                else {
                  ggplot(PrefForb, aes(x = Species, y = AH_SpeciesCover)) +
                    geom_boxplot(width = 0.6, outlier.shape = NA) +
                    geom_jitter(width = 0.15, size = 1) + scale_y_continuous(limits = c(0,
                    100)) + theme_light() + labs(y = "Percent Cover") +
                    ggtitle(paste("Percent Cover, Preferred Forbs: ",
                      toString(EcologicalSiteId))) + theme(panel.grid.major.y = element_blank()) +
                    coord_flip()
                }
            }
            return(SG_Plots)
        }
        if (SummaryVar == "SageGrouseGroup") {
            if (Interactive) {
                SG_Plots <- ggplot(Species_plots_ecosite, aes(x = SG_Group,
                  y = AH_SpeciesCover, text = paste("Primary Key: ",
                    PrimaryKey, "Plot ID: ", PlotID, "Species: ",
                    ScientificName, "Code: ", Species, "Sage-Grouse Group: ",
                    SG_Group, "Percent Cover: ", AH_SpeciesCover,
                    "Noxious: ", Noxious, sep = "<br>"))) + geom_boxplot(width = 0.6,
                  outlier.shape = NA) + geom_jitter(width = 0.15,
                  size = 1, shape = 21) + scale_y_continuous(limits = c(0,
                  100)) + theme_light() + ggtitle(paste("Percent Cover, Sage-Grouse Group: ",
                  toString(EcologicalSiteId))) + theme(axis.title.y = element_blank(),
                  axis.title.x = element_blank(), axis.text.y = element_blank(),
                  axis.ticks.y = element_blank(), panel.grid.major.y = element_blank()) +
                  facet_grid(rows = vars(SG_Group), drop = TRUE,
                    scales = "free") + coord_flip()
            }
            if (!Interactive) {
                SG_Plots <- ggplot(Species_plots_ecosite, aes(x = SG_Group,
                  y = AH_SpeciesCover)) + geom_boxplot(width = 0.6,
                  outlier.shape = NA) + geom_jitter(width = 0.15,
                  size = 1) + scale_y_continuous(limits = c(0,
                  100)) + theme_light() + labs(y = "Percent Cover") +
                  ggtitle(paste("Percent Cover, Sage-Grouse Group: ",
                    toString(EcologicalSiteId))) + theme(panel.grid.major.y = element_blank(),
                  axis.text.y = element_blank(), axis.title.y = element_blank()) +
                  facet_grid(rows = vars(SG_Group), switch = "y",
                    scales = "free", drop = TRUE) + coord_flip()
            }
            return(SG_Plots)
        }
    }
    if (SummaryVar == "Sagebrush") {
        SageBrushCover <- EcoSitePlots \%>\% dplyr::select(PlotID,
            PrimaryKey, AH_SagebrushCover, AH_SagebrushCover_Live,
            AH_SagebrushCover_Dead) \%>\% mutate_if(is.numeric,
            round, digits = 2) \%>\% gather(key = SageBrushCover,
            value = Percent, AH_SagebrushCover:AH_SagebrushCover_Dead)
        if (nrow(SageBrushCover) < 1) {
            SG_Plots <- NULL
        }
        else {
            if (Interactive) {
                SG_Plots <- ggplot(SageBrushCover, aes(x = SageBrushCover,
                  y = Percent, text = paste("Primary Key: ",
                    PrimaryKey, "Plot ID: ", PlotID, "Percent Cover: ",
                    Percent, sep = "<br>"))) + geom_boxplot(width = 0.6,
                  outlier.shape = NA) + geom_jitter(width = 0.15,
                  size = 1, shape = 21) + scale_y_continuous(limits = c(0,
                  100)) + theme_light() + labs(y = "Percent Cover") +
                  ggtitle(paste("Percent Cover, Sagebrush: ",
                    toString(EcologicalSiteId))) + theme(axis.title.y = element_blank(),
                  axis.text.y = element_blank(), axis.title.x = element_blank(),
                  axis.ticks.y = element_blank(), panel.grid.major.y = element_blank()) +
                  facet_grid(rows = vars(SageBrushCover), scales = "free",
                    drop = TRUE) + coord_flip()
            }
            if (!Interactive) {
                SG_Plots <- ggplot(SageBrushCover, aes(x = SageBrushCover,
                  y = Percent)) + geom_boxplot(width = 0.6, outlier.shape = NA) +
                  geom_jitter(width = 0.15, size = 1) + scale_y_continuous(limits = c(0,
                  100)) + theme_light() + labs(y = "Percent Cover",
                  x = "Sagebrush cover") + ggtitle(paste("Percent Cover, Sagebrush: ",
                  toString(EcologicalSiteId))) + theme(panel.grid.major.y = element_blank(),
                  axis.text.y = element_blank()) + facet_grid(rows = vars(SageBrushCover),
                  drop = TRUE, scales = "free") + coord_flip()
            }
            return(SG_Plots)
        }
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
