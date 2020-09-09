\name{PlotMetadata}
\alias{PlotMetadata}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{PlotMetadata
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PlotMetadata(dataframe)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataframe}{
  = TDat_LMF or EcoSitePlots
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
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
# To plot number of plots per year nationally:
PlotMetadata(dataframe = TDat_LMF)
# TDat_LMF is returned object from Combine_AIM_LMF()
# To plot number of plots per year in the your ecological site(s):
PlotMetadata(dataframe = EcoSitePlots)
# EcoSitePlots is returned object from Combine_AIM_LMF() and SubsetEcologicalSite()

## The function is currently defined as
function (dataframe)
{
    Plots <- c("Plots")
    Year_Palette <- (scales::seq_gradient_pal("grey85", "grey18",
        "Lab"))(seq(0, 1, length.out = length(unique(dataframe$Year))))
    Plots_Simple <- dataframe \%>\% dplyr::select(PrimaryKey, Year) \%>\%
        dplyr::mutate(PlotsPerYear = Plots) \%>\% dplyr::arrange(Year)
    PlotsPerYear <- ggplot(Plots_Simple, aes(Plots, text = stat(count))) +
        geom_bar(stat = "count", position = position_stack(reverse = TRUE),
            aes(fill = Year), width = 0.2, ) + scale_fill_manual(values = Year_Palette) +
        ggtitle("Plots Per Year") + coord_flip() + theme(axis.text.y = element_blank())
    PlotsPerYear <- plotly::ggplotly(PlotsPerYear, tooltip = "text")
    return(PlotsPerYear)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
