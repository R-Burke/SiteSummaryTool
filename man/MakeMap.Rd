\name{MakeMap}
\alias{MakeMap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
MakeMap
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
MakeMap(EcologicalSiteId, TDat_LMF, EcoSitePlots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{EcologicalSiteId}{Full ecological site id, list of ecological sites, or object

}
  \item{TDat_LMF}{
Combined TerrADat and LMF dataframe (returned object from Combine_AIM_LMF function)
}
  \item{EcoSitePlots}{
Combined TerrADat and LMF dataframe, subset to your ecological site. Returned object from consecutive Combine_AIM_LMF and SubsetEcologicalSite functions.
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
EcologicalSiteId <- c("R042XB012NM", "R042XB015NM")
MakeMap(EcologicalSiteId = EcologicalSiteId, TDat_LMF = TDat_LMF, EcoSitePlots = EcoSitePlots)

## The function is currently defined as

function(EcologicalSiteId, TDat_LMF, EcoSitePlots){
  ##Caption to use in your tables and plots
  Caption <- paste0("Cover Summaries for Ecological Site" , EcologicalSiteId)

  #Clean up
  TDat_LMF <- TDat_LMF %>%
        dplyr::arrange(EcologicalSiteId) %>%
            dplyr::filter(Latitude_NAD83 > 0)

  # List of ecological sites for legend
  EcoSiteList <- unique(TDat_LMF$EcologicalSiteId)

  #Set color palettes
  Pal_EcoSite <- leaflet::colorFactor(palette = 'YlOrRd' , domain = TDat_LMF$es_name)
  Pal_Date <- leaflet::colorFactor(palette = 'Greys' , domain = TDat_LMF$Year)
  Pal_EcoSiteID <- leaflet::colorFactor(palette = "Red" , domain = EcologicalSiteId)
  EcoSite_All <- TDat_LMF$es_name
  Year <- TDat_LMF$Year

  Map <- leaflet::leaflet(height = 650 , width = 650)

  #Convert vector to string to use in caption
  EcoSiteCaption <- toString(EcologicalSiteId)

  Map <- leaflet::addTiles(Map) %>% leaflet::addCircleMarkers(lng = ~Longitude_NAD83 , lat = ~Latitude_NAD83 , radius = 3 ,
                                            popup = paste("Ecological Site: " , TDat_LMF$es_name,
                                                          "Ecolgical Site Id: " , TDat_LMF$EcologicalSiteId,
                                                          sep = "<br>") ,
                                            color = ~Pal_Date(Year) ,
                                            fillOpacity = .5 , group = Year ,
                                            data = TDat_LMF) %>%
    leaflet::addCircleMarkers(lng = ~Longitude_NAD83 , lat = ~Latitude_NAD83 ,
                     radius = 3 ,
                     fillOpacity = 0.5 ,
                     popup = paste("Ecological Site: " ,EcoSitePlots$es_name ,
                                   "Ecological Site Id: " , EcoSitePlots$EcologicalSiteId ,
                                   sep = "<br>") ,
                     color = "red" , group = EcologicalSiteId ,
                     data = EcoSitePlots) %>%
    leaflet::addLayersControl(overlayGroups = c(EcologicalSiteId , Year) ,
                     options = leaflet::layersControlOptions(collapsed = TRUE)) %>%
    leaflet::addLegend(pal = Pal_Date , values = TDat_LMF$Year , opacity = 1 , group = Year) %>%
    leaflet::addLegend(pal = Pal_EcoSiteID , values = EcologicalSiteId , opacity = 1 , group = EcologicalSiteId)

  return(Map)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
