\name{SubsetEcologicalSite}
\alias{SubsetEcologicalSite}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
SubsetEcologicalSite
}
\description{
Subsets national AIM and LMF plots to those that fall within your specified ecological site(s)
}
\usage{
SubsetEcologicalSite(TDat_LMF, EcologicalSiteId)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{TDat_LMF}{
Combined TerrADat and LMF dataframe (returned object from Combine_AIM_LMF()
}
  \item{EcologicalSiteId}{
Full ecological site id, list of ecological sites, or object}
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
EcoSitePlots <- SubsetEcologicalSite(TDat_LMF, EcologicalSiteId)

## The function is currently defined as
function (TDat_LMF, EcologicalSiteId){

  EcoSitePlots <- TDat_LMF[TDat_LMF[["EcologicalSiteId"]] %in% EcologicalSiteId, ]
# Add sagebrush indicator
  EcoSitePlots <- EcoSitePlots %>%
                 mutate(AH_SagebrushCover_Dead = AH_SagebrushCover - AH_SagebrushCover_Live)

  return(EcoSitePlots)
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
