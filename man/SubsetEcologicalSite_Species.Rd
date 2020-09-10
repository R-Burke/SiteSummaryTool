\name{SubsetEcologicalSite_Species}
\alias{SubsetEcologicalSite_Species}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{SubsetEcologicalSite_Species
}
\description{
Subsets national AIM and LMF species indicators by plot to those that fall within your specified ecological site(s)
}
\usage{
SubsetEcologicalSite_Species(EcoSitePlots, Species_Indicator)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{EcoSitePlots}{
Combined TerrADat and LMF dataframe, subset to your ecological site. Returned object from consecutive Combine_AIM_LMF and SubsetEcologicalSite functions. This is required to obtain a list of PrimaryKeys associated with each plot within the specified ecological site(s).
}
  \item{Species_Indicator}{
Combined AIM and LMF species indicator. Returned object from Combine_AIM_LMF_Species().
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (EcoSitePlots, Species_Indicator)
{
    EcoSite_PKs <- EcoSitePlots$PrimaryKey
    Species_plots_ecosite <- Species_Indicator[(Species_Indicator[["PrimaryKey"]] \%in\%
        EcoSite_PKs), ]
    return(Species_plots_ecosite)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
