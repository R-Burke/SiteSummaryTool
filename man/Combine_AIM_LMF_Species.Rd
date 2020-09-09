\name{Combine_AIM_LMF_Species}
\alias{Combine_AIM_LMF_Species}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Combine_AIM_LMF_Species(TerrADat_Path, Internal)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{TerrADat_Path}{
Path to TerrADat geodatabase. Set to NULL if on Bureau of Land Management network/vpn. 
}
  \item{Internal}{logical. TRUE if within BLM network/on vpn. FALSE if accessing TerrADat via external geodatabase.
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
# If accessing TerrADat via external geodatabase:
Species_Indicator <- Combine_AIM_LMF_Species(TerrADat_Path = 
                     "TerrADat_Path"~/gdbs/TerrADat.gdb"", Internal = FALSE)
          
# If accessing TerrADat via BLM network:
Species_Indicator <- Combine_AIM_LMF_Species(TerrADat_Path = NULL, Internal = TRUE)
          
## The function is currently defined as
function (TerrADat_Path, Internal) 
{
    if (!Internal) {
        TerrADatSpecies <- sf::st_read(dsn = TerrADat_Path, layer = "TerrADatSpeciesIndicators")
        LMFSpecies <- sf::st_read(dsn = TerrADat_Path, layer = "LMFSpeciesIndicators")
        TerrADatSpecies <- as.data.frame(TerrADatSpecies)
        TerrADatSpecies <- dplyr::select(TerrADatSpecies, -Shape)
        LMFSpecies <- as.data.frame(LMFSpecies)
        LMFSpecies <- dplyr::select(LMFSpecies, -Shape)
    }
    if (Internal) {
        TerrADatSpecies <- TerrADatSpecies
        LMFSpecies <- LMFSpecies
        SpeciesList <- SpeciesList
    }
    LMFSpecies[setdiff(names(TerrADatSpecies), names(LMFSpecies))] <- NA
    TerrADatSpecies[setdiff(names(LMFSpecies), names(TerrADatSpecies))] <- NA
    output <- rbind(TerrADatSpecies, LMFSpecies)
    return(output)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
