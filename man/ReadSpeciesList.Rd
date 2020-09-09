\name{ReadSpeciesList}
\alias{ReadSpeciesList}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ReadSpeciesList
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ReadSpeciesList(TerrADat_Path, Internal)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{TerrADat_Path}{
file path to the TerrADat geodatabase. Set to NULL if you are internal (within Bureau of Land Management network/vpn)
}
  \item{Internal}{
logical. TRUE if within BLM network/on vpn. FALSE if accessing TerrADat via external geodatabase.
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
#If working on external geodatabase:
SpeciesList <- ReadSpeciesList(TerrADat_Path = "~/gdbs/TerrADat.gdb", Internal = FALSE)
#If working within BLM network:
SpeciesList <- ReadSpeciesList(TerrADat_Path = NULL, Internal = TRUE)

## The function is currently defined as
function (TerrADat_Path, Internal){
  if(!Internal){
    SpeciesList<- sf::st_read(dsn = TerrADat_Path ,
                              layer = "tblStateSpecies")}
  if(Internal){
    SpeciesList <- SpeciesList
  }

  SpeciesList <- SpeciesList %>% dplyr::rename(Species = SpeciesCode)
  return(SpeciesList)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
