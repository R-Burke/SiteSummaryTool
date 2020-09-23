\name{StateKey}
\alias{StateKey}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
StateKey
}
\description{
Keys individual plots to ecological state based on quantitative key. Returns a dataframe with ecological states attributed to each plot.
}
\usage{
StateKey(TerrADat_Path, Tall_Table_Path,
          StateKeyPath, StateKeyName, EDIT_List_Path,
          EcoSiteName, EcologicalSiteId, State, Degraded)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
   \item{TerrADat_Path}{file path to the TerrADat geodatabase. Set to NULL if you are internal (within Bureau of Land Management network/vpn)
}
  \item{Tall_Table_Path}{file path to R tall tables containing AIM&LMF data. These are necessary to calculate custom indicators.}
  \item{StateKeyPath}{file path to the Excel workbook that contains the general quantitative key and historic plant indicators.}
  \item{StateKeyName}{Name of the Excel workbook containing quantiative key and historic indicators.}
  \item{EcoSiteName}{General ecological site name, i.e. Sandy, Gravelly, etc.}
  \item{EcologicalSiteId}{Full ecological site id code}
  \item{EDIT_List_Path}{file path to the .csv with list of formal ecological site ids (on the sharepoint with Site Summary Tool template)
}
  \item{Internal}{logical.TRUE if within BLM network/on vpn. FALSE if accessing TerrADat via external geodatabase.}
  \item{State}{2 letter state abbreviation (political/administrative state)}
  \item{Degraded}{logical. Degraded = FALSE will err on less degraded ecological state if there is a tie between 2 states. Degraded = TRUE will err on more degraded ecological state if there is a tie between 2 states.
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x)
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
