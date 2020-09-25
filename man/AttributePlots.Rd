\name{AttributePlots}
\alias{AttributePlots}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{AttributePlots
}
\description{
Attributes TDat_LMF with spatial attribute (i.e. allotment, watershed unit, etc)
}
\usage{
AttributePlots(TDat_LMF, shapefile_name, shapefile_path, 
               attribute_title, attribute_name)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{TDat_LMF}{
Combined TerrADat and LMF dataframe. Returned from Combine_AIM_LMF()
}
  \item{shapefile_name}{name of the shapefile you are intersecting with the dataframe (either layer name if in a .gdb or shapefile name if in folder (no extension))
}
  \item{shapefile_path}{location of your shapefile. Full .gdb extension if shapefile is a layer in a geodatabase. 
}
  \item{attribute_title}{name or names of the attribute in the spaefile that you want added as field in TDat_LMF (i.e. "Allot_name", or c("Allot_name", "Allot_no", "Pasture_name")
 }
  \item{attribute_name}{optional. If included, will subset all data by this feature (i.e. the name of a specific allotment)
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
TDat_LMF_Attributed <- AttributePlots(TDat_LMF,  shapefile_name, shapefile_path, 
                           attribute_title, attribute_name)


## The function is currently defined as
function (TDat_LMF,  shapefile_name, shapefile_path, attribute_title, attribute_name)
{
 
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
