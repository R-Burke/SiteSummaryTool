\name{Combine_AIM_LMF}
\alias{Combine_AIM_LMF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Combine_AIM_LMF
%%  ~~function to do ... ~~
}
\description{
Puts TerrADat (Assessment, Inventory, and Monitoring dataset) and LMF (Landscape Monitoring Framework dataset) in the same format, adds R or F prefix to EcologicalSiteId for LMF based on the official ecological site list from EDIT, and merges the two datasets into one. Returns dataframe.
}
\usage{
Combine_AIM_LMF(TerrADat_Path, EDIT_List_Path, Internal)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{TerrADat_Path}{file path to the TerrADat geodatabase. Set to NULL if you are internal (within Bureau of Land Management network/vpn)
}
  \item{EDIT_List_Path}{file path to the .csv with list of formal ecological site ids (on the sharepoint with Site Summary Tool template)
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
# If external:
Combine_AIM_LMF(TerrADat_Path = "~/gdbs/TerrADat.gdb",
                   EDIT_List_Path = "~/Projects/ESSTool/Defaults",
                   Internal = FALSE)
# If internal:
Combine_AIM_LMF(TerrADat_Path = NULL,
                   EDIT_List_Path = "~/Projects/ESSTool/Defaults",
                   Internal = TRUE)

