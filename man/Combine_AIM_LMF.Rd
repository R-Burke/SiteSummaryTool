\name{Combine_AIM_LMF}
\alias{Combine_AIM_LMF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Combine_AIM_LMF
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
TDat_LMF <- Combine_AIM_LMF(TerrADat_Path = "~/gdbs/TerrADat.gdb",
                   EDIT_List_Path = "~/Projects/ESSTool/Defaults",
                   Internal = FALSE)
# If internal:
TDat_LMF <- Combine_AIM_LMF(TerrADat_Path = NULL,
                   EDIT_List_Path = "~/Projects/ESSTool/Defaults",
                   Internal = TRUE)

## The function is currently defined as
function (TerrADat_Path, EDIT_List_Path, Internal)
{
    if (!Internal) {
        TerrADat <- sf::st_read(dsn = TerrADat_Path, layer = "TerrADat")
        LMF <- sf::st_read(dsn = TerrADat_Path, layer = "LMF")
        TerrADat <- as.data.frame(TerrADat)
        TerrADat <- dplyr::select(TerrADat, -Shape)
        LMF <- as.data.frame(LMF)
        LMF <- dplyr::select(LMF, -Shape)
    }
    if (Internal) {
        TerrADat <- TerrADat
        LMF <- LMF
    }
    EDIT <- read.csv(file = paste0(EDIT_List_Path, "/", "EDIT_public_ecological_site_list.csv"))
    EDIT[["EcoSiteId_Stripped"]] <- gsub(EDIT[["new_es_symbol"]],
        pattern = "^[RF]", replacement = "")
    ecosite_lut <- unique(EDIT[, c("new_es_symbol", "EcoSiteId_Stripped")])
    any(table(ecosite_lut[["EcoSiteId_Stripped"]]) > 1)
    trouble_ids <- names(table(ecosite_lut[["EcoSiteId_Stripped"]]))[table(ecosite_lut[["EcoSiteId_Stripped"]]) >
        1]
    ecosite_lut_drop_bad <- dplyr::filter(ecosite_lut, !EcoSiteId_Stripped ==
        trouble_ids)
    EcoSites_Update <- dplyr::mutate(ecosite_lut_drop_bad, EcologicalSiteId = EcoSiteId_Stripped)
    LMF_EcoSite <- merge(LMF, EcoSites_Update, by = "EcologicalSiteId")
    LMF_EcoSite <- dplyr::select(LMF_EcoSite, -EcologicalSiteId)
    LMF_EcoSite <- dplyr::rename(LMF_EcoSite, EcologicalSiteId = new_es_symbol)
    LMF_EcoSite <- dplyr::select(LMF_EcoSite, -EcoSiteId_Stripped)
    TerrADat[setdiff(names(LMF_EcoSite), names(TerrADat))] <- NA
    LMF_EcoSite[setdiff(names(TerrADat), names(LMF_EcoSite))] <- NA
    TerrADat[setdiff(names(LMF_EcoSite), names(TerrADat))] <- NA
    LMF_EcoSite[setdiff(names(TerrADat), names(LMF_EcoSite))] <- NA
    TDat_LMF <- rbind(TerrADat, LMF_EcoSite)
    TDat_LMF$DateVisited <- sub("^(\\d{4}).*$", "\\1", TDat_LMF$DateVisited)
    TDat_LMF <- TDat_LMF \%>\% dplyr::rename(Year = DateVisited)
    EDIT_rename <- EDIT \%>\% rename(EcologicalSiteId = new_es_symbol)
    output <- merge(TDat_LMF, EDIT_rename, by = "EcologicalSiteId",
        all = TRUE)
    output <- output \%>\% dplyr::select(-EcoSiteId_Stripped)
    return(output)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
