#Install required packages
InstallPackages <- function(LibraryPath){
  
  .libPaths(LibraryPath)

  packages <- c("devtools", "tidyverse", "knitr", "DT", "leaflet", "RODBC",
                "sf", "rworldmap", "rworldxtra", "plotly", "htmltools",
                "ggplot2", "glue")

  package.check <- lapply(
    packages,
    FUN = function(x) {
      if (!require(x, character.only = TRUE)) {
        install.packages(x, dependencies = TRUE)
        library(x, character.only = TRUE)
      }
    }
  )

}

