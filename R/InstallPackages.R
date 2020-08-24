#Install required packages
InstallPackages <- function(LibraryPath){
  myPaths <- .libPaths()
  myPaths <- c(myPaths, LibraryPath)

  packages <- c("devtools", "tidyverse", "knitr", "DT", "leaflet", 
                "sf", "rworldmap", "rworldxtra", "plotly", "htmltools",
                "ggplot2")

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

