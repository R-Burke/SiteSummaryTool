#Install required packages
InstallPackages <- function(LibraryPath){
  myPaths <- .libPaths()
  myPaths <- c(myPaths, LibraryPath)

  packages <- c("devtools", "tidyverse", "knitr", "DT",
                "sf", "rworldmap", "rworldxtra", "plotly", "htmltools",
                "http://cran.r-project.org/src/contrib/Archive/ggplot2/ggplot2_3.2.1.tar.gz")

  package.check <- lapply(
    packages,
    FUN = function(x) {
      if (!require(x, character.only = TRUE)) {
        install.packages(x, dependencies = TRUE, type = "source")
        library(x, character.only = TRUE)
      }
    }
  )

}

