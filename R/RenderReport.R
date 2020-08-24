
EcologicalSiteId <- c("R023XY514OR")
TerrADat_Path <- "C:/Users/rburke/Documents/GDBs/TerrADat.gdb"
EDIT_List_Path <- "defaults/"
#Set the path to your R library (make sure it's on local folder not network drive)
LibraryPath <- "C:/Users/rburke/Documents/R/win-library/3.6"
BLM <- FALSE
State <- "OR"
rmarkdown::render("defaults/ESS_ExternalUsers.Rmd",
                  output_file = paste("C:/Users/rburke/Documents/Projects/ESS_Tool/",
                                        EcologicalSiteId, "_", Sys.Date()))
