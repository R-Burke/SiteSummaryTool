AttributePlots <- function(TDat_LMF, 
                           shapefile_name, shapefile_path, 
                           attribute_title, attribute_name){
                 if(missing(attribute_name)){
                   #if attribute_name is included, will subset data to attribute 
                   #(i.e. 1 or multiple allotments)
                   attribute_name <- NA
  }

# Match coordinate reference systems for intersection
projection <- sf::st_crs("+proj=longlat +datum=NAD83")

# Join by PrimaryKey to get GPS coordinates for species indicators entries
# TDat_LMF is output of Combine_AIM_LMF
coordinates <- TDat_LMF %>% dplyr::select(PrimaryKey, Latitude_NAD83, Longitude_NAD83)

# Convert Terradat data into "simple feature" object class for spatial reference. Not removing any NA or entries without coordinates.
TDat_LMF_Spatial <- sf::st_as_sf(TDat_LMF, coords = c("Longitude_NAD83", "Latitude_NAD83"), na.fail = FALSE, remove = FALSE, crs = projection)

##-----------------------------------
# Load in shapefiles and intersect with data (include AND exclude data in sf)
##-----------------------------------

# Read in shapefiles

shapefile  <-  sf::st_read(dsn = shapefile_path, layer = shapefile_name)
shapefile <- sf::st_transform(shapefile, crs = projection)
#Simplify shapefile to just the attributes we want
shapefile <- shapefile %>% dplyr::select(all_of(attribute_title))

# Intersect shapefile with plots to get attributed
TDat_LMF_Attributed <- sf::st_intersection(TDat_LMF_Spatial, sf::st_make_valid(shapefile))
  
TDat_LMF_Attributed <- TDat_LMF_Attributed %>% select(PrimaryKey, all_of(attribute_title))

return(TDat_LMF_Attributed)

}
