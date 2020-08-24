MakeMap <- function(EcologicalSiteId, TDat_LMF, EcoSitePlots){

  ##Caption to use in your tables and plots
  Caption <- paste0("Cover Summaries for Ecological Site" , EcologicalSiteId)

  #Clean up
  TDat_LMF <- TDat_LMF %>%
        dplyr::arrange(EcologicalSiteId) %>%
            dplyr::filter(Latitude_NAD83 > 0)

  # List of ecological sites for legend
  EcoSiteList <- unique(TDat_LMF$EcologicalSiteId)

  #Set color palettes
  Pal_EcoSite <- colorFactor(palette = 'YlOrRd' , domain = TDat_LMF$es_name)
  Pal_Date <- colorFactor(palette = 'Greys' , domain = TDat_LMF$Year)
  Pal_EcoSiteID <- colorFactor(palette = "Red" , domain = EcologicalSiteId)
  EcoSite_All <- TDat_LMF$es_name
  Year <- TDat_LMF$Year

  Map <- leaflet::leaflet(height = 650 , width = 650)

  #Convert vector to string to use in caption
  EcoSiteCaption <- toString(EcologicalSiteId)

  Map <- leaflet::addTiles(Map) %>% leaflet::addCircleMarkers(lng = ~Longitude_NAD83 , lat = ~Latitude_NAD83 , radius = 3 ,
                                            popup = paste("Ecological Site: " , TDat_LMF$es_name,
                                                          "Ecolgical Site Id: " , TDat_LMF$EcologicalSiteId,
                                                          sep = "<br>") ,
                                            color = ~Pal_Date(Year) ,
                                            fillOpacity = .5 , group = Year ,
                                            data = TDat_LMF) %>%
    leaflet::addCircleMarkers(lng = ~Longitude_NAD83 , lat = ~Latitude_NAD83 ,
                     radius = 3 ,
                     fillOpacity = 0.5 ,
                     popup = paste("Ecological Site: " ,EcoSitePlots$es_name ,
                                   "Ecological Site Id: " , EcoSitePlots$EcologicalSiteId ,
                                   sep = "<br>") ,
                     color = "red" , group = EcologicalSiteId ,
                     data = EcoSitePlots) %>%
    leaflet::addLayersControl(overlayGroups = c(EcologicalSiteId , Year) ,
                     options = layersControlOptions(collapsed = TRUE)) %>%
    leaflet::addLegend(pal = Pal_Date , values = TDat_LMF$Year , opacity = 1 , group = Year) %>%
    leaflet::addLegend(pal = Pal_EcoSiteID , values = EcologicalSiteId , opacity = 1 , group = EcologicalSiteId)

  return(Map)

}
