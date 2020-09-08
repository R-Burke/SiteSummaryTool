PlotMetadata <- function(dataframe){
  Plots <- c("Plots")

  #Create color palettes 
  Year_Palette <- scales::seq_gradient_pal("grey85", "grey18", "Lab")(seq(0,1,length.out= length(unique(dataframe$Year)))) 
  
  Plots_Simple <- dataframe %>% 
          dplyr::select(PrimaryKey , Year) %>%   
             dplyr::mutate(PlotsPerYear = Plots) %>%
                dplyr::arrange(Year)
  

  PlotsPerYear <- ggplot(Plots_Simple , aes(Plots, text = stat(count))) +
    geom_bar(stat = "count" , position = position_stack(reverse = TRUE) ,
             aes(fill = Year) , width = .2 , ) +
    scale_fill_manual(values = Year_Palette) +
    ggtitle("Plots Per Year") +
    coord_flip() + theme(axis.text.y = element_blank())
    PlotsPerYear<- plotly::ggplotly(PlotsPerYear , tooltip = "text")
  
  return(PlotsPerYear)

}
