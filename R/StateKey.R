StateKey <- function(TerrADat_Path, Tall_Table_Path,
                     StateKeyPath, StateKeyName,
                     EcoSiteName, EcologicalSiteId, State, Degraded){

  ### Set up custom indicator-
  ### This is list of species in "Historically Dominant" tab in generalized key sheet
  ### This tab is really the only thing that needs to be updated for sites in generalized key groups

  HistoricPlant_sheet <- paste("Hx", EcoSiteName, sep = " ")
  KeyIndicator_sheet <- "Key Indicators"
  GeneralKey_sheet <- "Generalized Key"
  TransitionRisk_sheet <- "Transition indicators"

  ### Need to gather tall tables if not already
  #terradactyl::gather_all(TerrADat_Path = TerrADat_Path, folder = Tall_Table_Path)


  ###   Read in LPI and header
  # Most recently run tall tables
  lpi <-  readRDS(paste(Tall_Table_Path , "lpi_tall.RData" , sep = ""))
  header <- readRDS(paste(Tall_Table_Path , "header.RData" , sep = ""))

  header_state <- header [(header[["State"]] %in% State), ]
  state_primarykeys <- header_state$PrimaryKey

  lpi_subset<- lpi[(lpi[["PrimaryKey"]] %in% state_primarykeys), ]

  ### Read in Historic Plants

  HxPlants <- readxl::read_excel(paste(StateKeyPath, StateKeyName, sep = "/"), sheet = paste("Hx", EcoSiteName, sep = " "))

  HxPlants <- HxPlants$Species

  ## Pull out everything in LPI with historic plant community present

  lpi_Hx <- lpi_subset[(lpi_subset[["code"]] %in% HxPlants), ]

  # Create a column named Historic and give it the value of Historic

  lpi_Hx$Historic <- "Historic"

  #Pull out everything without historic species

  lpi_NonHx <- lpi_subset[(!lpi_subset[["code"]] %in% HxPlants), ]

  # Create a column named Historic and give it value Non-Historic

  lpi_NonHx$Historic <- "Non-Historic"

  #Now bind them back together

  lpi_Hx_populated <- rbind(lpi_Hx , lpi_NonHx)

  #We need AH and FH because FH would indicate interspaces (see STM)

  Hx_AH <- terradactyl::pct_cover(lpi_tall = lpi_Hx_populated,
                                  tall = TRUE,
                                  hit = "any",
                                  by_year = FALSE,
                                  by_line = FALSE,
                                  Historic) # Group by historic plant variable (or other custom indicator in future)

  Hx_AH <- Hx_AH %>% subset(indicator == "HISTORIC") %>% dplyr::rename(AH_Historic = percent)

  Hx_FH <- terradactyl::pct_cover(lpi_tall = lpi_Hx_populated,
                                  tall = TRUE,
                                  hit = "first",
                                  by_year = FALSE,
                                  by_line = FALSE,
                                  Historic)

  Hx_FH <- Hx_FH %>% subset(indicator == "HISTORIC") %>% dplyr::rename(FH_Historic = percent)

  Hx_indicator <- merge(Hx_AH , Hx_FH , by = "PrimaryKey") %>% dplyr::select(PrimaryKey, AH_Historic, FH_Historic)

  ## Now read in the data
  ## Must preload Combine_AIM_LMF function
  TDat_LMF <- SiteSummaryTool::Combine_AIM_LMF(TerrADat_Path = TerrADat_Path,
                                               EDIT_List_Path = EDIT_List_Path,
                                               Internal = FALSE)

  #Subset based on EcologicalSiteId

  EcoSitePlots <- TDat_LMF[TDat_LMF[["EcologicalSiteId"]] %in% EcologicalSiteId, ]

  # Pull primary keys from subsetted plots

  EcoSite_PKs <- EcoSitePlots$PrimaryKey

  ##Now we can bind the custom indicator with the full data set
  ## Note that at this point the full data set has been subset to the ecological site but the custom indicator has not.

  #Subset the custom indicator to the ecological site

  Hx_indicator<- Hx_indicator[(Hx_indicator[["PrimaryKey"]] %in% EcoSite_PKs), ]
  all(Hx_indicator$PrimaryKey %in% EcoSite_PKs) # should be true

  #Get the data into tall format so that it can be merged with the rest of the indicators
  Hx_tall <- tidyr::gather(Hx_indicator, key = Historic, value = Percent , 2:3)

  #### Pull in generalized key ####

  ## Read in Excel Key built from SDM
  GeneralizedKey <- readxl::read_excel(paste(StateKeyPath, StateKeyName, sep = "/"), sheet = "Generalized Key")

  ## These are the only indicators necessary to apply key logic
  KeyIndicators <- readxl::read_excel(paste(StateKeyPath, StateKeyName, sep = "/"), sheet = "Key Indicators")
  KeyIndicators <- unique(KeyIndicators$Indicator)

  # Trim the full dataset to just the key indicators

  FullDataTrim <- EcoSitePlots %>% dplyr::select(PrimaryKey, matches(paste(KeyIndicators, collapse = "|")))
  col <- ncol(FullDataTrim)

  # Get tall

  FullData_Tall <- FullDataTrim %>% tidyr::gather(key = Indicator, value = Percent , 2:col)

  ## Now combine

  Hx_tall <- Hx_tall %>% dplyr::rename(Indicator = Historic)
  FullData_PlusCustom <- rbind(FullData_Tall, Hx_tall)

  #Now we can join the full data set with the key and apply the key logic.
  Joined <- dplyr::full_join(FullData_PlusCustom, GeneralizedKey, by = c("Indicator"))

  #Adding the wide table to populate relative fields
  Joined_Populate <- dplyr::full_join(Joined, FullDataTrim, by = "PrimaryKey")

  #Pull unique values in Key, mutate values for relative values based on indicators

  Upper_unique <- unique(Joined_Populate$Upper.Limit)

  Lower_unique <- unique(Joined_Populate$Lower.Limit)
  names(Joined_Populate)

  Joined_Populated <- Joined_Populate %>% dplyr::mutate(Upper_derived = ifelse(Upper.Limit == "AH_NonNoxCover", AH_NonNoxCover,
                                                                               ifelse(Upper.Limit == "AH_NonNoxPerenGrassCover", AH_NonNoxPerenGrassCover, Upper.Limit)),
                                                        Lower_derived = ifelse(Lower.Limit == "AH_NonNoxShrubCover + AH_NonNoxSubShrubCover + AH_NonNoxAnnGrassCover + AH_NonNoxAnnForbCover + AH_NonNoxPerenForbCover",
                                                                               AH_NonNoxShrubCover + AH_NonNoxSubShrubCover + AH_NonNoxAnnGrassCover + AH_NonNoxAnnForbCover + AH_NonNoxPerenForbCover,
                                                                               ifelse(Lower.Limit == "AH_NonNoxShrubCover + AH_NonNoxSubShrubCover", AH_NonNoxShrubCover + AH_NonNoxSubShrubCover,
                                                                                      ifelse(Lower.Limit == "AH_NonNoxCover", AH_NonNoxCover,
                                                                                             ifelse(Lower.Limit == "FH_NonNoxPerenGrassCover", FH_NonNoxPerenGrassCover, Lower.Limit))))) %>%
    dplyr::select(-Lower.Limit, -Upper.Limit) %>% dplyr::rename(Lower.Limit = Lower_derived, Upper.Limit = Upper_derived) %>% dplyr::filter(!is.na(Upper.Limit))

  length(unique(Joined$PrimaryKey)) # Make sure no plots dropped

  # Paste the evaluation criteria into 2 columns

  Conditional_Paste <- Joined_Populated %>% dplyr::mutate(Eval_Lower = paste(Percent, Lower.Relation, Lower.Limit), Eval_Upper = paste(Percent, Upper.Relation, Upper.Limit))

  eval_vars <- names(Conditional_Paste)[grep(names(Conditional_Paste) , pattern = "^Eval_")]

  ## Need to remove NA values (artifact of merging, joining)

  Conditional_Paste <- Conditional_Paste %>% dplyr::filter(!is.na(STM_Indicator))

  benchmark_vector <- sapply(X = 1:nrow(Conditional_Paste),
                             data = Conditional_Paste,
                             eval_vars = eval_vars,
                             FUN = function(X, data, eval_vars){
                               all(sapply(X = eval_vars,
                                          data = data[X, ],
                                          FUN = function(X, data){
                                            evalstring <- data[[X]]
                                            eval(parse(text = evalstring))
                                          }))
                             })
  Conditional_Paste$benchmark_vector <- benchmark_vector

  #This first summary will only include plots that meet all critera for a state/phase combo

  Summary <- Conditional_Paste %>% dplyr::group_by(PrimaryKey , State, StateName) %>% dplyr::summarize(Final.State = all(benchmark_vector))

  output_summary1 <- Summary[Summary$Final.State, c("PrimaryKey", "State" , "StateName")]

  # This summary will rank the likelihood of a plot belonging to a certain state/phase
  Summary2 <- Conditional_Paste %>% dplyr::group_by(PrimaryKey , State, StateName) %>%
    dplyr::summarize(ProportionCriteriaMet = sum(benchmark_vector)/length(benchmark_vector)) %>%
    dplyr::filter(ProportionCriteriaMet > 0)

  # Here I am checking to make sure that if a plot has a 1 for rank, it was icluded in the first summary
  AllTrue <- Summary2 %>% subset(ProportionCriteriaMet >= 1)
  length(unique(Summary2$PrimaryKey)) #This should equal the number of total plots.

  #Top 3 most likely state
  Summary_Rank_Top <- Summary2 %>% dplyr::group_by(PrimaryKey) %>%
    dplyr::top_n(3, ProportionCriteriaMet) %>%
    dplyr::rename(EcologicalState = State)

  # Combine with Tdat for plotting

  output_spatial_top <- merge(Summary_Rank_Top, EcoSitePlots , by = "PrimaryKey") %>%
    dplyr::select(PrimaryKey , PlotID, EcologicalSiteId, EcologicalState , StateName , ProportionCriteriaMet, Latitude_NAD83 , Longitude_NAD83)

  ## If there is a tie between likelihood of 2 states,
  ## Degraded = TRUE will err on side of more degraded
  ## Degraded = FALSE will err on side of less degraded
  ### Trying again
  Summary_Rank_Top_Slice <- Summary_Rank_Top %>% dplyr::group_by(PrimaryKey) %>%
    dplyr::mutate(EcologicalState_Final = ifelse(Degraded == TRUE, max(EcologicalState),
                                                 ifelse(Degraded == FALSE, min(EcologicalState), NA))) %>%
    dplyr::select(-EcologicalState) %>%
    dplyr::rename(EcologicalState = EcologicalState_Final) %>%
    unique() %>% dplyr::filter(1:dplyr::n() == 1)

  output_spatial_full <- merge(Summary_Rank_Top_Slice, EcoSitePlots , by = "PrimaryKey")
  write.csv(output_spatial_full, file = paste(EcologicalSiteId, "FullOutput_GeneralKey.csv", sep ="") , row.names = FALSE)
  write.csv(Summary_Rank_Top_Slice , file = paste(EcologicalSiteId, "GeneralKeyEcologicalState.csv", sep = ""), row.names = FALSE)

  return(output_spatial_full)

  #######
}
