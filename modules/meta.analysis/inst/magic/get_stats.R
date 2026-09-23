library(dplyr)

get_stats <- function(data, value_column, trait_column, species_column = "AccSpeciesName", species_name) {
  
  # Filter by species
  data_filtered <- data %>%
    filter(.data[[species_column]] == species_name)
  
  if(nrow(data_filtered) == 0) {
    warning(paste("No data found for species:", species_name))
    return(data.frame(TraitID = NA, mean_value = NA, sd = NA, n = 0))
  }
  
  # Convert value column to numeric
  data_filtered[[value_column]] <- as.numeric(data_filtered[[value_column]])
  
  # Summarize statistics
  summary_df <- data_filtered %>%
    group_by(across(all_of(trait_column))) %>%
    summarise(
      mean_value = mean(.data[[value_column]], na.rm = TRUE),
      sd         = sd(.data[[value_column]], na.rm = TRUE),
      n          = sum(!is.na(.data[[value_column]])),
      .groups    = "drop"
    ) 
  
  summary_df <- summary_df %>%
  rename(TraitID = all_of(trait_column))
  
  return(summary_df)
}


