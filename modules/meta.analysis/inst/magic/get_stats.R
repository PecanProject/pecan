library(dplyr)

get_stats <- function(data, value_column, group_column, species_column = "SpeciesName", species_name) {
  
  # Filter by species
  data_filtered <- data %>%
    filter(.data[[species_column]] == species_name)
  
  # Convert value column to numeric
  data_filtered[[value_column]] <- as.numeric(data_filtered[[value_column]])
  
  # Summarize statistics
  summary_df <- data_filtered %>%
    group_by(across(all_of(group_column))) %>%
    summarise(
      mean_value = mean(.data[[value_column]], na.rm = TRUE),
      sd         = sd(.data[[value_column]], na.rm = TRUE),
      n          = sum(!is.na(.data[[value_column]])),
      .groups    = "drop"
    ) %>%
    rename(TraitID = 1)
  
  return(summary_df)
}


