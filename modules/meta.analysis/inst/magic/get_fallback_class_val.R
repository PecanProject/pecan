library(dplyr)
source("find_same_class.R")
source("get_trait_value.R")

get_fallback_class_value <- function(group_name, traitID) {
  fallback_class <- find_same_class(group_name)
  
  all_species_stats <- data.frame()   # empty data frame
  
  for (i in fallback_class) {
    species_stats <- get_stats(master_data,
                               value_column   = "OrigValueStr",
                               trait_column   = "TraitID",
                               species_column = "AccSpeciesName",
                               species_name   = i)
    
    all_species_stats <- rbind(all_species_stats, species_stats)
  }
  
  average_fallback_data <- average_by_trait(all_species_stats)
  
  fallback_class_val <- get_trait_value(average_fallback_data, traitID)
  
  
  return (fallback_class_val)
}

