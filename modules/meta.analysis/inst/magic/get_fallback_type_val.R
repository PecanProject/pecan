# This file is for the last fallback for type
library(dplyr)
source("find_same_class.R")
source("get_trait_value.R")
source("find_same_type.R")

get_fallback_type_value <- function(group_name, trait_id) {
  fallback_type <- find_same_type(group_name)
  
  all_species_type_stats <- data.frame()
  
  for (i in fallback_type) {
    species_stats <- get_stats(master_data,
                               value_column   = "OrigValueStr",
                               trait_column   = "TraitID",
                               species_column = "AccSpeciesName",
                               species_name   = i)
    
    all_species_type_stats <- rbind(all_species_type_stats, species_stats)
  }
  

  average_fallback_data <- average_by_trait(all_species_type_stats)
  
  #this could be what is taking so long, will return to this later
  
  
  fallback_type_val <- get_trait_value(average_fallback_data, trait_id)
  
  return (fallback_type_val)

}
