#sifts the species through the entire list of species within the master data and returns all similar related species by group
library(dplyr)
source("get_stats.R")


find_same_group_stats <- function(species_name) {
  desired_group <- map_species_to_group(species_name)
  
  if (is.na(desired_group)) {
    return("SpeciesName not found within genus_mapping")
  }
  
  list_fallback_species <- c()
  for (i in master_data$AccSpeciesName) {
    target_group <- map_species_to_group(i)
    if (target_group == desired_group) {
      list_fallback_species <- c(list_fallback_species, i)
    }
  }
  
  
  stats_list <- list()
  for (sp in list_fallback_species) {
    single_stats <- get_stats(master_data,
                              value_column = "OrigValueStr",
                              trait_column = "TraitID",
                              species_column = "AccSpeciesName",
                              species_name = sp)
    stats_list[[sp]] <- single_stats 
  }
  
  combined <- do.call(rbind, stats_list)
  
  return (combined)
 
  
}

