# This is the file to find all the species within the same type
library(dplyr)

source("type_classing.R")

find_same_type <- function(group_name) {
  desired_type <- map_group_to_type(group_name)
  
  if (is.na(desired_type) || desired_type == "NA") {
    return("Group name is not within type mapping")
  }
  
  find_same_groups_in_type <- names(group_to_type[group_to_type == desired_type])
  
  find_same_species_in_group <- names(genus_to_group[genus_to_group %in% find_same_groups_in_type])
  
  master_genus <- sub(" .*", "", master_data$AccSpeciesName)
  target_species <- master_data$AccSpeciesName[master_genus %in% find_same_species_in_group]
  target_species <- unique(target_species)
  
  return (target_species)
  
  
}

