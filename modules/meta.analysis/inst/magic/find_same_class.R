library(dplyr)

find_same_class<- function(group_name) {
  desired_class <- map_group_to_class(group_name)
  
  if (is.na(desired_class)) {
    return("Group name not found within class-mapping")
  }
  
  find_same_groups_in_class <- names(group_to_class[group_to_class == desired_class])
  
  find_same_species_in_group <- names(genus_to_group[genus_to_group %in% find_same_groups_in_class])
  
  master_genus <- sub(" .*", "", master_data$AccSpeciesName)
  target_species <- master_data$AccSpeciesName[master_genus %in% find_same_species_in_group]
  target_species <- unique(target_species)
  
  return (target_species)

}

