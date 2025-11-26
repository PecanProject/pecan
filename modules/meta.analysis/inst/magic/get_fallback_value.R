library(dplyr)
source("find_same_group_stats.R")
source("get_trait_value.R")

get_fallback_value <- function(group_name, trait_id) {
  
  #Finds the species that are within the group
  fallback_group <- find_same_group_stats(group_name)
  
  #Finds the averages of ALL the traits within that group
  average_fallback_data <- average_by_trait(fallback_group)
  
  fallback_value <- get_trait_value(average_fallback_data, trait_id)
  
  return (fallback_value)
  
}

