library(dplyr)
library(rotl)
library(ape)

find_default_species <- function(target_species) {
  
  clean_name <- function(x) {
    x <- gsub("\\s+(sp\\.|cf\\.|var\\..*|subsp\\..*)", "", x)
    x <- trimws(x)
    return(x)
  }
  
  target_species <- clean_name(target_species)
  
  # master_data must be in environment, with column AccSpeciesName
  reference_species <- unique(master_data$AccSpeciesName)
  reference_species <- sapply(reference_species, clean_name)
  reference_species <- reference_species[reference_species != ""]
  
  all_species <- unique(c(target_species, reference_species))
  resolved <- tnrs_match_names(all_species)
  resolved <- resolved[!is.na(resolved$ott_id), ]
  resolved <- resolved[!is.na(resolved$pruned_ott_id), ]   # remove pruned taxa
  
  if (nrow(resolved) == 0) return(NA)
  
  if (!(target_species %in% resolved$search_string)) {
    genus <- strsplit(target_species, " ")[[1]][1]
    resolved_genus <- tnrs_match_names(genus)
    resolved_genus <- resolved_genus[!is.na(resolved_genus$ott_id), ]
    resolved_genus <- resolved_genus[!is.na(resolved_genus$pruned_ott_id), ]
    
    if (nrow(resolved_genus) == 0) return(NA)
    
    target_species <- resolved_genus$search_string[1]
    
    if (!(target_species %in% resolved$search_string)) {
      resolved <- rbind(resolved, resolved_genus[1, ])
    }
  }
  
  ott_ids <- resolved$pruned_ott_id
  name_map <- resolved$unique_name
  names(name_map) <- resolved$search_string
  
  # Build induced subtree - this should work without HTTP 400 errors now
  tree <- tol_induced_subtree(ott_ids = ott_ids)
  
  dist_matrix <- cophenetic(tree)
  
  target_name <- name_map[target_species]
  if (!(target_name %in% rownames(dist_matrix))) return(NA)
  
  distances_to_target <- dist_matrix[target_name, ]
  distances_to_target <- distances_to_target[names(distances_to_target) != target_name]
  
  matched_reference <- reference_species[reference_species %in% resolved$search_string]
  reference_names <- name_map[matched_reference]
  
  distances_to_target <- distances_to_target[names(distances_to_target) %in% reference_names]
  
  if (length(distances_to_target) == 0) return(NA)
  
  closest_name <- names(which.min(distances_to_target))
  original_closest <- names(name_map[name_map == closest_name])[1]
  
  return(original_closest)
}
