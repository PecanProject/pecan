get_trait_value <- function(species_stats, trait_id) {
  # Check required columns
  required_cols <- c("TraitID", "mean_value")
  if (!all(required_cols %in% names(species_stats))) {
    stop(paste("species_stats must contain columns:", paste(required_cols, collapse = ", ")))
  }
  
  # Convert TraitID column to numeric
  trait_ids <- as.numeric(as.character(species_stats$TraitID))
  trait_id <- as.numeric(trait_id)
  
  # Convert mean_value column to numeric safely
  orig_values_num <- suppressWarnings(as.numeric(as.character(species_stats$mean_value)))
  
  # Filter rows by trait_id
  matched_rows <- trait_ids == trait_id
  
  #if (!any(matched_rows)) {
    #return(NA_real_)  # No matching trait_id
  #}
  
  # Extract numeric OrigValueStr values for matched rows
  values <- orig_values_num[matched_rows]
  values <- values[!is.na(values)]
  
  if (length(values) == 0) {
    return(NA_real_)  # No valid numeric values found
  }
  
  return(values[1])  # Return first numeric value
}

