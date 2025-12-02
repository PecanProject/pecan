library(dplyr)

initialize_planting <- function(species_name) {
  
  species_stats <- data.frame(
    get_stats(planting_df, "OrigValueStr", "TraitID", "SpeciesName", species_name)
  )
  return(species_stats)
  
  #calculate for each trait 
  
  #3441 leafC

  #128 wood/stemC
  
  #3450 rootC
  
  #2005 fine-rootC
  
  #1534 coarse-rootC
  
  #output a table with the information

}