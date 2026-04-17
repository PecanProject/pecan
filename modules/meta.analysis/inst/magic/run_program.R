#this will run the entire program

#example run: test_df1 <- run_program(time = "planting", species = "Rubus idaeus")

library(readxl)
library(dplyr)
library(taxize)

source("get_stats.R")

run_program <- function(time, species) {
  #time is harvest/planting
  #pool is part of plant
  #species is species
  
  temp_plant_df <- get_stats(master_data, value_column = "OrigValueStr", trait_column = "TraitID", species_column = "SpeciesName", species_name = species)
  
  #need to implement if NA, return species with like traits
  
  if (time == "planting") {
    temp_plant_df <- temp_plant_df[temp_plant_df$TraitID %in% c("3441", "128", "2005", "1534"),]
  }
  else if (time == "harvest") {
    temp_plant_df <- temp_plant_df[temp_plant_df$TraitID %in% c("3962", "470"),]
    
  }
  
  if (nrow(temp_plant_df) == 0) {
    return(paste(species, "has nothing"))
  }
  
  return (temp_plant_df)
  
}

