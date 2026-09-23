#this will run the entire program

#example run: test_df1 <- run_program(time = "planting", species = "Rubus idaeus")

library(readxl)
library(dplyr)
library(taxize)

source("initialize_planting.R")

run_program <- function(time, species) {
  #time is harvest/planting
  #pool is part of plant
  #species is species

  if(is.character(time) && time == "planting") {
    temp_response <- (initialize_planting(species_name = species))
  } else if (is.character(time) && time == "harvest"){
    temp_response <- (initialize_harvest(species_name = species))
  } else {
    return("Invalid time input. Use either planting or harvest.")
  }
  
}

