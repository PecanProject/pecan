#this will be the main file where the entire program will be run
library(readxl)
library(dplyr)

source("extract_rows_traitID.R")

main <- function(files, trait_IDs, sheet_name = 1) {
  combined_data <- data.frame()
  
  for (file in files) {
    for (id in trait_IDs) {
      temp <- extract_rows_traitID(file, sheet_name = 1, trait_ID = id) 
      combined_data <- bind_rows(combined_data, temp)
    }
  }
  return (combined_data)
}

