library(readxl)
library(dplyr)

extract_rows_traitID <- function(excel_path, sheet_name = 1, trait_ID) {
  
  #reads the excel sheet
  data <- readxl::read_excel(excel_path, sheet = sheet_name, col_types = NULL)
  
  #throws an error if TraitID is not within the sheet
  if (!"TraitID" %in% colnames(data)) {
    stop("Column TraitID not found within the excel sheet")
  }
  
  # finds the rows with the desired TraitID
  matched_rows <- which(data$TraitID == trait_ID)
  
  if (length(matched_rows) > 0) {
    filtered_rows <- data[matched_rows, ]
  } else {
    filtered_rows <- data.frame()
    message("No rows found with TraitID of ", trait_ID)
  }
  
  
  # df2 now has a `common_name` column right after `SpeciesName`
  
  return (filtered_rows)
}


