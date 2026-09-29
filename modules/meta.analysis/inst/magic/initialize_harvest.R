library(dplyr)

source("get_stats.R")
source("get_trait_value.R")

initialize_harvest <- function(UniqueID, Date, CLASS_SUBCLASS, LAI) {
  
  group_name <- get_crop_name(CLASS_SUBCLASS)
  
  
  val_3446 <- get_fallback_value(group_name, 3446)
  
  if (is.na(val_3446)) val_3446 <- get_fallback_class_value(group_name, 3446)
  
  if (is.na(val_3446)) val_3446 <- get_fallback_type_value(group_name, 3446)
  

  initialize_harvest_df <- data.frame(
    LOC = UniqueID,
    DATE = Date,
    FRACTION_of_AGB_Removed = val_3446,
    ENSEMBLE_SIZE = 1,
    stringsAsFactors = FALSE
  )
  
  return(initialize_harvest_df)
}
  
  

