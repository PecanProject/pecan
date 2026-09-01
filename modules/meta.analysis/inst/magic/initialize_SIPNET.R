source("initialize_harvest.R")
source("initialize_planting.R")
source("Class_Subclass_Mapping.R")
library(dplyr)

# File that will initialize SIPNET. Takes in the UniqueID(polygon), the date (year and day), 
# CLASS and SUBCLASS (ex. C2), and LAI
initialize_SIPNET <- function(UniqueID, Date, Event, CLASS_SUBCLASS, LAI) {
  
  # Map CLASS and SUBCLASS 
  group_name <- get_crop_name(CLASS_SUBCLASS)
  
  LAI = LAI * 10000
  
  if (Event == "planting") {
    planting_params <- initialize_planting(group_name, LAI)
    
    # Extract values with explicit NA handling
    C_leaf <- planting_params[["leaf_carbon"]]
    C_stem <- planting_params[["stem_carbon"]]
    C_fineRoot <- planting_params[["fineroot_carbon"]]
    C_coarseRoot <- planting_params[["coarseroot_carbon"]]
    N_leaf <- planting_params[["leaf_nitrogen"]]
    N_stem <- planting_params[["stem_nitrogen"]]
    N_fineRoot <- planting_params[["fineroot_nitrogen"]]
    N_coarseRoot <- planting_params[["coarseroot_nitrogen"]]
    
    
    initialize_SIPNET_df <- data.frame(
      LOC = UniqueID,
      DATE = Date,
      EVENT = Event,
      C_LEAF = C_leaf,
      C_STEM = C_stem,
      C_FINEROOT = C_fineRoot,
      C_COARSEROOT = C_coarseRoot,
      N_LEAF = N_leaf,
      N_STEM = N_stem,
      N_FINEROOT = N_fineRoot,
      N_COARSEROOT = N_coarseRoot,
      FRACTION_of_AGB_Removed = NA,
      ENSEMBLE_SIZE = 1,
      stringsAsFactors = FALSE
    )
    
  } else if (Event == "harvest") {
    harvest_params <- initialize_harvest(group_name)
    
    initialize_SIPNET_df <- data.frame(
      LOC = UniqueID,
      DATE = Date,
      EVENT = Event,
      C_LEAF = NA,
      C_STEM = NA,
      C_FINEROOT = NA,
      C_COARSEROOT = NA,
      N_LEAF = NA,
      N_STEM = NA,
      N_FINEROOT = NA,
      N_COARSEROOT = NA,
      FRACTION_of_AGB_Removed = harvest_params,
      ENSEMBLE_SIZE = 1,
      stringsAsFactors = FALSE
    )
  }
  
  return(initialize_SIPNET_df)
}

