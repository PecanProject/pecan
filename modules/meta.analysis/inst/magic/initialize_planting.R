source("get_stats.R")
source("get_trait_value.R")
source("get_fallback_class_val.R")
source("get_fallback_type_val.R")

initialize_planting <- function(UniqueID, Date, CLASS_SUBCLASS, LAI) {
  
  if (!is.numeric(UniqueID)||!is.character(Date)||!is.character(CLASS_SUBCLASS)||!is.numeric(LAI)){
    stop ("Incorrect types!")
  }
  
  group_name <- get_crop_name(CLASS_SUBCLASS)
  
  LAI = LAI * 10000
  
  #val_601 <- get_fallback_value(group_name, trait_id = 601)
  val_3441 <- get_fallback_value(group_name, trait_id = 3441)
  val_128  <- get_fallback_value(group_name, trait_id = 128)
  val_3450 <- get_fallback_value(group_name, trait_id = 3450)
  val_2005 <- get_fallback_value(group_name, trait_id = 2005)
  val_1534 <- get_fallback_value(group_name, trait_id = 1534)
  val_14 <- get_fallback_value(group_name, trait_id = 14)
  val_2057 <- get_fallback_value(group_name, trait_id = 2057)
  val_3115 <- get_fallback_value(group_name, trait_id = 3115)
  val_3116 <- get_fallback_value(group_name, trait_id = 3116)
  val_3117 <- get_fallback_value(group_name, trait_id = 3117)
  val_1055 <- get_fallback_value(group_name, trait_id = 1055)
  
  
  #if (is.na(val_3441)|is.na(val_128)|is.na(val_3450)|is.na(val_2005)|is.na(val_1534))  val_601  <- get_fallback_class_value(group_name, 601)
  if (is.na(val_3441)) val_3441 <- get_fallback_class_value(group_name, 3441)
  if (is.na(val_128))  val_128  <- get_fallback_class_value(group_name, 128)
  if (is.na(val_3450)) val_3450 <- get_fallback_class_value(group_name, 3450)
  if (is.na(val_2005)) val_2005 <- get_fallback_class_value(group_name, 2005)
  if (is.na(val_1534)) val_1534 <- get_fallback_class_value(group_name, 1534)
  if (is.na(val_14)) val_14 <- get_fallback_class_value(group_name, 14)
  if (is.na(val_2057)) val_2057 <- get_fallback_class_value(group_name, 2057)
  if (is.na(val_3115)) val_3115 <- get_fallback_class_value(group_name, 3115)
  if (is.na(val_3116)) val_3116 <- get_fallback_class_value(group_name, 3116)
  if (is.na(val_3117)) val_3117 <- get_fallback_class_value(group_name, 3117)
  if (is.na(val_1055)) val_1055 <- get_fallback_class_value(group_name, 1055)
  
  #if (is.na(val_3441)|is.na(val_128)|is.na(val_3450)|is.na(val_2005)|is.na(val_1534)) val_601  <- get_fallback_type_value(group_name, 601) 
  if (is.na(val_3441)) val_3441 <- get_fallback_type_value(group_name, 3441)
  if (is.na(val_128))  val_128  <- get_fallback_type_value(group_name, 128)
  if (is.na(val_3450)) val_3450 <- get_fallback_type_value(group_name, 3450)
  if (is.na(val_2005)) val_2005 <- get_fallback_type_value(group_name, 2005)
  if (is.na(val_1534)) val_1534 <- get_fallback_type_value(group_name, 1534)
  if (is.na(val_14)) val_14 <- get_fallback_type_value(group_name, 14)
  if (is.na(val_2057)) val_2057 <- get_fallback_type_value(group_name, 2057)
  if (is.na(val_3115)) val_3115 <- get_fallback_type_value(group_name, 3115)
  if (is.na(val_3116)) val_3116 <- get_fallback_type_value(group_name, 3116)
  if (is.na(val_3117)) val_3117 <- get_fallback_type_value(group_name, 3117)
  if (is.na(val_1055)) val_1055 <- get_fallback_type_value("miscellaneous field", 1055)
  
  
  #Variables used to calculate Carbon and Nitrogen content
  M_stem <- val_128
  M_Leaf <- val_3441
  M_fineRoot <- val_2005
  M_coarseRoot <- val_1534
  N_leaf.mass <- val_14
  CN_fineRoot <- val_2057
  C_leaf.fraction <- 0.47
  C_stem.fraction <- 0.47
  C_fineRoot.fraction <- 0.47
  C_coarseRoot.fraction <- 0.5
  CN_root <- val_1055
  CN_stem <- 70 #placeholder
  SLA <- (val_3115 + val_3116 + val_3117) / 3
  
  
  #Calculate Leaf dry area per mass
  M_leaf.area <-  LAI / SLA #LAI/SLA
  
  #Compute Fractions
  F_stem <- M_stem / M_Leaf
  F_fineRoot <- M_fineRoot / M_Leaf
  F_coarseRoot <- M_coarseRoot / M_Leaf
  
  #Compute Mass Ratios
  M_stem.area <- F_stem * M_leaf.area
  M_fineRoot.area <- F_fineRoot * M_leaf.area
  M_coarseRoot.area <- F_coarseRoot * M_leaf.area
  
  #Compute Carbon content
  
  C_leaf <- M_leaf.area * C_leaf.fraction
  C_stem <- M_stem.area * C_stem.fraction
  C_fineRoot <- M_fineRoot.area * C_fineRoot.fraction
  C_coarseRoot <- M_coarseRoot.area * C_coarseRoot.fraction

  #Compute Nitrogen Content
  N_leaf <- M_leaf.area * N_leaf.mass
  N_stem <- M_stem.area * C_stem.fraction / CN_stem
  N_fineRoot <- C_fineRoot / CN_fineRoot
  N_coarseRoot <- M_coarseRoot.area * C_coarseRoot.fraction / CN_root
    
  planting_params <- list(
    #planting_density = val_601,
    leaf_carbon = C_leaf,
    stem_carbon = C_stem,
    fineroot_carbon = C_fineRoot,
    coarseroot_carbon = C_coarseRoot,
    leaf_nitrogen = N_leaf,
    stem_nitrogen = N_stem,
    fineroot_nitrogen = N_fineRoot,
    coarseroot_nitrogen = N_coarseRoot
  )

  C_leaf <- planting_params[["leaf_carbon"]]
  C_stem <- planting_params[["stem_carbon"]]
  C_fineRoot <- planting_params[["fineroot_carbon"]]
  C_coarseRoot <- planting_params[["coarseroot_carbon"]]
  N_leaf <- planting_params[["leaf_nitrogen"]]
  N_stem <- planting_params[["stem_nitrogen"]]
  N_fineRoot <- planting_params[["fineroot_nitrogen"]]
  N_coarseRoot <- planting_params[["coarseroot_nitrogen"]]
  
  
  initialize_planting_df <- data.frame(
    LOC = UniqueID,
    DATE = Date,
    C_LEAF = C_leaf,
    C_STEM = C_stem,
    C_FINEROOT = C_fineRoot,
    C_COARSEROOT = C_coarseRoot,
    N_LEAF = N_leaf,
    N_STEM = N_stem,
    N_FINEROOT = N_fineRoot,
    N_COARSEROOT = N_coarseRoot, 
    ENSEMBLE_SIZE = 1,
    stringsAsFactors = FALSE
  )
  
  return(initialize_planting_df)
}

