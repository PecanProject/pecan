pacman::p_load(PEcAn.logger, dplyr, readr, arrow, purrr)

#----set up----

#"NBS_Targets" or "BAU_Targets"
scenario_variable = "NBS_Targets" 

config = list(scenario = scenario_variable,
              predictions_dir = sprintf("/projectnb/dietzelab/ananyak/county_landiq_predictions_with_phenology_cleaned/%s", scenario_variable),

              fert_dir = sprintf("/projectnb/dietzelab/ananyak/fertilization/%s", scenario_variable),
              
              ncc_dir = sprintf("/projectnb/dietzelab/ananyak/ncc/%s", scenario_variable),
  
              output_dir = sprintf("/projectnb/dietzelab/ananyak/county_landiq_predictions_complete/%s",scenario_variable))

dir.create(config$output_dir, showWarnings = FALSE, recursive = TRUE)

#----1: Read Parquet Inputs----

PEcAn.logger::logger.info("Loading Fertilization and NCC Parquet datasets...")

fert_files = list.files(config$fert_dir, pattern = "\\.parquet$",full.names = TRUE)

ncc_files = list.files(config$ncc_dir, pattern = "\\.parquet$", full.names = TRUE)

if (length(fert_files) == 0) {stop("No fertilization parquet files found.")}
if (length(ncc_files) == 0) {stop("No NCC parquet files found.")}

#read synthetic N fertilization data
fert = purrr::map_dfr(
  fert_files,
  arrow::read_parquet
) |>
  dplyr::select(parcel_id, year, crop_code, fert_date = date, synthetic_nh4_n_kg_m2 = nh4_n_kg_m2, synthetic_no3_n_kg_m2 = no3_n_kg_m2)

#Read NCC compost data
ncc = purrr::map_dfr(
  ncc_files,
  arrow::read_parquet
) |>
  dplyr::select(parcel_id, year, crop_code, compost_date = date, compost_org_c_kg_m2 = org_c_kg_m2, compost_org_n_kg_m2 = org_n_kg_m2)


#make sure parcel + year + crop uniquely identifies each event
fert_duplicates = fert |>
  dplyr::count(parcel_id, year, crop_code) |>
  dplyr::filter(n > 1)

if (nrow(fert_duplicates) > 0) {
  stop("Duplicate synthetic fertilizer events found for parcel/year/crop_code.")
}

ncc_duplicates = ncc |>
  dplyr::count(parcel_id, year, crop_code) |>
  dplyr::filter(n > 1)

if (nrow(ncc_duplicates) > 0) {stop("Duplicate compost events found for parcel/year/crop_code.")}

#----2: Merge Rates into Prediction CSVs----

pred_files = list.files(config$predictions_dir, pattern = "_predicted_2024_2045\\.csv$", full.names = TRUE)

PEcAn.logger::logger.info(sprintf("Processing %d county prediction files...", length(pred_files)))

purrr::walk(pred_files, function(pred_path) {
  
  county_name = basename(pred_path)
  
  #Read base county predictions and create crop_code from CLASS + SUBCLASS
  pred = readr::read_csv(
    pred_path,
    show_col_types = FALSE
  ) |>
    dplyr::mutate(parcel_id = as.integer(parcel_id), year = as.integer(year), crop_code = paste0(as.character(CLASS), as.character(SUBCLASS)))
  
  original_n = nrow(pred)
  
  
  #Join fertilization & NCC by parcel_id, year, and crop_code
  updated_pred = pred |>
    dplyr::left_join(
      fert,
      by = c("parcel_id", "year", "crop_code")
    ) |>
    dplyr::left_join(
      ncc,
      by = c("parcel_id", "year", "crop_code")
    ) |>
    dplyr::mutate(synthetic_nh4_n_kg_m2 = dplyr::coalesce(synthetic_nh4_n_kg_m2, 0),
      synthetic_no3_n_kg_m2 = dplyr::coalesce(synthetic_no3_n_kg_m2, 0),
      
      compost_org_c_kg_m2 = dplyr::coalesce(compost_org_c_kg_m2, 0),
      
      compost_org_n_kg_m2 = dplyr::coalesce(compost_org_n_kg_m2, 0))
  
  
  #make sure joining management data did not duplicate prediction rows
  if (nrow(updated_pred) != original_n) {stop("Management join changed row count for ", county_name, ": ", original_n, " -> ", nrow(updated_pred))}
  
  
  #Save to output folder
  out_file = file.path(config$output_dir, county_name)
  
  readr::write_csv(updated_pred, out_file)
})


PEcAn.logger::logger.info("Successfully merged fertilization & NCC rates into prediction files.")