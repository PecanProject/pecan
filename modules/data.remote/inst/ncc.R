# modified ncc-statewide projection workflow from PR #4003
# scenario-goal-aware version for compost / organic amendments

setwd("/projectnb/dietzelab/ananyak")

library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(purrr)
library(data.table)

#scenario = either "BAU_Targets" or "NBS_Targets" 
#keeping scenario_variable outside config to switch between scenario/output paths 
#use n_parcels = NULL for full scale run 

scenario_variable = "NBS_Targets"

config = list(seed = 42, years = 2024:2045, scenario = scenario_variable, n_parcels = NULL, 
              n_ensemble = 20, batch_size = 500, workers = 1,
              scenarios_path = sprintf( "/projectnb/dietzelab/ananyak/MAGiC_scenarios_FINAL/%s.csv", scenario_variable),
              lookup_path = "/projectnb/dietzelab/ccmmf/management/LandIQ_cropCode_lookup_table.csv",
              projection_dir = "/projectnb/dietzelab/ananyak/county_landiq_predictions_with_phenology",
              output_dir = file.path("/projectnb/dietzelab/ananyak/ncc", scenario_variable))

set.seed(config[["seed"]])

staging_dir = file.path(config[["output_dir"]], "_staging")
dir.create(staging_dir, showWarnings = FALSE, recursive = TRUE)

logger_info = function(...) message(paste0(...))
logger_warn = function(...) warning(paste0(...), call. = FALSE)
logger_severe = function(...) stop(paste0(...), call. = FALSE)

lb_acre_to_kg_m2 = function(x) {x * 0.45359237 / 4046.8564224}

normalize_crop_key = function(x) {
  x |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("&", "and") |>
    stringr::str_replace_all("[[:punct:]]", " ") |>
    stringr::str_squish()}

#mapping scenario crop type to lookup table 
build_ncc_crop_lookup = function(lookup_path) {
  if (!file.exists(lookup_path)) {
    logger_severe("LandIQ lookup table not found: ", lookup_path)}
  
  code_map = readr::read_csv(lookup_path, show_col_types = FALSE) |>
    dplyr::mutate(
      CLASS = as.character(.data$CLASS), SUBCLASS = as.character(.data$SUBCLASS),
      code = paste0(.data$CLASS, .data$SUBCLASS),
      desc_key = normalize_crop_key(paste(.data$CLASS_desc, .data$SUBCLASS_desc, .data$crops_included)))

  required_lookup_cols = c("CLASS", "SUBCLASS", "CLASS_desc", "SUBCLASS_desc")
  missing_lookup_cols = setdiff(required_lookup_cols, names(code_map))
  
  if (length(missing_lookup_cols) > 0) {logger_severe(
      "Lookup table missing columns: ",
      paste(missing_lookup_cols, collapse = ", "))}
  
  crop_lookup = code_map |>
    dplyr::mutate(
      scenario_crop = dplyr::case_when(
        # berries / strawberries
        .data$CLASS == "T" & .data$SUBCLASS %in% c("19", "28") ~
          "All Other Berries",
        .data$CLASS == "T" & .data$SUBCLASS == "20" ~
          "Strawberries (Fresh Market)",
        
        # almonds / nuts
        .data$CLASS == "D" & .data$SUBCLASS == "12" ~
          "Almonds",
        .data$CLASS == "D" & .data$SUBCLASS %in% c("13", "14", "17") ~
          "All Other Nut Crops",
        
        # pome / stone / other deciduous fruit
        .data$CLASS == "D" & .data$SUBCLASS %in% c("1", "6") ~
          "Pome Fruit",
        .data$CLASS == "D" & .data$SUBCLASS %in% c("2", "3", "5", "7", "8", "16") ~
          "Stone Fruit",
        .data$CLASS == "D" ~
          "All Other Fruit Crops",
        
        # matches your scenario transition-state map where C -> Citrus
        .data$CLASS == "C" ~
          "Citrus",
        
        # grapes
        .data$CLASS == "V" & .data$SUBCLASS == "1" ~
          "Grapes, Table",
        .data$CLASS == "V" & .data$SUBCLASS == "2" ~
          "Grapes, Wine",
        .data$CLASS == "V" & .data$SUBCLASS == "3" ~
          "Grapes Dried, Raisins",
        .data$CLASS == "V" ~
          "Grapes, Wine",
        
        # fallow
        .data$CLASS == "X" ~
          "Fallow",
        
        # broad annual groups
        .data$CLASS %in% c("F", "P") ~
          "All Other Field Crops (Incl. Pasture /Rangeland)",
        .data$CLASS %in% c("G", "R", "T") ~
          "Annual Cropland",
        
        TRUE ~ NA_character_
      ),
      scenario_crop_key = normalize_crop_key(.data$scenario_crop)
    ) |>
    dplyr::select(
      "code", "CLASS", "SUBCLASS", "CLASS_desc", "SUBCLASS_desc", "scenario_crop", "scenario_crop_key"
    ) |>
    dplyr::distinct()
  
  dup_codes = crop_lookup |>
    dplyr::count(.data$code) |>
    dplyr::filter(.data$n > 1)
  
  if (nrow(dup_codes) > 0) {logger_warn("Some LandIQ codes map to multiple scenario crops. Check lookup.")
    print(dup_codes)}
  
  crop_lookup}

pft_family = function(pft) {
  dplyr::case_when(
    pft %in% c("row", "hay", "rice") ~ "annual",
    pft == "woody" ~ "perennial",
    TRUE ~ NA_character_)}

##-----script 1, read projected crop files and build design table-----
read_projected_county = function(fn) {
  logger_info("Reading: ", basename(fn))
  
  dt = readr::read_csv(fn, show_col_types = FALSE)
  
  if (!"season" %in% names(dt)) {dt$season = 0L}
  
  required_cols = c("parcel_id", "county", "year", "CLASS", "SUBCLASS", "PFT", "planting_date", "ACRES")
  
  missing_cols = setdiff(required_cols, names(dt))
  
  if (length(missing_cols) > 0) {
    logger_severe("Projected file is missing required columns: ", paste(missing_cols, collapse = ", "),
      "\nFile: ", fn, "\nAvailable columns: ", paste(names(dt), collapse = ", "))}
  
  dt |>
    dplyr::filter(
      .data$year %in% config[["years"]],
      !is.na(.data$CLASS),
      !is.na(.data$SUBCLASS),
      !is.na(.data$PFT),
      !is.na(.data$planting_date),
      !is.na(.data$ACRES),
      .data$ACRES > 0
    ) |>
    dplyr::transmute(
      parcel_id = as.integer(.data$parcel_id),
      county = as.character(.data$county),
      year = as.integer(.data$year),
      season = as.integer(dplyr::coalesce(.data$season, 0L)),
      anchor = as.Date(.data$planting_date),
      code = paste0(as.character(.data$CLASS), as.character(.data$SUBCLASS)),
      PFT = as.character(.data$PFT),
      ACRES = as.numeric(.data$ACRES),
      scenario = config[["scenario"]])
}

scenario_dir = file.path(config[["projection_dir"]], config[["scenario"]])

if (!dir.exists(scenario_dir)) {logger_severe("Scenario directory does not exist: ", scenario_dir)}

projected_files = list.files(scenario_dir, pattern = "_predicted_2024_2045\\.csv$", full.names = TRUE)

if (length(projected_files) == 0) {logger_severe("No projected county CSVs found in: ", scenario_dir)}

logger_info("Found ", length(projected_files), " projected files under: ", scenario_dir)

plant = purrr::map_dfr(projected_files, read_projected_county)

if (nrow(plant) == 0) {logger_severe("Projected files were found, but no valid rows survived filtering.\n",
    "Check years, CLASS, SUBCLASS, PFT, planting_date, and ACRES.")}

ncc_crop_lookup = build_ncc_crop_lookup(config[["lookup_path"]])

plant = plant |>
  dplyr::left_join(ncc_crop_lookup, by = "code") |>
  dplyr::mutate(
    pft_family = pft_family(.data$PFT))

unmapped_codes = plant |>
  dplyr::filter(is.na(.data$scenario_crop)) |>
  dplyr::count(.data$code, .data$PFT, sort = TRUE)

if (nrow(unmapped_codes) > 0) {
  logger_warn("Dropping ", sum(unmapped_codes$n), " rows with no scenario crop mapping. Top unmapped codes:")
  print(head(unmapped_codes, 20))}

unknown_pft = plant |>
  dplyr::filter(is.na(.data$pft_family)) |>
  dplyr::count(.data$PFT, sort = TRUE)

if (nrow(unknown_pft) > 0) {
  logger_warn("Dropping rows with unknown PFT family. Breakdown:")
  print(unknown_pft)}

design = plant |>
  dplyr::filter(
    !is.na(.data$scenario_crop),
    !is.na(.data$pft_family))

#Subsample parcels for testing
n_parcels = config[["n_parcels"]]

if (!is.null(n_parcels) && n_parcels < dplyr::n_distinct(design$parcel_id)) {
  picked = design |>
    dplyr::distinct(.data$parcel_id) |>
    dplyr::slice_sample(n = n_parcels) |>
    dplyr::pull(.data$parcel_id)
  
  design = design |>
    dplyr::filter(.data$parcel_id %in% picked)
  
  logger_info("Sampled ", length(picked), " parcels using n_parcels = ", n_parcels)}

design = design |>
  dplyr::select(
    "parcel_id", "county", "year", "season", "anchor", "code", "PFT", "pft_family", "ACRES", "scenario",
    "scenario_crop", "scenario_crop_key")

if (nrow(design) == 0) {logger_severe("Design table has 0 rows after filtering.")}

logger_info("Design table: ",
  nrow(design), " cycles, ",
  dplyr::n_distinct(design$parcel_id), " parcels, ",
  dplyr::n_distinct(design$year), " years")

logger_info("PFT family split:")
print(table(design$pft_family))

logger_info("Scenario crop split:")
print(sort(table(design$scenario_crop), decreasing = TRUE))

design_file = file.path(staging_dir, "_staging_01_design.rds")
saveRDS(design, design_file)

logger_info("Wrote staging file: ", design_file)

##-----script 2, assign from scenario goal sheet instead of default %-----
if (!file.exists(design_file)) {logger_severe("Stage 01 output not found: ", design_file)}

logger_info("Reading design from ", design_file)
design = readRDS(design_file)

required_design_cols = c("parcel_id", "county", "year", "season", "anchor", "code", "PFT",
                         "pft_family","ACRES", "scenario", "scenario_crop", "scenario_crop_key")

missing_design_cols = setdiff(required_design_cols, names(design))

if (length(missing_design_cols) > 0) {
  logger_severe("Design file is missing required columns: ",
    paste(missing_design_cols, collapse = ", "))}

if (!file.exists(config[["scenarios_path"]])) {logger_severe("Scenario goals sheet not found: ", config[["scenarios_path"]])}

logger_info("Reading scenario goals from ", config[["scenarios_path"]])

goals_raw = read.csv(
  config[["scenarios_path"]],
  stringsAsFactors = FALSE)

##compost columns have csv formatted names which is why there are so many periods in those names 
required_goal_cols = c("Crop", "County", "Year", "Compost.acres..CPS.808.", 
                       "Compost.N..lbs.per.acre.", "Compost.C..lbs.per.acre.")

missing_goal_cols = setdiff(required_goal_cols, names(goals_raw))

if (length(missing_goal_cols) > 0) {
  logger_severe("Scenario sheet missing required columns: ",
    paste(missing_goal_cols, collapse = ", "))}

goals = goals_raw |>
  dplyr::transmute(
    scenario = config[["scenario"]],
    county = as.character(.data$County),
    year = as.integer(.data$Year),
    scenario_crop = as.character(.data$Crop),
    scenario_crop_key = normalize_crop_key(.data$Crop),
    target_compost_acres = as.numeric(.data$Compost.acres..CPS.808.),
    compost_n_lbs_acre = as.numeric(.data$Compost.N..lbs.per.acre.),
    compost_c_lbs_acre = as.numeric(.data$Compost.C..lbs.per.acre.)
  ) |>
  dplyr::filter(
    .data$year %in% config[["years"]],
    !is.na(.data$target_compost_acres),
    .data$target_compost_acres > 0,
    !is.na(.data$compost_n_lbs_acre),
    !is.na(.data$compost_c_lbs_acre)
  )

if (nrow(goals) == 0) {
  logger_severe("No usable compost goal rows found for scenario=", config[["scenario"]],
    " and years=",
    paste(config[["years"]], collapse = ","))}

logger_info("Loaded ", nrow(goals), " compost target rows from scenario sheet.")

design_targets = design |>
  dplyr::inner_join(
    goals,
    by = c("scenario", "county", "year", "scenario_crop_key"),
    suffix = c("", "_goal"))

if (nrow(design_targets) == 0) {logger_severe("No design rows matched scenario compost targets. ",
    "Check county/year/scenario_crop mapping.")}

unmatched_goals = goals |>
  dplyr::anti_join(design, by = c("scenario", "county", "year", "scenario_crop_key"))

if (nrow(unmatched_goals) > 0) {
  logger_warn(nrow(unmatched_goals), " compost goal rows had no matching projected parcels. Preview:")
  print(head(unmatched_goals, 20))}

logger_info("Matched ", nrow(design_targets), " parcel cycles to compost target rows.")

assign_to_target = function(df) {
  target = unique(df$target_compost_acres)
  
  if (length(target) != 1 || is.na(target) || target <= 0) {
    return(df[0, ])}
  
  df2 = df |>
    dplyr::slice_sample(prop = 1) |>
    dplyr::mutate(cum_acres = cumsum(.data$ACRES))
  
  df2 |>
    dplyr::filter(
      .data$cum_acres <= target |
        dplyr::lag(.data$cum_acres, default = 0) < target)}

n_ensemble = as.integer(config[["n_ensemble"]])

if (is.na(n_ensemble) || n_ensemble < 1) {logger_severe("n_ensemble must be a positive integer.")}

assign_events_fast = function(design_targets, n_ensemble) {
  dt = data.table::as.data.table(design_targets)
  
  out_list = vector("list", n_ensemble)
  
  group_cols = c("scenario", "county", "year", "scenario_crop_key")
  
  for (e in seq_len(n_ensemble)) {
    logger_info("Assigning ensemble ", e, " of ", n_ensemble)
    
    dt_e = data.table::copy(dt)
    
    # randomize parcel order within each target group
    dt_e[, rand := runif(.N)]
    data.table::setorderv(dt_e, c(group_cols, "rand"))
    
    # cumulative acres within each target group
    dt_e[, cum_acres := cumsum(ACRES), by = group_cols]
    dt_e[, prev_cum_acres := shift(cum_acres, fill = 0), by = group_cols]
    
    # keep parcels until target acres are reached
    dt_e = dt_e[
      cum_acres <= target_compost_acres |
        prev_cum_acres < target_compost_acres]
    
    dt_e[, ensemble_member := e]
    dt_e[, ens_id := sprintf("ens_%03d", e)]
    
    out_list[[e]] = dt_e}
  
  dplyr::as_tibble(data.table::rbindlist(out_list, use.names = TRUE, fill = TRUE))}

events = assign_events_fast(design_targets, n_ensemble)

if (nrow(events) == 0) {logger_severe("No NCC events assigned. Check scenario targets and crop matching.")}

target_check = events |>
  dplyr::group_by(
    .data$scenario,
    .data$county,
    .data$year,
    .data$scenario_crop,
    .data$ens_id
  ) |>
  dplyr::summarise(
    assigned_acres = sum(.data$ACRES, na.rm = TRUE),
    target_acres = dplyr::first(.data$target_compost_acres),
    diff_acres = assigned_acres - target_acres,
    .groups = "drop")

logger_info("Compost target assignment check:")
print(head(target_check, 20))
print(summary(target_check$diff_acres))

ANNUAL_OFFSET_MIN = 14L
ANNUAL_OFFSET_MAX = 180L
PERENNIAL_OFFSET_MIN = 30L
PERENNIAL_OFFSET_MAX = 210L

events = events |>
  dplyr::mutate(
    date_offset_days = ifelse(
      .data$pft_family == "annual",
      sample(ANNUAL_OFFSET_MIN:ANNUAL_OFFSET_MAX, dplyr::n(), replace = TRUE),
      sample(PERENNIAL_OFFSET_MIN:PERENNIAL_OFFSET_MAX, dplyr::n(), replace = TRUE)
    ),
    date = .data$anchor - .data$date_offset_days,
    material = "scenario_compost")

logger_info("Assigned ", nrow(events), " NCC compost events across ",
  dplyr::n_distinct(events$parcel_id), " parcels.")

logger_info("Events preview:")
print(head(events))
print(table(events$ens_id))

events_file = file.path(staging_dir, "_staging_02_events.rds")
saveRDS(events, events_file)

logger_info("Wrote staging file: ", events_file)

##-----script 3, write new files-----

if (!file.exists(events_file)) {logger_severe("Stage 02 output not found: ", events_file)}

logger_info("Reading events from ", events_file)
events = readRDS(events_file)

required_event_cols = c("parcel_id", "ens_id", "date", "code", "compost_n_lbs_acre", 
                        "compost_c_lbs_acre", "material")

missing_event_cols = setdiff(required_event_cols, names(events))

if (length(missing_event_cols) > 0) {
  logger_severe("Stage 02 events missing required columns: ",
    paste(missing_event_cols, collapse = ", "))}

out = events |>
  dplyr::mutate(
    # Scenario sheet gives compost N/C rates in lbs per acre.
    # Treat these as organic amendment pools.
    nh4_n_kg_m2 = 0,
    no3_n_kg_m2 = 0,
    org_n_kg_m2 = lb_acre_to_kg_m2(.data$compost_n_lbs_acre),
    org_c_kg_m2 = lb_acre_to_kg_m2(.data$compost_c_lbs_acre)
  ) |>
  dplyr::transmute(
    parcel_id = as.integer(.data$parcel_id),
    ens_id = .data$ens_id,
    date = as.Date(.data$date),
    nh4_n_kg_m2 = .data$nh4_n_kg_m2,
    no3_n_kg_m2 = .data$no3_n_kg_m2,
    org_c_kg_m2 = .data$org_c_kg_m2,
    org_n_kg_m2 = .data$org_n_kg_m2,
    crop_code = .data$code,
    material = .data$material
  )

if (nrow(out) == 0) {logger_severe("Stage 03 output table has 0 rows.")}

logger_info("Stage 03 output preview:")
print(head(out))
print(summary(out$org_n_kg_m2))
print(summary(out$org_c_kg_m2))
print(table(format(out$date, "%Y")))

out_path = config[["output_dir"]]
dir.create(out_path, showWarnings = FALSE, recursive = TRUE)

all_parcels = sort(unique(out[["parcel_id"]]))

batch_size = as.integer(config[["batch_size"]])

if (is.na(batch_size) || batch_size < 1) {logger_severe("batch_size must be a positive integer.")}

batches = split(all_parcels, ceiling(seq_along(all_parcels) / batch_size))

logger_info(sprintf("Writing %d rows across %d parcel batches to %s", nrow(out), length(batches), out_path))

if (!requireNamespace("arrow", quietly = TRUE)) {logger_severe("Package arrow is required to write parquet files.")}

parquet_codec = if (arrow::codec_is_available("zstd")) "ZSTD" else "SNAPPY"

logger_info("Parquet compression codec: ", parquet_codec)

write_batch = function(pids) {
  shard = out |>
    dplyr::filter(.data$parcel_id %in% pids)
  
  pid_min = min(shard[["parcel_id"]])
  pid_max = max(shard[["parcel_id"]])
  
  fn = file.path(out_path, sprintf("%d_%d.parquet", pid_min, pid_max))
  
  arrow::write_parquet(shard, fn, compression = parquet_codec)
  
  fn}

workers = as.integer(config[["workers"]])

if (is.na(workers) || workers < 1) {workers = 1L}

if (workers > 1) {
  logger_info(sprintf("Using mclapply with %d workers", workers))
  written = parallel::mclapply(batches, write_batch, mc.cores = workers)
} else {
  written = lapply(batches, write_batch)
}

logger_info(sprintf("Done. wrote %d shards, %d total rows, parcels=%d, years=%d, ensemble=%d",
  length(written),
  nrow(out),
  dplyr::n_distinct(out[["parcel_id"]]),
  dplyr::n_distinct(format(out[["date"]], "%Y")),
  dplyr::n_distinct(out[["ens_id"]])))