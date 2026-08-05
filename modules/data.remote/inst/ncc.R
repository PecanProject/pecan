pacman::p_load(PEcAn.logger, PEcAn.remote, PEcAn.utils, PEcAn.data.atmosphere, PEcAn.data.land,
               dplyr, tidyr, purrr, readr, data.table, arrow)
options(arrow.unsafe_metadata = TRUE)

source('/projectnb/dietzelab/ananyak/fertilization_functions.R')

scenario_variable = "NBS_Targets"

config = list(seed = 42, years = 2024:2045, scenario = scenario_variable, n_parcels = NULL, n_ensemble = 20, batch_size = 500, workers = 1,
              scenarios_path = sprintf("/projectnb/dietzelab/ananyak/MAGiC_scenarios_FINAL/%s.csv", scenario_variable),
              lookup_path = "/projectnb/dietzelab/ccmmf/management/LandIQ_cropCode_lookup_table.csv",
              projection_dir = "/projectnb/dietzelab/ananyak/county_landiq_predictions_with_phenology",
              output_dir = file.path("/projectnb/dietzelab/ananyak/ncc", scenario_variable))
set.seed(config[["seed"]])

staging_dir = file.path(config[["output_dir"]], "_staging")
dir.create(staging_dir, showWarnings = FALSE, recursive = TRUE)

##---- 1 Building design and N application rates ----
PEcAn.logger::logger.info("--- Starting Stage 01: Build Parcel Design ---")

scenario_dir = file.path(config[["projection_dir"]], config[["scenario"]])
if (!dir.exists(scenario_dir)) {
  PEcAn.logger::logger.severe("Scenario directory does not exist: ", scenario_dir)
}

projected_files = list.files(scenario_dir, pattern = "_predicted_2024_2045\\.csv$", full.names = TRUE)
if (length(projected_files) == 0) {
  PEcAn.logger::logger.severe("No projected county CSVs found in: ", scenario_dir)
}

PEcAn.logger::logger.info(sprintf("Found %d projected files under: %s", length(projected_files), scenario_dir))

plant = purrr::map_dfr(projected_files, read_projected_county)

if (nrow(plant) == 0) {
  PEcAn.logger::logger.severe("Projected files were found, but no valid rows survived filtering.")
}

ncc_crop_lookup = build_ncc_crop_lookup(config[["lookup_path"]])

plant = plant |>
  dplyr::left_join(ncc_crop_lookup, by = "code") |>
  dplyr::mutate(pft_family = pft_family(.data$PFT))

unmapped_codes = plant |>
  dplyr::filter(is.na(.data$scenario_crop)) |>
  dplyr::count(.data$code, .data$PFT, sort = TRUE)

if (nrow(unmapped_codes) > 0) {
  PEcAn.logger::logger.warn(sprintf("Dropping %d rows with no scenario crop mapping.", sum(unmapped_codes$n)))
}

unknown_pft = plant |>
  dplyr::filter(is.na(.data$pft_family)) |>
  dplyr::count(.data$PFT, sort = TRUE)

if (nrow(unknown_pft) > 0) {
  PEcAn.logger::logger.warn("Dropping rows with unknown PFT family.")
}

design = plant |>
  dplyr::filter(
    !is.na(.data$scenario_crop),
    !is.na(.data$pft_family)
  )

n_parcels = config[["n_parcels"]]
if (!is.null(n_parcels) && n_parcels < dplyr::n_distinct(design$parcel_id)) {
  picked = design |>
    dplyr::distinct(.data$parcel_id) |>
    dplyr::slice_sample(n = n_parcels) |>
    dplyr::pull(.data$parcel_id)
  
  design = design |> dplyr::filter(.data$parcel_id %in% picked)
  PEcAn.logger::logger.info(sprintf("Sampled %d parcels using n_parcels = %d", length(picked), n_parcels))
}

design = design |>
  dplyr::select("parcel_id", "county", "year", "season", "anchor", "code", "PFT", 
                "pft_family", "ACRES", "scenario", "scenario_crop", "scenario_crop_key")

if (nrow(design) == 0) {
  PEcAn.logger::logger.severe("Design table has 0 rows after filtering.")
}

PEcAn.logger::logger.info(sprintf(
  "Design table: %d cycles, %d parcels, %d years",
  nrow(design), dplyr::n_distinct(design$parcel_id), dplyr::n_distinct(design$year)
))

design_file = file.path(staging_dir, "_staging_01_design.rds")
saveRDS(design, design_file)
PEcAn.logger::logger.info("Wrote Stage 01 output: ", design_file)

##---- 2 Assign N&C from MAGiC scenario target sheet ----

PEcAn.logger::logger.info("--- Starting Stage 02: Assign Goals & Sample Events ---")

if (!file.exists(config[["scenarios_path"]])) {
  PEcAn.logger::logger.severe("Scenario goals sheet not found: ", config[["scenarios_path"]])
}

goals_raw = read.csv(config[["scenarios_path"]], stringsAsFactors = FALSE)

required_goal_cols = c("Crop", "County", "Year", "Compost.acres..CPS.808.", 
                       "Compost.N..lbs.per.acre.", "Compost.C..lbs.per.acre.")

missing_goal_cols = setdiff(required_goal_cols, names(goals_raw))
if (length(missing_goal_cols) > 0) {
  PEcAn.logger::logger.severe("Scenario sheet missing required columns: ", paste(missing_goal_cols, collapse = ", "))
}

# compost columns from the magic scenarios sheet are read with lots of periods for some reason, that is why 
# there are column names like Compost.N..lbs.per.acre.
goals = goals_raw |>
  dplyr::transmute(scenario = config[["scenario"]], county = as.character(.data$County), year = as.integer(.data$Year), 
    scenario_crop = as.character(.data$Crop), scenario_crop_key = normalize_crop_key(.data$Crop), 
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
  PEcAn.logger::logger.severe("No usable compost goal rows found.")
}

design_targets = design |>
  dplyr::inner_join(goals, by = c("scenario", "county", "year", "scenario_crop_key"), suffix = c("", "_goal"))

if (nrow(design_targets) == 0) {
  PEcAn.logger::logger.severe("No design rows matched scenario compost targets.")
}

n_ensemble = as.integer(config[["n_ensemble"]])
events = assign_events(design_targets, n_ensemble)

if (nrow(events) == 0) {
  PEcAn.logger::logger.severe("No NCC events assigned. Check scenario targets and crop matching.")
}

ANNUAL_OFFSET_MIN = 14L
ANNUAL_OFFSET_MAX = 180L
PERENNIAL_OFFSET_MIN = 30L
PERENNIAL_OFFSET_MAX = 210L

events = events |>
  dplyr::mutate(date_offset_days = ifelse(
      .data$pft_family == "annual",
      sample(ANNUAL_OFFSET_MIN:ANNUAL_OFFSET_MAX, dplyr::n(), replace = TRUE),
      sample(PERENNIAL_OFFSET_MIN:PERENNIAL_OFFSET_MAX, dplyr::n(), replace = TRUE)
    ),
    date = .data$anchor - .data$date_offset_days,
    material = "scenario_compost"
  )

events_file = file.path(staging_dir, "_staging_02_events.rds")
saveRDS(events, events_file)
PEcAn.logger::logger.info("Wrote Stage 02 output: ", events_file)

##---- 3 Unit Conversion & Parquet Export ----
PEcAn.logger::logger.info("--- Starting Stage 03: Export to Parquet ---")

required_event_cols = c("parcel_id", "ens_id", "date", "code", "compost_n_lbs_acre", 
                        "compost_c_lbs_acre", "material")
missing_event_cols = setdiff(required_event_cols, names(events))

if (length(missing_event_cols) > 0) {
  PEcAn.logger::logger.severe("Stage 02 events missing required columns: ", paste(missing_event_cols, collapse = ", "))
}

out = events |>
  dplyr::mutate(
    nh4_n_kg_m2 = 0,
    no3_n_kg_m2 = 0,
    org_n_kg_m2 = PEcAn.utils::ud_convert(.data$compost_n_lbs_acre, "lb/acre", "kg/m^2"),
    org_c_kg_m2 = PEcAn.utils::ud_convert(.data$compost_c_lbs_acre, "lb/acre", "kg/m^2")
  ) |>
  dplyr::transmute(
    parcel_id   = as.integer(.data$parcel_id),
    ens_id      = .data$ens_id,
    date        = as.Date(.data$date),
    nh4_n_kg_m2 = .data$nh4_n_kg_m2,
    no3_n_kg_m2 = .data$no3_n_kg_m2,
    org_c_kg_m2 = .data$org_c_kg_m2,
    org_n_kg_m2 = .data$org_n_kg_m2,
    crop_code   = .data$code,
    material    = .data$material
  )

if (nrow(out) == 0) {
  PEcAn.logger::logger.severe("Stage 03 output table has 0 rows.")
}

out_path = config[["output_dir"]]
dir.create(out_path, showWarnings = FALSE, recursive = TRUE)

existing = list.files(out_path, pattern = "\\.parquet$", full.names = TRUE)
if (length(existing) > 0) {
  PEcAn.logger::logger.info(sprintf("Removing %d existing parquet shards", length(existing)))
  unlink(existing)
}

all_parcels = sort(unique(out[["parcel_id"]]))
batch_size = as.integer(config[["batch_size"]])

if (is.na(batch_size) || batch_size < 1) {
  PEcAn.logger::logger.severe("batch_size must be a positive integer.")
}

batches = split(all_parcels, ceiling(seq_along(all_parcels) / batch_size))

PEcAn.logger::logger.info(sprintf("Writing %d rows across %d parcel batches to %s", nrow(out), length(batches), out_path))

parquet_codec = if (arrow::codec_is_available("zstd")) "ZSTD" else "SNAPPY"
PEcAn.logger::logger.info("Parquet compression codec: ", parquet_codec)

# Workers setup
workers = as.integer(config[["workers"]])
if (is.na(workers) || workers < 1) workers = 1L

# Calling write_batch() from fertilization_functions.R with explicit arguments
if (workers > 1) {
  PEcAn.logger::logger.info(sprintf("Using mclapply with %d workers", workers))
  written = parallel::mclapply(
    batches, 
    function(b) write_batch(pids = b, df = out, out_path = out_path, codec = parquet_codec), 
    mc.cores = workers
  )
} else {
  written = lapply(
    batches, 
    function(b) write_batch(pids = b, df = out, out_path = out_path, codec = parquet_codec)
  )
}

# Remove any NULL elements from empty shards before summarizing
written = compact(written)

PEcAn.logger::logger.info(sprintf(
  "Done. Wrote %d shards, %d total rows (parcels=%d, years=%d, ensemble=%d)",
  length(written), nrow(out),
  dplyr::n_distinct(out[["parcel_id"]]),
  dplyr::n_distinct(format(out[["date"]], "%Y")),
  dplyr::n_distinct(out[["ens_id"]])
))