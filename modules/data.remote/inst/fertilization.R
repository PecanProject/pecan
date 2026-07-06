#modified files from fertilization statewide part of the pull request 4003

#after future_landiq is created join scenario compost targets by county/crop/year/scenario
#assign compost to enough parcels so parcel ACRES sums to Compost acres, then attach Compost N and 
#Compost C rates and write this as an NCC/amendment scenario input

#replace the Inventory input source in 01-build-parcel-design.R
#need to switch the input from historical matched LandIQ + MSLSP parquet to ur county_landiq_predictions_with_phenology files

setwd("/projectnb/dietzelab/ananyak")

library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(purrr)
library(data.table)

#scenario = either bau or nbs targets 
config = list(seed = 42, scenario = "NBS_Targets",years = 2024:2045, 
              n_parcels = 1000, n_ensemble = 20, nh4_fraction = 0.5,
              batch_size = 100, workers = 1,
              projection_dir = "/projectnb/dietzelab/ananyak/county_landiq_predictions_with_phenology",
              crosswalk_path = "/projectnb/dietzelab/ccmmf/management/fertilization/CCMMF_Fertilization_Crop_types.tsv",
              output_dir = "/projectnb/dietzelab/ananyak/fertilization")

set.seed(config[["seed"]])

staging_dir = file.path(config[["output_dir"]], "_staging")
dir.create(staging_dir, showWarnings = FALSE, recursive = TRUE)

##-----script 1-----
#logger replacements for pecan package parts 
logger_info = function(...) message(paste0(...))
logger_warn = function(...) warning(paste0(...), call. = FALSE)
logger_severe = function(...) stop(paste0(...), call. = FALSE)


normalize_name = function(s) {
  s |>
    tolower() |>
    stringr::str_replace_all("\\(.*?\\)", "") |>
    stringr::str_replace_all("grouped for remote sensing only", "") |>
    stringr::str_replace_all("\\bor\\b", "") |>
    stringr::str_replace_all("\\band\\b", "") |>
    stringr::str_replace_all("&", "") |>
    stringr::str_replace_all("[[:punct:]]", " ") |>
    stringr::str_squish()
}

parse_candidates = function(s, known) {
  if (is.na(s) || nchar(s) == 0) return(character(0))
  
  parts = stringr::str_split(s, " ?/ ?")[[1]] |>
    stringr::str_squish()
  
  prefix = if (stringr::str_detect(parts[1], ",")) {
    stringr::str_extract(parts[1], "^[^,]+, ")
  } else {
    ""
  }
  
  result = character()
  
  for (p in parts) {
    if (tolower(p) %in% tolower(known)) {
      result = c(result, p)
    } else if (nchar(prefix) > 0) {
      combined = paste0(prefix, p)
      if (tolower(combined) %in% tolower(known)) {
        result = c(result, combined)
      }
    }
  }
  
  unique(result)
}

logger_info("Loading crosswalk and bundled N-rate lookup tables")

if (!file.exists(config[["crosswalk_path"]])) {
  logger_severe("crosswalk_path does not exist: ", config[["crosswalk_path"]])}

crosswalk = readr::read_tsv(config[["crosswalk_path"]], show_col_types = FALSE) |>
  dplyr::select(landiq = "LandIQ Name", frep = "FREP Name", ucanr = "UC ANR")

ca_rates = fread(file.path('/projectnb/dietzelab/ccmmf/management/fertilization/n_application_rates.csv'))
code_map = fread(file.path('/projectnb/dietzelab/ccmmf/management/LandIQ_cropCode_lookup_table.csv'))
code_map = code_map %>% rename(subclass_name = SUBCLASS_desc)


required_rate_cols = c("crop", "min_n_lbs_acre", "max_n_lbs_acre")
missing_rate_cols = setdiff(required_rate_cols, names(ca_rates))
if (length(missing_rate_cols) > 0) {
  logger_severe("ca_rates missing columns: ", paste(missing_rate_cols, collapse = ", "))}

required_code_cols = c("CLASS", "SUBCLASS", "subclass_name")
missing_code_cols = setdiff(required_code_cols, names(code_map))
if (length(missing_code_cols) > 0) {
  logger_severe("code_map missing columns: ", paste(missing_code_cols, collapse = ", "))}


known_crops = ca_rates$crop

xw_norm = crosswalk |>
  dplyr::mutate(key = normalize_name(.data$landiq), candidates = dplyr::coalesce(.data$ucanr, .data$frep))

code_lookup = code_map |>
  dplyr::mutate(
    CLASS = as.character(.data$CLASS),
    SUBCLASS = as.character(.data$SUBCLASS),
    code = paste0(.data$CLASS, .data$SUBCLASS),
    key = normalize_name(.data$subclass_name)
  ) |>
  dplyr::left_join(
    xw_norm |> dplyr::select("key", "candidates"),
    by = "key"
  ) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    matched_crops = list(parse_candidates(.data$candidates, known_crops))
  ) |>
  dplyr::ungroup() |>
  dplyr::filter(lengths(.data$matched_crops) > 0) |>
  dplyr::mutate(
    rates = lapply(.data$matched_crops, function(cc) {
      ca_rates |>
        dplyr::filter(.data$crop %in% cc) |>
        dplyr::summarize(min_n_lbs_acre = min(.data$min_n_lbs_acre, na.rm = TRUE),
                         max_n_lbs_acre = max(.data$max_n_lbs_acre, na.rm = TRUE))
    })
  ) |>
  tidyr::unnest("rates") |>
  dplyr::select("code", "min_n_lbs_acre", "max_n_lbs_acre") |>
  dplyr::distinct()

logger_info("Resolved ", nrow(code_lookup), " LandIQ crop codes via crosswalk")

if (nrow(code_lookup) == 0) {
  logger_severe("code_lookup has 0 rows. Crosswalk matching failed.")
}


read_projected_county = function(fn) {
  logger_info("Reading: ", basename(fn))
  
  dt = readr::read_csv(fn, show_col_types = FALSE)
  
  # Make script robust to missing season
  if (!"season" %in% names(dt)) {
    dt$season = 0L
  }
  
  required_cols = c("parcel_id", "year", "CLASS", "SUBCLASS", "planting_date")
  missing_cols = setdiff(required_cols, names(dt))
  if (length(missing_cols) > 0) {
    logger_severe(
      "Projected file is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      "\nFile: ", fn,
      "\nAvailable columns: ", paste(names(dt), collapse = ", "))}
  
  dt |>
    dplyr::filter(
      .data$year %in% config[["years"]],
      !is.na(.data$CLASS),
      !is.na(.data$SUBCLASS),
      !is.na(.data$planting_date)
    ) |>
    dplyr::transmute(parcel_id = as.integer(.data$parcel_id), year = as.integer(.data$year),
                     season = as.integer(dplyr::coalesce(.data$season, 0L)),
                     date = as.Date(.data$planting_date),
                     code = paste0(as.character(.data$CLASS), as.character(.data$SUBCLASS)))}

scenario_dir = file.path(config[["projection_dir"]], config[["scenario"]])

if (!dir.exists(scenario_dir)) {logger_severe("Scenario directory does not exist: ", scenario_dir)}

projected_files = list.files(scenario_dir, pattern = "_predicted_2024_2045\\.csv$", full.names = TRUE)

if (length(projected_files) == 0) {logger_severe("No projected county CSVs found in: ", scenario_dir)}

logger_info("Found ", length(projected_files), " projected files under: ", scenario_dir)

plant = purrr::map_dfr(projected_files, read_projected_county)

if (nrow(plant) == 0) {
  logger_severe("Projected files were found, but no valid rows survived filtering.\n",
    "Check years, CLASS, SUBCLASS, and planting_date.")}

logger_info("Loaded ", nrow(plant), " projected cycles across ",
  dplyr::n_distinct(plant$parcel_id)," parcels")

logger_info("Plant table preview:")
print(head(plant))
print(summary(plant$date))
print(table(format(plant$date, "%Y")))

n_parcels = config[["n_parcels"]]

if (!is.null(n_parcels) && n_parcels < dplyr::n_distinct(plant$parcel_id)) {
  picked = plant |>
    dplyr::distinct(.data$parcel_id) |>
    dplyr::slice_sample(n = n_parcels) |>
    dplyr::pull(.data$parcel_id)
  
  plant = plant |>
    dplyr::filter(.data$parcel_id %in% picked)
  
  logger_info("Sampled ", length(picked), " parcels using n_parcels = ", n_parcels)}


design = plant |>
  dplyr::left_join(code_lookup, by = "code") |>
  dplyr::mutate(
    rate_source = dplyr::case_when(
      is.na(.data$min_n_lbs_acre) ~ "skip_no_rate",
      .data$min_n_lbs_acre == 0 & .data$max_n_lbs_acre == 0 ~ "skip_zero_envelope",
      TRUE ~ "crosswalk"))

#Log missing rates
unresolved = design |>
  dplyr::filter(.data$rate_source == "skip_no_rate")

if (nrow(unresolved) > 0) {
  by_code = unresolved |>
    dplyr::count(.data$code, name = "n_events", sort = TRUE) |>
    head(15)
  
  logger_warn("Dropping ", nrow(unresolved),
    " cycles across ", dplyr::n_distinct(unresolved$code),
    " codes with no resolvable N rate. Top offenders:")
  
  print(by_code)}

#Log intentional zero envelopes
zero_env = design |>
  dplyr::filter(.data$rate_source == "skip_zero_envelope")

if (nrow(zero_env) > 0) {
  by_code = zero_env |>
    dplyr::count(.data$code, name = "n_events", sort = TRUE) |>
    head(15)
  
  logger_info("Dropping ", nrow(zero_env),
    " cycles across ", dplyr::n_distinct(zero_env$code),
    " codes with cited 0-to-0 N rate.")
  
  print(by_code)}

design = design |>
  dplyr::filter(.data$rate_source == "crosswalk") |>
  dplyr::select(
    "parcel_id", "year", "season", "date", "code", "min_n_lbs_acre", "max_n_lbs_acre")

if (nrow(design) == 0) {
  logger_severe("Design table has 0 rows after attaching N rates.\n",
    "Most likely your projected CLASS/SUBCLASS codes do not match code_lookup.")}

logger_info("Design table: ",
  nrow(design), " events, ",
  dplyr::n_distinct(design$parcel_id), " parcels, ",
  dplyr::n_distinct(design$year), " years")

logger_info("Design preview:")
print(head(design))
print(summary(design$date))
print(summary(design$min_n_lbs_acre))
print(summary(design$max_n_lbs_acre))

staging_file = file.path(staging_dir, "_staging_01_design.rds")
saveRDS(design, staging_file)

logger_info("Wrote staging file: ", staging_file)

##-----script 2-----

logger_info = function(...) message(paste0(...))
logger_severe = function(...) stop(paste0(...), call. = FALSE)

staging_dir = file.path(config[["output_dir"]], "_staging")
design_file = file.path(staging_dir, "_staging_01_design.rds")

if (!file.exists(design_file)) {
  logger_severe("Stage 01 output not found: ", design_file,". Run fertilization_projection_design.R first.")}

logger_info("Reading design file: ", design_file)

design = readRDS(design_file)

required_cols = c("parcel_id", "year", "season", "date", "code", "min_n_lbs_acre", "max_n_lbs_acre")

missing_cols = setdiff(required_cols, names(design))

if (length(missing_cols) > 0) {logger_severe("Design file is missing required columns: ",
    paste(missing_cols, collapse = ", "))}

if (nrow(design) == 0) {logger_severe("Design file has 0 rows.")}

n_ensemble = config[["n_ensemble"]]

logger_info(sprintf("Sampling %d ensemble members across %d design rows",
  n_ensemble, nrow(design)))

events = design |>
  tidyr::crossing(ensemble_member = seq_len(n_ensemble)) |>
  dplyr::mutate(
    annual_n_lb_acre = stats::runif(
      dplyr::n(),
      min = .data$min_n_lbs_acre,
      max = .data$max_n_lbs_acre
    ),
    ens_id = sprintf("ens_%03d", .data$ensemble_member))

logger_info(sprintf("Sampled %d events. annual N range: %.2f to %.2f lb/acre",
  nrow(events),
  min(events$annual_n_lb_acre, na.rm = TRUE), max(events$annual_n_lb_acre, na.rm = TRUE)))

logger_info("Events preview:")
print(head(events))
print(summary(events$annual_n_lb_acre))
print(table(events$ens_id))

staging_file = file.path(staging_dir, "_staging_02_events.rds")
saveRDS(events, staging_file)

logger_info("Wrote staging file: ", staging_file)

##-----script 3-----
events_file = file.path(staging_dir, "_staging_02_events.rds")

if (!file.exists(events_file)) {
  logger_severe("Stage 02 output not found: ", events_file,
    ". Run Stage 02 first.")}

logger_info("Reading events from: ", events_file)
events = readRDS(events_file)

required_event_cols = c("parcel_id", "date", "code", "annual_n_lb_acre", "ens_id")

missing_event_cols = setdiff(required_event_cols, names(events))

if (length(missing_event_cols) > 0) {logger_severe("Stage 02 events missing required columns: ",
    paste(missing_event_cols, collapse = ", "))}

nh4_frac = config[["nh4_fraction"]]

if (is.null(nh4_frac)) {logger_severe("config[['nh4_fraction']] is missing.")}

if (!is.numeric(nh4_frac) || nh4_frac < 0 || nh4_frac > 1) {
  logger_severe("nh4_fraction must be a number between 0 and 1.")}

lb_acre_to_kg_m2 = function(x) {x * 0.45359237 / 4046.8564224}

out = events |>
  dplyr::mutate(
    total_n_kg_m2 = lb_acre_to_kg_m2(.data$annual_n_lb_acre),
    nh4_n_kg_m2 = .data$total_n_kg_m2 * .env$nh4_frac,
    no3_n_kg_m2 = .data$total_n_kg_m2 * (1 - .env$nh4_frac),
    org_c_kg_m2 = 0,
    org_n_kg_m2 = 0
  ) |>
  dplyr::transmute(
    parcel_id = as.integer(.data$parcel_id),
    ens_id = .data$ens_id,
    date = as.Date(.data$date),
    nh4_n_kg_m2 = .data$nh4_n_kg_m2,
    no3_n_kg_m2 = .data$no3_n_kg_m2,
    org_c_kg_m2 = .data$org_c_kg_m2,
    org_n_kg_m2 = .data$org_n_kg_m2,
    crop_code = .data$code)

if (nrow(out) == 0) {logger_severe("Stage 03 output table has 0 rows.")}

logger_info("Stage 03 output preview:")
print(head(out))
print(summary(out$nh4_n_kg_m2))
print(summary(out$no3_n_kg_m2))
print(table(format(out$date, "%Y")))

out_path = config[["output_dir"]]
dir.create(out_path, showWarnings = FALSE, recursive = TRUE)

#Clean prior parquet shards only, not staging RDS files
existing = list.files(out_path, pattern = "\\.parquet$", full.names = TRUE)

if (length(existing) > 0) {
  logger_info(sprintf("Removing %d existing parquet shards", length(existing)))
  unlink(existing)}

all_parcels = sort(unique(out[["parcel_id"]]))

batch_size = config[["batch_size"]]

if (is.null(batch_size)) {logger_severe("config[['batch_size']] is missing.")}

n_batches = ceiling(length(all_parcels) / batch_size)
batches = split(all_parcels, ceiling(seq_along(all_parcels) / batch_size))

logger_info(sprintf("Writing %d rows across %d parcel batches (batch_size=%d) to %s", 
  nrow(out), n_batches, batch_size, out_path))

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
  
  fn
}

workers = as.integer(config[["workers"]])

if (is.na(workers) || workers < 1) {workers = 1L}

if (workers > 1) {
  logger_info(sprintf("Using mclapply with %d workers", workers))
  written = parallel::mclapply(batches, write_batch, mc.cores = workers)
} else {
  written = lapply(batches, write_batch)}

logger_info(sprintf("Done. wrote %d shards, %d total rows, parcels=%d, years=%d, ensemble=%d",
  length(written), nrow(out),
  dplyr::n_distinct(out[["parcel_id"]]),
  dplyr::n_distinct(format(out[["date"]], "%Y")),
  dplyr::n_distinct(out[["ens_id"]])))




