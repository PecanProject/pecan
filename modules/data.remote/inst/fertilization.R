pacman::p_load(PEcAn.logger, PEcAn.remote, PEcAn.utils, PEcAn.data.atmosphere, PEcAn.data.land, 
               dplyr, tidyr, purrr, readr, data.table, arrow)

#source('/pecan/modules/data.remote/inst/fertilization_functions.R')
source('/projectnb/dietzelab/ananyak/fertilization_functions.R')

#scenario = NBS_Targets or BAU_Targets, corresponding to the defined magic scenarios spreadsheet   
config = list(seed = 42, scenario = "NBS_Targets", years = 2024:2045, n_parcels = 1000, n_ensemble = 20, nh4_fraction = 0.5, batch_size = 100, workers = 1,
              projection_dir = "/projectnb/dietzelab/ananyak/county_landiq_predictions_with_phenology", 
              crosswalk_path = "/projectnb/dietzelab/ccmmf/management/fertilization/CCMMF_Fertilization_Crop_types.tsv",
              output_dir = "/projectnb/dietzelab/ananyak/fertilization")

set.seed(config[["seed"]])

out_path = config[["output_dir"]]
dir.create(out_path, showWarnings = FALSE, recursive = TRUE)

##---- 1 Building design and N application rates ----

PEcAn.logger::logger.info("--- Starting Stage 01: Build Parcel Design ---")

if (!file.exists(config[["crosswalk_path"]])) {
  PEcAn.logger::logger.severe("crosswalk_path does not exist: ", config[["crosswalk_path"]])
}

crosswalk = readr::read_tsv(config[["crosswalk_path"]], show_col_types = FALSE) |>
  dplyr::select(landiq = "LandIQ Name", frep = "FREP Name", ucanr = "UC ANR")

ca_rates = PEcAn.data.land::ca_n_application_rate
code_map = PEcAn.data.land::landiq_crop_mapping_codes

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
  dplyr::left_join(xw_norm |> dplyr::select("key", "candidates"), by = "key") |> 
  dplyr::rowwise() |>
  dplyr::mutate(matched_crops = list(parse_candidates(.data$candidates, known_crops))) |> 
  dplyr::ungroup() |>
  dplyr::filter(lengths(.data$matched_crops) > 0) |> 
  dplyr::mutate(
    rates = lapply(.data$matched_crops, function(cc) {
      ca_rates |>
        dplyr::filter(.data$crop %in% cc) |>
        dplyr::summarize(
          min_n_lbs_acre = min(.data$min_n_lbs_acre, na.rm = TRUE), 
          max_n_lbs_acre = max(.data$max_n_lbs_acre, na.rm = TRUE) 
        )
    })
  ) |>
  tidyr::unnest("rates") |> 
  dplyr::select("code", "min_n_lbs_acre", "max_n_lbs_acre") |> 
  dplyr::distinct()

PEcAn.logger::logger.info(sprintf("Resolved %d LandIQ crop codes via crosswalk", nrow(code_lookup))) 

if (nrow(code_lookup) == 0) {PEcAn.logger::logger.severe("code_lookup has 0 rows. Crosswalk matching failed.")}

scenario_dir = file.path(config[["projection_dir"]], config[["scenario"]])
if (!dir.exists(scenario_dir)) {PEcAn.logger::logger.severe("Scenario directory does not exist: ", scenario_dir)}

projected_files = list.files(scenario_dir, pattern = "_predicted_2024_2045\\.csv$", full.names = TRUE)
if (length(projected_files) == 0) {PEcAn.logger::logger.severe("No projected county CSVs found in: ", scenario_dir)}

PEcAn.logger::logger.info(sprintf("Found %d projected files under: %s", length(projected_files), scenario_dir))

plant = purrr::map_dfr(projected_files, read_projected_county) 

if (nrow(plant) == 0) {PEcAn.logger::logger.severe("Projected files were found, but no valid rows survived filtering.")}

PEcAn.logger::logger.info(sprintf("Loaded %d projected cycles across %d parcels", nrow(plant), dplyr::n_distinct(plant$parcel_id))) 

n_parcels = config[["n_parcels"]]
if (!is.null(n_parcels) && n_parcels < dplyr::n_distinct(plant$parcel_id)) {
  picked = plant |>
    dplyr::distinct(.data$parcel_id) |>
    dplyr::slice_sample(n = n_parcels) |> 
    dplyr::pull(.data$parcel_id)
  
  plant = plant |> dplyr::filter(.data$parcel_id %in% picked)
  PEcAn.logger::logger.info(sprintf("Sampled %d parcels using n_parcels = %d", length(picked), n_parcels)) 
}

design = plant |>
  dplyr::left_join(code_lookup, by = "code") |> 
  dplyr::mutate(
    rate_source = dplyr::case_when(
      is.na(.data$min_n_lbs_acre) ~ "skip_no_rate", 
      .data$min_n_lbs_acre == 0 & .data$max_n_lbs_acre == 0 ~ "skip_zero_envelope", 
      TRUE ~ "crosswalk" 
    )
  )

unresolved = design |> dplyr::filter(.data$rate_source == "skip_no_rate")

if (nrow(unresolved) > 0) {
  PEcAn.logger::logger.warn(sprintf(
    "Dropping %d cycles across %d codes with no resolvable N rate.",
    nrow(unresolved), dplyr::n_distinct(unresolved$code)
  )) 
}

zero_env = design |> dplyr::filter(.data$rate_source == "skip_zero_envelope")
if (nrow(zero_env) > 0) {
  PEcAn.logger::logger.info(sprintf(
    "Dropping %d cycles across %d codes with cited 0-to-0 N rate.",
    nrow(zero_env), dplyr::n_distinct(zero_env$code)
  )) 
}

# Added 'anchor' back into the select statement here!
design = design |>
  dplyr::filter(.data$rate_source == "crosswalk") |>
  dplyr::select("parcel_id", "year", "season", "anchor", "code", "min_n_lbs_acre", "max_n_lbs_acre") 

if (nrow(design) == 0) {
  PEcAn.logger::logger.severe("Design table has 0 rows after attaching N rates.")
}

PEcAn.logger::logger.info(sprintf("Design table created: %d events across %d parcels", 
                                  nrow(design), dplyr::n_distinct(design$parcel_id))) 


##---- 2 Sample N Rates Across Ensemble Members ----

PEcAn.logger::logger.info("--- Starting Stage 02: Sample N Rates ---")

n_ensemble = config[["n_ensemble"]]
PEcAn.logger::logger.info(sprintf("Sampling %d ensemble members across %d design rows", n_ensemble, nrow(design))) 

events = design |>
  tidyr::crossing(ensemble_member = seq_len(n_ensemble)) |> 
  dplyr::mutate(
    annual_n_lb_acre = stats::runif(dplyr::n(), min = .data$min_n_lbs_acre, max = .data$max_n_lbs_acre), 
    ens_id = sprintf("ens_%03d", .data$ensemble_member) )

PEcAn.logger::logger.info(sprintf("Sampled %d events across ensembles.", nrow(events))) 

##---- 3 Unit Conversions & Parquet Export ----
PEcAn.logger::logger.info("--- Starting Stage 03: Unit Conversion & Parquet Export ---")

nh4_frac = config[["nh4_fraction"]]
if (is.null(nh4_frac) || !is.numeric(nh4_frac) || nh4_frac < 0 || nh4_frac > 1) {
  PEcAn.logger::logger.severe("nh4_fraction must be a number between 0 and 1.")
}

out = events |>
  dplyr::mutate(
    # Using PEcAn.utils::ud_convert directly
    total_n_kg_m2 = PEcAn.utils::ud_convert(.data$annual_n_lb_acre, "lb/acre", "kg/m^2"), 
    nh4_n_kg_m2 = .data$total_n_kg_m2 * nh4_frac, 
    no3_n_kg_m2 = .data$total_n_kg_m2 * (1 - nh4_frac), 
    org_c_kg_m2 = 0, 
    org_n_kg_m2 = 0 
  ) |>
  dplyr::transmute(
    parcel_id   = as.integer(.data$parcel_id), 
    ens_id      = .data$ens_id, 
    date        = as.Date(.data$anchor), 
    nh4_n_kg_m2 = .data$nh4_n_kg_m2, 
    no3_n_kg_m2 = .data$no3_n_kg_m2, 
    org_c_kg_m2 = .data$org_c_kg_m2, 
    org_n_kg_m2 = .data$org_n_kg_m2, 
    crop_code   = .data$code 
  )

if (nrow(out) == 0) {PEcAn.logger::logger.severe("Final output table has 0 rows.")}

existing = list.files(out_path, pattern = "\\.parquet$", full.names = TRUE) 
if (length(existing) > 0) {
  PEcAn.logger::logger.info(sprintf("Removing %d existing parquet shards", length(existing))) 
  unlink(existing) 
}

all_parcels = sort(unique(out[["parcel_id"]])) 
batch_size  = config[["batch_size"]]
if (is.null(batch_size)) PEcAn.logger::logger.severe("config[['batch_size']] is missing.")

n_batches = ceiling(length(all_parcels) / batch_size) 
batches   = split(all_parcels, ceiling(seq_along(all_parcels) / batch_size)) 

PEcAn.logger::logger.info(sprintf("Writing %d rows across %d parcel batches (batch_size=%d) to %s", 
                                  nrow(out), n_batches, batch_size, out_path)) 

parquet_codec = if (arrow::codec_is_available("zstd")) "ZSTD" else "SNAPPY" 
PEcAn.logger::logger.info("Parquet compression codec: ", parquet_codec) 

workers = as.integer(config[["workers"]])
if (is.na(workers) || workers < 1) workers = 1L

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

written = purrr::compact(written)

PEcAn.logger::logger.info(sprintf(
  "Finished successfully! Wrote %d shards across %d rows (parcels=%d, years=%d, ensemble=%d)",
  length(written), nrow(out),
  dplyr::n_distinct(out[["parcel_id"]]),
  dplyr::n_distinct(format(out[["date"]], "%Y")),
  dplyr::n_distinct(out[["ens_id"]])
))