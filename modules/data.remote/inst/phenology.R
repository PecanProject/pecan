## Projects future leaf-on / leaf-off dates using historical phenology.
## Phenology is conditional on county, crop class, and cover-crop status.
## BAU and NBS are projected separately because cover-crop assignments differ.

pacman::p_load(data.table, arrow, bit64)

# ---- setup ----
work_root = Sys.getenv("PROJECTION_WORK_ROOT")
phenology_dir = Sys.getenv("PROJ_PHENOLOGY_DIR")
matched_dir = Sys.getenv("PROJ_MATCHED_PHENO_DIR")
crops_path = Sys.getenv("PROJ_CROPS_PATH")

if (!nzchar(work_root)) {stop("PROJECTION_WORK_ROOT is not set. Source setup_projection_env.sh first.")
}

if (!nzchar(phenology_dir)) {stop("PROJ_PHENOLOGY_DIR is not set. Source setup_projection_env.sh first.")
}

if (!nzchar(matched_dir)) {stop("PROJ_MATCHED_PHENO_DIR is not set. Source setup_projection_env.sh first.")
}

if (!nzchar(crops_path)) {stop("PROJ_CROPS_PATH is not set. Source setup_projection_env.sh first.")
}

config = list(historical_years = 2016:2023, prediction_years = 2024:2045,
  scenario_names = c("BAU_Targets", "NBS_Targets"),
  
  #Shared projection inputs
  event_dir = phenology_dir,
  matched_dir = matched_dir,
  landiq_identity_path = crops_path,
  
  #Outputs from previous projection step
  prediction_dir = file.path(work_root, "crop_predictions"),
  
  #Outputs from this script
  output_dir = file.path(work_root, "phenology_projections"))

dir.create(config$output_dir, recursive = TRUE, showWarnings = FALSE)

## ---- helpers ----
safe_mean = function(x) {
  x = as.numeric(x)
  x = x[is.finite(x)]
  if (!length(x)) NA_real_ else mean(x)
}

date_offset = function(date, year) {
  as.numeric(as.Date(date) - as.Date(paste0(as.integer(year), "-01-01")))
}

offset_to_date = function(year, offset) {
  as.IDate(as.Date(paste0(as.integer(year), "-01-01")) + as.integer(round(offset)))
}

read_pheno_events = function(path, yy) {
  p = as.data.table(read_parquet(path))
  id_col = if ("parcel_id" %in% names(p)) "parcel_id" else if ("site_id" %in% names(p)) "site_id" else {
    stop("Phenology file has neither parcel_id nor site_id: ", path)
  }
  
  if (all(c("event_type", "date") %in% names(p))) {
    p = p[event_type %chin% c("leafon", "leafoff"),
          .(parcel_id = bit64::as.integer64(as.character(get(id_col))),
            event_type = as.character(event_type), date = as.IDate(date))]
    
    dup = p[, .N, by = .(parcel_id, event_type)][N > 1]
    if (nrow(dup)) stop("Multiple leafon/leafoff events found for the same parcel in ", path)
    
    p = dcast(p, parcel_id ~ event_type, value.var = "date")
    if (!"leafon" %in% names(p)) p[, leafon := as.IDate(NA)]
    if (!"leafoff" %in% names(p)) p[, leafoff := as.IDate(NA)]
    setnames(p, c("leafon", "leafoff"), c("leafonday", "leafoffday"))
    
  } else if (all(c("leafonday", "leafoffday") %in% names(p))) {
    p = p[, .(
      parcel_id = bit64::as.integer64(as.character(get(id_col))),
      leafonday = as.IDate(leafonday),
      leafoffday = as.IDate(leafoffday)
    )]
  } else {
    stop("Unrecognized phenology schema in ", path,
         ". Expected event_type/date or leafonday/leafoffday.")
  }
  
  p[, year := as.integer(yy)]
  p[]
}

# ---- fixed parcel county from authoritative LandIQ v4.1.2 ----
if (!file.exists(config$landiq_identity_path)) {
  stop("Missing LandIQ identity file: ", config$landiq_identity_path)
}

landiq_ds = arrow::read_parquet(config$landiq_identity_path, as_data_frame = FALSE)

landiq_county = data.table::as.data.table(
  landiq_ds |>
    dplyr::filter(.data$year <= 2023, !is.na(.data$COUNTY)) |>
    dplyr::select(parcel_id, year, COUNTY) |>
    dplyr::collect()
)

landiq_names = names(landiq_ds)

if (!"COVER" %in% landiq_names) {
  stop("LandIQ identity file is missing required COVER flag.")
}

cover_col = "COVER"

landiq_cycle = data.table::as.data.table(
  landiq_ds |>
    dplyr::filter(.data$year <= 2023) |>
    dplyr::select(parcel_id, year, season, dplyr::all_of(cover_col)
    ) |>
    dplyr::collect()
)

if (cover_col != "COVER") {
  setnames(landiq_cycle, cover_col, "COVER")
}

landiq_cycle[, `:=`(
  parcel_id = bit64::as.integer64(as.character(parcel_id)), year = as.integer(year),
  season = as.integer(season), COVER = as.integer(as.numeric(COVER) > 0)
)]

landiq_cycle = landiq_cycle[
  ,
  .(COVER = as.integer(any(COVER > 0, na.rm = TRUE))),
  by = .(parcel_id, year, season)
]

landiq_county[, `:=`(
  parcel_id = bit64::as.integer64(as.character(parcel_id)), year = as.integer(year), COUNTY = as.character(COUNTY)
)]

# Match the exact fixed-county logic used by crop projection
parcel_county = landiq_county[
  !is.na(COUNTY),
  .SD[which.max(year)],
  by = parcel_id
][, .(
  parcel_id,
  county = COUNTY
)]

# ---- historical derived phenology + matched LandIQ crop class ----
pheno_hist = rbindlist(lapply(config$historical_years, function(yy) {
  pheno_file = file.path(config$event_dir, paste0("phenology_statewide_", yy, ".parquet"))
  matched_file = file.path(config$matched_dir, paste0("assigned_year=", yy, "_gapfilled.parquet"))
  
  if (!file.exists(pheno_file)) stop("Missing phenology file: ", pheno_file)
  if (!file.exists(matched_file)) stop("Missing matched phenology file: ", matched_file)
  
  p = read_pheno_events(pheno_file, yy)
  m = as.data.table(read_parquet(matched_file))
  
  required_m = c("parcel_id", "season", "landiq_CLASS", "mslsp_50PCGI", "mslsp_50PCGD")

  missing_m = setdiff(required_m, names(m))
  if (length(missing_m)) stop("Matched phenology product missing: ", paste(missing_m, collapse = ", "))
  
  m = unique(m[, .(
    parcel_id = bit64::as.integer64(as.character(parcel_id)), year = as.integer(yy),
    season = as.integer(season), CLASS = as.character(landiq_CLASS), leafonday = as.IDate(mslsp_50PCGI),
    leafoffday = as.IDate(mslsp_50PCGD)
  )])
  
  x = merge(p, m, by = c("parcel_id", "year", "leafonday", "leafoffday"), all.x = TRUE)
  
  x
}), fill = TRUE)

message("Historical phenology rows matched to crop class: ", format(pheno_hist[!is.na(CLASS), .N], big.mark = ","), " of ",
  format(nrow(pheno_hist), big.mark = ","))

pheno_hist = merge(pheno_hist, landiq_cycle, by = c("parcel_id", "year", "season"), all.x = TRUE)

if (pheno_hist[!is.na(CLASS) & is.na(COVER), .N]) {
  stop("Historical phenology rows matched to crop class but missing COVER status.")
}

pheno_hist = merge(pheno_hist, parcel_county, by = "parcel_id", all.x = TRUE)

pheno_hist[, `:=`(
  leafon_offset = date_offset(leafonday, year), leafoff_offset = date_offset(leafoffday, year)
)]

# ---- historical phenology lookup hierarchy ----

phenology_lookup = pheno_hist[
  !is.na(county) & !is.na(CLASS) & !is.na(COVER),
  .(
    leafon_offset = safe_mean(leafon_offset), leafoff_offset = safe_mean(leafoff_offset)
  ),
  by = .(county, CLASS, COVER)
]

phenology_class_fallback = pheno_hist[
  !is.na(CLASS) & !is.na(COVER),
  .(
    class_leafon_offset = safe_mean(leafon_offset), class_leafoff_offset = safe_mean(leafoff_offset)
  ),
  by = .(CLASS, COVER)
]

phenology_county_fallback = pheno_hist[
  !is.na(county) & !is.na(COVER),
  .(
    county_leafon_offset = safe_mean(leafon_offset), county_leafoff_offset = safe_mean(leafoff_offset)
  ),
  by = .(county, COVER)
]

phenology_global_fallback = pheno_hist[
  !is.na(COVER),
  .(
    global_leafon_offset = safe_mean(leafon_offset), global_leafoff_offset = safe_mean(leafoff_offset)
  ),
  by = COVER
]

# ---- scenario-specific future projection ----

for (scen in config$scenario_names) {
  
  crop_dir = file.path(config$prediction_dir, scen)
  output_dir = file.path(config$output_dir, scen)
  
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  for (yy in config$prediction_years) {
    
    crop_file = file.path(crop_dir, paste0("crop_identity_statewide_", yy, ".parquet"))
    
    if (!file.exists(crop_file)) {
      stop("Missing projected crop file: ", crop_file)
    }
    
    future = as.data.table(read_parquet(crop_file))
    
    required = c("parcel_id", "COUNTY", "year", "season", "CLASS", "COVER")
    
    missing = setdiff(required, names(future))
    if (length(missing)) {
      stop("Projected crop identity missing: ", paste(missing, collapse = ", "))
    }
    
    future = future[, .(
      parcel_id = bit64::as.integer64(as.character(parcel_id)), county = as.character(COUNTY), year = as.integer(year),
      season = as.integer(season), CLASS = as.character(CLASS), COVER = as.integer(as.numeric(COVER) > 0)
    )]
    
    dup = future[, .N, by = .(parcel_id, year, season)][N > 1L]
    if (nrow(dup)) {
      stop(scen, " ", yy, " has duplicate parcel-year-season crop rows.")
    }
    
    pred = merge(future, phenology_lookup, by = c("county", "CLASS", "COVER"), all.x = TRUE)
    
    pred = merge(pred, phenology_class_fallback, by = c("CLASS", "COVER"), all.x = TRUE)
    
    pred = merge(pred, phenology_county_fallback, by = c("county", "COVER"), all.x = TRUE)
    
    pred = merge(pred, phenology_global_fallback, by = "COVER", all.x = TRUE)
    
    pred[, `:=`(
      leafon_offset = fcoalesce(leafon_offset, class_leafon_offset, county_leafon_offset, global_leafon_offset
      ),
      
      leafoff_offset = fcoalesce(leafoff_offset, class_leafoff_offset, county_leafoff_offset, global_leafoff_offset
      )
    )]
    
    pred[, `:=`(
      leafonday = offset_to_date(year, leafon_offset), leafoffday = offset_to_date(year, leafoff_offset)
    )]
    
    if (pred[is.na(leafonday) | is.na(leafoffday), .N]) {
      stop(scen, " ", yy, " has crop cycles missing projected phenology.")
    }
    
    pheno_year = pred[, .(
      parcel_id, leafonday, leafoffday
    )]
    
    pheno_year[, parcel_id := bit64::as.integer64(parcel_id)]
    
    setorder(pheno_year, parcel_id, leafonday)
    
    out_path = file.path(output_dir, paste0("phenology_statewide_", yy, ".parquet"))
    
    write_parquet(pheno_year, out_path, compression = "zstd")
    
    message("Wrote: ", out_path, " (", format(nrow(pheno_year), big.mark = ","), " crop cycles)")
  }
}