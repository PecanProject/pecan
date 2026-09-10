## Projects shared future leaf-on / leaf-off events using historical
## county-by-crop-class means from the derived phenology product.
## Crop projections are shared across BAU/NBS, so phenology is projected once.

pacman::p_load(data.table, arrow, bit64)

# ---- setup ----
#REQUIRED: Choose a folder to define work_root, where you want this framework to save intermediate and output files
#Uncomment the line below and replace the example path.
#work_root = "/path/to/your/folder"

#Shared Data: Shared project data, most users should not need to change this. 
ccmmf_root = "/projectnb/dietzelab/ccmmf"

config = list(historical_years = 2018:2023, prediction_years = 2024:2045,
  event_dir = file.path(ccmmf_root, "management", "event_files_v4.1.2"),
  matched_dir = file.path(ccmmf_root, "management", "phenology", "matched_landiq_mslsp_v4.1.2", "gapfill_dates"),
  prediction_dir = file.path(work_root, "crop_predictions"),
  output_dir = file.path(work_root, "phenology_projections"),
  landiq_identity_path = file.path(ccmmf_root, "LandIQ-harmonized-v4.1.2", "crops_all_years.parq"))

dir.create(config$output_dir, recursive = TRUE, showWarnings = FALSE)

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

landiq_ds = arrow::open_dataset(config$landiq_identity_path, format = "parquet")

landiq_county = data.table::as.data.table(
  landiq_ds |>
    dplyr::filter(.data$year <= 2023, !is.na(.data$COUNTY)) |>
    dplyr::select(parcel_id, year, COUNTY) |>
    dplyr::collect()
)

landiq_county[, `:=`(
  parcel_id = bit64::as.integer64(as.character(parcel_id)),
  year = as.integer(year),
  COUNTY = as.character(COUNTY)
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
  
  if ("season" %in% names(m)) {
    m[, season := as.integer(season)]
    m = m[season == 2L]
  }
  
  required_m = c("parcel_id", "landiq_CLASS", "mslsp_50PCGI", "mslsp_50PCGD")
  missing_m = setdiff(required_m, names(m))
  if (length(missing_m)) stop("Matched phenology product missing: ", paste(missing_m, collapse = ", "))
  
  m = unique(m[, .(
    parcel_id = bit64::as.integer64(as.character(parcel_id)), year = as.integer(yy),
    CLASS = as.character(landiq_CLASS), leafonday = as.IDate(mslsp_50PCGI), leafoffday = as.IDate(mslsp_50PCGD)
  )])
  
  merge(p, m, by = c("parcel_id", "year", "leafonday", "leafoffday"), all.x = TRUE)
}), fill = TRUE)

message("Historical phenology rows matched to crop class: ",
        format(pheno_hist[!is.na(CLASS), .N], big.mark = ","), " of ",
        format(nrow(pheno_hist), big.mark = ","))

pheno_hist = merge(pheno_hist, parcel_county, by = "parcel_id", all.x = TRUE)
pheno_hist[, `:=`(
  leafon_offset = date_offset(leafonday, year), leafoff_offset = date_offset(leafoffday, year)
)]

phenology_lookup = pheno_hist[
  !is.na(county) & !is.na(CLASS),
  .(leafon_offset = safe_mean(leafon_offset),
    leafoff_offset = safe_mean(leafoff_offset)),
  by = .(county, CLASS)
]

phenology_fallback = pheno_hist[
  !is.na(CLASS),
  .(
    fallback_leafon_offset = safe_mean(leafon_offset),
    fallback_leafoff_offset = safe_mean(leafoff_offset)
  ),
  by = CLASS
]

# ---- shared future projection ----
for (yy in config$prediction_years) {
  crop_file = file.path(config$prediction_dir, paste0("crop_identity_statewide_", yy, ".parquet"))
  if (!file.exists(crop_file)) stop("Missing projected crop file: ", crop_file)
  
  future = as.data.table(read_parquet(crop_file))
  required = c("parcel_id", "COUNTY", "year", "CLASS")
  missing = setdiff(required, names(future))
  if (length(missing)) stop("Projected crop identity missing: ", paste(missing, collapse = ", "))
  
  future = future[, .(
    parcel_id = bit64::as.integer64(as.character(parcel_id)), county = as.character(COUNTY),
    year = as.integer(year), CLASS = as.character(CLASS)
  )]
  
  pred = merge(future, phenology_lookup, by = c("county", "CLASS"), all.x = TRUE)
  pred = merge(pred, phenology_fallback, by = "CLASS", all.x = TRUE)
  
  pred[, `:=`(
    leafon_offset = fcoalesce(leafon_offset, fallback_leafon_offset),
    leafoff_offset = fcoalesce(leafoff_offset, fallback_leafoff_offset)
  )]
  
  pred[, `:=`(
    leafonday = offset_to_date(year, leafon_offset),
    leafoffday = offset_to_date(year, leafoff_offset)
  )]
  
  message(yy, " rows without county/class phenology mean: ",
          format(pred[is.na(leafonday) & is.na(leafoffday), .N], big.mark = ","))
  
  pheno_year = rbindlist(list(
    pred[!is.na(leafonday), .(event_type = "leafon", parcel_id, date = leafonday)],
    pred[!is.na(leafoffday), .(event_type = "leafoff", parcel_id, date = leafoffday)]
  ), use.names = TRUE)
  
  pheno_year[, parcel_id := bit64::as.integer64(parcel_id)]
  setorder(pheno_year, parcel_id, date, event_type)
  
  out_path = file.path(config$output_dir, paste0("phenology_statewide_", yy, ".parquet"))
  write_parquet(pheno_year, out_path, compression = "zstd")
  message("Wrote: ", out_path)
}

message("Shared phenology projection complete: ", config$output_dir)