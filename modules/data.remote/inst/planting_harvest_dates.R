## Shared V1 planting + harvest projection
## Future crops = dominant season 2
## X/I receive no planting/harvest events
## Harvest = projected planting date + historical planting->harvest duration

pacman::p_load(data.table, arrow, bit64)

# ---- setup ----
#REQUIRED: Choose a folder to define work_root, where you want this framework to save intermediate and output files
#Uncomment the line below and replace the example path.
#work_root = "/path/to/your/folder"

#Shared Data: Shared project data, most users should not need to change this.
ccmmf_root = "/projectnb/dietzelab/ccmmf"

config = list(historical_years = 2018:2023, prediction_years = 2024:2045,
              event_dir = file.path(ccmmf_root, "management", "event_files_v4.1.2"),
              crop_history_path = file.path(work_root, "crops_full_counties.csv"),
              prediction_dir = file.path(work_root, "crop_predictions"),
              planting_output_root = file.path(work_root, "planting_projections"),
              harvest_output_root = file.path(work_root, "harvest_projections"))

# ---- helpers ----
normalize_geoid = function(x) {
  x = trimws(as.character(x)); x = sub("\\.0$", "", x)
  x[x %chin% c("", "NA", "NaN", "NULL")] = NA_character_
  ok = !is.na(x)
  if (any(ok & !grepl("^[0-9]+$", x))) stop("county_geoid contains non-numeric values.")
  x[ok] = sprintf("%05d", as.integer(x[ok]))
  x
}

normalize_subclass = function(x) {
  x = trimws(as.character(x)); x = sub("\\.0+$", "", x)
  x[x %chin% c("", "NA", "NaN", "NULL")] = NA_character_
  x
}

make_crop_code = function(class, subclass) {
  class = trimws(as.character(class))
  class[class %chin% c("", "NA", "NaN", "NULL")] = NA_character_
  subclass = normalize_subclass(subclass)
  fifelse(is.na(subclass), class, paste0(class, subclass))
}

get_crop_class = function(x) fifelse(x == "**", "**", sub("[0-9].*$", "", as.character(x)))

safe_mean = function(x) {
  x = as.numeric(x); x = x[is.finite(x)]
  if (!length(x)) NA_real_ else mean(x)
}

fix_wrap_doy = function(x, low_cutoff = 45, high_cutoff = 320) {
  x = as.numeric(x); x = x[is.finite(x)]
  if (!length(x)) return(x)
  
  wraps = stats::quantile(x, 0.05, na.rm = TRUE) <= low_cutoff &&
    stats::quantile(x, 0.95, na.rm = TRUE) >= high_cutoff
  
  if (wraps) x[x <= low_cutoff] = x[x <= low_cutoff] + 365
  x
}

mean_wrapped_day = function(x) {
  x = fix_wrap_doy(x)
  if (!length(x)) NA_real_ else mean(x)
}

mode_character = function(x) {
  x = as.character(x); x = x[!is.na(x) & nzchar(x)]
  if (!length(x)) return(NA_character_)
  names(sort(table(x), decreasing = TRUE))[[1]]
}

rename_first = function(dt, target, aliases, label, required = TRUE) {
  if (target %in% names(dt)) return(invisible(dt))
  hit = intersect(aliases, names(dt))
  if (!length(hit)) {
    if (required) stop(label, " missing `", target, "`.")
    return(invisible(dt))
  }
  setnames(dt, hit[[1]], target)
  invisible(dt)
}

assert_columns = function(dt, required, label) {
  missing = setdiff(required, names(dt))
  if (length(missing)) stop(label, " missing: ", paste(missing, collapse = ", "))
}

relative_event_day = function(date, year) {
  as.integer(as.Date(date) - as.Date(paste0(as.integer(year), "-01-01"))) + 1L
}

relative_day_to_date = function(year, day) {
  out = rep(as.Date(NA), length(year))
  ok = !is.na(year) & !is.na(day)
  out[ok] = as.Date(paste0(as.integer(year[ok]), "-01-01")) + as.integer(round(day[ok])) - 1L
  as.IDate(out)
}

days_in_year = function(year) {
  as.integer(as.Date(paste0(as.integer(year) + 1L, "-01-01")) -
               as.Date(paste0(as.integer(year), "-01-01")))
}

make_lookup = function(dt, keys, values, prefix, wrapped_values = character()) {
  out = dt[complete.cases(dt[, ..keys]), {
    ans = lapply(values, function(x) {
      if (x %chin% wrapped_values) mean_wrapped_day(get(x)) else safe_mean(get(x))
    })
    setNames(ans, values)
  }, by = keys]
  
  setnames(out, values, paste0(prefix, values))
  out
}

coalesce_values = function(dt, values, prefixes, global) {
  for (x in values) {
    cols = paste0(prefixes, x)
    dt[, (x) := do.call(fcoalesce, mget(cols))]
    dt[is.na(get(x)), (x) := global[[x]]]
  }
}

drop_lookup_cols = function(dt, values, prefixes) {
  cols = intersect(unlist(lapply(prefixes, function(p) paste0(p, values))), names(dt))
  if (length(cols)) dt[, (cols) := NULL]
}

write_years = function(events, dir, stem, cols) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  
  for (yy in config$prediction_years) {
    out = events[projection_year == yy, ..cols]
    setorder(out, parcel_id, date)
    out[, parcel_id := bit64::as.integer64(parcel_id)]
    
    if (out[!complete.cases(out), .N])
      stop(stem, " ", yy, " contains missing required values.")
    
    path = file.path(dir, paste0(stem, "_statewide_", yy, ".parquet"))
    write_parquet(out, path, compression = "zstd")
    message("Wrote: ", path, " (", format(nrow(out), big.mark = ","), " events)")
  }
}

# ---- fixed parcel county ----
crop_history = fread(config$crop_history_path, integer64 = "integer64")
if ("V1" %in% names(crop_history)) crop_history[, V1 := NULL]

rename_first(crop_history, "parcel_id", c("site_id"), "Crop history")
rename_first(crop_history, "county_geoid", c("COUNTY_GEOID"), "Crop history")
assert_columns(crop_history, c("parcel_id", "year", "county_geoid"), "Crop history")

crop_history[, `:=`(
  parcel_id = bit64::as.integer64(as.character(parcel_id)),
  year = as.integer(year),
  county_geoid = normalize_geoid(county_geoid))]

parcel_county = crop_history[
  !is.na(parcel_id) & !is.na(year) & year <= 2023L & !is.na(county_geoid),
  .SD[which.max(year)], by = parcel_id
][, .(parcel_id, county_geoid)]

if (parcel_county[, anyDuplicated(parcel_id)])
  stop("Fixed parcel county lookup still contains duplicate parcel IDs.")

# ---- historical planting ----
plant_files = file.path(config$event_dir, paste0("planting_statewide_", config$historical_years, ".parquet"))

if (any(!file.exists(plant_files))) stop("Missing historical planting files.")

planting_hist = rbindlist(Map(function(f, yy) {
  x = as.data.table(read_parquet(f))
  x[, source_year := as.integer(yy)]
  x
}, plant_files, config$historical_years), fill = TRUE)

rename_first(planting_hist, "parcel_id", c("site_id"), "Historical planting")
rename_first(planting_hist, "crop_code", c("code", "CLASS_SUBCLASS"), "Historical planting")
rename_first(planting_hist, "PFT", c("landiq_PFT"), "Historical planting")
rename_first(planting_hist, "date", c("planting_date"), "Historical planting")

plant_alias = list(leaf_c_kg_m2 = c("C_LEAF"), wood_c_kg_m2 = c("C_STEM"),
  fine_root_c_kg_m2 = c("C_FINEROOT"), coarse_root_c_kg_m2 = c("C_COARSEROOT"), 
  leaf_n_kg_m2 = c("N_LEAF"), wood_n_kg_m2 = c("N_STEM"), fine_root_n_kg_m2 = c("N_FINEROOT"),
  coarse_root_n_kg_m2 = c("N_COARSEROOT"))

for (x in names(plant_alias))
  rename_first(planting_hist, x, plant_alias[[x]], "Historical planting")

plant_pool_cols = names(plant_alias)

assert_columns(planting_hist, c("parcel_id", "crop_code", "PFT", "date", "source_year", plant_pool_cols),
               "Historical planting")

planting_hist[, `:=`(
  parcel_id = bit64::as.integer64(as.character(parcel_id)), crop_code = trimws(as.character(crop_code)),
  PFT = as.character(PFT), date = as.IDate(date), planting_relative_day = relative_event_day(date, source_year))]

planting_hist[crop_code %chin% c("", "NA", "NaN", "NULL"), crop_code := NA_character_]
planting_hist[, crop_class := get_crop_class(crop_code)]
planting_hist[, (plant_pool_cols) := lapply(.SD, as.numeric), .SDcols = plant_pool_cols]
planting_hist = merge(planting_hist, parcel_county, by = "parcel_id", all.x = TRUE)

plant_values = c("planting_relative_day", plant_pool_cols)

plant_county_code = make_lookup(planting_hist, c("county_geoid", "crop_code"), plant_values, "pc_",
                                wrapped_values = "planting_relative_day")

plant_county_class = make_lookup(planting_hist, c("county_geoid", "crop_class"), plant_values, "pcl_",
                                 wrapped_values = "planting_relative_day")

plant_code = make_lookup(planting_hist, "crop_code", plant_values, "pcode_",
                         wrapped_values = "planting_relative_day")

plant_class = make_lookup(planting_hist, "crop_class", plant_values, "pclass_",
                          wrapped_values = "planting_relative_day")

plant_pft = make_lookup(planting_hist, "PFT", plant_values, "ppft_",
                        wrapped_values = "planting_relative_day")

plant_global = vapply(plant_values, function(x) {
  if (x == "planting_relative_day") mean_wrapped_day(planting_hist[[x]])
  else safe_mean(planting_hist[[x]])
}, numeric(1))

if (anyNA(plant_global))
  stop("Planting global fallback contains missing values.")

pft_code = planting_hist[
  !is.na(crop_code) & !is.na(PFT) & nzchar(PFT),
  .(lookup_PFT_code = mode_character(PFT)), by = crop_code]

pft_class = planting_hist[
  !is.na(crop_class) & !is.na(PFT) & nzchar(PFT),
  .(lookup_PFT_class = mode_character(PFT)), by = crop_class]

# ---- historical harvest ----
harvest_files = file.path(config$event_dir, paste0("harvest_statewide_", config$historical_years, ".parquet"))

if (any(!file.exists(harvest_files))) stop("Missing historical harvest files.")

harvest_hist = rbindlist(Map(function(f, yy) {
  x = as.data.table(read_parquet(f))
  x[, source_year := as.integer(yy)]
  x
}, harvest_files, config$historical_years), fill = TRUE)

rename_first(harvest_hist, "parcel_id", c("site_id"), "Historical harvest")
rename_first(harvest_hist, "crop_code", c("CLASS_SUBCLASS", "code"), "Historical harvest")
rename_first(harvest_hist, "date", c("harvest_date"), "Historical harvest")

harvest_fraction_cols = c("frac_above_removed_0to1", "frac_below_removed_0to1",
  "frac_above_to_litter_0to1", "frac_below_to_litter_0to1")

assert_columns(harvest_hist, c("parcel_id", "crop_code", "date", "source_year", harvest_fraction_cols),
               "Historical harvest")

harvest_hist[, `:=`(
  parcel_id = bit64::as.integer64(as.character(parcel_id)), crop_code = trimws(as.character(crop_code)),
  date = as.IDate(date), harvest_relative_day = relative_event_day(date, source_year))]

harvest_hist[crop_code %chin% c("", "NA", "NaN", "NULL"), crop_code := NA_character_]
harvest_hist[, crop_class := get_crop_class(crop_code)]
harvest_hist[, (harvest_fraction_cols) := lapply(.SD, as.numeric),
             .SDcols = harvest_fraction_cols]

for (x in harvest_fraction_cols) {
  if (harvest_hist[is.finite(get(x)) & (get(x) < 0 | get(x) > 1), .N])
    stop("Historical harvest fraction outside [0,1]: ", x)
}

# ---- pair planting -> harvest timing ----
plant_pairs = planting_hist[
  !is.na(crop_code),
  .(
    county_geoid = mode_character(county_geoid), crop_class = mode_character(crop_class),
    PFT = mode_character(PFT), planting_relative_day = mean_wrapped_day(planting_relative_day)
  ),
  by = .(parcel_id, source_year, crop_code)]

harvest_pairs = harvest_hist[
  !is.na(crop_code),
  c(
    list(harvest_relative_day = safe_mean(harvest_relative_day)),
    lapply(.SD, safe_mean)
  ),
  by = .(parcel_id, source_year, crop_code),
  .SDcols = harvest_fraction_cols]

paired_hist = merge(plant_pairs, harvest_pairs, by = c("parcel_id", "source_year", "crop_code"), all = FALSE)

if (!nrow(paired_hist))
  stop("No historical planting and harvest events could be paired.")

paired_hist[, harvest_lag_days := harvest_relative_day - planting_relative_day]

paired_hist[harvest_lag_days <= 0,
            harvest_lag_days := harvest_lag_days + days_in_year(source_year)]

paired_hist = paired_hist[
  is.finite(harvest_lag_days) & harvest_lag_days > 0]

if (!nrow(paired_hist))
  stop("No positive historical planting-to-harvest durations available.")

message("Historical paired planting/harvest rows: ",
        format(nrow(paired_hist), big.mark = ","))

harvest_values = c("harvest_lag_days", harvest_fraction_cols)

harv_county_code = make_lookup(paired_hist, c("county_geoid", "crop_code"), harvest_values, "hc_")

harv_county_class = make_lookup(paired_hist, c("county_geoid", "crop_class"), harvest_values, "hcl_")

harv_code = make_lookup(paired_hist, "crop_code", harvest_values, "hcode_")
harv_class = make_lookup(paired_hist, "crop_class", harvest_values, "hclass_")
harv_pft = make_lookup(paired_hist, "PFT", harvest_values, "hpft_")

harv_global = vapply(harvest_values,
                     function(x) safe_mean(paired_hist[[x]]), numeric(1))

if (anyNA(harv_global))
  stop("Harvest global fallback contains missing values.")

# ---- future crop projections ----
prediction_files = list.files(config$prediction_dir, 
                  pattern = "^crop_identity_statewide_[0-9]{4}\\.parquet$", full.names = TRUE)

if (!length(prediction_files))
  stop("No shared crop prediction files found.")

future = rbindlist(lapply(prediction_files, function(f) {
  as.data.table(read_parquet(f))
}), fill = TRUE)

assert_columns(future, c("parcel_id", "year", "CLASS"), "Future crop predictions")

if (!"SUBCLASS" %in% names(future)) future[, SUBCLASS := NA_character_]
if (!"PFT" %in% names(future)) future[, PFT := NA_character_]

future[, `:=`(
  parcel_id = bit64::as.integer64(as.character(parcel_id)), year = as.integer(year),
  CLASS = trimws(as.character(CLASS)), SUBCLASS = normalize_subclass(SUBCLASS), PFT = as.character(PFT))]

if ("season" %in% names(future)) {
  future[, season := as.integer(season)]
  if (anyNA(future$season) || any(future$season != 2L))
    stop("Future crop predictions must contain only season 2.")
}

future[, crop_code := make_crop_code(CLASS, SUBCLASS)]
future[, crop_class := get_crop_class(crop_code)]

if (!"county_geoid" %in% names(future)) {
  future = merge(future, parcel_county, by = "parcel_id", all.x = TRUE)
} else {
  future[, county_geoid := normalize_geoid(county_geoid)]
}

future = future[year %in% config$prediction_years]

if (anyNA(future$CLASS) || any(!nzchar(future$CLASS)))
  stop("Future crop predictions contain missing/blank CLASS values.")

# X = unclassified fallow; I = idle
future = future[!CLASS %chin% c("X", "I")]

if (anyNA(future$parcel_id))
  stop("Future crop predictions contain missing parcel IDs.")

if (anyNA(future$county_geoid))
  stop("Future crop predictions contain missing county GEOIDs.")

dups = future[, .N, by = .(parcel_id, year)][N > 1L]
if (nrow(dups))
  stop("Crop projections still contain duplicate parcel-year rows: ", nrow(dups))

# fill PFT where crop product does not contain it
future = merge(future, pft_code, by = "crop_code", all.x = TRUE)
future = merge(future, pft_class, by = "crop_class", all.x = TRUE)

future[is.na(PFT) | !nzchar(PFT),
       PFT := fcoalesce(lookup_PFT_code, lookup_PFT_class)]

future[, c("lookup_PFT_code", "lookup_PFT_class") := NULL]

message("Active future crop rows: ", format(nrow(future), big.mark = ","))

# ---- project planting ----
events = future[, .(
  parcel_id, county_geoid, year, crop_code, crop_class, PFT)]

events = merge(events, plant_county_code, by = c("county_geoid", "crop_code"), all.x = TRUE)

events = merge(events, plant_county_class, by = c("county_geoid", "crop_class"), all.x = TRUE)

events = merge(events, plant_code, by = "crop_code", all.x = TRUE)
events = merge(events, plant_class, by = "crop_class", all.x = TRUE)
events = merge(events, plant_pft, by = "PFT", all.x = TRUE)

coalesce_values(events, plant_values, c("pc_", "pcl_", "pcode_", "pclass_", "ppft_"), plant_global)

events[, planting_date := relative_day_to_date(year, planting_relative_day)]

drop_lookup_cols(events, plant_values, c("pc_", "pcl_", "pcode_", "pclass_", "ppft_"))

# ---- project harvest using duration from planting ----
events = merge(events, harv_county_code, by = c("county_geoid", "crop_code"), all.x = TRUE)

events = merge(events, harv_county_class, by = c("county_geoid", "crop_class"), all.x = TRUE)

events = merge(events, harv_code, by = "crop_code", all.x = TRUE)
events = merge(events, harv_class, by = "crop_class", all.x = TRUE)
events = merge(events, harv_pft, by = "PFT", all.x = TRUE)

coalesce_values(events, harvest_values, c("hc_", "hcl_", "hcode_", "hclass_", "hpft_"), harv_global)

events[, harvest_date := as.IDate(
  as.Date(planting_date) + as.integer(round(harvest_lag_days)))]

drop_lookup_cols(events, harvest_values, c("hc_", "hcl_", "hcode_", "hclass_", "hpft_"))

# ---- QC ----
missing_plant = events[is.na(planting_date), .N]
missing_harv = events[is.na(harvest_date), .N]

bad_order = events[
  !is.na(planting_date) & !is.na(harvest_date) &
    harvest_date <= planting_date,
  .N]

message("Active rows without planting date: ", format(missing_plant, big.mark = ","))
message("Active rows without harvest date: ", format(missing_harv, big.mark = ","))
message("Rows with harvest not after planting: ", format(bad_order, big.mark = ","))

if (missing_plant) stop("Some active crops have no projected planting date.")
if (missing_harv) stop("Some active crops have no projected harvest date.")
if (bad_order) stop("Some projected harvest dates are not after planting.")

# ---- final event products ----
planting = events[, c(list(projection_year = year, event_type = "planting",
    parcel_id = parcel_id, date = planting_date, crop_code = crop_code),
  mget(plant_pool_cols))]

harvest = events[, c(list(projection_year = year, event_type = "harvest",
    parcel_id = parcel_id, date = harvest_date),
  mget(harvest_fraction_cols))]

for (x in harvest_fraction_cols) {
  if (harvest[get(x) < 0 | get(x) > 1, .N])
    stop("Projected harvest fraction outside [0,1]: ", x)
}

planting_cols = c("event_type", "parcel_id", "date", "crop_code", plant_pool_cols)

harvest_cols = c("event_type", "parcel_id", "date", harvest_fraction_cols)

write_years(planting, config$planting_output_root, "planting", planting_cols)
write_years(harvest, config$harvest_output_root, "harvest", harvest_cols)

message("Shared planting and harvest projection complete.")