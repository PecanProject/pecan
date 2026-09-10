## Predicts shared parcel-level crop identity through 2045 from the single
## set of optimized county crop-transition matrices. Crop projections are
## scenario-independent; BAU/NBS branching happens in downstream scripts.

pacman::p_load(data.table, arrow, bit64, dplyr)

# ---- setup ----
#REQUIRED: Choose a folder to define work_root, where you want this framework to save intermediate and output files
#Uncomment the line below and replace the example path.
#work_root = "/path/to/your/folder"

#Shared Data: Shared project data, most users should not need to change this.
ccmmf_root = "/projectnb/dietzelab/ccmmf"

config = list(seed = 42, start_year = 2023L, end_year = 2045L,
              year_states_path = file.path(work_root, "crop_year_states_cleaned.csv"),
              crop_history_path = file.path(work_root, "crops_full_counties.csv"),
              landiq_identity_path = file.path(ccmmf_root, "LandIQ-harmonized-v4.1.2", "crops_all_years.parq"),
              lookup_path = file.path(ccmmf_root, "management", "LandIQ_cropCode_lookup_table.csv"),
              crop_matrix_dir = file.path(work_root, "county_optimized_matrices"),
              prediction_dir = file.path(work_root, "crop_predictions"))

set.seed(config$seed)
start_year = config$start_year
end_year = config$end_year
crop_matrix_dir = config$crop_matrix_dir
prediction_dir = config$prediction_dir
dir.create(prediction_dir, recursive = TRUE, showWarnings = FALSE)

# ---- helpers ----
safe_county_name = function(x) gsub("[^A-Za-z0-9_]+", "_", x)

normalize_subclass = function(x) {
  x = trimws(as.character(x))
  x = sub("\\.0+$", "", x)
  x[x %chin% c("", "NA", "NaN", "NULL")] = NA_character_
  x
}

mode_value = function(x) {
  x = x[!is.na(x)]
  if (is.character(x)) x = x[nzchar(x)]
  if (!length(x)) return(NA)
  names(sort(table(x), decreasing = TRUE))[1]
}

mode_with_missing = function(x) {
  x = as.character(x)
  x[is.na(x) | !nzchar(x)] = "__MISSING__"
  winner = names(sort(table(x), decreasing = TRUE))[1]
  if (!length(winner) || winner == "__MISSING__") NA_character_ else winner
}

safe_mean = function(x) {
  x = as.numeric(x)
  x = x[is.finite(x)]
  if (!length(x)) NA_real_ else mean(x)
}

fix_wrap_doy = function(x, low_cutoff = 45, high_cutoff = 320) {
  x = as.numeric(x)
  x = x[is.finite(x)]
  if (!length(x)) return(x)
  wraps = stats::quantile(x, 0.05, na.rm = TRUE) <= low_cutoff &&
    stats::quantile(x, 0.95, na.rm = TRUE) >= high_cutoff
  if (wraps) x[x <= low_cutoff] = x[x <= low_cutoff] + 365
  x
}

mean_wrapped_doy = function(x) {
  x = fix_wrap_doy(x)
  if (!length(x)) return(NA_real_)
  ((round(mean(x)) - 1) %% 365) + 1
}

read_tmat = function(path) {
  tmat_df = fread(path)
  row_id_col = names(tmat_df)[1]
  states = as.character(names(tmat_df)[-1])
  row_states = as.character(tmat_df[[row_id_col]])
  A = as.matrix(tmat_df[, -1, with = FALSE])
  rownames(A) = row_states
  colnames(A) = states
  storage.mode(A) = "double"
  stopifnot(all(rownames(A) == colnames(A)))
  A
}

repair_transition_matrix = function(A, matrix_name = "matrix") {
  A[is.na(A)] = 0
  if (any(A < 0, na.rm = TRUE)) {
    warning(matrix_name, " has negative probabilities; clamping to 0.")
    A[A < 0] = 0
  }
  if (any(A > 1, na.rm = TRUE)) {
    warning(matrix_name, " has probabilities >1; clamping to 1.")
    A[A > 1] = 1
  }
  
  row_sums = rowSums(A)
  zero_rows = names(row_sums)[is.na(row_sums) | row_sums == 0]
  
  if (length(zero_rows)) {
    warning(matrix_name, " has zero-sum rows; setting self-loop for: ", paste(zero_rows, collapse = ", "))
    for (s in zero_rows) {
      A[s, ] = 0
      A[s, s] = 1
    }
  }
  
  sweep(A, 1, rowSums(A), "/")
}

load_crop_matrices = function(crop_matrix_dir) {
  matrix_files = list.files(crop_matrix_dir, pattern = "_crop_matrix\\.csv$", full.names = TRUE)
  if (!length(matrix_files)) stop("No optimized crop matrix files found in: ", crop_matrix_dir)
  
  mats = list()
  for (f in matrix_files) {
    matrix_name = sub("_crop_matrix\\.csv$", "", basename(f))
    mats[[matrix_name]] = repair_transition_matrix(read_tmat(f), paste0("optimized crop matrix ", matrix_name))
  }
  mats
}

# ---- crop prediction functions ----
predict_county_sequential = function(start_info, tmat, start_year, end_year, state_col = "crop_class") {
  dt = copy(start_info)
  states = rownames(tmat)
  years = seq.int(start_year + 1L, end_year)
  
  dt = dt[get(state_col) %in% states]
  dt[, ACRES := as.numeric(ACRES)]
  dt = dt[!is.na(ACRES) & is.finite(ACRES) & ACRES > 0]
  
  if (!nrow(dt)) return(list(predictions = data.table(), qc = data.table()))
  
  dt[, current_CLASS := as.character(get(state_col))]
  
  expected_vec = setNames(rep(0, length(states)), states)
  x0_dt = dt[, .(acres = sum(ACRES, na.rm = TRUE)), by = current_CLASS]
  expected_vec[x0_dt$current_CLASS] = x0_dt$acres
  
  pred_list = vector("list", length(years))
  qc_list = vector("list", length(years))
  
  for (k in seq_along(years)) {
    yy = years[k]
    
    # Predict next year's class from the parcel's current class.
    dt[, next_CLASS := {
      from_state = current_CLASS[1]
      p = as.numeric(tmat[from_state, states])
      p[!is.finite(p)] = 0
      
      if (sum(p) <= 0) rep(from_state, .N)
      else sample(states, size = .N, replace = TRUE, prob = p / sum(p))
    }, by = current_CLASS]
    
    dt[, prob_crop_class := tmat[cbind(current_CLASS, next_CLASS)]]
    
    # Expected county acreage after one more transition.
    expected_vec = as.numeric(expected_vec %*% tmat)
    names(expected_vec) = states
    
    # Realized county acreage from sampled parcel states.
    realized_dt = dt[, .(realized_acres = sum(ACRES, na.rm = TRUE)), by = next_CLASS]
    realized_vec = setNames(rep(0, length(states)), states)
    realized_vec[realized_dt$next_CLASS] = realized_dt$realized_acres
    
    qc_list[[k]] = data.table(year = yy, CLASS = states,
                              expected_acres = as.numeric(expected_vec[states]),
                              realized_acres = as.numeric(realized_vec[states]))
    
    qc_list[[k]][, `:=`(
      difference_acres = realized_acres - expected_acres,
      abs_difference_acres = abs(realized_acres - expected_acres))]
    
    pred_list[[k]] = dt[, .(parcel_id, year = yy, CLASS = next_CLASS, prob_crop_class)]
    
    # Next year's transition depends on the class sampled this year.
    dt[, current_CLASS := next_CLASS]
    dt[, next_CLASS := NULL]
  }
  
  list(predictions = rbindlist(pred_list, use.names = TRUE, fill = TRUE),
       qc = rbindlist(qc_list, use.names = TRUE, fill = TRUE))
}

predict_grouped_markov = function(year_states, transition_mats, group_col, start_year, end_year, state_col = "crop_class") {
  dt = copy(year_states)
  
  # One starting row per parcel: latest observed state at or before start_year.
  start_all = dt[year <= start_year, .SD[which.max(year)], by = parcel_id]
  
  if (start_all[, anyDuplicated(parcel_id)]) {
    stop("Latest crop-state table still contains duplicate parcel_id values.")
  }
  
  groups = intersect(unique(na.omit(start_all[[group_col]])), names(transition_mats))
  if (!length(groups)) stop("No overlapping groups between crop data and transition matrices.")
  
  prediction_list = list()
  qc_list = list()
  
  for (g in groups) {
    message("Predicting crop class with optimized matrix for county: ", g)
    
    start_info = start_all[get(group_col) == g]
    
    ans = predict_county_sequential(start_info = start_info, tmat = transition_mats[[g]],
                                    start_year = start_year, end_year = end_year, state_col = state_col)
    
    if (!nrow(ans$predictions)) next
    
    ans$predictions[, (group_col) := g]
    ans$qc[, (group_col) := g]
    
    prediction_list[[g]] = ans$predictions
    qc_list[[g]] = ans$qc
  }
  
  predictions = rbindlist(prediction_list, fill = TRUE)
  qc = rbindlist(qc_list, fill = TRUE)
  
  dup = predictions[, .N, by = .(parcel_id, year)][N > 1L]
  if (nrow(dup)) stop("Crop projection produced duplicate parcel-year rows.")
  
  list(predictions = predictions, qc = qc)
}

# ---- load crop-year states ----
if (!file.exists(config$year_states_path)) {
  stop("crop_year_states_cleaned.csv not found: ", config$year_states_path)
}

crop_data = fread(config$year_states_path)
if ("V1" %in% names(crop_data)) crop_data[, V1 := NULL]

required_crop_cols = c("parcel_id", "year", "county", "county_geoid", "state", "ACRES")
missing_crop_cols = setdiff(required_crop_cols, names(crop_data))

if (length(missing_crop_cols)) {
  stop("crop_year_states_cleaned.csv is missing: ", paste(missing_crop_cols, collapse = ", "))
}

crop_data[, `:=`(
  parcel_id = as.character(parcel_id), year = as.integer(year), county = as.character(county),
  county_geoid = as.character(county_geoid), crop_class = trimws(as.character(state)),
  ACRES = as.numeric(ACRES), county_safe = safe_county_name(county)
)]

# ---- crop lookup / historical subclass ----
if (!file.exists(config$lookup_path)) stop("LandIQ crop lookup not found: ", config$lookup_path)

lookup = fread(config$lookup_path)
lookup[, `:=`(CLASS = as.character(CLASS), SUBCLASS = normalize_subclass(SUBCLASS))]

lookup_subclass = unique(lookup[, .(CLASS, SUBCLASS, CLASS_desc, SUBCLASS_desc, PFT)],
                         by = c("CLASS", "SUBCLASS"))

if (!file.exists(config$crop_history_path)) {
  stop("Historical subclass file not found: ", config$crop_history_path)
}

with_subclass = fread(config$crop_history_path)
if ("V1" %in% names(with_subclass)) with_subclass[, V1 := NULL]

if (!"CLASS" %in% names(with_subclass) && "crop_class" %in% names(with_subclass)) {
  with_subclass[, CLASS := as.character(crop_class)]
}

if (!"SUBCLASS" %in% names(with_subclass) && "subclass" %in% names(with_subclass)) {
  setnames(with_subclass, "subclass", "SUBCLASS")
}

if (!"county_safe" %in% names(with_subclass) && "county" %in% names(with_subclass)) {
  with_subclass[, county_safe := safe_county_name(county)]
}

if (!"season" %in% names(with_subclass)) with_subclass[, season := 0L]

required_subclass_cols = c("parcel_id", "year", "county_safe", "CLASS", "SUBCLASS")
missing_subclass_cols = setdiff(required_subclass_cols, names(with_subclass))

if (length(missing_subclass_cols)) {
  stop("crops_full_counties.csv is missing: ", paste(missing_subclass_cols, collapse = ", "))
}

with_subclass[, `:=`(
  parcel_id = as.character(parcel_id), year = as.integer(year), county_safe = as.character(county_safe),
  CLASS = trimws(as.character(CLASS)), SUBCLASS = normalize_subclass(SUBCLASS), season = as.integer(season)
)]

with_subclass[is.na(season), season := 0L]

# ---- LandIQ crop identity history ----
if (!file.exists(config$landiq_identity_path)) {
  stop("LandIQ crop identity parquet not found: ", config$landiq_identity_path)
}

landiq_ds = arrow::open_dataset(config$landiq_identity_path, format = "parquet")
landiq_names = landiq_ds$schema$names

cover_source = if ("COVER" %in% landiq_names) "COVER" else if ("PCNT" %in% landiq_names) "PCNT" else {
  stop("LandIQ crop identity parquet has neither COVER nor legacy PCNT.")
}

landiq_required = c("parcel_id", "COUNTY", "year", "season", "CLASS", "SUBCLASS",
                    "SPECOND", "MULTIUSE", "ADOY", cover_source)

landiq_missing = setdiff(landiq_required, landiq_names)

if (length(landiq_missing)) {
  stop("LandIQ crop identity parquet is missing: ", paste(landiq_missing, collapse = ", "))
}

landiq_identity_hist = data.table::as.data.table(
  landiq_ds |>
    dplyr::filter(.data$year <= start_year) |>
    dplyr::select(dplyr::all_of(landiq_required)) |>
    dplyr::collect()
)

if (cover_source != "COVER") setnames(landiq_identity_hist, cover_source, "COVER")

landiq_identity_hist[, `:=`(
  parcel_id = bit64::as.integer64(as.character(parcel_id)), COUNTY = as.character(COUNTY),
  year = as.integer(year), season = as.integer(season), CLASS = trimws(as.character(CLASS)),
  SUBCLASS = normalize_subclass(SUBCLASS), SPECOND = as.character(SPECOND), MULTIUSE = as.numeric(MULTIUSE),
  ADOY = as.numeric(ADOY), COVER = as.numeric(COVER)
)]

landiq_identity_hist[CLASS %chin% c("", "NA", "NaN", "***"), CLASS := NA_character_]
landiq_identity_hist[SPECOND %chin% c("", "NA", "NaN", "***"), SPECOND := NA_character_]

identity_cols = c("parcel_id", "COUNTY", "year", "season", "CLASS", "SUBCLASS",
                  "SPECOND", "MULTIUSE", "ADOY", "COVER")

landiq_identity_hist = landiq_identity_hist[, ..identity_cols]

parcel_county_fixed = landiq_identity_hist[
  !is.na(COUNTY), .SD[which.max(year)], by = parcel_id
][, .(parcel_id, COUNTY)]

# Future crop projections represent dominant LandIQ season 2.
identity_hist_dominant = landiq_identity_hist[season == 2L]

make_identity_lookup = function(dt, keys, prefix) {
  keep = complete.cases(dt[, ..keys])
  out = dt[keep, .(
    SPECOND = mode_with_missing(SPECOND),
    MULTIUSE = as.numeric(mode_value(MULTIUSE)),
    ADOY = mean_wrapped_doy(ADOY),
    COVER = safe_mean(COVER)
  ), by = keys]
  
  setnames(out, c("SPECOND", "MULTIUSE", "ADOY", "COVER"),
           paste0(prefix, c("SPECOND", "MULTIUSE", "ADOY", "COVER")))
  
  out
}

identity_lookup_county_code = make_identity_lookup(identity_hist_dominant,
                                                   c("COUNTY", "CLASS", "SUBCLASS"), "county_code_")

identity_lookup_code = make_identity_lookup(identity_hist_dominant,
                                            c("CLASS", "SUBCLASS"), "code_")

identity_lookup_county_class = make_identity_lookup(identity_hist_dominant,
                                                    c("COUNTY", "CLASS"), "county_class_")

identity_lookup_class = make_identity_lookup(identity_hist_dominant,
                                             "CLASS", "class_")

attach_identity_attributes = function(future_landiq) {
  dt = copy(future_landiq)
  dt[, row_id_identity := .I]
  dt[, parcel_id := bit64::as.integer64(as.character(parcel_id))]
  dt[, SUBCLASS := normalize_subclass(SUBCLASS)]
  
  dt = merge(dt, parcel_county_fixed, by = "parcel_id", all.x = TRUE)
  dt = merge(dt, identity_lookup_county_code, by = c("COUNTY", "CLASS", "SUBCLASS"), all.x = TRUE)
  dt = merge(dt, identity_lookup_code, by = c("CLASS", "SUBCLASS"), all.x = TRUE)
  dt = merge(dt, identity_lookup_county_class, by = c("COUNTY", "CLASS"), all.x = TRUE)
  dt = merge(dt, identity_lookup_class, by = "CLASS", all.x = TRUE)
  
  dt[, SPECOND := fcoalesce(county_code_SPECOND, code_SPECOND, county_class_SPECOND, class_SPECOND)]
  dt[, MULTIUSE := fcoalesce(county_code_MULTIUSE, code_MULTIUSE, county_class_MULTIUSE, class_MULTIUSE)]
  dt[, ADOY := fcoalesce(county_code_ADOY, code_ADOY, county_class_ADOY, class_ADOY)]
  dt[, COVER := NA_real_]
  
  dt[, `:=`(
    year = as.integer(year),
    season = 2L,
    MULTIUSE = as.numeric(round(MULTIUSE)),
    ADOY = as.numeric(round(ADOY)),
    COVER = as.numeric(COVER)
  )]
  
  if (anyNA(dt$COUNTY)) {
    stop("Projected crop identity contains parcels without fixed COUNTY values.")
  }
  
  setorder(dt, row_id_identity)
  dt[, ..identity_cols]
}

assign_predicted_subclass = function(future_landiq, subclass_obs, lookup_subclass,
                                     crop_col = "CLASS", group_col = "county_safe", start_year = 2023L) {
  
  dt = copy(future_landiq)
  obs = copy(subclass_obs)
  dt[, orig_order := .I]
  
  obs[, `:=`(
    parcel_id = as.character(parcel_id), year = as.integer(year),
    county_safe = as.character(county_safe), CLASS = as.character(CLASS),
    SUBCLASS = normalize_subclass(SUBCLASS)
  )]
  
  if (!"season" %in% names(obs)) obs[, season := 0L]
  obs[, season := as.integer(season)]
  obs[is.na(season), season := 0L]
  obs = obs[year <= start_year & season == 2L]
  
  dt[, `:=`(parcel_id = as.character(parcel_id), CLASS = as.character(get(crop_col)))]
  dt[, (group_col) := as.character(get(group_col))]
  
  old_lookup_cols = intersect(c("SUBCLASS", "CLASS_desc", "SUBCLASS_desc", "PFT"), names(dt))
  if (length(old_lookup_cols)) dt[, (old_lookup_cols) := NULL]
  
  global_probs = obs[!is.na(CLASS) & !is.na(SUBCLASS), .N, by = .(CLASS, SUBCLASS)]
  if (nrow(global_probs)) global_probs[, prob := N / sum(N), by = CLASS]
  
  group_probs = obs[!is.na(CLASS) & !is.na(SUBCLASS), .N, by = .(county_safe, CLASS, SUBCLASS)]
  if (nrow(group_probs)) group_probs[, prob := N / sum(N), by = .(county_safe, CLASS)]
  
  lookup_probs = unique(lookup_subclass[!is.na(CLASS) & !is.na(SUBCLASS), .(CLASS, SUBCLASS)])
  if (nrow(lookup_probs)) lookup_probs[, prob := 1 / .N, by = CLASS]
  
  last_obs_source = obs[!is.na(CLASS) & !is.na(SUBCLASS)]
  last_obs = last_obs_source[
    order(year, season), .SD[.N], by = .(parcel_id, county_safe)
  ][, .(parcel_id, county_safe, last_CLASS = CLASS, last_SUBCLASS = SUBCLASS)]
  
  dt = merge(dt, last_obs, by = c("parcel_id", "county_safe"), all.x = TRUE)
  setorder(dt, parcel_id, year)
  
  dt[, prev_CLASS := shift(CLASS), by = .(parcel_id, county_safe)]
  dt[is.na(prev_CLASS), prev_CLASS := last_CLASS]
  dt[, new_run := fifelse(is.na(CLASS), FALSE, is.na(prev_CLASS) | CLASS != prev_CLASS)]
  dt[, run_id := cumsum(new_run), by = .(parcel_id, county_safe)]
  dt[, SUBCLASS := NA_character_]
  
  dt[run_id == 0 & !is.na(CLASS) & !is.na(last_CLASS) &
       CLASS == last_CLASS & !is.na(last_SUBCLASS), SUBCLASS := last_SUBCLASS]
  
  run_table = unique(dt[!is.na(CLASS) & is.na(SUBCLASS),
                        .(parcel_id, county_safe, run_id, CLASS)])
  
  if (nrow(run_table)) {
    run_table[, drawn_SUBCLASS := NA_character_]
    draw_groups = unique(run_table[, .(county_safe, CLASS)])
    
    for (ii in seq_len(nrow(draw_groups))) {
      cty = draw_groups$county_safe[ii]
      cls = draw_groups$CLASS[ii]
      idx = which(run_table$county_safe == cty & run_table$CLASS == cls)
      
      choices = group_probs[county_safe == cty & CLASS == cls]
      if (!nrow(choices)) choices = global_probs[CLASS == cls]
      if (!nrow(choices)) choices = lookup_probs[CLASS == cls]
      if (!"prob" %in% names(choices)) choices[, prob := NA_real_]
      
      choices = choices[!is.na(SUBCLASS) & !is.na(prob) & is.finite(prob) & prob > 0]
      
      if (nrow(choices)) {
        choices[, prob := prob / sum(prob)]
        run_table$drawn_SUBCLASS[idx] = sample(
          choices$SUBCLASS, size = length(idx), replace = TRUE, prob = choices$prob)
      }
    }
    
    dt = merge(dt, run_table[, .(parcel_id, county_safe, run_id, drawn_SUBCLASS)],
               by = c("parcel_id", "county_safe", "run_id"), all.x = TRUE)
    
    dt[is.na(SUBCLASS), SUBCLASS := drawn_SUBCLASS]
    dt[, drawn_SUBCLASS := NULL]
  }
  
  helper_cols = intersect(c("last_CLASS", "last_SUBCLASS", "prev_CLASS", "new_run", "run_id"), names(dt))
  dt[, (helper_cols) := NULL]
  
  dt = merge(dt, lookup_subclass, by = c("CLASS", "SUBCLASS"), all.x = TRUE)
  setorder(dt, orig_order)
  dt[, orig_order := NULL]
  
  dt[]
}

# ---- run one shared crop projection ----
message("Using shared optimized crop matrices from: ", crop_matrix_dir)
message("Writing shared crop projections to: ", prediction_dir)

crop_mats = load_crop_matrices(crop_matrix_dir)

missing_mats = setdiff(unique(crop_data$county_safe), names(crop_mats))
if (length(missing_mats)) {
  message("Counties in crop_year_states_cleaned without crop matrices: ", length(missing_mats))
  message("Missing counties: ", paste(sort(missing_mats), collapse = ", "))
}

crop_projection = predict_grouped_markov(year_states = crop_data, transition_mats = crop_mats,
                                         group_col = "county_safe", start_year = start_year, end_year = end_year, state_col = "crop_class")

future_crop = crop_projection$predictions
crop_projection_qc = crop_projection$qc

if (!nrow(future_crop)) stop("No future crop predictions were generated.")

arrow::write_parquet(crop_projection_qc,
                     file.path(prediction_dir, "crop_projection_qc.parquet"), compression = "zstd")

# add stable parcel metadata needed for subclass assignment / diagnostics
parcel_meta = crop_data[
  year <= start_year, .SD[which.max(year)], by = parcel_id
][, .(parcel_id, county, county_geoid, county_safe, ACRES)]

future_landiq = merge(future_crop, parcel_meta, by = c("parcel_id", "county_safe"), all.x = TRUE)
future_landiq = future_landiq[year >= start_year + 1L & year <= end_year]

future_landiq = assign_predicted_subclass(future_landiq = future_landiq,
                                          subclass_obs = with_subclass, lookup_subclass = lookup_subclass,
                                          crop_col = "CLASS", group_col = "county_safe", start_year = start_year)

future_landiq[, SUBCLASS := normalize_subclass(SUBCLASS)]
projected_crop_identity = attach_identity_attributes(future_landiq)

# ---- write annual future crop-identity parquets ----
for (yy in seq(start_year + 1L, end_year)) {
  crop_year = copy(projected_crop_identity[year == yy])
  
  crop_year[, `:=`(
    parcel_id = bit64::as.integer64(as.character(parcel_id)),
    year = as.integer(year),
    season = as.integer(season)
  )]
  
  setorder(crop_year, parcel_id, season)
  
  if (!identical(names(crop_year), identity_cols)) {
    stop("Crop identity schema error for year ", yy, ".")
  }
  
  out_path = file.path(prediction_dir, paste0("crop_identity_statewide_", yy, ".parquet"))
  arrow::write_parquet(crop_year, out_path, compression = "zstd")
  message("Wrote: ", out_path)
}

# optional combined historical + projected crop identity file
crop_identity_all_years = rbindlist(
  list(landiq_identity_hist, projected_crop_identity),
  use.names = TRUE, fill = FALSE
)

crop_identity_all_years[, parcel_id := bit64::as.integer64(parcel_id)]
setorder(crop_identity_all_years, parcel_id, year, season)

if (!identical(names(crop_identity_all_years), identity_cols)) {
  stop("Internal all-years crop identity schema error.")
}

all_years_path = file.path(prediction_dir, "crops_all_years.parq")
arrow::write_parquet(crop_identity_all_years, all_years_path, compression = "zstd")
message("Wrote combined crop identity parquet: ", all_years_path)

# ---- manifest ----
crop_manifest = future_landiq[, .(
  n_rows = .N,
  n_parcels = uniqueN(parcel_id),
  total_acres = sum(ACRES, na.rm = TRUE)
), by = county_safe]

manifest_path = file.path(prediction_dir, "crop_prediction_manifest.parquet")
arrow::write_parquet(crop_manifest, manifest_path, compression = "zstd")

message("Finished shared crop projection.")
print(crop_manifest)