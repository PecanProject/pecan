## Predicts parcel-level crop classes through 2045 from optimized county
## transition matrices and writes county-separated BAU/NBS outputs.

pacman::p_load(PEcAn.data.remote, data.table)

# ---- set up ----
#Sys.setenv(CCMMF_WORK_ROOT = "/path/to/yourusername")
work_root = Sys.getenv("CCMMF_WORK_ROOT")

if (!nzchar(work_root)) {
  stop("CCMMF_WORK_ROOT is not set. Set it to the workspace containing ", "crop_year_states_cleaned.csv and the scenario/matrix outputs.")
}

ccmmf_root = Sys.getenv("CCMMF_SHARED_ROOT", unset = "/projectnb/dietzelab/ccmmf")

config = list(seed = 42, run_scenarios = c("BAU_Targets", "NBS_Targets"), start_year = 2023L, end_year = 2045L,

  all_data_path = file.path(work_root, "all_data.csv"),

  year_states_path = file.path(work_root, 'crop_year_states_cleaned.csv'),

  crop_history_path = file.path(work_root, "crops_full_counties.csv"),

  lookup_path = file.path(ccmmf_root, "management", "LandIQ_cropCode_lookup_table.csv"),

  prediction_root_dir = file.path(work_root, "county_landiq_predictions"),

  crop_matrix_dir = file.path(work_root, "county_optimized_matrices"),

  tillage_target_root = file.path(work_root, "county_tillage_targets")
  )

set.seed(config[["seed"]])

run_scenarios = config[["run_scenarios"]]
prediction_root_dir = config[["prediction_root_dir"]]
crop_matrix_dir = config[["crop_matrix_dir"]]
tillage_target_root = config[["tillage_target_root"]]
start_year = config[["start_year"]]
end_year = config[["end_year"]]

##-------helper functions-------
safe_county_name = function(x) {gsub("[^A-Za-z0-9_]+", "_", x)}


read_tmat = function(path) {

  tmat_df = fread(path)

  row_id_col = colnames(tmat_df)[1]
  states = as.character(colnames(tmat_df)[-1])
  row_states = as.character(tmat_df[[row_id_col]])

  tmat_final = as.matrix(tmat_df[, -1, with = FALSE])
  rownames(tmat_final) = row_states
  colnames(tmat_final) = states
  storage.mode(tmat_final) = "double"

  stopifnot(all(rownames(tmat_final) == colnames(tmat_final)))

  return(tmat_final)}


repair_transition_matrix = function(A, matrix_name = "matrix") {

  A[is.na(A)] = 0

  if (any(A < 0, na.rm = TRUE)) {
    warning(matrix_name, " has negative probabilities. Minimum = ",
            min(A, na.rm = TRUE), ". Clamping negatives to 0.")
    A[A < 0] = 0}

  if (any(A > 1, na.rm = TRUE)) {
    warning(matrix_name, " has probabilities > 1. Maximum = ", max(A, na.rm = TRUE),
            ". Clamping values above 1 to 1.")
    A[A > 1] = 1}

  row_sums = rowSums(A)
  zero_rows = names(row_sums)[is.na(row_sums) | row_sums == 0]

  if (length(zero_rows) > 0) {
    warning(matrix_name,  " has zero-sum rows. Setting those rows to self-loop: ",
            paste(zero_rows, collapse = ", "))

    for (s in zero_rows) {A[s, ] = 0
    A[s, s] = 1}}

  row_sums = rowSums(A)
  A = sweep(A, 1, row_sums, "/")

  return(A)}


load_crop_matrices = function(crop_matrix_dir) {

  matrix_files = list.files(crop_matrix_dir, pattern = "_crop_matrix\\.csv$",
                            full.names = TRUE)

  if (length(matrix_files) == 0) {stop("No optimized crop matrix files found in: ", crop_matrix_dir)}

  transition_mats = list()

  for (f in matrix_files) {matrix_name = sub("_crop_matrix\\.csv$", "", basename(f))

  A = read_tmat(f)
  A = repair_transition_matrix(A, paste0("optimized crop matrix ", matrix_name))

  transition_mats[[matrix_name]] = A}

  return(transition_mats)}


load_crop_targets = function(target_dir, scenario_safe, end_year) {

  pattern = paste0("crop_targets_.*_", scenario_safe, "_", end_year, "\\.csv$")
  files = list.files(target_dir, pattern = pattern, full.names = TRUE)

  if (length(files) == 0) {stop("No crop target files found in: ", target_dir,
                                " with pattern: ", pattern)}

  out = rbindlist(lapply(files, fread), fill = TRUE)

  out[, county_safe := as.character(county_safe)]
  out[, crop_state := as.character(crop_state)]

  if ("target_acres_used_for_opt" %in% names(out)) {
    out[, target_acres := as.numeric(target_acres_used_for_opt)]
  } else {
    out[, target_acres := as.numeric(target_acres_raw)]}

  out = out[
    !is.na(county_safe) & !is.na(crop_state),
    .(target_acres = sum(target_acres, na.rm = TRUE)),
    by = .(county_safe, crop_state)
  ]

  return(out[])}


make_matrix_powers = function(tmat, n_years) {

  states = rownames(tmat)
  powers = vector("list", n_years)

  A_power = diag(length(states))
  rownames(A_power) = states
  colnames(A_power) = states

  for (i in seq_len(n_years)) {
    A_power = A_power %*% tmat
    rownames(A_power) = states
    colnames(A_power) = states
    powers[[i]] = A_power}

  return(powers)}

##-------crop prediction functions-------
predict_county_to_targets = function(start_info, tmat, targets_cty,
                                     start_year, end_year,
                                     state_col = "crop_class") {

  dt = copy(start_info)
  states = rownames(tmat)
  n_years = end_year - start_year
  years = seq(start_year + 1L, end_year)

  dt = dt[get(state_col) %in% states]
  dt[, start_CLASS := as.character(get(state_col))]
  dt[, ACRES := as.numeric(ACRES)]
  dt = dt[!is.na(ACRES) & ACRES > 0]

  if (nrow(dt) == 0) {return(data.table())}

  powers = make_matrix_powers(tmat, n_years)
  A_final = powers[[n_years]]

  county_total = sum(dt$ACRES, na.rm = TRUE)

  #Full scenario target vector, used only for diagnostics/checking.
  target_vec = setNames(rep(0, length(states)), states)

  if (nrow(targets_cty) > 0) {matched = intersect(targets_cty$crop_state, states)
  target_vec[matched] = targets_cty[match(matched, crop_state), target_acres]}

  if (sum(target_vec, na.rm = TRUE) > 0) {target_vec = target_vec / sum(target_vec, na.rm = TRUE) * county_total}

  #Current county crop acreage vector
  x0_dt = dt[, .(acres = sum(ACRES, na.rm = TRUE)), by = start_CLASS]

  X0_vec = setNames(rep(0, length(states)), states)
  X0_vec[x0_dt$start_CLASS] = x0_dt$acres

  #Expected 2045 county acres from the optimized scenario matrix
  matrix_target_vec = as.numeric(X0_vec %*% A_final)
  names(matrix_target_vec) = states
  matrix_target_vec[!is.finite(matrix_target_vec)] = 0

  #Preserve county total
  if (sum(matrix_target_vec, na.rm = TRUE) > 0) {
    matrix_target_vec = matrix_target_vec / sum(matrix_target_vec, na.rm = TRUE) * county_total
  } else {
    matrix_target_vec = X0_vec}

  #Assign parcels to final classes so county totals follow the optimized matrix projection.
  dt[, final_CLASS := NA_character_]
  dt[, prob_crop_class_final := NA_real_]

  remaining_target = copy(matrix_target_vec)

  for (to_state in states[order(-matrix_target_vec)]) {

    need = remaining_target[to_state]

    if (!is.finite(need) || need <= 0) {
      next
    }

    candidates = copy(dt[is.na(final_CLASS)])

    if (nrow(candidates) == 0) {
      break
    }

    candidates[, prob_to := A_final[cbind(start_CLASS, rep(to_state, .N))]]
    candidates[is.na(prob_to) | !is.finite(prob_to), prob_to := 0]

    #Prefer parcels that the optimized matrix says are likely to become this class.
    #Smaller acreage parcels help reduce overshoot.
    setorder(candidates, -prob_to, ACRES)

    candidates[, cum_acres := cumsum(ACRES)]

    take_ids = candidates[
      cum_acres <= need | shift(cum_acres, fill = 0) < need,
      parcel_id]

    if (length(take_ids) > 0) {

      dt[parcel_id %in% take_ids, `:=`(
        final_CLASS = to_state,
        prob_crop_class_final = A_final[cbind(start_CLASS, rep(to_state, .N))]
      )]

      assigned_acres = dt[parcel_id %in% take_ids, sum(ACRES, na.rm = TRUE)]
      remaining_target[to_state] = max(0, remaining_target[to_state] - assigned_acres)}}

  #Any unassigned parcels stay in their original class.
  dt[is.na(final_CLASS), `:=`(
    final_CLASS = start_CLASS,
    prob_crop_class_final = A_final[cbind(start_CLASS, start_CLASS)])]

  #Diagnostic: compare optimized-matrix expected acres vs parcel-assigned acres.
  assigned_vec_dt = dt[, .(assigned_acres = sum(ACRES, na.rm = TRUE)), by = final_CLASS]

  assigned_vec = setNames(rep(0, length(states)), states)
  assigned_vec[assigned_vec_dt$final_CLASS] = assigned_vec_dt$assigned_acres

  matrix_assignment_check = data.table(CLASS = states,
                                       start_acres = as.numeric(X0_vec[states]),
                                       scenario_target_acres = as.numeric(target_vec[states]),
                                       optimized_matrix_expected_acres = as.numeric(matrix_target_vec[states]),
                                       parcel_assigned_acres = as.numeric(assigned_vec[states]))

  matrix_assignment_check[, matrix_assignment_diff_acres :=
                            parcel_assigned_acres - optimized_matrix_expected_acres]
  matrix_assignment_check[, matrix_assignment_abs_diff_acres :=
                            abs(matrix_assignment_diff_acres)]
  matrix_assignment_check[, scenario_target_diff_acres :=
                            parcel_assigned_acres - scenario_target_acres]
  matrix_assignment_check[, scenario_target_abs_diff_acres :=
                            abs(scenario_target_diff_acres)]

  message("Optimized-matrix parcel assignment check:")
  print(matrix_assignment_check[order(-matrix_assignment_abs_diff_acres)])

  #Build annual time series: keep start class until conversion year.
  pred_list = vector("list", nrow(dt))

  for (i in seq_len(nrow(dt))) {

    parcel = dt$parcel_id[i]
    start_state = dt$start_CLASS[i]
    final_state = dt$final_CLASS[i]

    pred_state = rep(start_state, n_years)
    pred_prob = numeric(n_years)

    if (final_state == start_state) {

      for (yy in seq_len(n_years)) {
        pred_prob[yy] = powers[[yy]][start_state, start_state]
      }

      pred_prob[n_years] = dt$prob_crop_class_final[i]

    } else {

      conversion_idx = which(sapply(seq_len(n_years), function(k) {
        powers[[k]][start_state, final_state] >= powers[[k]][start_state, start_state]
      }))

      if (length(conversion_idx) == 0) {
        conversion_idx = n_years
      } else {
        conversion_idx = conversion_idx[1]
      }

      pred_state[conversion_idx:n_years] = final_state

      for (yy in seq_len(n_years)) {
        pred_prob[yy] = powers[[yy]][start_state, pred_state[yy]]
      }

      pred_prob[n_years] = dt$prob_crop_class_final[i]
    }

    pred_list[[i]] = data.table(
      parcel_id = parcel,
      year = years,
      CLASS = pred_state,
      prob_crop_class = as.numeric(pred_prob)
    )
  }

  out = rbindlist(pred_list, fill = TRUE)
  return(out[])}


predict_grouped_markov_to_targets = function(year_states, transition_mats,
                                             crop_targets_2045,
                                             group_col,
                                             start_year,
                                             end_year,
                                             state_col = "crop_class") {

  dt = copy(year_states)
  all_preds = list()

  groups = intersect(unique(na.omit(dt[[group_col]])), names(transition_mats))

  if (length(groups) == 0) {stop("No overlapping groups between crop_data and transition matrices.")}

  for (g in groups) {

    message("Predicting crop class with optimized scenario matrix for county: ", g)

    dt_g = dt[get(group_col) == g]
    tmat_g = transition_mats[[g]]

    start_info = dt_g[
      year <= start_year,
      .SD[which.max(year)],
      by = parcel_id]

    targets_g = crop_targets_2045[county_safe == g]

    preds_g = predict_county_to_targets(start_info = start_info, tmat = tmat_g,
                                        targets_cty = targets_g, start_year = start_year,
                                        end_year = end_year, state_col = state_col)

    if (nrow(preds_g) == 0) {
      next
    }

    preds_g[, (group_col) := g]
    all_preds[[g]] = preds_g
  }

  return(rbindlist(all_preds, fill = TRUE))}
##--------load & clean all_data---------
if (!file.exists(config[["all_data_path"]])) {
  stop("all_data.csv not found: ", config[["all_data_path"]])
}

all_data = fread(config[["all_data_path"]])

if ("V1" %in% names(all_data)) {all_data[, V1 := NULL]}

if ("state" %in% names(all_data) && !"till_state" %in% names(all_data)) {
  setnames(all_data, "state", "till_state")}

if ("non_dom_prob" %in% names(all_data) && !"till_non_dom_prob" %in% names(all_data)) {
  setnames(all_data, "non_dom_prob", "till_non_dom_prob")}

setDT(all_data)

required_cols = c("parcel_id", "year", "county", "crop_class", "ACRES")
missing_cols = setdiff(required_cols, names(all_data))

if (length(missing_cols) > 0) {
  stop("all_data is missing required columns: ", paste(missing_cols, collapse = ", "))}

all_data[, `:=`(
  parcel_id = as.character(parcel_id), year = as.integer(year), county = as.character(county),
  crop_class = trimws(as.character(crop_class)), ACRES = as.numeric(ACRES))]

if (!"till_state" %in% names(all_data)) {all_data[, till_state := NA_character_]
} else {
  all_data[, till_state := trimws(as.character(till_state))]
}

if (!"county_geoid" %in% names(all_data)) {all_data[, county_geoid := NA_character_]}

if (!"season" %in% names(all_data)) {all_data[, season := NA_integer_]}

all_data[, county_safe := safe_county_name(county)]

## ---- load full crop-year states ----

if (!file.exists(config[["year_states_path"]])) {
  stop("crop_year_states_cleaned.csv not found: ", config[["year_states_path"]])
}

crop_data = fread(config[["year_states_path"]])

if ("V1" %in% names(crop_data)) {
  crop_data[, V1 := NULL]
}

setDT(crop_data)

required_crop_cols = c("parcel_id", "year", "county", "county_geoid", "state", "ACRES")

missing_crop_cols = setdiff(required_crop_cols, names(crop_data))

if (length(missing_crop_cols) > 0) {
  stop("crop_year_states_cleaned.csv is missing required columns: ",
    paste(missing_crop_cols, collapse = ", "))
}

crop_data[, `:=`(
  parcel_id = as.character(parcel_id), year = as.integer(year), county = as.character(county),
  county_geoid = as.character(county_geoid), crop_class = trimws(as.character(state)), ACRES = as.numeric(ACRES)
)]

crop_data[
  ,
  county_safe := safe_county_name(county)
]

##--------load lookup table---------
if (!file.exists(config[["lookup_path"]])) {
  stop("LandIQ crop lookup not found: ", config[["lookup_path"]])
}

lookup = fread(config[["lookup_path"]])

lookup[, `:=`(
  CLASS = as.character(CLASS), SUBCLASS = as.character(SUBCLASS))]

lookup_subclass = unique(lookup[, .(CLASS, SUBCLASS, CLASS_desc, SUBCLASS_desc, PFT)],
                         by = c("CLASS", "SUBCLASS"))


##--------load historical LandIQ source with subclass---------
if (!file.exists(config[["crop_history_path"]])) {
  stop("Historical subclass file not found: ", config[["crop_history_path"]])
}

with_subclass = fread(config[["crop_history_path"]])

if ("V1" %in% names(with_subclass)) {with_subclass[, V1 := NULL]}

if (!"CLASS" %in% names(with_subclass) && "crop_class" %in% names(with_subclass)) {
  with_subclass[, CLASS := as.character(crop_class)]}

if (!"SUBCLASS" %in% names(with_subclass) && "subclass" %in% names(with_subclass)) {
  setnames(with_subclass, "subclass", "SUBCLASS")}

if (!"county_safe" %in% names(with_subclass) && "county" %in% names(with_subclass)) {
  with_subclass[, county_safe := safe_county_name(county)]}

if (!"season" %in% names(with_subclass)) {with_subclass[, season := 0L]}

required_subclass_cols = c("parcel_id", "year", "county_safe", "CLASS", "SUBCLASS")
missing_subclass_cols = setdiff(required_subclass_cols, names(with_subclass))

if (length(missing_subclass_cols) > 0) {
  stop("crops_full_counties.csv is missing required columns: ",
       paste(missing_subclass_cols, collapse = ", "),
       "\nAvailable columns: ", paste(names(with_subclass), collapse = ", "))}

with_subclass[, `:=`(
  parcel_id = as.character(parcel_id), year = as.integer(year),
  county_safe = as.character(county_safe), CLASS = trimws(as.character(CLASS)),
  SUBCLASS = trimws(as.character(SUBCLASS)), season = as.integer(season))]

with_subclass[CLASS == "" | CLASS == "NA", CLASS := NA_character_]
with_subclass[SUBCLASS == "" | SUBCLASS == "NA", SUBCLASS := NA_character_]
with_subclass[is.na(season), season := 0L]

message("Loaded subclass source with ", nrow(with_subclass[!is.na(SUBCLASS)]),
        " rows with non-NA SUBCLASS.")

if (!"CLASS" %in% names(all_data)) {all_data[, CLASS := as.character(crop_class)]}


assign_predicted_subclass = function(future_landiq, subclass_obs, lookup_subclass,
                                     crop_col = "CLASS", group_col = "county_safe",
                                     start_year = 2023L) {

  dt = copy(future_landiq)
  obs = copy(subclass_obs)

  dt[, orig_order := .I]

  obs[, `:=`(
    parcel_id = as.character(parcel_id), year = as.integer(year),
    county_safe = as.character(county_safe),CLASS = as.character(CLASS), SUBCLASS = as.character(SUBCLASS))]

  if (!"season" %in% names(obs)) {
    obs[, season := 0L]
  } else {
    obs[, season := as.integer(season)]
    obs[is.na(season), season := 0L]
  }

  obs = obs[year <= start_year]

  dt[, parcel_id := as.character(parcel_id)]
  dt[, CLASS := as.character(get(crop_col))]
  dt[, (group_col) := as.character(get(group_col))]

  old_lookup_cols = intersect(c("SUBCLASS", "CLASS_desc", "SUBCLASS_desc", "PFT"),
                              names(dt))

  if (length(old_lookup_cols) > 0) {dt[, (old_lookup_cols) := NULL]}

  global_probs = obs[
    !is.na(CLASS) & !is.na(SUBCLASS),
    .N, by = .(CLASS, SUBCLASS)]

  if (nrow(global_probs) > 0) {global_probs[, prob := N / sum(N), by = CLASS]}

  group_probs = obs[
    !is.na(CLASS) & !is.na(SUBCLASS),
    .N, by = .(county_safe, CLASS, SUBCLASS)]

  if (nrow(group_probs) > 0) {group_probs[, prob := N / sum(N), by = .(county_safe, CLASS)]}

  lookup_probs = unique(lookup_subclass[!is.na(CLASS) & !is.na(SUBCLASS), .(CLASS, SUBCLASS)])

  if (nrow(lookup_probs) > 0) {lookup_probs[, N := 1L]
    lookup_probs[, prob := 1 / .N, by = CLASS]}

  last_obs_source = obs[!is.na(CLASS) & !is.na(SUBCLASS)]

  last_obs = last_obs_source[
    order(year, season),
    .SD[.N], by = .(parcel_id, county_safe)]

  if (!"SUBCLASS" %in% names(last_obs)) {last_obs[, SUBCLASS := NA_character_]}

  if (!"CLASS" %in% names(last_obs)) {last_obs[, CLASS := NA_character_]}

  last_obs = last_obs[
    ,
    .(parcel_id, county_safe, last_CLASS = CLASS, last_SUBCLASS = SUBCLASS)
  ]

  dt = merge(dt, last_obs, by = c("parcel_id", "county_safe"), all.x = TRUE)

  setorder(dt, parcel_id, year)

  dt[, prev_CLASS := shift(CLASS), by = .(parcel_id, county_safe)]
  dt[is.na(prev_CLASS), prev_CLASS := last_CLASS]

  dt[, new_run := fifelse(is.na(CLASS), FALSE, is.na(prev_CLASS) | CLASS != prev_CLASS)]
  dt[, run_id := cumsum(new_run), by = .(parcel_id, county_safe)]

  dt[, SUBCLASS := NA_character_]

  dt[
    run_id == 0 &
      !is.na(CLASS) &
      !is.na(last_CLASS) &
      CLASS == last_CLASS &
      !is.na(last_SUBCLASS),
    SUBCLASS := last_SUBCLASS]

  run_table = unique(dt[!is.na(CLASS) & is.na(SUBCLASS), .(parcel_id, county_safe, run_id, CLASS)])

  if (nrow(run_table) > 0) {

    run_table[, drawn_SUBCLASS := NA_character_]
    draw_groups = unique(run_table[, .(county_safe, CLASS)])

    for (ii in seq_len(nrow(draw_groups))) {

      cty = draw_groups$county_safe[ii]
      cls = draw_groups$CLASS[ii]

      idx = which(run_table$county_safe == cty & run_table$CLASS == cls)

      choices = group_probs[county_safe == cty & CLASS == cls]

      if (nrow(choices) == 0) {
        choices = global_probs[CLASS == cls]
      }

      if (nrow(choices) == 0) {
        choices = lookup_probs[CLASS == cls]
      }

      if (!("prob" %in% names(choices))) {
        choices[, prob := NA_real_]
      }

      choices = choices[
        !is.na(SUBCLASS) &
          !is.na(prob) &
          is.finite(prob) &
          prob > 0]

      if (nrow(choices) > 0) {
        choices[, prob := prob / sum(prob)]

        run_table$drawn_SUBCLASS[idx] = sample(
          choices$SUBCLASS,
          size = length(idx),
          replace = TRUE,
          prob = choices$prob
        )
      }
    }

    dt = merge(dt, run_table[, .(parcel_id, county_safe, run_id, drawn_SUBCLASS)],
               by = c("parcel_id", "county_safe", "run_id"), all.x = TRUE)

    dt[is.na(SUBCLASS), SUBCLASS := drawn_SUBCLASS]
    dt[, drawn_SUBCLASS := NULL]
  }

  helper_cols = intersect(c("last_CLASS", "last_SUBCLASS", "prev_CLASS", "new_run", "run_id"),
                          names(dt))

  dt[, (helper_cols) := NULL]

  if (!"SUBCLASS" %in% names(dt)) {
    dt[, SUBCLASS := NA_character_]
  }

  if (!"CLASS" %in% names(dt)) {
    dt[, CLASS := NA_character_]
  }

  dt = merge(dt, lookup_subclass, by = c("CLASS", "SUBCLASS"), all.x = TRUE)

  setorder(dt, orig_order)
  dt[, orig_order := NULL]

  return(dt[])}
##--------till target functions---------
load_crop_by_till_targets = function(target_dir, scenario_safe, end_year) {

  pattern = paste0("crop_by_till_targets_.*_", scenario_safe, "_", end_year, "\\.csv$")
  files = list.files(target_dir, pattern = pattern, full.names = TRUE)

  if (length(files) == 0) {stop("No crop-by-till target files found in: ", target_dir,
                                " with pattern: ", pattern)}

  out = rbindlist(lapply(files, fread), fill = TRUE)

  out[, county_safe := as.character(county_safe)]
  out[, crop_state := as.character(crop_state)]
  out[, till_state := as.character(till_state)]
  out[, target_acres_raw := as.numeric(target_acres_raw)]

  return(out[])}

build_annual_till_targets = function(all_data, till_targets_2045, start_year, end_year) {

  latest_obs = all_data[
    year <= start_year & !is.na(crop_class) & !is.na(till_state),
    .SD[which.max(year)],
    by = parcel_id]

  baseline_till = latest_obs[
    ,
    .(baseline_acres = sum(ACRES, na.rm = TRUE)), by = .(county_safe, crop_state = crop_class, till_state)]

  if (nrow(baseline_till) > 0) {
    baseline_till[, baseline_share := baseline_acres / sum(baseline_acres),
                  by = .(county_safe, crop_state)]
  } else {
    baseline_till[, baseline_share := numeric()]
  }

  target_till = copy(till_targets_2045)
  target_till[, target_share := target_acres_raw / sum(target_acres_raw),
              by = .(county_safe, crop_state)]

  all_counties = unique(target_till$county_safe)
  all_crops = unique(target_till$crop_state)
  all_till_states = unique(c(baseline_till$till_state, target_till$till_state))

  annual_till_targets = CJ(county_safe = all_counties, crop_state = all_crops,
                           till_state = all_till_states, year = seq(start_year + 1L, end_year))

  annual_till_targets = merge(annual_till_targets,
                              baseline_till[, .(county_safe, crop_state, till_state, baseline_share)],
                              by = c("county_safe", "crop_state", "till_state"), all.x = TRUE)

  annual_till_targets = merge(annual_till_targets,
                              target_till[, .(county_safe, crop_state, till_state, target_share)],
                              by = c("county_safe", "crop_state", "till_state"), all.x = TRUE)

  annual_till_targets[is.na(baseline_share), baseline_share := 0]
  annual_till_targets[is.na(target_share), target_share := 0]

  annual_till_targets[, ramp := (year - start_year) / (end_year - start_year)]

  annual_till_targets[, till_share :=
                        (1 - ramp) * baseline_share + ramp * target_share]

  annual_till_targets[
    ,
    share_sum := sum(till_share, na.rm = TRUE), by = .(county_safe, crop_state, year)]

  annual_till_targets[share_sum > 0, till_share := till_share / share_sum]
  annual_till_targets[share_sum <= 0, till_share := 1 / .N,
                      by = .(county_safe, crop_state, year)]

  annual_till_targets[, share_sum := NULL]

  return(annual_till_targets[])}

assign_till_by_targets = function(future_landiq, annual_till_targets) {

  dt = copy(future_landiq)
  targets_dt = copy(annual_till_targets)

  dt[, CLASS := as.character(CLASS)]
  dt[, ACRES := as.numeric(ACRES)]
  dt[is.na(ACRES) | !is.finite(ACRES) | ACRES < 0, ACRES := 0]

  targets_dt[, county_safe := as.character(county_safe)]
  targets_dt[, crop_state := as.character(crop_state)]
  targets_dt[, till_state := as.character(till_state)]
  targets_dt[, year := as.integer(year)]
  targets_dt[, till_share := as.numeric(till_share)]

  targets_dt = targets_dt[
    !is.na(till_state) &
      !is.na(till_share) &
      is.finite(till_share) &
      till_share > 0]

  setkey(targets_dt, county_safe, year, crop_state)

  dt[, row_id := .I]

  out = dt[, {

    cty = county_safe[1]
    yr = year[1]
    cls = CLASS[1]

    targets = targets_dt[.(cty, yr, cls), nomatch = 0]
    temp = copy(.SD)

    if (nrow(targets) == 0 || sum(targets$till_share, na.rm = TRUE) <= 0) {

      temp[, till_state := NA_character_]
      temp[, prob_till_state := NA_real_]
      temp

    } else {

      targets = copy(targets)
      targets[, till_share := till_share / sum(till_share)]
      setorder(targets, till_state)

      group_total_acres = sum(temp$ACRES, na.rm = TRUE)

      if (!is.finite(group_total_acres) || group_total_acres <= 0) {

        assigned = sample(targets$till_state, size = nrow(temp), replace = TRUE,
                          prob = targets$till_share)

        prob_lookup = setNames(targets$till_share, targets$till_state)

        temp[, till_state := assigned]
        temp[, prob_till_state := prob_lookup[till_state]]
        temp

      } else {

        temp[, rand_order := runif(.N)]
        setorder(temp, rand_order)

        targets[, target_acres := till_share * group_total_acres]
        targets[, upper_acres := cumsum(target_acres)]

        temp[, cum_mid_acres := cumsum(ACRES) - ACRES / 2]

        idx = findInterval(temp$cum_mid_acres, targets$upper_acres) + 1L
        idx[idx < 1L] = 1L
        idx[idx > nrow(targets)] = nrow(targets)

        temp[, till_state := targets$till_state[idx]]

        prob_lookup = setNames(targets$till_share, targets$till_state)
        temp[, prob_till_state := prob_lookup[till_state]]

        temp[, c("rand_order", "cum_mid_acres") := NULL]
        temp
      }
    }

  }, by = .(county_safe, year, CLASS)]

  setorder(out, row_id)
  out[, row_id := NULL]

  return(out[])}


##--------run predictions---------
#Load shared optimized crop matrices once. These are NOT scenario-nested anymore.
crop_mats = load_crop_matrices(crop_matrix_dir = crop_matrix_dir)

missing_mats = setdiff(unique(crop_data$county_safe), names(crop_mats))
if (length(missing_mats) > 0) {message("Counties in crop_year_states_cleaned without crop matrices: ", length(missing_mats))}

run_one_prediction_scenario = function(run_scenario) {

  scenario_safe = safe_county_name(run_scenario)
  scenario_prediction_dir = file.path(prediction_root_dir, scenario_safe)
  target_dir = file.path(tillage_target_root, scenario_safe)

  dir.create(scenario_prediction_dir, recursive = TRUE, showWarnings = FALSE)

  message("Running prediction scenario: ", run_scenario)
  message("Scenario-safe folder name: ", scenario_safe)
  message("Reading tillage/crop targets from: ", target_dir)
  message("Writing predictions to: ", scenario_prediction_dir)

  if (!dir.exists(target_dir)) {
    stop("Scenario target folder not found: ", target_dir)}

  ##The actual crop-class prediction is driven by the shared optimized crop matrix.
  crop_targets_2045 = load_crop_targets(target_dir = target_dir,
                                        scenario_safe = scenario_safe, end_year = end_year)

  future_crop = predict_grouped_markov_to_targets(
    year_states = crop_data, transition_mats = crop_mats,
    crop_targets_2045 = crop_targets_2045, group_col = "county_safe",
    start_year = start_year, end_year = end_year, state_col = "crop_class")

  if (nrow(future_crop) == 0) {stop("No future crop predictions were generated for scenario: ", run_scenario)}

  ##--------add parcel metadata---------
  parcel_meta = crop_data[
    year <= start_year,
    .SD[which.max(year)],
    by = parcel_id
  ][
    ,
    .(parcel_id, county, county_geoid, county_safe, ACRES)
  ]

  future_landiq = merge(
    future_crop,
    parcel_meta,
    by = c("parcel_id", "county_safe"),
    all.x = TRUE
  )

  ##--------assign till states from scenario-specific tillage targets---------
  message("Assigning till states using ", run_scenario, " crop-by-till targets.")

  till_targets_2045 = load_crop_by_till_targets(
    target_dir = target_dir,
    scenario_safe = scenario_safe,
    end_year = end_year
  )

  annual_till_targets = build_annual_till_targets(
    all_data = all_data,
    till_targets_2045 = till_targets_2045,
    start_year = start_year,
    end_year = end_year
  )

  future_landiq = assign_till_by_targets(
    future_landiq = future_landiq,
    annual_till_targets = annual_till_targets
  )

  ##--------final formatting---------
  future_landiq[, season := NA_integer_]
  future_landiq[, source := "predicted"]
  future_landiq[, scenario := run_scenario]
  future_landiq[, scenario_safe := scenario_safe]

  future_landiq = future_landiq[year >= start_year + 1L & year <= end_year]

  future_landiq = assign_predicted_subclass(
    future_landiq = future_landiq,
    subclass_obs = with_subclass,
    lookup_subclass = lookup_subclass,
    crop_col = "CLASS",
    group_col = "county_safe",
    start_year = start_year
  )

  optional_cols = c("SUBCLASS", "CLASS_desc", "SUBCLASS_desc", "PFT")

  for (cc in optional_cols) {
    if (!(cc %in% names(future_landiq))) {
      future_landiq[, (cc) := NA_character_]
    }
  }

  final_cols = c("parcel_id", "county", "county_geoid", "county_safe", "year", "season",
                 "CLASS", "SUBCLASS", "CLASS_desc", "SUBCLASS_desc", "PFT",
                 "till_state", "prob_crop_class", "prob_till_state", "ACRES", "source",
                 "scenario")

  future_landiq = future_landiq[, ..final_cols]
  setorder(future_landiq, county_safe, parcel_id, year)

  ##--------write county-separated outputs---------
  for (cty in unique(na.omit(future_landiq$county_safe))) {

    out_path = file.path(scenario_prediction_dir, paste0(cty, "_predicted_2024_", end_year, ".csv"))

    fwrite(future_landiq[county_safe == cty], out_path)}

  scenario_manifest = future_landiq[, .(
    n_rows = .N,
    n_parcels = uniqueN(parcel_id),
    total_acres = sum(ACRES, na.rm = TRUE)
  ), by = .(scenario, county_safe)]

  manifest_path = file.path(scenario_prediction_dir, paste0("prediction_manifest_", scenario_safe, ".csv"))
  fwrite(scenario_manifest, manifest_path)

  message("Finished writing predictions to: ", scenario_prediction_dir)
  return(scenario_manifest[])}

all_prediction_manifests = rbindlist(lapply(run_scenarios, function(scen) {
  tryCatch(
    run_one_prediction_scenario(scen),
    error = function(e) {
      data.table(
        scenario = scen,
        scenario_safe = safe_county_name(scen),
        run_status = "error",
        error_message = conditionMessage(e)
      )
    }
  )
}), fill = TRUE)

all_prediction_manifest_path = file.path(prediction_root_dir, "all_prediction_manifests.csv")
dir.create(prediction_root_dir, recursive = TRUE, showWarnings = FALSE)
fwrite(all_prediction_manifests, all_prediction_manifest_path)

print(all_prediction_manifests)


