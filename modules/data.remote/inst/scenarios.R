## Optimizes county crop transition matrices toward 2045 scenario acreage
## targets and writes scenario-specific crop/tillage target tables.

pacman::p_load(PEcAn.data.remote, data.table, nloptr, expm)

# ---- set up ----
#Sys.setenv(CCMMF_WORK_ROOT = "/path/to/yourusername")
work_root = Sys.getenv("CCMMF_WORK_ROOT")

if (!nzchar(work_root)) {
  stop("CCMMF_WORK_ROOT is not set. Set it to the workspace containing ", "crop_year_states_cleaned.csv, scenario inputs, and transition-matrix outputs.")
}

config = list(
  scenario_names_for_tillage = c("BAU_Targets", "NBS_Targets"), matrix_target_scenario_name = "BAU_Targets",
  start_year = 2023L, end_year = 2045L, lambda_target = 1e6,
  maxeval_optimizer = 50000, maxtime_optimizer = 300,
  scale_crop_targets_to_x0 = TRUE, nominal_zero_acres = 0.01,
  run_all_counties = TRUE, counties_manual = character(),
  crop_data_path = file.path(work_root, "crop_year_states_cleaned.csv"),  
  scenario_folder = file.path(work_root, "MAGiC_scenarios_FINAL"),
  crop_matrix_dir = file.path(work_root, "county_crop_matrices"),
  matrix_out_dir = file.path(work_root, "county_optimized_matrices"),
  tillage_out_root = file.path(work_root, "county_tillage_targets")
)

scenario_names_for_tillage = config[["scenario_names_for_tillage"]]
matrix_target_scenario_name = config[["matrix_target_scenario_name"]]
start_year = config[["start_year"]]
end_year = config[["end_year"]]
steps = end_year - start_year
lambda_target = config[["lambda_target"]]
maxeval_optimizer = config[["maxeval_optimizer"]]
maxtime_optimizer = config[["maxtime_optimizer"]]
scale_crop_targets_to_x0 = config[["scale_crop_targets_to_x0"]]
nominal_zero_acres = config[["nominal_zero_acres"]]
run_all_counties = config[["run_all_counties"]]
counties_manual = config[["counties_manual"]]
matrix_out_dir = config[["matrix_out_dir"]]
tillage_out_root = config[["tillage_out_root"]]

dir.create(matrix_out_dir, recursive = TRUE, showWarnings = FALSE)

dir.create( tillage_out_root, recursive = TRUE, showWarnings = FALSE)

##-------helper functions-------
safe_county_name = function(x) {gsub("[^A-Za-z0-9_]+", "_", x)}

normalize_crop_key = function(x) {
  x = tolower(trimws(as.character(x)))
  x = gsub("&", "and", x)
  x = gsub("[[:punct:]]+", " ", x)
  x = gsub("\\s+", " ", x)
  trimws(x)}

check_required_cols = function(dt, required_cols, dt_name) {
  missing_cols = setdiff(required_cols, names(dt))
  if (length(missing_cols) > 0) {
    stop(dt_name, " is missing required columns: ", paste(missing_cols, collapse = ", "),
         "\nAvailable columns: ", paste(names(dt), collapse = ", "))}}

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
  
  row_sums = rowSums(A)
  zero_rows = names(row_sums)[is.na(row_sums) | row_sums == 0]
  
  if (length(zero_rows) > 0) {
    warning(matrix_name, " has zero-sum rows. Setting those rows to self-loop: ",
            paste(zero_rows, collapse = ", "))
    
    for (s in zero_rows) {A[s, ] = 0
    A[s, s] = 1}}
  
  row_sums = rowSums(A)
  A = sweep(A, 1, row_sums, "/")
  
  return(A)}

write_tmat = function(A, path) {out = as.data.table(A, keep.rownames = "state")
fwrite(out, path)}

build_x0_last_observed = function(crop_data, county_name, start_year, states, state_col = "crop_class") {
  dt = copy(crop_data[county_safe == county_name & year <= start_year])
  if (nrow(dt) == 0) stop("No data found for county up to start_year: ", county_name, " / ", start_year)
  
  dt[, state_value := trimws(as.character(get(state_col)))]
  latest_dt = dt[!is.na(state_value), .SD[which.max(year)], by = parcel_id]
  latest_dt = latest_dt[state_value %in% states]
  
  x0_dt = latest_dt[, .(acres = sum(ACRES, na.rm = TRUE), n_parcels = uniqueN(parcel_id),
                        min_obs_year = min(year, na.rm = TRUE), max_obs_year = max(year, na.rm = TRUE)),
                    by = state_value]
  
  X0_vec = setNames(rep(0, length(states)), states)
  matched_states = intersect(x0_dt$state_value, states)
  X0_vec[matched_states] = x0_dt[match(matched_states, state_value), acres]
  
  X0_mat = matrix(X0_vec, nrow = 1)
  colnames(X0_mat) = states
  attr(X0_mat, "latest_observed_summary") = x0_dt
  return(X0_mat)}

scale_target_to_x0_total = function(target_vec, X0) {
  target_total = sum(target_vec, na.rm = TRUE)
  x0_total = sum(X0, na.rm = TRUE)
  if (target_total <= 0 || x0_total <= 0) return(target_vec)
  target_vec / target_total * x0_total}

prep_target_for_opt = function(target_vec, X0, scale_to_x0_total = TRUE, nominal_zero_acres = 0.01) {
  target_for_opt = target_vec
  if (scale_to_x0_total) target_for_opt = scale_target_to_x0_total(target_for_opt, X0)
  target_for_opt[!is.na(target_for_opt) & target_for_opt == 0] = nominal_zero_acres
  if (scale_to_x0_total && sum(target_for_opt, na.rm = TRUE) > 0) {
    target_for_opt = target_for_opt / sum(target_for_opt, na.rm = TRUE) * sum(X0, na.rm = TRUE)}
  return(target_for_opt)}

##-----optimizer-----
make_full_target_vec = function(raw_target_vec, states) {
  out = setNames(rep(0, length(states)), states)
  matched = intersect(names(raw_target_vec), states)
  out[matched] = as.numeric(raw_target_vec[matched])
  return(out)}

optimize_county_matrix = function(cty, A_orig, X0, target_vec, steps,
                                  lambda_target = 1e6, target_vec_report = NULL,
                                  maxeval = maxeval_optimizer,
                                  maxtime = maxtime_optimizer) {
  
  states = rownames(A_orig)
  n = length(states)
  
  #make sure target includes every crop state in the matrix
  target_vec = make_full_target_vec(target_vec, states)
  if (is.null(target_vec_report)) {
    target_vec_report = target_vec
  } else {
    target_vec_report = make_full_target_vec(target_vec_report, states)
  }
  
  target_states = states
  
  #pack only first n-1 columns of each row, last column is calculated as 1 - rowSum(first n-1).
  pack_A = function(A) {as.vector(t(A[, 1:(n - 1), drop = FALSE]))}
  
  unpack_x = function(x) {
    A_part = matrix(x, nrow = n, ncol = n - 1, byrow = TRUE)
    A_last = 1 - rowSums(A_part)
    A_new = cbind(A_part, A_last)
    
    rownames(A_new) = states
    colnames(A_new) = states
    
    return(A_new)}
  
  init_x = pack_A(A_orig)
  
  obj_fun = function(x) {
    A_new = unpack_x(x)
    
    if (any(!is.finite(A_new)) || any(A_new < -1e-8) || any(A_new > 1 + 1e-8)) {
      return(1e20)
    }
    
    X_end = X0 %*% (A_new %^% steps)
    colnames(X_end) = states
    
    matrix_change_penalty = sum((A_new - A_orig)^2)
    
    X_end_share = as.numeric(X_end[1, states]) / sum(X_end[1, states])
    target_share = as.numeric(target_vec[states]) / sum(target_vec[states])
    
    target_error_penalty = sum((X_end_share - target_share)^2)
    
    matrix_change_penalty + lambda_target * target_error_penalty
  }  
  #inequality constraint: sum(first n-1 row probs) <= 1, guarantees the last probability is >= 0.
  constr_fun = function(x) {A_part = matrix(x, nrow = n, ncol = n - 1, byrow = TRUE)
  rowSums(A_part) - 1}
  
  res = nloptr(x0 = init_x, eval_f = obj_fun, eval_g_ineq = constr_fun, 
               lb = rep(0, n * (n - 1)), ub = rep(1, n * (n - 1)),
               opts = list(algorithm = "NLOPT_LN_COBYLA", xtol_rel = 1e-5, maxeval = maxeval,
                           maxtime = maxtime, print_level = 0))
  
  A_final = unpack_x(res$solution)
  
  #clean optimizer numerical noise before writing matrix
  A_final[is.na(A_final)] = 0
  A_final[!is.finite(A_final)] = 0
  
  if (any(A_final < 0, na.rm = TRUE)) {
    warning(cty, " optimized matrix had negative values. Minimum = ",
            min(A_final, na.rm = TRUE), ". Clamping negatives to 0.")
    A_final[A_final < 0] = 0}
  
  if (any(A_final > 1, na.rm = TRUE)) {
    warning(cty, " optimized matrix had values > 1. Maximum = ",
            max(A_final, na.rm = TRUE), ". Clamping values above 1 to 1.")
    A_final[A_final > 1] = 1}
  
  row_sums = rowSums(A_final)
  
  zero_rows = names(row_sums)[is.na(row_sums) | row_sums == 0]
  
  if (length(zero_rows) > 0) {
    warning(cty, " optimized matrix had zero rows after cleanup. Setting to self-loop: ",
            paste(zero_rows, collapse = ", "))
    
    for (s in zero_rows) {A_final[s, ] = 0
    A_final[s, s] = 1}}
  
  A_final = sweep(A_final, 1, rowSums(A_final), "/")
  
  rownames(A_final) = states
  colnames(A_final) = states
  
  X_end_orig = X0 %*% (A_orig %^% steps)
  X_end_final = X0 %*% (A_final %^% steps)
  colnames(X_end_orig) = states
  colnames(X_end_final) = states
  
  summary = data.table(county_safe = cty, target_state = target_states,
                       start_acres = as.numeric(X0[1, target_states]),
                       target_acres_raw = as.numeric(target_vec_report[target_states]),
                       target_acres_used_for_opt = as.numeric(target_vec[target_states]),
                       original_projected_acres = as.numeric(X_end_orig[1, target_states]),
                       optimized_projected_acres = as.numeric(X_end_final[1, target_states]),
                       raw_difference_after_optimization = 
                         as.numeric(X_end_final[1, target_states]) - as.numeric(target_vec_report[target_states]),
                       opt_difference_after_optimization =
                         as.numeric(X_end_final[1, target_states]) - as.numeric(target_vec[target_states]),
                       optimizer_status = res$status, optimizer_message = res$message,
                       max_matrix_change = max(abs(A_final - A_orig)),
                       row_sum_error = max(abs(rowSums(A_final) - 1)))
  
  return(list(A_final = A_final, summary = summary, res = res))}
##-------scenario crop to transition-state mapping-------
scenario_crop_map_single = data.table(
  Crop = c("All Other Berries", "Strawberries (Fresh Market)", "All Other Fruit Crops",
           "All Other Nut Crops", "Almonds", "Pome Fruit", "Stone Fruit", "Citrus",
           "Grapes Dried, Raisins", "Grapes, Table", "Grapes, Wine", "Fallow"),
  crop_state = c("T", "T", "D", "D", "D", "D", "D", "C", "V", "V", "V", "X"))

scenario_crop_map_single[, crop_key := normalize_crop_key(Crop)]

scenario_crop_map_split = data.table(
  Crop = c("All Other Field Crops (Incl. Pasture /Rangeland)", "Annual Cropland"),
  split_group = c("field_pasture", "annual_cropland"))

scenario_crop_map_split[, crop_key := normalize_crop_key(Crop)]

get_split_states = function(split_group, crop_states) {
  if (split_group == "field_pasture") return(intersect(c("F", "P"), crop_states))
  if (split_group == "annual_cropland") return(intersect(c("F", "G", "T", "R"), crop_states))
  return(character(0))}

get_x0_split_weights = function(crop_data, cty, start_year, split_states) {
  dt = copy(crop_data[county_safe == cty & year <= start_year])
  if (nrow(dt) == 0) {
    weight = rep(1 / length(split_states), length(split_states))
    return(data.table(crop_state = split_states, split_weight = weight))}
  
  latest_dt = dt[!is.na(crop_class), .SD[which.max(year)], by = parcel_id]
  latest_dt = latest_dt[crop_class %in% split_states]
  
  if (nrow(latest_dt) == 0) {
    weight = rep(1 / length(split_states), length(split_states))
    return(data.table(crop_state = split_states, split_weight = weight))}
  
  out = latest_dt[, .(x0_acres = sum(ACRES, na.rm = TRUE)), by = crop_class]
  out = merge(data.table(crop_state = split_states), out, by.x = "crop_state", by.y = "crop_class", all.x = TRUE)
  out[is.na(x0_acres), x0_acres := 0]
  
  if (sum(out$x0_acres) == 0) out[, split_weight := 1 / .N] else out[, split_weight := x0_acres / sum(x0_acres)]
  return(out[, .(crop_state, split_weight)])}

expand_scenario_rows_to_crop_states = function(scenarios, crop_data, cty, end_year, start_year, crop_states) {
  scen_cty = copy(scenarios[county_safe == cty & Year == end_year])
  if (nrow(scen_cty) == 0) return(list(expanded = data.table(), unmatched = data.table()))
  
  scen_cty[, scenario_row_id := .I]
  scen_cty[, crop_key := normalize_crop_key(Crop)]
  
  single = merge(scen_cty, scenario_crop_map_single[, .(crop_key, crop_state)], by = "crop_key", all.x = FALSE)
  if (nrow(single) > 0) {
    single[, split_group := "single"]
    single[, split_weight := 1]}
  
  split_rows = merge(scen_cty, scenario_crop_map_split[, .(crop_key, split_group)], by = "crop_key", all.x = FALSE)
  split_expanded_list = list()
  
  if (nrow(split_rows) > 0) {
    for (sg in unique(split_rows$split_group)) {
      rows_sg = split_rows[split_group == sg]
      split_states = get_split_states(sg, crop_states)
      if (length(split_states) == 0) {
        warning("No valid split states found for split group: ", sg)
        next
      }
      
      weights = get_x0_split_weights(crop_data = crop_data, cty = cty, start_year = start_year, split_states = split_states)
      expanded = CJ(scenario_row_id = rows_sg$scenario_row_id, crop_state = weights$crop_state)
      expanded = merge(expanded, rows_sg, by = "scenario_row_id", all.x = TRUE, allow.cartesian = TRUE)
      expanded = merge(expanded, weights, by = "crop_state", all.x = TRUE)
      split_expanded_list[[sg]] = expanded}}
  
  split_expanded = if (length(split_expanded_list) > 0) rbindlist(split_expanded_list, fill = TRUE) else data.table()
  expanded = rbindlist(list(single, split_expanded), fill = TRUE)
  
  if (nrow(expanded) > 0) {
    expanded = expanded[crop_state %in% crop_states]
    expanded[, `:=`(
      Acres_Total_mapped = Acres_Total * split_weight,
      Tilled_acres_mapped = `Tilled acres` * split_weight,
      Reduced_till_acres_mapped = `Reduced till acres (CPS 345)` * split_weight,
      No_till_acres_mapped = `No till acres (CPS 329)` * split_weight)]}
  
  unmatched = scen_cty[
    !(crop_key %in% scenario_crop_map_single$crop_key) &
      !(crop_key %in% scenario_crop_map_split$crop_key),
    .(scenario_row_id, Crop, crop_key, Acres_Total)]
  
  if (nrow(unmatched) > 0) {
    warning("Some scenario crops are not mapped: ", paste(unique(unmatched$Crop), collapse = ", "))}
  
  return(list(expanded = expanded, unmatched = unmatched))}

build_scenario_crop_targets = function(scenarios, crop_data, cty, end_year, start_year, crop_states) {
  expanded_info = expand_scenario_rows_to_crop_states(scenarios = scenarios, crop_data = crop_data, cty = cty,
                                                      end_year = end_year, start_year = start_year,
                                                      crop_states = crop_states)
  expanded = expanded_info$expanded
  if (nrow(expanded) == 0) return(NULL)
  
  target_dt = expanded[, .(target_acres_raw = sum(Acres_Total_mapped, na.rm = TRUE),
                           scenario_crops = paste(sort(unique(Crop)), collapse = "; "),
                           n_scenario_rows = uniqueN(scenario_row_id)), by = crop_state]
  target_dt = target_dt[crop_state %in% crop_states]
  if (nrow(target_dt) == 0) return(NULL)
  
  target_vec = setNames(target_dt$target_acres_raw, target_dt$crop_state)
  return(list(target_vec = target_vec, target_dt = target_dt,
              unmatched = expanded_info$unmatched, expanded_rows = expanded))}

make_crop_by_till_targets = function(expanded_rows) {
  no_till_targets = expanded_rows[, .(target_acres_raw = sum(No_till_acres_mapped, na.rm = TRUE),
                                      scenario_crops = paste(sort(unique(Crop)), collapse = "; ")),
                                  by = .(county_safe, Year, crop_state)]
  no_till_targets[, till_state := "no_till"]
  
  low_till_targets = expanded_rows[, .(target_acres_raw = sum(Reduced_till_acres_mapped, na.rm = TRUE),
                                       scenario_crops = paste(sort(unique(Crop)), collapse = "; ")),
                                   by = .(county_safe, Year, crop_state)]
  low_till_targets[, till_state := "low_till"]
  
  high_till_targets = expanded_rows[, .(target_acres_raw = sum(Tilled_acres_mapped, na.rm = TRUE),
                                        scenario_crops = paste(sort(unique(Crop)), collapse = "; ")),
                                    by = .(county_safe, Year, crop_state)]
  high_till_targets[, till_state := "high_till"]
  
  out = rbindlist(list(no_till_targets, low_till_targets, high_till_targets), fill = TRUE)
  setcolorder(out, c("county_safe", "Year", "crop_state", "till_state", "target_acres_raw", "scenario_crops"))
  return(out)}

##-------validation helpers-------
check_matrix = function(A, matrix_name = "matrix") {
  out = data.table(matrix_name = matrix_name, min_value = min(A, na.rm = TRUE),
                   max_value = max(A, na.rm = TRUE),
                   min_row_sum = min(rowSums(A), na.rm = TRUE),
                   max_row_sum = max(rowSums(A), na.rm = TRUE),
                   max_row_sum_error = max(abs(rowSums(A) - 1), na.rm = TRUE),
                   any_negative = any(A < -1e-10, na.rm = TRUE),
                   any_over_one = any(A > 1 + 1e-10, na.rm = TRUE))
  print(out)
  if (out$any_negative) warning(matrix_name, " has negative probabilities.")
  if (out$any_over_one) warning(matrix_name, " has probabilities > 1.")
  if (out$max_row_sum_error > 1e-6) warning(matrix_name, " rows do not sum to 1.")
  return(out)}

check_mapping_totals = function(raw_rows, expanded_rows) {
  raw_crop_total = sum(raw_rows$Acres_Total, na.rm = TRUE)
  mapped_crop_total = sum(expanded_rows$Acres_Total_mapped, na.rm = TRUE)
  out = data.table(raw_crop_total = raw_crop_total, mapped_crop_total = mapped_crop_total,
                   crop_total_diff = mapped_crop_total - raw_crop_total)
  print(out)
  if (abs(out$crop_total_diff) > 1e-4) warning("Mapped crop acres do not equal raw scenario crop acres.")
  return(out)}

# ---- load & clean crop data ----

if (!file.exists(config[["crop_data_path"]])) {
  stop("crop_year_states_cleaned.csv not found: ", config[["crop_data_path"]])
}

crop_data = fread(config[["crop_data_path"]])

if ("V1" %in% names(crop_data)) {
  crop_data[, V1 := NULL]
}

setDT(crop_data)

required_crop_data_cols = c("parcel_id", "year", "county", "state", "ACRES")

check_required_cols(crop_data, required_crop_data_cols, "crop_data")

crop_data[, `:=`(
  parcel_id = as.character(parcel_id), year = as.integer(year),
  county = as.character(county), crop_class = trimws(as.character(state)), ACRES = as.numeric(ACRES)
)]

crop_data[
  ,
  county_safe := safe_county_name(county)
]

##-------load scenario CSV files-------
## Scenario inputs are separate CSV files under config[["scenario_folder"]].
scenario_folder = config[["scenario_folder"]]

scenario_csv_files = c("BAU_Targets" = file.path(scenario_folder, "BAU_Targets.csv"),
  "NBS_Targets" = file.path(scenario_folder, "NBS_Targets.csv"))

load_scenario_sheet = function(scenario_name) {
  if (!(scenario_name %in% names(scenario_csv_files))) {
    stop("No CSV file defined for scenario_name: ", scenario_name,
      "\nAvailable scenario names: ", paste(names(scenario_csv_files), collapse = ", "))
  }
  
  scenario_path = scenario_csv_files[[scenario_name]]
  
  if (!file.exists(scenario_path)) {
    stop("Scenario CSV file not found: ", scenario_path)
  }
  
  scen = fread(scenario_path)
  setDT(scen)
  setnames(scen, names(scen), trimws(names(scen)))
  
  required_scenario_cols = c("Crop", "County", "Year", "Acres_Total", "Tilled acres", 
                             "Reduced till acres (CPS 345)", "No till acres (CPS 329)")
  check_required_cols(scen, required_scenario_cols, paste0("scenario CSV: ", scenario_path))
  
  scen[, `:=`(
    Crop = trimws(as.character(Crop)), County = trimws(as.character(County)),
    Year = as.integer(Year), Acres_Total = as.numeric(Acres_Total),
    `Tilled acres` = as.numeric(`Tilled acres`),
    `Reduced till acres (CPS 345)` = as.numeric(`Reduced till acres (CPS 345)`),
    `No till acres (CPS 329)` = as.numeric(`No till acres (CPS 329)`))]
  
  scen[, county_safe := safe_county_name(County)]
  return(scen)}

all_scenario_names = unique(c(matrix_target_scenario_name, scenario_names_for_tillage))
scenarios_by_name = setNames(lapply(all_scenario_names, load_scenario_sheet), all_scenario_names)

matrix_scenarios = scenarios_by_name[[matrix_target_scenario_name]]

##-------write tillage targets for one county/scenario-------
write_county_tillage_targets = function(focus_county_safe, scenario_name, scenarios_dt, crop_states, X0_crop) {
  scenario_safe = safe_county_name(scenario_name)
  tillage_out_dir = file.path(tillage_out_root, scenario_safe)
  dir.create(tillage_out_dir, recursive = TRUE, showWarnings = FALSE)
  
  county_scenario_rows = scenarios_dt[county_safe == focus_county_safe & Year == end_year]
  if (nrow(county_scenario_rows) == 0) {
    stop("No scenario rows found for county/year: ", focus_county_safe, " / ", scenario_name, " / ", end_year)
  }
  
  raw_target_path = file.path(tillage_out_dir,
    paste0("raw_scenario_rows_", focus_county_safe, "_", scenario_safe, "_", end_year, ".csv"))
  
  fwrite(county_scenario_rows, raw_target_path)
  
  crop_target = build_scenario_crop_targets(
    scenarios = scenarios_dt, crop_data = crop_data, cty = focus_county_safe,
    end_year = end_year, start_year = start_year, crop_states = crop_states)
  
  if (is.null(crop_target)) {
    stop("No crop/tillage target vector could be built for county: ", focus_county_safe, " / ", scenario_name)
  }
  
  crop_target_vec_raw = make_full_target_vec(crop_target$target_vec, crop_states)
  crop_target_vec_opt = prep_target_for_opt(
    target_vec = crop_target_vec_raw, X0 = X0_crop,
    scale_to_x0_total = scale_crop_targets_to_x0, nominal_zero_acres = nominal_zero_acres)
  
  crop_targets_out = copy(crop_target$target_dt)
  crop_targets_out[, `:=`(
    scenario_name = scenario_name, scenario_safe = scenario_safe, county_safe = focus_county_safe,
    start_year = start_year, end_year = end_year,
    target_state_col = "manual_scenario_crop_to_landiq_class_map",
    target_acres_used_for_opt = as.numeric(crop_target_vec_opt[crop_state])
  )]
  
  crop_targets_path = file.path(tillage_out_dir,
    paste0("crop_targets_", focus_county_safe, "_", scenario_safe, "_", end_year, ".csv"))
  
  expanded_rows_path = file.path(tillage_out_dir,
    paste0("expanded_scenario_rows_", focus_county_safe, "_", scenario_safe, "_", end_year, ".csv"))
  
  fwrite(crop_targets_out, crop_targets_path)
  fwrite(crop_target$expanded_rows, expanded_rows_path)
  
  crop_by_till_targets = make_crop_by_till_targets(crop_target$expanded_rows)
  
  crop_by_till_targets_path = file.path(tillage_out_dir,
    paste0("crop_by_till_targets_", focus_county_safe, "_", scenario_safe, "_", end_year, ".csv"))
  
  fwrite(crop_by_till_targets, crop_by_till_targets_path)
  
  crop_by_till_check = crop_by_till_targets[, .(
    till_target_total = sum(target_acres_raw, na.rm = TRUE)
  ), by = crop_state]
  
  crop_by_till_check = merge(crop_by_till_check,
    crop_targets_out[, .(crop_state, crop_target_total = target_acres_raw)],
    by = "crop_state", all = TRUE)
  
  crop_by_till_check[, diff := till_target_total - crop_target_total]
  
  crop_by_till_check_path = file.path(tillage_out_dir,
    paste0("crop_by_till_check_", focus_county_safe, "_", scenario_safe, "_", end_year, ".csv"))
  
  fwrite(crop_by_till_check, crop_by_till_check_path)
  
  unmatched_path = NA_character_
  if (nrow(crop_target$unmatched) > 0) {
    unmatched_path = file.path(tillage_out_dir,
      paste0("unmatched_scenario_crops_", focus_county_safe, "_", scenario_safe, "_", end_year, ".csv")
    )
    
    fwrite(crop_target$unmatched, unmatched_path)
    warning("Wrote unmatched scenario crops to: ", unmatched_path)
  }
  
  scenario_total = county_scenario_rows[, sum(Acres_Total, na.rm = TRUE)]
  mapping_total_check = check_mapping_totals(raw_rows = county_scenario_rows, expanded_rows = crop_target$expanded_rows)
  
  split_weight_check = crop_target$expanded_rows[, .(
    weight_sum = sum(split_weight, na.rm = TRUE)
  ), by = scenario_row_id]
  
  bad_weights = split_weight_check[abs(weight_sum - 1) > 1e-6]
  if (nrow(bad_weights) > 0) {
    warning("Some scenario split weights do not sum to 1 for county: ", focus_county_safe, " / ", scenario_name)
  }
  
  manifest = data.table(output_type = "tillage_targets", run_status = "success",
    error_message = NA_character_, scenario_name = scenario_name, scenario_safe = scenario_safe,
    focus_county_safe = focus_county_safe, start_year = start_year, end_year = end_year,
    x0_total_acres = sum(X0_crop), scenario_target_acres = scenario_total, raw_scenario_rows_path = raw_target_path,
    expanded_scenario_rows_path = expanded_rows_path, crop_targets_path = crop_targets_path,
    crop_by_till_targets_path = crop_by_till_targets_path, crop_by_till_check_path = crop_by_till_check_path,
    unmatched_scenario_crops_path = unmatched_path, crop_total_diff_after_mapping = mapping_total_check$crop_total_diff)
  
  manifest_path = file.path(tillage_out_dir,
    paste0("tillage_manifest_", focus_county_safe, "_", scenario_safe, ".csv"))
  
  fwrite(manifest, manifest_path)
  
  return(manifest)
}

##-------run one county-------
run_county = function(focus_county) {
  focus_county_safe = safe_county_name(focus_county)
  
  message("Running county: ", focus_county_safe)
  
  if (!(focus_county_safe %in% matrix_scenarios$county_safe)) {
    stop("County not found in matrix target scenario sheet: ", focus_county_safe)
  }
  
  if (!(focus_county_safe %in% crop_data$county_safe)) {
    stop("County not found in crop_data: ", focus_county_safe)
  }
  
  county_year_acres = crop_data[county_safe == focus_county_safe, .(
    n_rows = .N, n_parcels = uniqueN(parcel_id), total_acres = sum(ACRES, na.rm = TRUE)
  ), by = year][order(year)]
  
  print(county_year_acres)
  
  crop_matrix_file = file.path(config[["crop_matrix_dir"]], paste0(focus_county_safe, "_crop_matrix.csv"))
  if (!file.exists(crop_matrix_file)) stop("Crop matrix file not found: ", crop_matrix_file)
  
  A_crop_orig = repair_transition_matrix(read_tmat(crop_matrix_file), 
                                         paste0("crop matrix ", focus_county_safe))
  
  crop_states = rownames(A_crop_orig)
  
  X0_crop = build_x0_last_observed(crop_data = crop_data, county_name = focus_county_safe,
                                   start_year = start_year, states = crop_states, 
                                   state_col = "crop_class")
  
  if (sum(X0_crop, na.rm = TRUE) <= 0) {stop("X0 crop total is zero for county: ", focus_county_safe)}
  
  #Build the crop-acre target once, using matrix_target_scenario_name.
  crop_target = build_scenario_crop_targets(
    scenarios = matrix_scenarios, crop_data = crop_data, cty = focus_county_safe,
    end_year = end_year, start_year = start_year, crop_states = crop_states)
  
  if (is.null(crop_target)) {stop("No crop target vector could be built for county: ", focus_county_safe)}
  
  crop_target_vec_raw = make_full_target_vec(crop_target$target_vec, crop_states)
  
  crop_target_vec_opt = prep_target_for_opt(
    target_vec = crop_target_vec_raw, X0 = X0_crop, 
    scale_to_x0_total = scale_crop_targets_to_x0, nominal_zero_acres = nominal_zero_acres)
  
  scenario_total = matrix_scenarios[
    county_safe == focus_county_safe & Year == end_year,
    sum(Acres_Total, na.rm = TRUE)]
  
  acreage_basis_check = data.table(county_safe = focus_county_safe,
                                   matrix_target_scenario_name = matrix_target_scenario_name,
                                   start_year = start_year,end_year = end_year,
                                   x0_latest_observed_acres = sum(X0_crop),
                                   scenario_target_acres = scenario_total,
                                   scenario_minus_x0 = scenario_total - sum(X0_crop),
                                   scenario_divided_by_x0 = scenario_total / sum(X0_crop),
                                   target_acres_used_for_opt_total = sum(crop_target_vec_opt))
  
  print(acreage_basis_check)
  
  orig_matrix_check = check_matrix(A_crop_orig, paste0("original crop matrix ", focus_county_safe))
  
  message("Starting optimizer for: ", focus_county_safe)
  opt_start_time = Sys.time()
  
  opt_crop = optimize_county_matrix(cty = focus_county_safe, A_orig = A_crop_orig,
                                    X0 = X0_crop, target_vec = crop_target_vec_opt, steps = steps, 
                                    lambda_target = lambda_target, target_vec_report = crop_target_vec_raw, 
                                    maxeval = maxeval_optimizer, maxtime = maxtime_optimizer)
  
  message("Finished optimizer for: ", focus_county_safe,
          " in ", round(as.numeric(difftime(Sys.time(), opt_start_time, units = "mins")), 2),
          " minutes")
  
  crop_out_path = file.path(matrix_out_dir, paste0(focus_county_safe, "_crop_matrix.csv"))
  write_tmat(opt_crop$A_final, crop_out_path)
  
  optimization_summary = opt_crop$summary
  
  optimization_summary[, `:=`(
    matrix_target_scenario_name = matrix_target_scenario_name,
    matrix_type = "crop", focus_group = "crop_class",
    focus_county = focus_county, start_year = start_year,
    end_year = end_year, x0_rule = "latest_observed_crop_state_per_parcel_up_to_start_year",
    target_note = ifelse(scale_crop_targets_to_x0,
      "Crop targets scaled to X0 total for feasible row-stochastic optimization",
      "Raw crop targets used directly"
    )
  )]
  
  optimization_summary[, abs_error_opt := optimized_projected_acres - target_acres_used_for_opt]
  optimization_summary[, pct_error_opt := abs_error_opt / pmax(abs(target_acres_used_for_opt), 1)]
  optimization_summary[, abs_error_raw := optimized_projected_acres - target_acres_raw]
  optimization_summary[, pct_error_raw := abs_error_raw / pmax(abs(target_acres_raw), 1)]
  
  total_opt_error_share = sum(abs(
    optimization_summary$optimized_projected_acres / sum(optimization_summary$optimized_projected_acres) -
      optimization_summary$target_acres_used_for_opt / sum(optimization_summary$target_acres_used_for_opt)
  ), na.rm = TRUE)
  
  true_run_status = ifelse(opt_crop$res$status < 0,
    "optimizer_failed", ifelse(total_opt_error_share > 0.05, "poor_fit", "success"))
  
  summary_path = file.path(matrix_out_dir, paste0("optimization_summary_", focus_county_safe, ".csv"))
  fit_check_path = file.path(matrix_out_dir, paste0("optimization_fit_check_", focus_county_safe, ".csv"))
  
  fwrite(optimization_summary, summary_path)
  fwrite(optimization_summary, fit_check_path)
  
  opt_matrix_check = check_matrix(opt_crop$A_final, paste0("optimized crop matrix ", focus_county_safe))
  
  matrix_manifest = data.table(
    output_type = "optimized_crop_matrix",
    run_status = true_run_status,
    error_message = ifelse(true_run_status == "success", NA_character_, opt_crop$res$message),
    matrix_target_scenario_name = matrix_target_scenario_name,
    focus_county = focus_county,
    focus_county_safe = focus_county_safe,
    start_year = start_year, end_year = end_year, steps = steps,
    x0_total_acres = sum(X0_crop),
    scenario_target_acres = scenario_total,
    target_acres_used_for_opt_total = sum(crop_target_vec_opt),
    optimized_crop_matrix_path = crop_out_path,
    optimization_summary_path = summary_path,
    optimization_fit_check_path = fit_check_path,
    x0_rule = "latest_observed_crop_state_per_parcel_up_to_start_year",
    max_matrix_change = max(abs(opt_crop$A_final - A_crop_orig)),
    row_sum_error = max(abs(rowSums(opt_crop$A_final) - 1)),
    total_opt_error_share = total_opt_error_share
  )
  
  matrix_manifest_path = file.path(matrix_out_dir, paste0("run_manifest_", focus_county_safe, ".csv"))
  fwrite(matrix_manifest, matrix_manifest_path)
  
  ## Now build BAU/NBS tillage targets, without re-optimizing the crop matrix.
  tillage_manifests = rbindlist(lapply(scenario_names_for_tillage, function(scen_name) {
    tryCatch(write_county_tillage_targets(focus_county_safe = focus_county_safe, scenario_name = scen_name,
        scenarios_dt = scenarios_by_name[[scen_name]], crop_states = crop_states, X0_crop = X0_crop
      ),
      error = function(e) {
        data.table(output_type = "tillage_targets",
          run_status = "error", error_message = conditionMessage(e), scenario_name = scen_name,
          scenario_safe = safe_county_name(scen_name), focus_county_safe = focus_county_safe,
          start_year = start_year, end_year = end_year)
      }
    )
  }), fill = TRUE)
  
  county_manifest = rbindlist(list(matrix_manifest, tillage_manifests), fill = TRUE)
  
  county_manifest_path = file.path(matrix_out_dir, paste0("combined_manifest_", focus_county_safe, ".csv"))
  fwrite(county_manifest, county_manifest_path)
  
  message("Finished county: ", focus_county_safe)
  return(county_manifest)
}

##-----all county loop-----
if (run_all_counties) {
  counties_in_all_tillage_sheets = Reduce(intersect,
    lapply(scenarios_by_name[scenario_names_for_tillage], function(x) unique(x$county_safe)
           ))
  
  counties_to_run = sort(intersect(unique(crop_data$county_safe),
    intersect(unique(matrix_scenarios$county_safe), counties_in_all_tillage_sheets)))
  
} else {
  counties_to_run = safe_county_name(counties_manual)
}

all_manifests = rbindlist(lapply(counties_to_run, function(cty) {
  tryCatch(run_county(cty),
    error = function(e) {
      data.table(
        output_type = "county_run", run_status = "error", error_message = conditionMessage(e),
        focus_county = cty, focus_county_safe = safe_county_name(cty), start_year = start_year, end_year = end_year)
    }
  )
}), fill = TRUE)

all_manifest_path = file.path(matrix_out_dir, "all_county_run_manifest.csv")
fwrite(all_manifests, all_manifest_path)

print(all_manifests)