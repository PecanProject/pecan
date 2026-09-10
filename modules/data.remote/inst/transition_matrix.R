## Optimize county crop-class transition matrices toward shared 2045 crop acreage targets.
## BAU and NBS use the same crop acreage targets, so crop matrices are optimized once.

pacman::p_load(PEcAn.data.remote, data.table, nloptr, expm, parallel, parallelly)

# ---- setup ----
#REQUIRED: Choose a folder to define work_root, where you want this framework to save intermediate and output files
#Uncomment the line below and replace the example path.
#work_root = "/path/to/your/folder"

##BAU and NBS target scenarios share the same acreage targets - can set crop target variables to just one and optimize matrices once

config = list(crop_target_source = "BAU_Targets", start_year = 2023L, end_year = 2045L, workers = 6L,
  lambda_target = 1e6, maxeval_optimizer = 50000, maxtime_optimizer = 360,
  scale_crop_targets_to_x0 = TRUE, nominal_zero_acres = 0.01,
  run_all_counties = TRUE, counties_manual = character(),
  
  crop_data_path = file.path(work_root, "crop_year_states_cleaned.csv"),
  crop_target_path = file.path(work_root, "MAGiC_scenarios_FINAL", "BAU_Targets.csv"),
  crop_matrix_dir = file.path(work_root, "county_crop_matrices"),
  
  ##output folder the optimized matrices are going to be stored in 
  matrix_out_dir = file.path(work_root, "county_optimized_matrices"))

start_year = config$start_year
end_year = config$end_year
steps = end_year - start_year
dir.create(config$matrix_out_dir, recursive = TRUE, showWarnings = FALSE)

# ---- helpers ----
safe_county_name = function(x) gsub("[^A-Za-z0-9_]+", "_", x)

normalize_crop_key = function(x) {
  x = tolower(trimws(as.character(x)))
  x = gsub("&", "and", x)
  x = gsub("[[:punct:]]+", " ", x)
  trimws(gsub("\\s+", " ", x))
}

check_required_cols = function(dt, required_cols, dt_name) {
  missing_cols = setdiff(required_cols, names(dt))
  if (length(missing_cols)) stop(dt_name, " is missing required columns: ", paste(missing_cols, collapse = ", "))
}

read_tmat = function(path) {
  x = fread(path)
  states = names(x)[-1]
  A = as.matrix(x[, -1, with = FALSE])
  rownames(A) = as.character(x[[1]])
  colnames(A) = states
  storage.mode(A) = "double"
  stopifnot(all(rownames(A) == colnames(A)))
  A
}

repair_transition_matrix = function(A, matrix_name = "matrix") {
  A[is.na(A)] = 0
  A[A < 0] = 0
  A[A > 1] = 1
  rs = rowSums(A)
  zero_rows = names(rs)[is.na(rs) | rs == 0]
  if (length(zero_rows)) {
    warning(matrix_name, " has zero-sum rows; converting to self-loops: ", paste(zero_rows, collapse = ", "))
    for (s in zero_rows) { A[s, ] = 0; A[s, s] = 1 }
  }
  sweep(A, 1, rowSums(A), "/")
}

write_tmat = function(A, path) {
  fwrite(as.data.table(A, keep.rownames = "state"), path)}

build_x0_last_observed = function(crop_data, county_name, start_year, states, state_col = "crop_class") {
  
  dt = copy(crop_data[county_safe == county_name & year <= start_year])
  
  if (!nrow(dt)) stop("No crop data for county up to start year: ", county_name)
  
  dt[, state_value := trimws(as.character(get(state_col)))]
  latest_dt = dt[!is.na(state_value), .SD[which.max(year)], by = parcel_id][state_value %in% states]
  
  x0_dt = latest_dt[, .(acres = sum(ACRES, na.rm = TRUE)), by = state_value]
  X0_vec = setNames(rep(0, length(states)), states)
  
  matched = intersect(x0_dt$state_value, states)
  
  X0_vec[matched] = x0_dt[match(matched, state_value), acres]
  X0 = matrix(X0_vec, nrow = 1)
  colnames(X0) = states
  
  X0
}

scale_target_to_x0_total = function(target_vec, X0) {
  if (sum(target_vec, na.rm = TRUE) <= 0 || sum(X0, na.rm = TRUE) <= 0) return(target_vec)
  target_vec / sum(target_vec, na.rm = TRUE) * sum(X0, na.rm = TRUE)
}

prep_target_for_opt = function(target_vec, X0, scale_to_x0_total = TRUE, nominal_zero_acres = 0.01) {
  out = if (scale_to_x0_total) scale_target_to_x0_total(target_vec, X0) else target_vec
  out[!is.na(out) & out == 0] = nominal_zero_acres
  if (scale_to_x0_total && sum(out, na.rm = TRUE) > 0) out = out / sum(out, na.rm = TRUE) * sum(X0, na.rm = TRUE)
  out
}

make_full_target_vec = function(raw_target_vec, states) {
  out = setNames(rep(0, length(states)), states)
  matched = intersect(names(raw_target_vec), states)
  out[matched] = as.numeric(raw_target_vec[matched])
  out
}

check_matrix = function(A, matrix_name = "matrix") {
  out = data.table(
    matrix_name = matrix_name, min_value = min(A, na.rm = TRUE), max_value = max(A, na.rm = TRUE),
    min_row_sum = min(rowSums(A), na.rm = TRUE), max_row_sum = max(rowSums(A), na.rm = TRUE),
    max_row_sum_error = max(abs(rowSums(A) - 1), na.rm = TRUE)
  )
  print(out)
  if (out$max_row_sum_error > 1e-6) warning(matrix_name, " rows do not sum to 1.")
  out
}

# ---- optimizer function ----
optimize_county_matrix = function(cty, A_orig, X0, target_vec, steps, lambda_target = config$lambda_target,
                                  target_vec_report = NULL, maxeval = config$maxeval_optimizer, maxtime = config$maxtime_optimizer) {
  states = rownames(A_orig)
  n = length(states)
  target_vec = make_full_target_vec(target_vec, states)
  target_vec_report = if (is.null(target_vec_report)) target_vec else make_full_target_vec(target_vec_report, states)
  
  pack_A = function(A) as.vector(t(A[, 1:(n - 1), drop = FALSE]))
  unpack_x = function(x) {
    A_part = matrix(x, nrow = n, ncol = n - 1, byrow = TRUE)
    A_new = cbind(A_part, 1 - rowSums(A_part))
    rownames(A_new) = colnames(A_new) = states
    A_new
  }
  
  obj_fun = function(x) {
    A_new = unpack_x(x)
    if (any(!is.finite(A_new)) || any(A_new < -1e-8) || any(A_new > 1 + 1e-8)) return(1e20)
    
    X_end = X0 %*% (A_new %^% steps)
    matrix_change_penalty = sum((A_new - A_orig)^2)
    X_end_share = as.numeric(X_end[1, states]) / sum(X_end[1, states])
    target_share = as.numeric(target_vec[states]) / sum(target_vec[states])
    target_error_penalty = sum((X_end_share - target_share)^2)
    
    matrix_change_penalty + lambda_target * target_error_penalty
  }
  
  constr_fun = function(x) {
    A_part = matrix(x, nrow = n, ncol = n - 1, byrow = TRUE)
    rowSums(A_part) - 1
  }
  
  res = nloptr(x0 = pack_A(A_orig), eval_f = obj_fun, eval_g_ineq = constr_fun, lb = rep(0, n * (n - 1)), ub = rep(1, n * (n - 1)),
    opts = list(algorithm = "NLOPT_LN_COBYLA", xtol_rel = 1e-5, maxeval = maxeval, maxtime = maxtime, print_level = 0)
  )
  
  A_final = unpack_x(res$solution)
  A_final[is.na(A_final) | !is.finite(A_final)] = 0
  A_final[A_final < 0] = 0
  A_final[A_final > 1] = 1
  
  rs = rowSums(A_final)
  zero_rows = names(rs)[is.na(rs) | rs == 0]
  if (length(zero_rows)) for (s in zero_rows) { A_final[s, ] = 0; A_final[s, s] = 1 }
  
  A_final = sweep(A_final, 1, rowSums(A_final), "/")
  rownames(A_final) = colnames(A_final) = states
  
  X_end_orig = X0 %*% (A_orig %^% steps)
  X_end_final = X0 %*% (A_final %^% steps)
  
  summary = data.table(county_safe = cty, target_state = states, start_acres = as.numeric(X0[1, states]),
    target_acres_raw = as.numeric(target_vec_report[states]), target_acres_used_for_opt = as.numeric(target_vec[states]),
    original_projected_acres = as.numeric(X_end_orig[1, states]), optimized_projected_acres = as.numeric(X_end_final[1, states]),
    raw_difference_after_optimization = as.numeric(X_end_final[1, states]) - as.numeric(target_vec_report[states]),
    opt_difference_after_optimization = as.numeric(X_end_final[1, states]) - as.numeric(target_vec[states]),
    optimizer_status = res$status, optimizer_message = res$message, max_matrix_change = max(abs(A_final - A_orig)), 
    row_sum_error = max(abs(rowSums(A_final) - 1)))
  
  list(A_final = A_final, summary = summary, res = res)
}

# ---- map scenario crop names to LandIQ classes ----
scenario_crop_map_single = data.table(
  Crop = c("All Other Berries", "Strawberries (Fresh Market)", "All Other Fruit Crops",
           "All Other Nut Crops", "Almonds", "Pome Fruit", "Stone Fruit", "Citrus",
           "Grapes Dried, Raisins", "Grapes, Table", "Grapes, Wine", "Fallow"),
  
  crop_state = c("T", "T", "D", "D", "D", "D", "D", "C", "V", "V", "V", "X"))

scenario_crop_map_single[, crop_key := normalize_crop_key(Crop)]

scenario_crop_map_split = data.table(Crop = c("All Other Field Crops (Incl. Pasture /Rangeland)", "Annual Cropland"),
  split_group = c("field_pasture", "annual_cropland"))

scenario_crop_map_split[, crop_key := normalize_crop_key(Crop)]

get_split_states = function(split_group, crop_states) {
  if (split_group == "field_pasture") return(intersect(c("F", "P"), crop_states))
  if (split_group == "annual_cropland") return(intersect(c("F", "G", "T", "R"), crop_states))
  character()
}

get_x0_split_weights = function(crop_data, cty, start_year, split_states) {
  dt = copy(crop_data[county_safe == cty & year <= start_year])
  if (!nrow(dt)) return(data.table(crop_state = split_states, split_weight = rep(1 / length(split_states), length(split_states))))
  
  latest_dt = dt[!is.na(crop_class), .SD[which.max(year)], by = parcel_id][crop_class %in% split_states]
  if (!nrow(latest_dt)) return(data.table(crop_state = split_states, split_weight = rep(1 / length(split_states), length(split_states))))
  
  out = latest_dt[, .(x0_acres = sum(ACRES, na.rm = TRUE)), by = crop_class]
  out = merge(data.table(crop_state = split_states), out, by.x = "crop_state", by.y = "crop_class", all.x = TRUE)
  out[is.na(x0_acres), x0_acres := 0]
  if (sum(out$x0_acres) == 0) out[, split_weight := 1 / .N] else out[, split_weight := x0_acres / sum(x0_acres)]
  out[, .(crop_state, split_weight)]
}

expand_scenario_rows_to_crop_states = function(scenarios, crop_data, cty, end_year, start_year, crop_states) {
  scen_cty = copy(scenarios[county_safe == cty & Year == end_year])
  if (!nrow(scen_cty)) return(list(expanded = data.table(), unmatched = data.table()))
  
  scen_cty[, `:=`(scenario_row_id = .I, crop_key = normalize_crop_key(Crop))]
  
  single = merge(scen_cty, scenario_crop_map_single[, .(crop_key, crop_state)], by = "crop_key", all.x = FALSE)
  if (nrow(single)) single[, `:=`(split_group = "single", split_weight = 1)]
  
  split_rows = merge(scen_cty, scenario_crop_map_split[, .(crop_key, split_group)], by = "crop_key", all.x = FALSE)
  split_list = list()
  
  if (nrow(split_rows)) {
    for (sg in unique(split_rows$split_group)) {
      rows_sg = split_rows[split_group == sg]
      split_states = get_split_states(sg, crop_states)
      if (!length(split_states)) next
      
      weights = get_x0_split_weights(crop_data, cty, start_year, split_states)
      expanded = CJ(scenario_row_id = rows_sg$scenario_row_id, crop_state = weights$crop_state)
      expanded = merge(expanded, rows_sg, by = "scenario_row_id", all.x = TRUE, allow.cartesian = TRUE)
      expanded = merge(expanded, weights, by = "crop_state", all.x = TRUE)
      split_list[[sg]] = expanded
    }
  }
  
  split_expanded = if (length(split_list)) rbindlist(split_list, fill = TRUE) else data.table()
  expanded = rbindlist(list(single, split_expanded), fill = TRUE)
  
  if (nrow(expanded)) {
    expanded = expanded[crop_state %in% crop_states]
    expanded[, Acres_Total_mapped := Acres_Total * split_weight]
  }
  
  unmatched = scen_cty[
    !(crop_key %in% scenario_crop_map_single$crop_key) &
      !(crop_key %in% scenario_crop_map_split$crop_key),
    .(scenario_row_id, Crop, crop_key, Acres_Total)
  ]
  
  list(expanded = expanded, unmatched = unmatched)
}

build_scenario_crop_targets = function(scenarios, crop_data, cty, end_year, start_year, crop_states) {
  info = expand_scenario_rows_to_crop_states(scenarios, crop_data, cty, end_year, start_year, crop_states)
  if (!nrow(info$expanded)) return(NULL)
  
  target_dt = info$expanded[
    , .(target_acres_raw = sum(Acres_Total_mapped, na.rm = TRUE),
        scenario_crops = paste(sort(unique(Crop)), collapse = "; "),
        n_scenario_rows = uniqueN(scenario_row_id)),
    by = crop_state
  ][crop_state %in% crop_states]
  
  if (!nrow(target_dt)) return(NULL)
  list(target_vec = setNames(target_dt$target_acres_raw, target_dt$crop_state),
    target_dt = target_dt, unmatched = info$unmatched)
}

# ---- load crop data ----
if (!file.exists(config$crop_data_path)) stop("Missing crop data: ", config$crop_data_path)
crop_data = fread(config$crop_data_path)
if ("V1" %in% names(crop_data)) crop_data[, V1 := NULL]

check_required_cols(crop_data, c("parcel_id", "year", "county", "state", "ACRES"), "crop_data")
crop_data[, `:=`(
  parcel_id = as.character(parcel_id), year = as.integer(year), county = as.character(county),
  crop_class = trimws(as.character(state)), ACRES = as.numeric(ACRES), county_safe = safe_county_name(county)
)]

# ---- load shared crop targets ----
if (!file.exists(config$crop_target_path)) stop("Missing crop target CSV: ", config$crop_target_path)
matrix_scenarios = fread(config$crop_target_path)
setnames(matrix_scenarios, names(matrix_scenarios), trimws(names(matrix_scenarios)))

check_required_cols(matrix_scenarios, c("Crop", "County", "Year", "Acres_Total"), "crop target CSV")
matrix_scenarios[, `:=`(
  Crop = trimws(as.character(Crop)), County = trimws(as.character(County)), Year = as.integer(Year),
  Acres_Total = as.numeric(Acres_Total),
  county_safe = safe_county_name(County)
)]

message("Using shared crop target source: ", config$crop_target_source)

# ---- optimize one county ----
run_county = function(focus_county) {
  cty = safe_county_name(focus_county)
  message("Running county: ", cty)
  
  if (!(cty %in% matrix_scenarios$county_safe)) stop("County not found in crop targets: ", cty)
  if (!(cty %in% crop_data$county_safe)) stop("County not found in crop data: ", cty)
  
  matrix_file = file.path(config$crop_matrix_dir, paste0(cty, "_crop_matrix.csv"))
  if (!file.exists(matrix_file)) stop("Crop matrix file not found: ", matrix_file)
  
  A_orig = repair_transition_matrix(read_tmat(matrix_file), paste0("crop matrix ", cty))
  crop_states = rownames(A_orig)
  X0 = build_x0_last_observed(crop_data, cty, start_year, crop_states)
  
  if (sum(X0, na.rm = TRUE) <= 0) stop("X0 crop total is zero for county: ", cty)
  
  crop_target = build_scenario_crop_targets(matrix_scenarios, crop_data, cty, end_year, start_year, crop_states)
  if (is.null(crop_target)) stop("No crop target vector could be built for county: ", cty)
  
  target_raw = make_full_target_vec(crop_target$target_vec, crop_states)
  target_opt = prep_target_for_opt(target_raw, X0, scale_to_x0_total = config$scale_crop_targets_to_x0,
    nominal_zero_acres = config$nominal_zero_acres)
  
  check_matrix(A_orig, paste0("original crop matrix ", cty))
  message("Starting optimizer for: ", cty)
  t0 = Sys.time()
  
  opt = optimize_county_matrix(cty = cty, A_orig = A_orig, X0 = X0, target_vec = target_opt, steps = steps,
    lambda_target = config$lambda_target, target_vec_report = target_raw,
    maxeval = config$maxeval_optimizer, maxtime = config$maxtime_optimizer)
  
  message("Finished optimizer for ", cty, " in ",
          round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 2), " minutes")
  
  matrix_out = file.path(config$matrix_out_dir, paste0(cty, "_crop_matrix.csv"))
  write_tmat(opt$A_final, matrix_out)
  
  summary = copy(opt$summary)
  summary[, `:=`(
    crop_target_source = config$crop_target_source, matrix_type = "crop", focus_group = "crop_class", focus_county = focus_county,
    start_year = start_year, end_year = end_year, x0_rule = "latest_observed_crop_state_per_parcel_up_to_start_year"
  )]
  summary[, `:=`(
    abs_error_opt = optimized_projected_acres - target_acres_used_for_opt,
    pct_error_opt = (optimized_projected_acres - target_acres_used_for_opt) / pmax(abs(target_acres_used_for_opt), 1),
    abs_error_raw = optimized_projected_acres - target_acres_raw,
    pct_error_raw = (optimized_projected_acres - target_acres_raw) / pmax(abs(target_acres_raw), 1)
  )]
  
  total_opt_error_share = sum(abs(
    summary$optimized_projected_acres / sum(summary$optimized_projected_acres) -
      summary$target_acres_used_for_opt / sum(summary$target_acres_used_for_opt)
  ), na.rm = TRUE)
  
  run_status = ifelse(opt$res$status < 0, "optimizer_failed",
                      ifelse(total_opt_error_share > 0.05, "poor_fit", "success"))
  
  summary_path = file.path(config$matrix_out_dir, paste0("optimization_summary_", cty, ".csv"))
  fwrite(summary, summary_path)
  
  manifest = data.table(output_type = "optimized_crop_matrix", run_status = run_status,
    error_message = ifelse(run_status == "success", NA_character_, opt$res$message),
    crop_target_source = config$crop_target_source, focus_county = focus_county, focus_county_safe = cty,
    start_year = start_year, end_year = end_year, steps = steps, x0_total_acres = sum(X0),
    scenario_target_acres = matrix_scenarios[county_safe == cty & Year == end_year, sum(Acres_Total, na.rm = TRUE)],
    target_acres_used_for_opt_total = sum(target_opt), optimized_crop_matrix_path = matrix_out, optimization_summary_path = summary_path,
    max_matrix_change = max(abs(opt$A_final - A_orig)), row_sum_error = max(abs(rowSums(opt$A_final) - 1)), total_opt_error_share = total_opt_error_share)
  
  fwrite(manifest, file.path(config$matrix_out_dir, paste0("run_manifest_", cty, ".csv")))
  if (nrow(crop_target$unmatched)) fwrite(crop_target$unmatched,
                                          file.path(config$matrix_out_dir, paste0("unmatched_scenario_crops_", cty, ".csv")))
  
  check_matrix(opt$A_final, paste0("optimized crop matrix ", cty))
  message("Finished county: ", cty)
  manifest
}

# ---- run all counties ----
counties_to_run = if (config$run_all_counties) {
  sort(intersect(
    unique(crop_data$county_safe),
    unique(matrix_scenarios$county_safe)
  ))
} else {
  safe_county_name(config$counties_manual)
}

message("Counties to optimize: ", length(counties_to_run))

#Number of workers cannot exceed available cores or number of counties
n_workers = min(config$workers, as.integer(parallelly::availableCores()), length(counties_to_run))

message("Starting ", n_workers, " workers.")

cl = parallel::makePSOCKcluster(n_workers, outfile = "")

#Load required packages on every worker
parallel::clusterEvalQ(cl, {
  library(data.table)
  library(nloptr)
  library(expm)
  NULL
})

#Send required objects/functions to workers
parallel::clusterExport(cl,
  c("config", "start_year", "end_year", "steps", "crop_data", "matrix_scenarios",
    
    "safe_county_name", "normalize_crop_key", "check_required_cols", "read_tmat",
    "repair_transition_matrix", "write_tmat", "build_x0_last_observed", "scale_target_to_x0_total",
    "prep_target_for_opt", "make_full_target_vec", "check_matrix",
    
    "optimize_county_matrix",
    
    "scenario_crop_map_single", "scenario_crop_map_split", "get_split_states",
    "get_x0_split_weights", "expand_scenario_rows_to_crop_states","build_scenario_crop_targets",
    
    "run_county"),
  envir = .GlobalEnv
)

all_manifests = tryCatch(
  parallel::parLapplyLB(
    cl,
    counties_to_run,
    function(cty) {
      tryCatch(
        run_county(cty),
        error = function(e) {
          data.table(
            output_type = "optimized_crop_matrix",
            run_status = "error",
            error_message = conditionMessage(e),
            focus_county = cty,
            focus_county_safe = safe_county_name(cty),
            start_year = start_year,
            end_year = end_year
          )
        }
      )
    }
  ),
  finally = parallel::stopCluster(cl)
)

all_manifests = rbindlist(all_manifests, fill = TRUE)

fwrite(all_manifests, file.path(config$matrix_out_dir, "all_county_run_manifest.csv"))

print(all_manifests)

message("Crop matrix optimization complete: ", config$matrix_out_dir)