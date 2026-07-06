#goes through all counties and predicts up until end_year using the optimized matrices & stores county-separated 
#LandIQ-style datasets inside bau and nbs target subfolders 

setwd("/projectnb/dietzelab/ananyak")

library(data.table)

##-------scenario setup-------
run_scenario = "BAU"  #"BAU" or "NBS_Targets"

prediction_root_dir = "county_landiq_predictions"
scenario_prediction_dir = file.path(prediction_root_dir, run_scenario)

dir.create(scenario_prediction_dir, recursive = TRUE, showWarnings = FALSE)

start_year = 2023L
end_year = 2045L

crop_matrix_dir = file.path("county_optimized_matrices", run_scenario)
crop_matrix_pattern = paste0("_crop_matrix_", run_scenario, "\\.csv$")
target_dir = file.path("county_optimized_targets", run_scenario)

use_till_targets = dir.exists(target_dir) &&
  length(list.files(target_dir, pattern = paste0("crop_by_till_targets_.*_", run_scenario, "_", end_year, "\\.csv$")
  )) > 0

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
  
  #clamp tiny numerical optimizer noise
  if (any(A < 0, na.rm = TRUE)) {
    warning(
      matrix_name,
      " has negative probabilities. Minimum = ",
      min(A, na.rm = TRUE),
      ". Clamping negatives to 0.")
    A[A < 0] = 0}
  
  if (any(A > 1, na.rm = TRUE)) {
    warning(
      matrix_name,
      " has probabilities > 1. Maximum = ",
      max(A, na.rm = TRUE),
      ". Clamping values above 1 to 1.")
    A[A > 1] = 1}
  
  row_sums = rowSums(A)
  zero_rows = names(row_sums)[is.na(row_sums) | row_sums == 0]
  
  if (length(zero_rows) > 0) {
    warning(
      matrix_name, " has zero-sum rows. Setting those rows to self-loop: ",
      paste(zero_rows, collapse = ", "))
    
    for (s in zero_rows) {
      A[s, ] = 0
      A[s, s] = 1}}
  
  row_sums = rowSums(A)
  A = sweep(A, 1, row_sums, "/")
  
  return(A)}

load_crop_matrices = function(crop_matrix_dir, crop_matrix_pattern, run_scenario) {
  
  matrix_files = list.files(crop_matrix_dir, pattern = crop_matrix_pattern, full.names = TRUE)
  
  if (length(matrix_files) == 0) {
    stop("No optimized crop matrix files found in: ", crop_matrix_dir)}
  
  transition_mats = list()
  
  for (f in matrix_files) {
    
    matrix_name = sub(paste0("_crop_matrix_", run_scenario, "\\.csv$"), "",  basename(f))
    
    A = read_tmat(f)
    A = repair_transition_matrix(A, paste0("optimized crop matrix ", matrix_name))
    
    transition_mats[[matrix_name]] = A}
  
  return(transition_mats)}

load_crop_targets = function(target_dir, run_scenario, end_year) {
  
  pattern = paste0("crop_targets_.*_", run_scenario, "_", end_year, "\\.csv$")
  files = list.files(target_dir, pattern = pattern, full.names = TRUE)
  
  if (length(files) == 0) {
    stop("No crop target files found in: ", target_dir)
  }
  
  out = rbindlist(lapply(files, fread), fill = TRUE)
  
  out[, county_safe := as.character(county_safe)]
  out[, crop_state := as.character(crop_state)]
  
  if ("target_acres_used_for_opt" %in% names(out)) {
    out[, target_acres := as.numeric(target_acres_used_for_opt)]
  } else {
    out[, target_acres := as.numeric(target_acres_raw)]
  }
  
  out = out[
    !is.na(county_safe) &
      !is.na(crop_state),
    .(target_acres = sum(target_acres, na.rm = TRUE)),
    by = .(county_safe, crop_state)
  ]
  
  return(out[])
}

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
    powers[[i]] = A_power
  }
  
  return(powers)
}

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
  
  if (nrow(dt) == 0) return(data.table())
  
  powers = make_matrix_powers(tmat, n_years)
  A_final = powers[[n_years]]
  
  # full target vector; states not in scenario target file become zero
  target_vec = setNames(rep(0, length(states)), states)
  if (nrow(targets_cty) > 0) {
    matched = intersect(targets_cty$crop_state, states)
    target_vec[matched] = targets_cty[match(matched, crop_state), target_acres]
  }
  
  # force target total onto this county's modeled parcel-acre total
  county_total = sum(dt$ACRES, na.rm = TRUE)
  if (sum(target_vec, na.rm = TRUE) > 0) {
    target_vec = target_vec / sum(target_vec, na.rm = TRUE) * county_total
  } else {
    # fallback: if no target file exists, preserve current acreage
    cur = dt[, .(acres = sum(ACRES, na.rm = TRUE)), by = start_CLASS]
    target_vec[cur$start_CLASS] = cur$acres
  }
  
  # start by keeping all parcels in their observed class
  dt[, final_CLASS := start_CLASS]
  dt[, locked := FALSE]
  
  get_current = function(x) {
    cur = x[, .(acres = sum(ACRES, na.rm = TRUE)), by = final_CLASS]
    out = setNames(rep(0, length(states)), states)
    out[cur$final_CLASS] = cur$acres
    return(out)
  }
  
  # greedily move parcels from surplus states into deficit states
  for (to_state in states[order(-target_vec)]) {
    
    current_vec = get_current(dt)
    need = target_vec[to_state] - current_vec[to_state]
    
    if (!is.finite(need) || need <= 0) next
    
    surplus_states = names(current_vec)[current_vec > target_vec + 1e-6]
    surplus_states = setdiff(surplus_states, to_state)
    
    candidates = dt[
      !locked &
        final_CLASS %in% surplus_states
    ]
    
    if (nrow(candidates) == 0) next
    
    candidates[, prob_to := A_final[start_CLASS, to_state]]
    candidates[, prob_current := A_final[start_CLASS, final_CLASS]]
    candidates[, score := prob_to - prob_current]
    
    setorder(candidates, -score, -prob_to)
    
    candidates[, cum_acres := cumsum(ACRES)]
    take_ids = candidates[cum_acres <= need | shift(cum_acres, fill = 0) < need, parcel_id]
    
    if (length(take_ids) > 0) {
      dt[parcel_id %in% take_ids, `:=`(
        final_CLASS = to_state,
        locked = TRUE
      )]
    }
  }
  
  # build annual time series: keep start class until conversion year
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
    }
    
    pred_list[[i]] = data.table(
      parcel_id = parcel,
      year = years,
      CLASS = pred_state,
      prob_crop_class = as.numeric(pred_prob)
    )
  }
  
  out = rbindlist(pred_list, fill = TRUE)
  return(out[])
}

predict_grouped_markov_to_targets = function(year_states, transition_mats,
                                             crop_targets_2045,
                                             group_col,
                                             start_year, end_year,
                                             state_col = "crop_class") {
  
  dt = copy(year_states)
  all_preds = list()
  
  groups = intersect(unique(na.omit(dt[[group_col]])), names(transition_mats))
  
  for (g in groups) {
    
    message("Predicting crop class with target allocation for county: ", g)
    
    dt_g = dt[get(group_col) == g]
    tmat_g = transition_mats[[g]]
    
    start_info = dt_g[
      year <= start_year,
      .SD[which.max(year)],
      by = parcel_id
    ]
    
    targets_g = crop_targets_2045[county_safe == g]
    
    preds_g = predict_county_to_targets(
      start_info = start_info,
      tmat = tmat_g,
      targets_cty = targets_g,
      start_year = start_year,
      end_year = end_year,
      state_col = state_col
    )
    
    if (nrow(preds_g) == 0) next
    
    preds_g[, (group_col) := g]
    all_preds[[g]] = preds_g
  }
  
  return(rbindlist(all_preds, fill = TRUE))
}



##--------load & clean all_data---------
all_data = fread("all_data.csv")

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

if (!"till_state" %in% names(all_data)) {all_data[, till_state := NA_character_]} else {
  all_data[, till_state := trimws(as.character(till_state))]}

if (!"county_geoid" %in% names(all_data)) {all_data[, county_geoid := NA_character_]}

if (!"season" %in% names(all_data)) {all_data[, season := NA_integer_]}

all_data[, county_safe := safe_county_name(county)]

##--------load lookup table---------

lookup = fread("/projectnb/dietzelab/ccmmf/management/LandIQ_cropCode_lookup_table.csv")

lookup[, `:=`(
  CLASS = as.character(CLASS), SUBCLASS = as.character(SUBCLASS))]

lookup_subclass = unique(lookup[, .(
  CLASS, SUBCLASS, CLASS_desc, SUBCLASS_desc, PFT)],
  by = c("CLASS", "SUBCLASS"))

##--------load historical LandIQ source with subclass---------

with_subclass = fread("crops_full_counties.csv")

if ("V1" %in% names(with_subclass)) {with_subclass[, V1 := NULL]}

#standardize class, sublcass, season, and county name columns
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
       paste(missing_subclass_cols, collapse = ", "), "\nAvailable columns: ",
       paste(names(with_subclass), collapse = ", "))}

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
    county_safe = as.character(county_safe), CLASS = as.character(CLASS), 
    SUBCLASS = as.character(SUBCLASS))]
  
  if (!"season" %in% names(obs)) {
    obs[, season := 0L]
  } else {
    obs[, season := as.integer(season)]
    obs[is.na(season), season := 0L]
  }
  
  #only use historical data up to the prediction start year
  obs = obs[year <= start_year]
  
  dt[, parcel_id := as.character(parcel_id)]
  dt[, CLASS := as.character(get(crop_col))]
  dt[, (group_col) := as.character(get(group_col))]
  
  #remove old lookup/subclass fields if function is accidentally rerun
  old_lookup_cols = intersect(c("SUBCLASS", "CLASS_desc", "SUBCLASS_desc", "PFT"),
    names(dt))
  
  if (length(old_lookup_cols) > 0) {dt[, (old_lookup_cols) := NULL]}
  
  #global subclass probabilities 
  global_probs = obs[
    !is.na(CLASS) & !is.na(SUBCLASS),
    .N, by = .(CLASS, SUBCLASS)]
  
  if (nrow(global_probs) > 0) {global_probs[, prob := N / sum(N), by = CLASS]}
  
  #county-specific subclass probabilities 
  group_probs = obs[
    !is.na(CLASS) & !is.na(SUBCLASS),
    .N, by = .(county_safe, CLASS, SUBCLASS)]
  
  if (nrow(group_probs) > 0) {group_probs[, prob := N / sum(N), by = .(county_safe, CLASS)]}
  
  #lookup fallback if a predicted class has no observed subclass distribution
  lookup_probs = unique(lookup_subclass[
    !is.na(CLASS) & !is.na(SUBCLASS),
    .(CLASS, SUBCLASS)])
  
  if (nrow(lookup_probs) > 0) {lookup_probs[, N := 1L]
    lookup_probs[, prob := 1 / .N, by = CLASS]}
  
  # Last observed class/subclass per parcel + county from subclass source
  last_obs_source = obs[
    !is.na(CLASS) & !is.na(SUBCLASS)]
  
  last_obs = last_obs_source[
    order(year, season),
    .SD[.N], by = .(parcel_id, county_safe)]
  
  if (!"SUBCLASS" %in% names(last_obs)) {last_obs[, SUBCLASS := NA_character_]}
  
  if (!"CLASS" %in% names(last_obs)) {last_obs[, CLASS := NA_character_]}
  
  last_obs = last_obs[
    ,
    .(
      parcel_id, county_safe, last_CLASS = CLASS, last_SUBCLASS = SUBCLASS)]
  
  dt = merge(dt, last_obs, by = c("parcel_id", "county_safe"), all.x = TRUE)
  
  setorder(dt, parcel_id, year)
  
  #find runs of same predicted CLASS within each parcel
  dt[, prev_CLASS := shift(CLASS), by = .(parcel_id, county_safe)]
  dt[is.na(prev_CLASS), prev_CLASS := last_CLASS]
  
  dt[, new_run := fifelse(is.na(CLASS), FALSE, is.na(prev_CLASS) | CLASS != prev_CLASS)]
  
  dt[, run_id := cumsum(new_run), by = .(parcel_id, county_safe)]
  
  dt[, SUBCLASS := NA_character_]
  
  #if predicted class matches last observed class, carry forward real historical subclass
  dt[run_id == 0 &
      !is.na(CLASS) &
      !is.na(last_CLASS) &
      CLASS == last_CLASS &
      !is.na(last_SUBCLASS),
    SUBCLASS := last_SUBCLASS]
  
  #for remaining class-runs, draw subclass from county/class distribution
  run_table = unique(dt[
    !is.na(CLASS) & is.na(SUBCLASS),
    .(parcel_id, county_safe, run_id, CLASS)])
  
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
          prob > 0
      ]
      
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
    dt[, drawn_SUBCLASS := NULL]}
  
  helper_cols = intersect(c("last_CLASS", "last_SUBCLASS", "prev_CLASS", "new_run", "run_id"),
                          names(dt))
  
  dt[, (helper_cols) := NULL]
  
  if (!"SUBCLASS" %in% names(dt)) {dt[, SUBCLASS := NA_character_]}
  
  if (!"CLASS" %in% names(dt)) {dt[, CLASS := NA_character_]}
  
  #add lookup descriptions/PFT
  dt = merge(dt, lookup_subclass, by = c("CLASS", "SUBCLASS"), all.x = TRUE)
  
  setorder(dt, orig_order)
  dt[, orig_order := NULL]
  
  return(dt[])}
  
##--------crop prediction functions---------
predict_markov = function(start_info, tmat, end_year, id_col = "parcel_id", time_col = "year", state_col = "crop_class") {
  
  dt = copy(start_info)
  
  if (!(id_col %in% names(dt))) {stop("id_col not found in start_info: ", id_col)}
  if (!(time_col %in% names(dt))) {stop("time_col not found in start_info: ", time_col)}
  if (!(state_col %in% names(dt))) {stop("state_col not found in start_info: ", state_col)}
  
  if (id_col != "parcel_id") {setnames(dt, id_col, "parcel_id")}
  if (time_col != "year") {setnames(dt, time_col, "year")}
  
  states = rownames(tmat)
  
  dt = dt[get(state_col) %in% states]
  
  if (nrow(dt) == 0) {return(data.table())}
  
  #clean matrix 
  tmat[is.na(tmat)] = 0
  tmat[tmat < 0] = 0
  
  row_sums = rowSums(tmat)
  for (s in states[row_sums <= 0 | is.na(row_sums)]) {
    tmat[s, ] = 0
    tmat[s, s] = 1}
  
  tmat = sweep(tmat, 1, rowSums(tmat), "/")
  
  preds = dt[, {
    
    start_state = get(state_col)
    years = seq(year + 1L, end_year)
    n_years = length(years)
    
    if (is.na(start_state) || !(start_state %in% states) || n_years == 0) {
      .(year = integer(), CLASS = character(), prob_crop_class = numeric())
    } else {
      
      # Track cumulative transition probabilities from start year to each future year
      A_power = diag(length(states))
      rownames(A_power) = states
      colnames(A_power) = states
      
      yearly_probs = vector("list", n_years)
      
      for (i in seq_len(n_years)) {
        A_power = A_power %*% tmat
        rownames(A_power) = states
        colnames(A_power) = states
        
        p_i = as.numeric(A_power[start_state, ])
        names(p_i) = states
        p_i[is.na(p_i)] = 0
        
        if (sum(p_i) > 0) {
          p_i = p_i / sum(p_i)
        }
        
        yearly_probs[[i]] = p_i
      }
      
      final_probs = yearly_probs[[n_years]]
      final_state = names(final_probs)[which.max(final_probs)]
      final_prob = final_probs[final_state]
      
      # Default: parcel stays constant
      pred_state = rep(start_state, n_years)
      pred_prob = sapply(yearly_probs, function(p) p[start_state])
      
      # If final most-likely state is different, allow ONE conversion only
      if (!is.na(final_state) && final_state != start_state) {
        
        # convert only when final state becomes at least as likely as start state
        conversion_idx = which(sapply(yearly_probs, function(p) {
          p[final_state] >= p[start_state]
        }))
        
        if (length(conversion_idx) == 0) {
          conversion_idx = n_years
        } else {
          conversion_idx = conversion_idx[1]
        }
        
        pred_state[conversion_idx:n_years] = final_state
        pred_prob[conversion_idx:n_years] = sapply(
          yearly_probs[conversion_idx:n_years],
          function(p) p[final_state]
        )
      }
      
      .(
        year = years,
        CLASS = pred_state,
        prob_crop_class = as.numeric(pred_prob)
      )
    }
    
  }, by = parcel_id]
  
  return(preds)
}

predict_grouped_markov = function(year_states, transition_mats, group_col, start_year, end_year, id_col = "parcel_id",
                                  time_col = "year", state_col = "crop_class") {
  
  dt = copy(year_states)
  
  if (!(group_col %in% names(dt))) {stop("group_col not found in year_states: ", group_col)}
  
  if (id_col != "parcel_id") {setnames(dt, id_col, "parcel_id")}
  
  if (time_col != "year") {setnames(dt, time_col, "year")}
  
  all_preds = list()
  
  groups = intersect(unique(na.omit(dt[[group_col]])), names(transition_mats))
  
  if (length(groups) == 0) {stop("No overlapping groups between all_data and transition matrices.")}
  
  for (g in groups) {
    
    message("Predicting crop class for county: ", g)
    
    dt_g = dt[get(group_col) == g]
    tmat_g = transition_mats[[g]]
    
    # latest observed state for each parcel up to start_year
    start_info = dt_g[year <= start_year,
                      .SD[which.max(year)], by = parcel_id]
    
    preds_g = predict_markov(start_info = start_info, tmat = tmat_g, end_year = end_year, state_col = state_col)
    
    if (nrow(preds_g) == 0) {
      next}
    
    preds_g[, (group_col) := g]
    
    all_preds[[g]] = preds_g}
  
  return(rbindlist(all_preds, fill = TRUE))}

##--------till target functions---------

load_crop_by_till_targets = function(target_dir, run_scenario, end_year) {
  
  pattern = paste0("crop_by_till_targets_.*_", run_scenario, "_", end_year, "\\.csv$")
  
  files = list.files(target_dir, pattern = pattern, full.names = TRUE)
  
  if (length(files) == 0) {stop("No crop-by-till target files found in: ", target_dir)}
  
  out = rbindlist(lapply(files, fread), fill = TRUE)
  
  out[, county_safe := as.character(county_safe)]
  out[, crop_state := as.character(crop_state)]
  out[, till_state := as.character(till_state)]
  out[, target_acres_raw := as.numeric(target_acres_raw)]
  
  return(out)}

build_annual_till_targets = function(all_data, till_targets_2045, start_year, end_year) {
  
  latest_obs = all_data[year <= start_year & !is.na(crop_class) & !is.na(till_state),
                        .SD[which.max(year)], by = parcel_id]
  
  baseline_till = latest_obs[
    ,
    .(baseline_acres = sum(ACRES, na.rm = TRUE)), by = .(county_safe, crop_state = crop_class, till_state)]
  
  if (nrow(baseline_till) > 0) {baseline_till[, baseline_share := baseline_acres / sum(baseline_acres), 
                                              by = .(county_safe, crop_state)]
  } else {
    baseline_till[, baseline_share := numeric()]}
  
  target_till = copy(till_targets_2045)
  
  target_till[, target_share := target_acres_raw / sum(target_acres_raw), by = .(county_safe, crop_state)]
  
  all_counties = unique(target_till$county_safe)
  all_crops = unique(target_till$crop_state)
  all_till_states = unique(c(baseline_till$till_state, target_till$till_state))
  
  annual_till_targets = CJ(county_safe = all_counties, crop_state = all_crops, till_state = all_till_states,
                           year = seq(start_year + 1L, end_year))
  
  annual_till_targets = merge(annual_till_targets, baseline_till[, .(county_safe, crop_state, till_state, baseline_share)],
                              by = c("county_safe", "crop_state", "till_state"), all.x = TRUE)
  
  annual_till_targets = merge(annual_till_targets, target_till[, .(county_safe, crop_state, till_state, target_share)],
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
  
  annual_till_targets[share_sum <= 0, till_share := 1 / .N, by = .(county_safe, crop_state, year)]
  
  annual_till_targets[, share_sum := NULL]
  
  return(annual_till_targets)}

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
        
        #randomize row order so the same parcels are not always assigned first
        temp[, rand_order := runif(.N)]
        setorder(temp, rand_order)
        
        targets[, target_acres := till_share * group_total_acres]
        targets[, upper_acres := cumsum(target_acres)]
        
        #use cumulative parcel acreage midpoint for assignment
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
crop_mats = load_crop_matrices(crop_matrix_dir = crop_matrix_dir, crop_matrix_pattern = crop_matrix_pattern,
                               run_scenario = run_scenario)

missing_mats = setdiff(unique(all_data$county_safe), names(crop_mats))

if (length(missing_mats) > 0) {
  message("Counties in all_data without crop matrices: ", length(missing_mats))}

crop_targets_2045 = load_crop_targets(
  target_dir = target_dir,
  run_scenario = run_scenario,
  end_year = end_year
)

future_crop = predict_grouped_markov_to_targets(
  year_states = all_data,
  transition_mats = crop_mats,
  crop_targets_2045 = crop_targets_2045,
  group_col = "county_safe",
  start_year = start_year,
  end_year = end_year,
  state_col = "crop_class"
)

if (nrow(future_crop) == 0) {stop("No future crop predictions were generated.")}

##--------add parcel metadata---------
parcel_meta = all_data[year <= start_year,
                       .SD[which.max(year)], by = parcel_id
][
  ,
  .(
    parcel_id, county, county_geoid, county_safe, ACRES, till_state_2023 = till_state)]

future_landiq = merge(future_crop, parcel_meta, by = c("parcel_id", "county_safe"), all.x = TRUE)

##--------assign till states---------
if (use_till_targets) {
  
  message("Assigning till states using ", run_scenario, " crop-by-till targets.")
  
  till_targets_2045 = load_crop_by_till_targets(target_dir = target_dir, run_scenario = run_scenario, end_year = end_year)
  
  annual_till_targets = build_annual_till_targets(all_data = all_data, till_targets_2045 = till_targets_2045, 
                                                  start_year = start_year, end_year = end_year)
  
  future_landiq = assign_till_by_targets(future_landiq = future_landiq, annual_till_targets = annual_till_targets)
  
} else {
  
  message(run_scenario, " has no crop-by-till target files. Carrying forward 2023 observed till_state.")
  
  future_landiq[, till_state := till_state_2023]
  future_landiq[, prob_till_state := 1]}

##--------final formatting---------
future_landiq[, season := NA_integer_]
future_landiq[, source := "predicted"]
future_landiq[, scenario := run_scenario]

future_landiq = future_landiq[year >= start_year + 1L & year <= end_year]

#add subclass + lookup fields
future_landiq = assign_predicted_subclass( future_landiq = future_landiq, 
                                           subclass_obs = with_subclass,
                                           lookup_subclass = lookup_subclass,
                                           crop_col = "CLASS", group_col = "county_safe", 
                                           start_year = start_year)

#clean optional helper columns
if ("till_state_2023" %in% names(future_landiq)) {future_landiq[, till_state_2023 := NULL]}

#make sure optional columns exist before selecting
optional_cols = c("SUBCLASS", "CLASS_desc", "SUBCLASS_desc", "PFT")

for (cc in optional_cols) {if (!(cc %in% names(future_landiq))) {
  future_landiq[, (cc) := NA_character_]}}

final_cols = c("parcel_id", "county", "county_geoid", "county_safe", "year", "season", "CLASS", "SUBCLASS", "CLASS_desc",
               "SUBCLASS_desc", "PFT", "till_state", "prob_crop_class", "prob_till_state", "ACRES", "source", "scenario")

future_landiq = future_landiq[, ..final_cols]

setorder(future_landiq, county_safe, parcel_id, year)


pred_2045_check = future_landiq[
  year == end_year,
  .(predicted_acres = sum(ACRES, na.rm = TRUE)),
  by = .(county_safe, CLASS)
]

target_2045_check = copy(crop_targets_2045)
setnames(target_2045_check, "crop_state", "CLASS")

check_2045 = merge(
  pred_2045_check,
  target_2045_check,
  by = c("county_safe", "CLASS"),
  all = TRUE
)

check_2045[is.na(predicted_acres), predicted_acres := 0]
check_2045[is.na(target_acres), target_acres := 0]
check_2045[, diff_acres := predicted_acres - target_acres]
check_2045[, abs_diff_acres := abs(diff_acres)]

check_path = file.path(
  scenario_prediction_dir,
  paste0("prediction_target_check_", run_scenario, "_", end_year, ".csv")
)

fwrite(check_2045, check_path)

message("Wrote 2045 target check to: ", check_path)




##--------write county-separated outputs---------
for (cty in unique(na.omit(future_landiq$county_safe))) {
  
  out_path = file.path(scenario_prediction_dir, paste0(cty, "_predicted_2024_", end_year, ".csv"))
  
  fwrite(future_landiq[county_safe == cty], out_path)}

message("Finished writing predictions to: ", scenario_prediction_dir)