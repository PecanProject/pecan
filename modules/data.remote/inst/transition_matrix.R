##creates the transition matricies for all parcels in each county by creating crop class sequences

setwd("/projectnb/dietzelab/ananyak")
library(arrow)
library(data.table)
library(dplyr)

##-----setup------
path_management = "/projectnb/dietzelab/ccmmf/management"
path_landiq_v4  = "/projectnb/dietzelab/ccmmf/LandIQ-harmonized-v4.1"

lookup = fread(file.path(path_management, "LandIQ_cropCode_lookup_table.csv"))
ag_classes = unique(lookup[is_agricultural == TRUE, as.character(CLASS)])

year_min = 2018L
year_max = 2023L

crops_full <- as.data.table(
  arrow::open_dataset(file.path(path_landiq_v4, "crops_all_years.parq")) |>
    filter(year >= year_min, year <= year_max, CLASS %in% ag_classes) |>
    select(parcel_id, year, season, CLASS, SUBCLASS, centx, centy) |>
    collect()
)

crops_full[, `:=`(
  parcel_id = as.character(parcel_id),
  year = as.integer(year),
  season = as.integer(season),
  CLASS = as.character(CLASS),
  SUBCLASS = as.character(SUBCLASS)
)]

##-----adding county to crops file-----
library(sf)
library(tigris)
options(tigris_use_cache = TRUE)

#one row per parcel so repeated years/seasons do not duplicate spatial join
parcel_unique = crops_full[
  !is.na(centx) & !is.na(centy),
  .SD[1],
  by = parcel_id]

#convert centx/centy to sf; centx/centy in EPSG:3310
parcel_sf = st_as_sf(
  parcel_unique,
  coords = c("centx", "centy"),
  crs = 3310,
  remove = FALSE)

#California county boundaries
ca_counties = counties(state = "CA", cb = TRUE, class = "sf") |>
  st_transform(4326)
ca_counties = ca_counties |> st_transform(3310)

#spatial join parcels to counties
parcel_county = st_join(
  parcel_sf,
  ca_counties[, c("NAME", "GEOID")])

#convert back to data.table and rename columns 
parcel_county_dt = as.data.table(st_drop_geometry(parcel_county))
setnames(parcel_county_dt, "NAME", "county")

#keep only parcel_id + county info
parcel_county_lookup = parcel_county_dt[, .(
  parcel_id,
  county,
  county_geoid = GEOID
)]

# merge county back onto the full crops_full table
crops_full_county = merge(
  crops_full,
  parcel_county_lookup,
  by = "parcel_id",
  all.x = TRUE
)

##-------order for sequence building - add season if needed-------
setorder(crops_full_county, parcel_id, county, year, season)
write.csv(crops_full_county, 'crops_full_counties.csv')


##-------cleaning rules to lower the amount of messy/unrealistic sequences (for 'X' cases)------
fix_seq = function(seq) {
  parts = strsplit(seq, "-", fixed = TRUE)[[1]]
  n = length(parts)
  
  if (n == 0) return(seq)
  
  
  #Rule 1: if X between identical classes, replace with that class
  if (n >= 3) {
    for (i in 2:(n - 1)) {
      if (parts[i] == "X" && parts[i - 1] == parts[i + 1]) {
        parts[i] = parts[i - 1]
      }
    }
  }
  
  #Rule 2: similar to rule 2 but with a 'short run' of X's
  i = 1
  while (i <= n) {
    if (parts[i] == "X") {
      start = i
      while (i <= n && parts[i] == "X") i = i + 1
      end = i - 1
      run_len = end - start + 1
      
      if (
        run_len <= 2 &&
        start > 1 &&
        end < n &&
        parts[start - 1] == parts[end + 1]
      ) {
        parts[start:end] = parts[start - 1]
      }
    } else {
      i = i + 1
    }
  }
  
  #Rule 3: 'edge X's' where its all one class + one X -- replace with the majority 
  if (n >= 2) {
    if (parts[1] == "X") {
      parts[1] = parts[2]
    }
    if (parts[n] == "X") {
      parts[n] = parts[n - 1]
    }
  }
  
  #Rule 4: remaining short X run with one valid neighbor side --> fill from that side
  i = 1
  while (i <= n) {
    if (parts[i] == "X") {
      start = i
      while (i <= n && parts[i] == "X") i = i + 1
      end = i - 1
      run_len = end - start + 1
      
      left_val  = if (start > 1) parts[start - 1] else NA_character_
      right_val = if (end < n) parts[end + 1] else NA_character_
      
      if (run_len <= 2) {
        if (!is.na(left_val) && left_val != "X" && (is.na(right_val) || right_val == "X")) {
          parts[start:end] = left_val
        } else if (!is.na(right_val) && right_val != "X" && (is.na(left_val) || left_val == "X")) {
          parts[start:end] = right_val
        }
      }
    } else {
      i = i + 1
    }
  }
  paste(parts, collapse = "-")
}



##-----sequence formatting by parcel-----
crop_sequences = crops_full_county[
  ,
  .(
    crop_sequence = paste(CLASS, collapse = "-"),
    season_sequence = paste(season, collapse = "-")
  ),
  by = .(county, county_geoid, parcel_id, year)
]

##-------apply sequence-fixing rules-------
crop_sequences[, crop_sequence := vapply(crop_sequence, fix_seq, character(1))]

##------dominant crop and probabilities-------
seq_lookup = unique(crop_sequences[, .(crop_sequence, season_sequence)])

seq_lookup[, c("dominant_crop", "non_dom_prob") := {
  crop_split   = strsplit(crop_sequence, "-", fixed = TRUE)
  season_split = strsplit(season_sequence, "-", fixed = TRUE)
  
  dom  = character(length(crop_split))
  prob = numeric(length(crop_split))
  
  for (i in seq_along(crop_split)) {
    x = crop_split[[i]]
    s = as.integer(season_split[[i]])
    
    if (length(x) %in% c(2, 3) &&
        length(unique(x)) == length(x) &&
        2 %in% s) {
      
      dom[i] = x[which(s == 2)[1]]
      prob[i] = 1 - 1 / length(x)
      
    } else {
      tab = table(x)
      j = which.max(tab)
      dom_n = unname(tab[j])
      
      dom[i] = names(tab)[j]
      prob[i] = 1 - dom_n / length(x)
    }
  }
  
  .(dom, prob)
}]

crop_sequences = seq_lookup[crop_sequences, on = c("crop_sequence", "season_sequence")]

##-----standardized year states------
##parcel_id, year, state, non_dom_prob, optional grouping columns like county

year_states = copy(crop_sequences)[,.(
    county,
    county_geoid,
    parcel_id,
    year,
    state = dominant_crop,
    non_dom_prob)]

year_states[, state := trimws(as.character(state))]
year_states[, parcel_id := as.character(parcel_id)]
year_states[, year := as.integer(year)]
setorder(year_states, county, parcel_id, year)



##-----function for transition format------
make_transitions = function(
    year_states,
    id_col = "parcel_id",
    time_col = "year",
    state_col = "state",
    non_dom_col = "non_dom_prob",
    min_weight = 0.05) {
  
  dt = copy(as.data.table(year_states))
  
  setnames(dt, id_col, "id")
  setnames(dt, time_col, "time")
  setnames(dt, state_col, "state")
  
  if (non_dom_col %in% names(dt)) {
    setnames(dt, non_dom_col, "non_dom_prob")
  } else {
    dt[, non_dom_prob := 0]}
  
  setorder(dt, id, time)
  
  dt[, `:=`(
    from = state,
    to = shift(state, type = "lead"),
    next_time = shift(time, type = "lead"),
    from_non_dom = non_dom_prob,
    to_non_dom = shift(non_dom_prob, type = "lead")
  ), by = id]
  
  transitions = dt[
    !is.na(from) &
      !is.na(to) &
      next_time == time + 1]
  
  transitions[, weight := pmax(
    min_weight,
    (1 - from_non_dom) * (1 - to_non_dom))]
  
  setnames(transitions, "id", id_col)
  setnames(transitions, "time", time_col)
  
  return(transitions)
}

##-----function to make a transition matrix------
make_transition_matrix = function(
    dt,
    states_all,
    from_col = "from",
    to_col = "to",
    weight_col = "weight") {
  
  dt = copy(as.data.table(dt))
  
  setnames(dt, from_col, "from")
  setnames(dt, to_col, "to")
  
  if (weight_col %in% names(dt)) {
    setnames(dt, weight_col, "weight")
  } else {
    dt[, weight := 1]}
  
  transitions_weighted = dt[
    !is.na(from) & !is.na(to),
    .(N = sum(weight, na.rm = TRUE)),
    by = .(from, to)]
  
  if (nrow(transitions_weighted) == 0) {
    empty_mat = matrix(
      0,
      nrow = length(states_all),
      ncol = length(states_all),
      dimnames = list(states_all, states_all))
    return(empty_mat)}
  
  tmat_counts = dcast(
    transitions_weighted,
    from ~ to,
    value.var = "N",
    fill = 0)
  
  ## add missing columns
  missing_cols = setdiff(states_all, colnames(tmat_counts))
  for (mc in missing_cols) {
    tmat_counts[[mc]] = 0}
  
  ## add missing rows
  missing_rows = setdiff(states_all, tmat_counts$from)
  if (length(missing_rows) > 0) {
    zero_rows = data.table(from = missing_rows)
    for (s in states_all) {
      zero_rows[[s]] = 0}
    tmat_counts = rbind(tmat_counts, zero_rows, fill = TRUE)}
  
  ## order rows/cols
  tmat_counts[, ord := match(from, states_all)]
  setorder(tmat_counts, ord)
  tmat_counts[, ord := NULL]
  tmat_counts = tmat_counts[, c("from", states_all), with = FALSE]
  
  ## convert to probability matrix
  rn = tmat_counts$from
  prob_mat = as.matrix(tmat_counts[, ..states_all])
  storage.mode(prob_mat) = "double"
  
  row_totals = rowSums(prob_mat)
  tmat_final = prob_mat
  
  tmat_final[row_totals > 0, ] =
    prob_mat[row_totals > 0, ] / row_totals[row_totals > 0]
  
  tmat_final[row_totals == 0, ] = 0
  
  rownames(tmat_final) = rn
  colnames(tmat_final) = states_all
  
  stopifnot(all(rownames(tmat_final) == states_all))
  stopifnot(all(colnames(tmat_final) == states_all))
  stopifnot(all(abs(rowSums(tmat_final)[row_totals > 0] - 1) < 1e-10))
  
  return(tmat_final)}

##-----function to make transition matrices by a category (crop, county, etc)------
#avoids having to use split(... by=grouping)
make_grouped_transition_matrices = function(
    transitions,
    states_all,
    group_cols) {
  
  transition_groups = split(
    transitions,
    by = group_cols,
    keep.by = TRUE)
  
  transition_mats = lapply(
    transition_groups,
    make_transition_matrix,
    states_all = states_all)
  
  return(transition_mats)}


##-----using functions to make matrices by county------
transitions_full = make_transitions(
  year_states = year_states,
  id_col = "parcel_id",
  time_col = "year",
  state_col = "state",
  non_dom_col = "non_dom_prob")

states_all = c("YP","D","X","T","G","F","P","C","I","V","R")

county_transition_mats = make_grouped_transition_matrices(
  transitions = transitions_full,
  states_all = states_all,
  group_cols = "county")


##-----save------
dir.create("county_transition_matrices", showWarnings = FALSE)

for (cty in names(county_transition_mats)) {
  
  safe_name = gsub("[^A-Za-z0-9_]+", "_", cty)
  
  write.csv(
    county_transition_mats[[cty]],
    file = file.path(
      "county_transition_matrices",
      paste0(safe_name, "_transition_matrix.csv")),
    row.names = TRUE)}