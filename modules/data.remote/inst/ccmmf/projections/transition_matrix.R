## Creates county-level crop transition matrices from historical crop records.

pacman::p_load(PEcAn.data.remote, arrow, data.table, dplyr, sf, tigris)

# ---- set up ----
config = config::get(config = 'default', file = "config.yml")

lookup_path = config[["crop_lookup_path"]]
landiq_path = config[["crops_path"]]

crop_history_path = file.path(config[["work_root"]], config[["crop_history_file"]])

year_states_path = file.path(config[["work_root"]], config[["crop_data_file"]])

matrix_output_dir = file.path(config[["work_root"]], config[["crop_matrix_dir"]])

message("Set up complete. Moving onto processing landIQ data.")

# ---- load historical LandIQ records & add geoID information ----
lookup = data.table::fread(lookup_path)

ag_classes = unique(lookup[is_agricultural == TRUE, as.character(CLASS)])

crops_full = data.table::as.data.table(
  arrow::read_parquet(landiq_path) |>
    dplyr::filter(
      .data$year >= config[["historical_start_year"]],
      .data$year <= config[["start_year"]],
      .data$CLASS %in% ag_classes
    ) |>
    dplyr::select("parcel_id", "COUNTY", "year", "season", "CLASS", "SUBCLASS", "ACRES", "COVER")
  )

crops_full[, `:=`(
  parcel_id = as.character(parcel_id), COUNTY = as.character(COUNTY), year = as.integer(year), 
  season = as.integer(season), CLASS = as.character(CLASS), SUBCLASS = as.character(SUBCLASS),
  ACRES = as.numeric(ACRES)
)]

setnames(crops_full, "COUNTY", "county")

options(tigris_use_cache = TRUE)

ca_counties = tigris::counties(state = "CA", cb = TRUE, class = "sf")

county_lookup = data.table::as.data.table(
  sf::st_drop_geometry(ca_counties)
)[, .(
  county = NAME, county_geoid = GEOID)]

crops_full_county = merge(crops_full, county_lookup, by = "county", all.x = TRUE)

data.table::setorder(crops_full_county, parcel_id, year, season)

#Required later for projected SUBCLASS assignment
data.table::fwrite(crops_full_county, crop_history_path)

# ---- sequence cleaning ----
##functions that clean up X classes when they're not likely accurate
fix_seq = function(seq) {
  parts = strsplit(seq, "-", fixed = TRUE)[[1]]
  n = length(parts)
  
  if (n == 0) {
    return(seq)
  }
  
  # Rule 1: replace a single X between identical classes.
  if (n >= 3) {
    for (i in 2:(n - 1)) {
      if (parts[i] == "X" && parts[i - 1] == parts[i + 1]) {
        parts[i] = parts[i - 1]
      }
    }
  }
  
  # Rule 2: replace short X runs bounded by the same class.
  i = 1
  while (i <= n) {
    if (parts[i] == "X") {
      start = i
      
      while (i <= n && parts[i] == "X") {
        i = i + 1
      }
      
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
  
  # Rule 3: replace an edge X with its adjacent observed class.
  if (n >= 2) {
    if (parts[1] == "X") {
      parts[1] = parts[2]
    }
    
    if (parts[n] == "X") {
      parts[n] = parts[n - 1]
    }
  }
  
  # Rule 4: fill a remaining short X run from its one valid neighbor.
  i = 1
  while (i <= n) {
    if (parts[i] == "X") {
      start = i
      
      while (i <= n && parts[i] == "X") {
        i = i + 1
      }
      
      end = i - 1
      run_len = end - start + 1
      
      left_val = if (start > 1) {
        parts[start - 1]
      } else {
        NA_character_
      }
      
      right_val = if (end < n) {
        parts[end + 1]
      } else {
        NA_character_
      }
      
      if (run_len <= 2) {
        if (
          !is.na(left_val) &&
          left_val != "X" &&
          (is.na(right_val) || right_val == "X")
        ) {
          parts[start:end] = left_val
        } else if (
          !is.na(right_val) &&
          right_val != "X" &&
          (is.na(left_val) || left_val == "X")
        ) {
          parts[start:end] = right_val
        }
      }
    } else {
      i = i + 1
    }
  }
  
  paste(parts, collapse = "-")
}

# ---- annual parcel states ----
#Assigns one annual crop state per parcel-year.
#Prefer season 2 when all observed classes are unique; otherwise use the most frequent class.

crop_sequences = crops_full_county[
  ,
  .(
    crop_sequence = paste(CLASS, collapse = "-"),
    season_sequence = paste(season, collapse = "-")
  ),
  by = .(county, county_geoid, parcel_id, year, ACRES)
]

crop_sequences[
  ,
  crop_sequence := vapply(
    crop_sequence, fix_seq, character(1))
]

seq_lookup = unique(crop_sequences[
    ,
    .(crop_sequence, season_sequence
    )
  ]
)

seq_lookup[
  ,
  c("dominant_crop", "non_dom_prob") := {
    crop_split = strsplit(crop_sequence, "-", fixed = TRUE)
    
    season_split = strsplit(season_sequence, "-", fixed = TRUE)
    
    dom = character(length(crop_split))
    prob = numeric(length(crop_split))
    
    for (i in seq_along(crop_split)) {
      x = crop_split[[i]]
      s = as.integer(season_split[[i]])
      
      if (
        length(x) %in% c(2, 3) &&
        length(unique(x)) == length(x) &&
        2 %in% s
      ) {
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
  }
]

crop_sequences = seq_lookup[crop_sequences, on = c("crop_sequence", "season_sequence")
]

year_states = data.table::copy(crop_sequences)[
  ,
  .(county, county_geoid, parcel_id, year, state = dominant_crop, ACRES, non_dom_prob)
]

year_states[, `:=`(state = trimws(as.character(state)), parcel_id = as.character(parcel_id), 
                   year = as.integer(year), ACRES = as.numeric(ACRES)
)]

data.table::setorder(year_states, county, parcel_id, year)

data.table::fwrite(year_states, year_states_path)

message('Aggregated to one crop state per parcel-year. Moving on to transition matrix development.')

# ---- create county transition matrices ----
transitions_full = PEcAn.data.remote::make_transitions(
  year_states = year_states, id_col = "parcel_id", time_col = "year", state_col = "state",
  non_dom_col = "non_dom_prob")

states_all = c("YP", "D", "X", "T", "G", "F", "P", "C", "I", "V", "R")

county_transition_mats =
  PEcAn.data.remote::make_grouped_transition_matrices(
    transitions = transitions_full, states_all = states_all, group_cols = "county")

dir.create(matrix_output_dir, recursive = TRUE,showWarnings = FALSE)

for (cty in names(county_transition_mats)) {
  safe_name = gsub("[^A-Za-z0-9_]+", "_", cty)
  
  write.csv(county_transition_mats[[cty]], file = file.path(
    matrix_output_dir, paste0(safe_name, "_crop_matrix.csv")
    ),
    row.names = TRUE
  )
}

message("Crop transition matrices complete.\n", "Counties processed: ", length(county_transition_mats), "\n",
  "Outputs: ", matrix_output_dir)