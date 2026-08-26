## Creates county-level tillage transition matrices from historical NDTI records.
## Tillage classes are based on annual NDTI percent-change thresholds.

pacman::p_load(PEcAn.data.remote, arrow, data.table)

# ---- set up ----
#Set this environment variable to the user's SCC workspace, for example:
#Sys.setenv(CCMMF_WORK_ROOT = "/path/to/yourusername")
work_root = Sys.getenv("CCMMF_WORK_ROOT")

if (!nzchar(work_root)) {
  stop("CCMMF_WORK_ROOT is not set. Set it to the workspace used for ", 
    "intermediate and output files before running this script.")
}

# Shared project data can be overridden if the SCC path changes.
ccmmf_root = Sys.getenv("CCMMF_SHARED_ROOT", unset = "/projectnb/dietzelab/ccmmf")

config = list(tillage_input_dir = file.path(ccmmf_root, "management", "event_files"),
  crop_year_path = file.path(work_root, "crop_year_states_cleaned.csv"),
  all_data_path = file.path(work_root, "all_data.csv"),
  matrix_output_dir = file.path(work_root, "county_till_matrices"),
  
  years = 2018:2023, no_till_threshold = 30, low_till_threshold = 70)

if (!dir.exists(config[["tillage_input_dir"]])) {
  stop("Tillage input directory not found: ", config[["tillage_input_dir"]])
}

if (!file.exists(config[["crop_year_path"]])) {
  stop("Crop-year state file not found:", config[["crop_year_path"]], "\nRun transition_matrix.R first.")
}

# ---- load annual NDTI files ----

till_files = file.path(config[["tillage_input_dir"]], paste0("tillage_statewide_", config[["years"]],".parquet"))

missing_till_files = till_files[!file.exists(till_files)]

if (length(missing_till_files) > 0) {
  stop("Missing tillage parquet files:\n", paste(missing_till_files, collapse = "\n"))
}

tillage = data.table::rbindlist(
  lapply(till_files,
    function(f) {
      dt = data.table::as.data.table(
        arrow::read_parquet(f)
      )
      
      required_cols = c("parcel_id", "ndti_pct_change")
      
      missing_cols = setdiff(required_cols,names(dt))
      
      if (length(missing_cols) > 0) {
        stop(basename(f), " is missing required columns: ", paste(missing_cols, collapse = ", ")
        )
      }
      
      yr = as.integer(sub( ".*tillage_statewide_([0-9]{4})\\.parquet$", "\\1", basename(f)))
      
      dt[, year := yr]
      dt
    }
  ),
  fill = TRUE
)

if (nrow(tillage) == 0) {
  stop("No tillage observations were loaded.")
}

tillage[, `:=`(
  parcel_id = as.character(parcel_id), year = as.integer(year), ndti_pct_change = as.numeric(ndti_pct_change)
)]

data.table::setorder(
  tillage, parcel_id, year)

# ---- classify tillage observations ----

# NDTI percent-change thresholds: 0-30 = no till, >30 to <70 = low till, >=70 = high till
# Negative or missing NDTI percent changes are left unclassified.

tillage[, till_class := data.table::fcase(
  ndti_pct_change >= 0 &
    ndti_pct_change <= config[["no_till_threshold"]],
  "no_till",
  
  ndti_pct_change > config[["no_till_threshold"]] &
    ndti_pct_change < config[["low_till_threshold"]],
  "low_till",
  
  ndti_pct_change >= config[["low_till_threshold"]],
  "high_till",
  
  default = NA_character_
)]

# ---- load historical crop-year states ----

# This file is produced by transition_matrix.R and contains one dominant crop
# class per parcel-year. Reuse it here so crop and tillage states can be joined.

crop_year = data.table::fread(
  config[["crop_year_path"]])

required_crop_cols = c("parcel_id", "year", "state", "non_dom_prob", "county",
  "county_geoid", "ACRES")

missing_crop_cols = setdiff(required_crop_cols, names(crop_year))

if (length(missing_crop_cols) > 0) {
  stop("crop_year_states_cleaned.csv is missing required columns: ", paste(missing_crop_cols, collapse = ", "))
}

crop_year[, `:=`(
  parcel_id = as.character(parcel_id), year = as.integer(year), crop_class = as.character(state),
  crop_non_dom_prob = as.numeric(non_dom_prob), ACRES = as.numeric(ACRES)
)]

crop_year = crop_year[
  ,
  .(
    parcel_id, year, county, county_geoid, crop_class, crop_non_dom_prob, ACRES)
]

# ---- create annual tillage states ----
#A parcel can have multiple NDTI observations within a year. Use the most frequently observed tillage class 
#as that parcel-year's annual state. Non_dom_prob records the fraction of observations that disagree with the
#dominant annual class.

tillage_counts = tillage[
  !is.na(till_class),
  .N,
  by = .(
    parcel_id, year, till_class)
]

if (nrow(tillage_counts) == 0) {
  stop("No tillage observations could be classified using the configured ", "NDTI thresholds.")
}

tillage_counts[
  ,
  total_obs := sum(N),
  by = .(
    parcel_id, year)
]

data.table::setorder(
  tillage_counts, parcel_id, year,-N)

tillage_year_states = tillage_counts[
  ,
  .SD[1],
  by = .(
    parcel_id, year)
][
  ,
  .(
    parcel_id, year, state = till_class, n_obs = N, total_obs, non_dom_prob = 1 - N / total_obs)
]

# ---- merge crop and tillage states ----
#Keep all parcel-years with a classified tillage state. Crop information is attached when a matching crop-year 
#record exists.

tillage_year_states = merge(tillage_year_states, crop_year,
  by = c("parcel_id", "year"),
  all.x = TRUE)

data.table::setorder(
  tillage_year_states, crop_class, parcel_id, year)

data.table::fwrite(
  tillage_year_states, config[["all_data_path"]])

message("Wrote combined crop/tillage history: ", config[["all_data_path"]])

# ---- build tillage transition matrices ----
states = c("no_till", "low_till", "high_till")

tillage_transitions_annual =
  PEcAn.data.remote::make_transitions(
    year_states = tillage_year_states,
    id_col = "parcel_id", time_col = "year", state_col = "state", non_dom_col = "non_dom_prob")

if (nrow(tillage_transitions_annual) == 0) {
  stop("No consecutive annual tillage transitions were available after ", "state construction.")
}

#Statewide annual tillage transition matrix. This is printed for QC; county matrices below are the persisted 
#outputs used by downstream workflows.

till_mat =
  PEcAn.data.remote::make_transition_matrix(
    dt = tillage_transitions_annual, states_all = states)

print(till_mat)

tillage_transitions_county =
  tillage_transitions_annual[
    !is.na(tillage_transitions_annual[["crop_class"]]) &
      !is.na(tillage_transitions_annual[["county"]]),
    ,
    drop = FALSE
  ]

county_transition_mats =
  PEcAn.data.remote::make_grouped_transition_matrices(
    transitions = tillage_transitions_county, states_all = states, group_cols = "county")

if (length(county_transition_mats) == 0) {
  stop("No county-level tillage transition matrices were generated.")
}

dir.create(config[["matrix_output_dir"]], recursive = TRUE, showWarnings = FALSE)

for (cty in names(county_transition_mats)) {
  safe_cty = gsub("[^A-Za-z0-9_]+", "_", cty)
  
  out_path = file.path(config[["matrix_output_dir"]], paste0(safe_cty,"_till_matrix.csv"))
  
  data.table::fwrite(
    data.table::as.data.table(
      county_transition_mats[[cty]], keep.rownames = "state"
    ),
    out_path
  )
}

message("Wrote ", length(county_transition_mats), " county tillage matrices to: ",config[["matrix_output_dir"]])