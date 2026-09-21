## Predict parcel-level crop identity and cover-crop status through 2045.
## Crop CLASS/SUBCLASS projections are shared across BAU/NBS.
## Cover cropping is projected separately because BAU/NBS cover targets differ.

pacman::p_load(data.table, arrow, bit64, dplyr)

# ---- setup ----
config = config::get(config = "scc", file = "config.yml")

#Files produced by earlier workflow steps
config$year_states_path = file.path(config$work_root, config$crop_data_file)

config$crop_history_path = file.path(config$work_root, config$crop_history_file)

#Shared LandIQ inputs
config$landiq_identity_path = config$crops_path
config$lookup_path = config$crop_lookup_path

#Optimized matrices produced by the previous script
config$crop_matrix_dir = file.path(config$work_root, config$matrix_out_dir)

#Scenario target files
config$scenario_dir = file.path(config$work_root, config$scenario_dir)

#Prediction outputs
config$prediction_dir = file.path(config$work_root, config$prediction_dir)

cover_scenario_files = c(BAU_Targets = file.path(config$scenario_dir, "BAU_Targets.csv"),
  NBS_Targets = file.path(config$scenario_dir, "NBS_Targets.csv"))

set.seed(config$seed)

start_year = config$start_year
end_year = config$end_year
crop_matrix_dir = config$crop_matrix_dir
prediction_dir = config$prediction_dir

dir.create(prediction_dir, recursive = TRUE, showWarnings = FALSE)

message('Set up complete. Now loading required data')

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

# ---- cover crop helpers ----
make_cover_matrix = function(x) {
  A = matrix(0, nrow = 2, ncol = 2, dimnames = list(c("0", "1"), c("0", "1")))
  
  z = x[next_year == year + 1L & !is.na(next_cover),
        .N, by = .(cover_state, next_cover)]
  
  if (nrow(z)) {
    for (i in seq_len(nrow(z))) {
      A[as.character(z$cover_state[i]), as.character(z$next_cover[i])] = z$N[i]
    }
  }
  
  for (s in rownames(A)) {
    rs = sum(A[s, ])
    if (rs > 0) A[s, ] = A[s, ] / rs
    else {
      A[s, ] = 0
      A[s, s] = 1
    }
  }
  
  A
}

build_cover_history = function(landiq_identity_hist) {
  annual = landiq_identity_hist[
    !is.na(parcel_id) & !is.na(year) & !is.na(COUNTY),
    .(cover_state = as.integer(any(COVER > 0, na.rm = TRUE))),
    by = .(parcel_id, COUNTY, year)
  ]
  
  annual[, county_safe := safe_county_name(COUNTY)]
  setorder(annual, county_safe, parcel_id, year)
  
  annual[, `:=`(
    next_year = shift(year, type = "lead"),
    next_cover = shift(cover_state, type = "lead")
  ), by = .(county_safe, parcel_id)]
  
  counties = sort(unique(na.omit(annual$county_safe)))
  
  mats = setNames(
    lapply(counties, function(cty) make_cover_matrix(annual[county_safe == cty])),
    counties
  )
  
  mats[["__STATEWIDE__"]] = make_cover_matrix(annual)
  
  start_state = annual[
    year <= start_year,
    .SD[which.max(year)],
    by = parcel_id
  ][, .(
    parcel_id = as.character(parcel_id),
    current_cover = as.integer(cover_state)
  )]
  
  list(annual = annual, matrices = mats, start_state = start_state)
}

read_cover_targets = function(path) {
  if (!file.exists(path)) stop("Missing scenario file: ", path)
  
  x = fread(path)
  setnames(x, names(x), trimws(names(x)))
  
  required = c("County", "Year", "Acres_Total", "Cover crop acres (CPS 340)")
  missing = setdiff(required, names(x))
  if (length(missing)) stop(basename(path), " missing: ", paste(missing, collapse = ", "))
  
  x[, `:=`(
    County = trimws(as.character(County)),
    Year = as.integer(Year),
    Acres_Total = as.numeric(Acres_Total),
    cover_acres = as.numeric(`Cover crop acres (CPS 340)`),
    county_safe = safe_county_name(County)
  )]
  
  out = x[
    Year >= start_year + 1L & Year <= end_year,
    .(
      scenario_total_acres = sum(Acres_Total, na.rm = TRUE),
      scenario_cover_acres = sum(cover_acres, na.rm = TRUE)
    ),
    by = .(county_safe, year = Year)
  ]
  
  out[, cover_share := fifelse(scenario_total_acres > 0, scenario_cover_acres / scenario_total_acres, 0)]
  out[, cover_share := pmin(1, pmax(0, cover_share))]
  out
}

predict_cover_scenario = function(future_landiq, cover_info, targets, scenario_name, seed) {
  future_base = unique(future_landiq[, .(
    parcel_id = as.character(parcel_id),
    year = as.integer(year),
    county_safe = as.character(county_safe),
    ACRES = as.numeric(ACRES)
  )])
  
  state = unique(future_base[, .(parcel_id)])
  state = merge(state, cover_info$start_state, by = "parcel_id", all.x = TRUE)
  state[is.na(current_cover), current_cover := 0L]
  
  output = list()
  scenario_offset = if (scenario_name == "NBS_Targets") 100000L else 0L
  county_names = sort(unique(future_base$county_safe))
  
  for (yy in seq.int(start_year + 1L, end_year)) {
    d = copy(future_base[year == yy])
    d = merge(d, state, by = "parcel_id", all.x = TRUE)
    d[is.na(current_cover), current_cover := 0L]
    
    county_output = list()
    
    for (cty in unique(d$county_safe)) {
      g = copy(d[county_safe == cty])
      if (!nrow(g)) next
      
      A = cover_info$matrices[[cty]]
      if (is.null(A)) A = cover_info$matrices[["__STATEWIDE__"]]
      
      target = targets[county_safe == cty & year == yy]
      if (!nrow(target)) stop("Missing ", scenario_name, " cover target for ", cty, " in ", yy)
      
      target_share = target$cover_share[1]
      target_acres = target_share * sum(g$ACRES, na.rm = TRUE)
      
      g[, p_cover := A[cbind(as.character(current_cover), rep("1", .N))]]
      g[!is.finite(p_cover), p_cover := 0]
      
      if (target_acres <= 0) {
        g[, next_cover := 0L]
      } else if (target_acres >= sum(g$ACRES, na.rm = TRUE)) {
        g[, next_cover := 1L]
      } else {
        set.seed(seed + scenario_offset + yy * 100L + match(cty, county_names))
        g[, rand := runif(.N)]
        g[, rank_key := -log(pmax(rand, 1e-12)) / pmax(p_cover, 1e-6)]
        setorder(g, rank_key)
        
        g[, acres_mid := cumsum(ACRES) - ACRES / 2]
        g[, next_cover := as.integer(acres_mid < target_acres)]
      }
      
      county_output[[cty]] = g[, .(
        parcel_id,
        year = yy,
        COVER = as.numeric(next_cover)
      )]
    }
    
    yr = rbindlist(county_output, fill = TRUE)
    output[[as.character(yy)]] = yr
    state = yr[, .(parcel_id, current_cover = as.integer(COVER))]
  }
  
  rbindlist(output, fill = TRUE)
}

# Build representative historical cover-crop cycles.
# Cover crops are observed COVER>0 rows in non-dominant seasons.
make_cover_template_lookup = function(x, by_cols, prefix) {
  group_cols = c(by_cols, "season", "CLASS", "SUBCLASS")
  
  counts = x[
    !is.na(season) & season != 2L & !is.na(CLASS),
    .N,
    by = group_cols
  ]
  
  if (!nrow(counts)) return(data.table())
  
  # Most commonly observed cover crop identity/season within each group.
  setorderv(
    counts,
    c(by_cols, "N", "season", "CLASS", "SUBCLASS"),
    c(rep(1L, length(by_cols)), -1L, 1L, 1L, 1L),
    na.last = TRUE
  )
  
  winners = counts[, .SD[1L], by = by_cols]
  winners[, N := NULL]
  
  # Historical attributes for that cover crop identity.
  attrs = x[
    !is.na(season) & season != 2L & !is.na(CLASS),
    .(
      SPECOND = mode_with_missing(SPECOND),
      MULTIUSE = as.numeric(mode_value(MULTIUSE)),
      ADOY = mean_wrapped_doy(ADOY)
    ),
    by = group_cols
  ]
  
  out = merge(
    winners,
    attrs,
    by = group_cols,
    all.x = TRUE
  )
  
  value_cols = c(
    "season", "CLASS", "SUBCLASS",
    "SPECOND", "MULTIUSE", "ADOY"
  )
  
  setnames(
    out,
    value_cols,
    paste0(prefix, value_cols)
  )
  
  out[]
}


build_cover_cycle_lookups = function(landiq_identity_hist) {
  
  # Dominant crop in each historical parcel-year.
  dominant = landiq_identity_hist[
    season == 2L & !is.na(CLASS),
    .(dominant_CLASS = as.character(mode_value(CLASS))),
    by = .(parcel_id, year)
  ]
  
  # Only rows already identified as cover crops by the inventory workflow.
  cover_hist = copy(
    landiq_identity_hist[
      COVER > 0 &
        season != 2L &
        !is.na(CLASS)
    ]
  )
  
  if (!nrow(cover_hist)) {
    stop("No historical cover-crop rows found.")
  }
  
  cover_hist = merge(
    cover_hist,
    dominant,
    by = c("parcel_id", "year"),
    all.x = TRUE
  )
  
  cover_hist[, `:=`(
    county_safe = safe_county_name(COUNTY),
    global_key = 1L
  )]
  
  list(
    county_dominant = make_cover_template_lookup(
      cover_hist[
        !is.na(county_safe) &
          !is.na(dominant_CLASS)
      ],
      c("county_safe", "dominant_CLASS"),
      "cd_"
    ),
    
    dominant = make_cover_template_lookup(
      cover_hist[!is.na(dominant_CLASS)],
      "dominant_CLASS",
      "d_"
    ),
    
    county = make_cover_template_lookup(
      cover_hist[!is.na(county_safe)],
      "county_safe",
      "c_"
    ),
    
    global = make_cover_template_lookup(
      cover_hist,
      "global_key",
      "g_"
    )
  )
}


attach_projected_cover = function(
    projected_crop_identity,
    cover_projection,
    cover_cycle_lookups) {
  
  dt = copy(projected_crop_identity)
  dt[, parcel_id := as.character(parcel_id)]
  
  cv = copy(cover_projection)
  cv[, `:=`(
    parcel_id = as.character(parcel_id),
    year = as.integer(year),
    projected_COVER = as.integer(COVER)
  )]
  cv[, COVER := NULL]
  
  dt = merge(
    dt,
    cv,
    by = c("parcel_id", "year"),
    all.x = TRUE
  )
  
  if (anyNA(dt$projected_COVER)) {
    stop("Some projected crop rows are missing cover predictions.")
  }
  
  # Dominant crop remains the ordinary season-2 crop.
  # COVER is row-specific, so the dominant row itself is not a cover crop.
  main = copy(dt)
  main[, COVER := 0]
  main[, parcel_id := bit64::as.integer64(parcel_id)]
  main = main[, ..identity_cols]
  
  # Only COVER-assigned parcel-years get an additional crop cycle.
  cover_rows = copy(dt[projected_COVER == 1L])
  
  if (nrow(cover_rows)) {
    
    cover_rows[, `:=`(
      county_safe = safe_county_name(COUNTY),
      dominant_CLASS = as.character(CLASS),
      global_key = 1L
    )]
    
    cover_rows = merge(
      cover_rows,
      cover_cycle_lookups$county_dominant,
      by = c("county_safe", "dominant_CLASS"),
      all.x = TRUE
    )
    
    cover_rows = merge(
      cover_rows,
      cover_cycle_lookups$dominant,
      by = "dominant_CLASS",
      all.x = TRUE
    )
    
    cover_rows = merge(
      cover_rows,
      cover_cycle_lookups$county,
      by = "county_safe",
      all.x = TRUE
    )
    
    cover_rows = merge(
      cover_rows,
      cover_cycle_lookups$global,
      by = "global_key",
      all.x = TRUE
    )
    
    # Use one complete historical template tier rather than
    # mixing CLASS/SUBCLASS/season across fallback levels.
    cover_rows[, template_source := fcase(
      !is.na(cd_CLASS), "county_dominant",
      !is.na(d_CLASS),  "dominant",
      !is.na(c_CLASS),  "county",
      !is.na(g_CLASS),  "global",
      default = NA_character_
    )]
    
    if (anyNA(cover_rows$template_source)) {
      stop("Some projected cover crops have no historical cover template.")
    }
    
    cover_rows[, `:=`(
      season = as.integer(fcase(
        template_source == "county_dominant", cd_season,
        template_source == "dominant",        d_season,
        template_source == "county",          c_season,
        template_source == "global",          g_season
      )),
      
      CLASS = fcase(
        template_source == "county_dominant", cd_CLASS,
        template_source == "dominant",        d_CLASS,
        template_source == "county",          c_CLASS,
        template_source == "global",          g_CLASS
      ),
      
      SUBCLASS = fcase(
        template_source == "county_dominant", cd_SUBCLASS,
        template_source == "dominant",        d_SUBCLASS,
        template_source == "county",          c_SUBCLASS,
        template_source == "global",          g_SUBCLASS
      ),
      
      SPECOND = fcase(
        template_source == "county_dominant", cd_SPECOND,
        template_source == "dominant",        d_SPECOND,
        template_source == "county",          c_SPECOND,
        template_source == "global",          g_SPECOND
      ),
      
      MULTIUSE = as.numeric(fcase(
        template_source == "county_dominant", cd_MULTIUSE,
        template_source == "dominant",        d_MULTIUSE,
        template_source == "county",          c_MULTIUSE,
        template_source == "global",          g_MULTIUSE
      )),
      
      ADOY = as.numeric(fcase(
        template_source == "county_dominant", cd_ADOY,
        template_source == "dominant",        d_ADOY,
        template_source == "county",          c_ADOY,
        template_source == "global",          g_ADOY
      )),
      
      COVER = 1
    )]
    
    cover_rows[, SUBCLASS := normalize_subclass(SUBCLASS)]
    cover_rows[, parcel_id := bit64::as.integer64(parcel_id)]
    cover_rows = cover_rows[, ..identity_cols]
    
    if (cover_rows[season == 2L, .N]) {
      stop("Projected cover crop was assigned to dominant season 2.")
    }
    
  } else {
    cover_rows = main[0]
  }
  
  out = rbindlist(
    list(main, cover_rows),
    use.names = TRUE,
    fill = FALSE
  )
  
  dup = out[, .N, by = .(parcel_id, year, season)][N > 1L]
  if (nrow(dup)) {
    stop("Duplicate parcel-year-season rows after cover crop expansion.")
  }
  
  setorder(out, parcel_id, year, season)
  setcolorder(out, identity_cols)
  
  out[]
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
    
    dt[, next_CLASS := {
      from_state = current_CLASS[1]
      p = as.numeric(tmat[from_state, states])
      p[!is.finite(p)] = 0
      if (sum(p) <= 0) rep(from_state, .N)
      else sample(states, size = .N, replace = TRUE, prob = p / sum(p))
    }, by = current_CLASS]
    
    dt[, prob_crop_class := tmat[cbind(current_CLASS, next_CLASS)]]
    
    expected_vec = as.numeric(expected_vec %*% tmat)
    names(expected_vec) = states
    
    realized_dt = dt[, .(realized_acres = sum(ACRES, na.rm = TRUE)), by = next_CLASS]
    realized_vec = setNames(rep(0, length(states)), states)
    realized_vec[realized_dt$next_CLASS] = realized_dt$realized_acres
    
    qc_list[[k]] = data.table(
      year = yy,
      CLASS = states,
      expected_acres = as.numeric(expected_vec[states]),
      realized_acres = as.numeric(realized_vec[states])
    )
    
    qc_list[[k]][, `:=`(
      difference_acres = realized_acres - expected_acres,
      abs_difference_acres = abs(realized_acres - expected_acres)
    )]
    
    pred_list[[k]] = dt[, .(parcel_id, year = yy, CLASS = next_CLASS, prob_crop_class)]
    
    dt[, current_CLASS := next_CLASS]
    dt[, next_CLASS := NULL]
  }
  
  list(
    predictions = rbindlist(pred_list, use.names = TRUE, fill = TRUE),
    qc = rbindlist(qc_list, use.names = TRUE, fill = TRUE)
  )
}

predict_grouped_markov = function(year_states, transition_mats, group_col, start_year, end_year, state_col = "crop_class") {
  dt = copy(year_states)
  
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
    
    ans = predict_county_sequential(
      start_info = start_info,
      tmat = transition_mats[[g]],
      start_year = start_year,
      end_year = end_year,
      state_col = state_col
    )
    
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
if (!file.exists(config$year_states_path)) {stop("crop_year_states_cleaned.csv not found: ", config$year_states_path)
}

crop_data = fread(config$year_states_path)
if ("V1" %in% names(crop_data)) crop_data[, V1 := NULL]

required_crop_cols = c("parcel_id", "year", "county", "county_geoid", "state", "ACRES")
missing_crop_cols = setdiff(required_crop_cols, names(crop_data))

if (length(missing_crop_cols)) {stop("crop_year_states_cleaned.csv is missing: ", paste(missing_crop_cols, collapse = ", "))
}

crop_data[, `:=`(
  parcel_id = as.character(parcel_id), year = as.integer(year), county = as.character(county), county_geoid = as.character(county_geoid), 
  crop_class = trimws(as.character(state)), ACRES = as.numeric(ACRES),  county_safe = safe_county_name(county)
)]

message("Crop data loaded, moving on to crop and subclass lookups")

# ---- crop lookup / historical subclass ----
lookup = fread(config$lookup_path)
lookup[, `:=`(
  CLASS = as.character(CLASS), SUBCLASS = normalize_subclass(SUBCLASS)
)]

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

message('Subclasses loaded, now loading LandIQ crop history')

# ---- LandIQ crop identity history ----
if (!file.exists(config$landiq_identity_path)) {
  stop("LandIQ crop identity parquet not found: ", config$landiq_identity_path)
}

landiq_ds = arrow::read_parquet(config$landiq_identity_path, as_data_frame = FALSE)
landiq_names = names(landiq_ds)

cover_source = if ("COVER" %in% landiq_names) {
  "COVER"
} else if ("PCNT" %in% landiq_names) {
  "PCNT"
} else {
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
  parcel_id = bit64::as.integer64(as.character(parcel_id)), COUNTY = as.character(COUNTY), year = as.integer(year),
  season = as.integer(season), CLASS = trimws(as.character(CLASS)), SUBCLASS = normalize_subclass(SUBCLASS),
  SPECOND = as.character(SPECOND), MULTIUSE = as.numeric(MULTIUSE), ADOY = as.numeric(ADOY), COVER = as.numeric(COVER)
)]

landiq_identity_hist[CLASS %chin% c("", "NA", "NaN", "***"), CLASS := NA_character_]
landiq_identity_hist[SPECOND %chin% c("", "NA", "NaN", "***"), SPECOND := NA_character_]

identity_cols = c("parcel_id", "COUNTY", "year", "season", "CLASS", "SUBCLASS", "SPECOND", "MULTIUSE", "ADOY", "COVER")

landiq_identity_hist = landiq_identity_hist[, ..identity_cols]

parcel_county_fixed = landiq_identity_hist[
  !is.na(COUNTY),
  .SD[which.max(year)],
  by = parcel_id
][, .(parcel_id, COUNTY)]

message('LandIQ loaded and cleaned. Now building cover crop matrices')

# ---- cover history / matrices / scenario targets ----
cover_info = build_cover_history(landiq_identity_hist)

message("Built historical cover matrices for ", length(cover_info$matrices) - 1L, " counties.")

bau_cover_targets = read_cover_targets(cover_scenario_files[["BAU_Targets"]])
nbs_cover_targets = read_cover_targets(cover_scenario_files[["NBS_Targets"]])

cover_cycle_lookups = build_cover_cycle_lookups(landiq_identity_hist)

# ---- historical dominant-season identity lookups ----
identity_hist_dominant = landiq_identity_hist[season == 2L]

identity_hist_dominant = merge(identity_hist_dominant,
  unique(lookup_subclass[, .(CLASS, SUBCLASS, PFT)], by = c("CLASS", "SUBCLASS")), by = c("CLASS", "SUBCLASS"),
  all.x = TRUE)

identity_hist_dominant[, global_key := 1L]

make_identity_lookup = function(dt, keys, prefix) {
  keep = complete.cases(dt[, ..keys])
  
  out = dt[keep, .(
    SPECOND = mode_with_missing(SPECOND), MULTIUSE = as.numeric(mode_value(MULTIUSE)), ADOY = mean_wrapped_doy(ADOY),
    COVER = safe_mean(COVER)), by = keys]
  
  setnames(out, c("SPECOND", "MULTIUSE", "ADOY", "COVER"), paste0(prefix, c("SPECOND", "MULTIUSE", "ADOY", "COVER")))
  
  out
}

identity_lookup_county_code = make_identity_lookup(identity_hist_dominant, c("COUNTY", "CLASS", "SUBCLASS"), "county_code_")
identity_lookup_code = make_identity_lookup(identity_hist_dominant, c("CLASS", "SUBCLASS"), "code_")
identity_lookup_county_class = make_identity_lookup(identity_hist_dominant, c("COUNTY", "CLASS"),"county_class_")
identity_lookup_class = make_identity_lookup(identity_hist_dominant, "CLASS", "class_")
identity_lookup_pft    = make_identity_lookup(identity_hist_dominant, "PFT", "pft_")
identity_lookup_global = make_identity_lookup(identity_hist_dominant, "global_key", "global_")

attach_identity_attributes = function(future_landiq) {
  dt = copy(future_landiq)
  dt[, row_id_identity := .I]
  dt[, parcel_id := bit64::as.integer64(as.character(parcel_id))]
  dt[, SUBCLASS := normalize_subclass(SUBCLASS)]
  
  if (!"PFT" %in% names(dt)) stop("PFT column missing before attribute cascade.")
  dt[, global_key := 1L]
  
  dt = merge(dt, parcel_county_fixed, by = "parcel_id", all.x = TRUE)
  dt = merge(dt, identity_lookup_county_code,  by = c("COUNTY", "CLASS", "SUBCLASS"), all.x = TRUE)
  dt = merge(dt, identity_lookup_county_class, by = c("COUNTY", "CLASS"),             all.x = TRUE)
  dt = merge(dt, identity_lookup_code,         by = c("CLASS", "SUBCLASS"),           all.x = TRUE)
  dt = merge(dt, identity_lookup_class,        by = "CLASS",                          all.x = TRUE)
  dt = merge(dt, identity_lookup_pft,          by = "PFT",                            all.x = TRUE)
  dt = merge(dt, identity_lookup_global,       by = "global_key",                     all.x = TRUE)
  
  dt[, SPECOND := fcoalesce(county_code_SPECOND, county_class_SPECOND, code_SPECOND,
                            class_SPECOND, pft_SPECOND, global_SPECOND)]
  dt[, MULTIUSE := fcoalesce(county_code_MULTIUSE, county_class_MULTIUSE, code_MULTIUSE,
                             class_MULTIUSE, pft_MULTIUSE, global_MULTIUSE)]
  dt[, ADOY := fcoalesce(county_code_ADOY, county_class_ADOY, code_ADOY,
                         class_ADOY, pft_ADOY, global_ADOY)]
  
  # Filled later by scenario-specific cover projection.
  dt[, COVER := NA_real_]
  
  dt[, `:=`(
    year = as.integer(year), season = 2L, MULTIUSE = as.numeric(round(MULTIUSE)), ADOY = as.numeric(round(ADOY)), COVER = as.numeric(COVER)
  )]
  
  if (anyNA(dt$COUNTY)) {stop("Projected crop identity contains parcels without fixed COUNTY values.")
  }
  
  setorder(dt, row_id_identity)
  dt[, ..identity_cols]
}

assign_predicted_subclass = function(future_landiq, subclass_obs, lookup_subclass,
                                     crop_col = "CLASS", group_col = "county_safe",
                                     start_year = 2023L) {
  dt = copy(future_landiq)
  obs = copy(subclass_obs)
  dt[, orig_order := .I]
  
  obs[, `:=`(
    parcel_id = as.character(parcel_id), year = as.integer(year), county_safe = as.character(county_safe),
    CLASS = as.character(CLASS), SUBCLASS = normalize_subclass(SUBCLASS)
  )]
  
  if (!"season" %in% names(obs)) obs[, season := 0L]
  obs[, season := as.integer(season)]
  obs[is.na(season), season := 0L]
  obs = obs[year <= start_year & season == 2L]
  
  dt[, `:=`(
    parcel_id = as.character(parcel_id), CLASS = as.character(get(crop_col))
  )]
  
  dt[, (group_col) := as.character(get(group_col))]
  
  old_lookup_cols = intersect(c("SUBCLASS", "CLASS_desc", "SUBCLASS_desc", "PFT"), names(dt))
  
  if (length(old_lookup_cols)) dt[, (old_lookup_cols) := NULL]
  
  global_probs = obs[
    !is.na(CLASS) & !is.na(SUBCLASS),
    .N, by = .(CLASS, SUBCLASS)
  ]
  
  if (nrow(global_probs)) global_probs[, prob := N / sum(N), by = CLASS]
  
  group_probs = obs[
    !is.na(CLASS) & !is.na(SUBCLASS),
    .N, by = .(county_safe, CLASS, SUBCLASS)
  ]
  
  if (nrow(group_probs)) group_probs[, prob := N / sum(N), by = .(county_safe, CLASS)]
  
  lookup_probs = unique(lookup_subclass[
      !is.na(CLASS) & !is.na(SUBCLASS),
      .(CLASS, SUBCLASS)
    ]
  )
  
  if (nrow(lookup_probs)) lookup_probs[, prob := 1 / .N, by = CLASS]
  
  last_obs_source = obs[!is.na(CLASS) & !is.na(SUBCLASS)]
  
  last_obs = last_obs_source[
    order(year, season),
    .SD[.N], by = .(parcel_id, county_safe)
  ][, .(
    parcel_id, county_safe, last_CLASS = CLASS, last_SUBCLASS = SUBCLASS
  )]
  
  dt = merge(dt, last_obs, by = c("parcel_id", "county_safe"), all.x = TRUE)
  setorder(dt, parcel_id, year)
  
  dt[, prev_CLASS := shift(CLASS), by = .(parcel_id, county_safe)]
  dt[is.na(prev_CLASS), prev_CLASS := last_CLASS]
  
  dt[, new_run := fifelse(is.na(CLASS), FALSE, is.na(prev_CLASS) | CLASS != prev_CLASS
  )]
  
  dt[, run_id := cumsum(new_run), by = .(parcel_id, county_safe)]
  dt[, SUBCLASS := NA_character_]
  
  dt[
    run_id == 0 &
      !is.na(CLASS) &
      !is.na(last_CLASS) &
      CLASS == last_CLASS &
      !is.na(last_SUBCLASS),
    SUBCLASS := last_SUBCLASS
  ]
  
  run_table = unique(
    dt[
      !is.na(CLASS) & is.na(SUBCLASS),
      .(parcel_id, county_safe, run_id, CLASS)
    ]
  )
  
  if (nrow(run_table)) {
    run_table[, drawn_SUBCLASS := NA_character_]
    draw_groups = unique(run_table[, .(county_safe, CLASS)])
    
    for (ii in seq_len(nrow(draw_groups))) {
      cty = draw_groups$county_safe[ii]
      cls = draw_groups$CLASS[ii]
      
      idx = which(
        run_table$county_safe == cty &
          run_table$CLASS == cls
      )
      
      choices = group_probs[county_safe == cty & CLASS == cls]
      if (!nrow(choices)) choices = global_probs[CLASS == cls]
      if (!nrow(choices)) choices = lookup_probs[CLASS == cls]
      if (!"prob" %in% names(choices)) choices[, prob := NA_real_]
      
      choices = choices[
        !is.na(SUBCLASS) &
          !is.na(prob) &
          is.finite(prob) &
          prob > 0
      ]
      
      if (nrow(choices)) {
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
  
  helper_cols = intersect(c("last_CLASS", "last_SUBCLASS", "prev_CLASS", "new_run", "run_id"), names(dt))
  
  dt[, (helper_cols) := NULL]
  
  dt = merge(dt, lookup_subclass, by = c("CLASS", "SUBCLASS"), all.x = TRUE)
  
  setorder(dt, orig_order)
  dt[, orig_order := NULL]
  
  dt[]
}

# ---- shared crop projection ----
message("Using shared optimized crop matrices from: ", crop_matrix_dir)
message("Writing crop projections to: ", prediction_dir)

crop_mats = load_crop_matrices(crop_matrix_dir)

missing_mats = setdiff(unique(crop_data$county_safe), names(crop_mats))

if (length(missing_mats)) {
  message("Counties in crop_year_states_cleaned without crop matrices: ", length(missing_mats))
  message("Missing counties: ", paste(sort(missing_mats), collapse = ", "))
}

crop_projection = predict_grouped_markov(year_states = crop_data, transition_mats = crop_mats, group_col = "county_safe",
  start_year = start_year, end_year = end_year, state_col = "crop_class")

future_crop = crop_projection$predictions
crop_projection_qc = crop_projection$qc

if (!nrow(future_crop)) stop("No future crop predictions were generated.")

arrow::write_parquet(crop_projection_qc, file.path(prediction_dir, "crop_projection_qc.parquet"), compression = "zstd")

# ---- stable parcel metadata ----
parcel_meta = crop_data[
  year <= start_year,
  .SD[which.max(year)],
  by = parcel_id
][, .(
  parcel_id, county, county_geoid, county_safe, ACRES
)]

future_landiq = merge(future_crop, parcel_meta, by = c("parcel_id", "county_safe"), all.x = TRUE)

future_landiq = future_landiq[
  year >= start_year + 1L &
    year <= end_year
]

future_landiq = assign_predicted_subclass(future_landiq = future_landiq, subclass_obs = with_subclass,
  lookup_subclass = lookup_subclass, crop_col = "CLASS", group_col = "county_safe", start_year = start_year)

future_landiq[, SUBCLASS := normalize_subclass(SUBCLASS)]

# Shared CLASS/SUBCLASS/SPECOND/MULTIUSE/ADOY calculation.
projected_crop_identity = attach_identity_attributes(future_landiq)

# ---- scenario-specific cover projection ----
message("Projecting BAU cover crops...")

cover_bau = predict_cover_scenario(future_landiq = future_landiq, cover_info = cover_info, targets = bau_cover_targets, scenario_name = "BAU_Targets",
                                   seed = config$seed)

message("Projecting NBS cover crops...")

cover_nbs = predict_cover_scenario(future_landiq = future_landiq, cover_info = cover_info, targets = nbs_cover_targets, scenario_name = "NBS_Targets",
  seed = config$seed)

projected_crop_bau = attach_projected_cover( projected_crop_identity, cover_bau, cover_cycle_lookups)

projected_crop_nbs = attach_projected_cover(projected_crop_identity, cover_nbs, cover_cycle_lookups)

scenario_crop_predictions = list( BAU_Targets = projected_crop_bau, NBS_Targets = projected_crop_nbs)

# ---- write annual scenario-specific crop identity parquets ----
for (scen in names(scenario_crop_predictions)) {
  scenario_dir = file.path(prediction_dir, scen)
  dir.create(scenario_dir, recursive = TRUE, showWarnings = FALSE)
  
  scenario_data = scenario_crop_predictions[[scen]]
  
  for (yy in seq.int(start_year + 1L, end_year)) {
    crop_year = copy(scenario_data[year == yy])
    
    crop_year[, `:=`(
      parcel_id = bit64::as.integer64(as.character(parcel_id)), year = as.integer(year),
      season = as.integer(season), COVER = as.numeric(COVER)
    )]
    
    setorder(crop_year, parcel_id, season)
    
    if (!identical(names(crop_year), identity_cols)) {
      stop(scen, " crop identity schema error for year ", yy, ".")
    }
    
    out_path = file.path(scenario_dir, paste0("crop_identity_statewide_", yy, ".parquet"))
    
    arrow::write_parquet(crop_year, out_path, compression = "zstd")
    
    message("Wrote: ", out_path)
  }
}

# ---- scenario-specific historical + projected combined files ----
for (scen in names(scenario_crop_predictions)) {
  scenario_dir = file.path(prediction_dir, scen)
  projected = scenario_crop_predictions[[scen]]
  
  crop_identity_all_years = rbindlist(list(landiq_identity_hist, projected), use.names = TRUE, fill = FALSE)
  
  crop_identity_all_years[, parcel_id := bit64::as.integer64(parcel_id)]
  setorder(crop_identity_all_years, parcel_id, year, season)
  
  if (!identical(names(crop_identity_all_years), identity_cols)) {
    stop(scen, " all-years crop identity schema error.")
  }
  
  all_years_path = file.path(scenario_dir, "crops_all_years.parq")
  
  arrow::write_parquet(crop_identity_all_years, all_years_path, compression = "zstd")
  
  message("Wrote combined ", scen, " crop identity parquet: ", all_years_path)
}
