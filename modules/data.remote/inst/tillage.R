## V1 tillage projection
## all_data.csv -> historical county x crop tillage baseline
## BAU/NBS differ through future no/low/high till shares
## old v4.1 events -> historical county + PFT + till-state timing/NDTI
## old parcel IDs are NEVER joined to future v4.1.2 parcels

pacman::p_load(data.table, arrow, bit64)

# ---- setup ----
# REQUIRED: replace with path where outputs should be saved
work_root = "/path/to/your/folder"

# Shared project data; most users should not change this.
ccmmf_root = "/projectnb/dietzelab/ccmmf"

config = list(all_data_path = file.path(work_root, "all_data.csv"),
              crop_year_path = file.path(work_root, "crop_year_states_cleaned.csv"),
              crop_prediction_dir = file.path(work_root, "crop_predictions"),
              lookup_path = file.path(ccmmf_root, "management", "LandIQ_cropCode_lookup_table.csv"),
              tillage_event_dir = file.path(ccmmf_root, "management", "event_files"),
              scenario_dir = file.path(work_root, "MAGiC_scenarios_FINAL"),
              output_root = file.path(work_root, "tillage_projections"),
              historical_years = 2018:2023, prediction_years = 2024:2045,
              start_year = 2023L, end_year = 2045L,
              scenarios = c("BAU_Targets", "NBS_Targets"),
              no_till_threshold = 30, low_till_threshold = 70, seed = 1L)

scenario_files = c(BAU_Targets = file.path(config$scenario_dir, "BAU_Targets.csv"),
                   NBS_Targets = file.path(config$scenario_dir, "NBS_Targets.csv"))

# ---- helpers ----
safe_county_name = function(x) gsub("[^A-Za-z0-9_]+", "_", trimws(as.character(x)))

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

normalize_crop_key = function(x) {
  x = tolower(trimws(as.character(x))); x = gsub("&", "and", x)
  x = gsub("[[:punct:]]+", " ", x); trimws(gsub("\\s+", " ", x))
}

normalize_till_state = function(x) {
  x = tolower(trimws(as.character(x))); x = gsub("[ -]+", "_", x)
  x[x %chin% c("notill", "no_till", "no_till_acres")] = "no_till"
  x[x %chin% c("low_till", "reduced_till", "reduced")] = "low_till"
  x[x %chin% c("high_till", "tilled", "till", "tilled_acres")] = "high_till"
  x
}

safe_mean = function(x) {
  x = as.numeric(x); x = x[is.finite(x)]
  if (!length(x)) NA_real_ else mean(x)
}

mode_character = function(x) {
  x = as.character(x); x = x[!is.na(x) & nzchar(x)]
  if (!length(x)) return(NA_character_)
  names(sort(table(x), decreasing = TRUE))[1]
}

assert_cols = function(dt, req, label) {
  miss = setdiff(req, names(dt))
  if (length(miss)) stop(label, " missing: ", paste(miss, collapse = ", "))
}

# Use a fixed non-leap reference year so historical month/day timing is
# consistent between leap and non-leap years.
date_to_doy = function(x) {
  x = as.Date(x)
  out = rep(NA_integer_, length(x))
  ok = !is.na(x)
  md = format(x[ok], "%m-%d")
  md[md == "02-29"] = "02-28"
  out[ok] = as.integer(format(as.Date(paste0("2001-", md)), "%j"))
  out
}

fix_wrap_doy = function(x, low = 45, high = 320) {
  x = as.numeric(x); x = x[is.finite(x)]
  if (!length(x)) return(x)
  if (quantile(x, .05) <= low && quantile(x, .95) >= high) x[x <= low] = x[x <= low] + 365
  x
}

mean_wrapped_doy = function(x) {
  x = fix_wrap_doy(x)
  if (!length(x)) NA_real_ else ((round(mean(x)) - 1) %% 365) + 1
}

# Convert canonical 365-day DOY back to the same month/day in the target year.
doy_to_date = function(year, doy) {
  out = rep(as.Date(NA), length(year))
  ok = !is.na(year) & !is.na(doy)
  ref = as.Date("2001-01-01") + as.integer(round(doy[ok])) - 1L
  out[ok] = as.Date(paste0(as.integer(year[ok]), "-", format(ref, "%m-%d")))
  as.IDate(out)
}

# ---- MAGiC crop -> LandIQ CLASS mapping ----
map_single = data.table(
  Crop = c("All Other Berries", "Strawberries (Fresh Market)", "All Other Fruit Crops",
           "All Other Nut Crops", "Almonds", "Pome Fruit", "Stone Fruit", "Citrus",
           "Grapes Dried, Raisins", "Grapes, Table", "Grapes, Wine", "Fallow"),
  crop_state = c("T","T","D","D","D","D","D","C","V","V","V","X"))

map_single[, crop_key := normalize_crop_key(Crop)]

map_split = data.table(
  Crop = c("All Other Field Crops (Incl. Pasture /Rangeland)", "Annual Cropland"),
  split_group = c("field_pasture", "annual_cropland"))

map_split[, crop_key := normalize_crop_key(Crop)]

get_split_states = function(g, states) {
  if (g == "field_pasture") return(intersect(c("F","P"), states))
  if (g == "annual_cropland") return(intersect(c("F","G","T","R"), states))
  character()
}

get_split_weights = function(crop_data, cty, states, start_year) {
  d = crop_data[county_safe == cty & year <= start_year]
  latest = d[!is.na(crop_class), .SD[which.max(year)], by = parcel_id]
  latest = latest[crop_class %chin% states]
  
  if (!nrow(latest)) return(data.table(crop_state = states, split_weight = 1 / length(states)))
  
  x = latest[, .(acres = sum(ACRES, na.rm = TRUE)), by = crop_class]
  x = merge(data.table(crop_state = states), x, by.x = "crop_state", by.y = "crop_class", all.x = TRUE)
  x[is.na(acres), acres := 0]
  x[, split_weight := if (sum(acres) > 0) acres / sum(acres) else 1 / .N]
  x[, .(crop_state, split_weight)]
}

expand_targets = function(s, crop_data, cty, target_year, start_year, crop_states) {
  d = copy(s[county_safe == cty & Year == target_year])
  if (!nrow(d)) return(data.table())
  
  d[, `:=`(row_id = .I, crop_key = normalize_crop_key(Crop))]
  single = merge(d, map_single[, .(crop_key, crop_state)], by = "crop_key")
  if (nrow(single)) single[, split_weight := 1]
  
  sr = merge(d, map_split, by = "crop_key")
  parts = list()
  
  for (g in unique(sr$split_group)) {
    rows = sr[split_group == g]
    states = get_split_states(g, crop_states)
    if (!length(states)) next
    w = get_split_weights(crop_data, cty, states, start_year)
    z = CJ(row_id = rows$row_id, crop_state = states)
    z = merge(z, rows, by = "row_id", allow.cartesian = TRUE)
    z = merge(z, w, by = "crop_state")
    parts[[g]] = z
  }
  
  split = if (length(parts)) rbindlist(parts, fill = TRUE) else data.table()
  out = rbindlist(list(single, split), fill = TRUE)
  out = out[crop_state %chin% crop_states]
  
  out[, `:=`(
    NoTill = `No till acres (CPS 329)` * split_weight,
    LowTill = `Reduced till acres (CPS 345)` * split_weight,
    HighTill = `Tilled acres` * split_weight)]
  out
}

make_targets = function(x) {
  if (!nrow(x)) return(data.table())
  
  no = x[, .(target_acres = sum(NoTill, na.rm = TRUE)), by = .(county_safe, crop_state)][, till_state := "no_till"]
  low = x[, .(target_acres = sum(LowTill, na.rm = TRUE)), by = .(county_safe, crop_state)][, till_state := "low_till"]
  high = x[, .(target_acres = sum(HighTill, na.rm = TRUE)), by = .(county_safe, crop_state)][, till_state := "high_till"]
  
  rbindlist(list(no, low, high))
}

build_annual_targets = function(baseline, target, start_year, end_year) {
  target = copy(target)
  target[, target_total := sum(target_acres, na.rm = TRUE), by = .(county_safe, crop_state)]
  
  zero = unique(target[!is.finite(target_total) | target_total <= 0, .(county_safe, crop_state)])
  if (nrow(zero)) message(nrow(zero), " county-crop groups have zero total tillage target.")
  
  target = target[is.finite(target_total) & target_total > 0]
  target[, target_share := target_acres / target_total]
  
  combos = unique(target[, .(county_safe, crop_state)])
  annual = combos[, CJ(till_state = c("no_till","low_till","high_till"),
                       year = seq(start_year + 1L, end_year)), by = .(county_safe, crop_state)]
  
  annual = merge(annual, baseline[, .(county_safe, crop_state, till_state, baseline_share)],
                 by = c("county_safe","crop_state","till_state"), all.x = TRUE)
  annual = merge(annual, target[, .(county_safe, crop_state, till_state, target_share)],
                 by = c("county_safe","crop_state","till_state"), all.x = TRUE)
  
  annual[is.na(baseline_share), baseline_share := 0]
  annual[is.na(target_share), target_share := 0]
  annual[, ramp := (year - start_year) / (end_year - start_year)]
  annual[, till_share := (1 - ramp) * baseline_share + ramp * target_share]
  annual[, share_sum := sum(till_share, na.rm = TRUE), by = .(county_safe, crop_state, year)]
  annual[is.finite(share_sum) & share_sum > 0, till_share := till_share / share_sum]
  annual[!is.finite(share_sum) | share_sum <= 0, till_share := NA_real_]
  annual[, c("target_total","ramp","share_sum") := NULL]
  annual
}

assign_tillage = function(future, targets) {
  targets = copy(targets); setkey(targets, county_safe, year, crop_state)
  future = copy(future); future[, row_id := .I]
  
  out = future[, {
    t = targets[.(county_safe[1], year[1], CLASS[1]), nomatch = 0]
    temp = copy(.SD)
    valid = nrow(t) > 0 && any(is.finite(t$till_share))
    
    if (!valid) {
      temp[, `:=`(till_state = NA_character_, prob_till_state = NA_real_)]
    } else {
      t[!is.finite(till_share), till_share := 0]
      share_sum = sum(t$till_share)
      
      if (!is.finite(share_sum) || share_sum <= 0) {
        temp[, `:=`(till_state = NA_character_, prob_till_state = NA_real_)]
      } else {
        t[, till_share := till_share / share_sum]
        setorder(t, till_state)
        total = sum(temp$ACRES, na.rm = TRUE)
        
        if (!is.finite(total) || total <= 0) {
          assigned = sample(t$till_state, nrow(temp), replace = TRUE, prob = t$till_share)
          p = setNames(t$till_share, t$till_state)
          temp[, `:=`(till_state = assigned, prob_till_state = p[assigned])]
        } else {
          temp[, rand := runif(.N)]
          setorder(temp, rand)
          t[, target_acres := till_share * total]
          t[, upper := cumsum(target_acres)]
          temp[, midpoint := cumsum(ACRES) - ACRES / 2]
          idx = pmax(1L, pmin(findInterval(temp$midpoint, t$upper) + 1L, nrow(t)))
          temp[, till_state := t$till_state[idx]]
          p = setNames(t$till_share, t$till_state)
          temp[, prob_till_state := p[till_state]]
          temp[, c("rand","midpoint") := NULL]
        }
      }
    }
    temp
  }, by = .(county_safe, year, CLASS)]
  
  setorder(out, row_id)
  out[, row_id := NULL]
  out
}

# ---- historical tillage baseline ----
all_data = fread(config$all_data_path, integer64 = "integer64")
if ("V1" %in% names(all_data)) all_data[, V1 := NULL]
if ("state" %in% names(all_data) && !"till_state" %in% names(all_data)) setnames(all_data, "state", "till_state")

assert_cols(all_data, c("parcel_id","year","county","crop_class","ACRES","till_state"), "all_data.csv")

all_data[, `:=`(
  parcel_id = as.character(parcel_id), year = as.integer(year),
  county = trimws(as.character(county)), crop_class = trimws(as.character(crop_class)),
  ACRES = as.numeric(ACRES), till_state = normalize_till_state(till_state))]

all_data[, county_safe := safe_county_name(county)]
all_data = all_data[till_state %chin% c("no_till","low_till","high_till")]

latest_till = all_data[
  year <= config$start_year & !is.na(crop_class) & !is.na(county_safe),
  .SD[which.max(year)], by = parcel_id]

baseline_till = latest_till[, .(baseline_acres = sum(ACRES, na.rm = TRUE)),
                            by = .(county_safe, crop_state = crop_class, till_state)]

baseline_till[, baseline_share := baseline_acres / sum(baseline_acres),
              by = .(county_safe, crop_state)]

# Historical metadata used ONLY with old tillage parcels.
old_meta = all_data[!is.na(parcel_id) & !is.na(year),
                    .(county_safe = mode_character(county_safe), crop_class = mode_character(crop_class)),
                    by = .(parcel_id, year)]

# ---- historical crop metadata for future v4.1.2 parcels ----
crop_data = fread(config$crop_year_path, integer64 = "integer64")
if ("V1" %in% names(crop_data)) crop_data[, V1 := NULL]

assert_cols(crop_data, c("parcel_id","year","state","county","county_geoid","ACRES"),
            "crop_year_states_cleaned.csv")

crop_data[, `:=`(
  parcel_id = bit64::as.integer64(as.character(parcel_id)), year = as.integer(year),
  crop_class = trimws(as.character(state)), county = trimws(as.character(county)),
  county_geoid = normalize_geoid(county_geoid), ACRES = as.numeric(ACRES))]

crop_data[, county_safe := safe_county_name(county)]
crop_states = sort(unique(na.omit(crop_data$crop_class)))

parcel_meta = crop_data[year <= config$start_year,
                        .SD[which.max(year)], by = parcel_id
][, .(parcel_id, county, county_safe, county_geoid, ACRES)]

if (anyNA(parcel_meta[, .(county_safe, county_geoid, ACRES)]))
  stop("Latest crop metadata contains missing values.")

# ---- PFT lookup ----
lookup = fread(config$lookup_path)
assert_cols(lookup, c("CLASS","SUBCLASS","PFT"), "LandIQ crop lookup")

lookup[, `:=`(
  CLASS = trimws(as.character(CLASS)),
  SUBCLASS = normalize_subclass(SUBCLASS),
  PFT = as.character(PFT))]

pft_code = lookup[!is.na(CLASS) & !is.na(SUBCLASS) & !is.na(PFT),
                  .(PFT = mode_character(PFT)), by = .(CLASS, SUBCLASS)]

pft_class = lookup[!is.na(CLASS) & !is.na(PFT),
                   .(fallback_PFT = mode_character(PFT)), by = CLASS]

# ---- old v4.1 tillage events: timing/NDTI characteristics only ----
all_till_files = list.files(config$tillage_event_dir,
                            pattern = "^tillage_statewide_[0-9]{4}\\.parquet$", full.names = TRUE)

file_years = as.integer(sub(".*tillage_statewide_([0-9]{4})\\.parquet$", "\\1", all_till_files))
keep = file_years %in% config$historical_years
tillage_files = all_till_files[keep]
tillage_years = file_years[keep]

if (!length(tillage_files)) stop("No usable old v4.1 tillage files found for 2018-2023.")
message("Using old v4.1 tillage years: ", paste(sort(tillage_years), collapse = ", "))

tillage_hist = rbindlist(Map(function(f, yy) {
  x = as.data.table(read_parquet(f))
  x[, source_year := as.integer(yy)]
  x
}, tillage_files, tillage_years), fill = TRUE)

if ("site_id" %in% names(tillage_hist) && !"parcel_id" %in% names(tillage_hist))
  setnames(tillage_hist, "site_id", "parcel_id")

if ("landiq_PFT" %in% names(tillage_hist) && !"PFT" %in% names(tillage_hist))
  setnames(tillage_hist, "landiq_PFT", "PFT")

if ("ndti_pct_drop" %in% names(tillage_hist) && !"ndti_pct_change" %in% names(tillage_hist))
  setnames(tillage_hist, "ndti_pct_drop", "ndti_pct_change")

if (!"date" %in% names(tillage_hist) && "min_date" %in% names(tillage_hist))
  setnames(tillage_hist, "min_date", "date")

assert_cols(tillage_hist, c("parcel_id","PFT","date","ndti_pct_change"), "Old v4.1 tillage")

tillage_hist[, `:=`(
  parcel_id = as.character(parcel_id), PFT = as.character(PFT),
  date = as.IDate(date), ndti_pct_change = as.numeric(ndti_pct_change))]

tillage_hist[!is.finite(ndti_pct_change), ndti_pct_change := NA_real_]

tillage_hist[, till_state := fcase(
  ndti_pct_change >= 0 & ndti_pct_change <= config$no_till_threshold, "no_till",
  ndti_pct_change > config$no_till_threshold & ndti_pct_change < config$low_till_threshold, "low_till",
  ndti_pct_change >= config$low_till_threshold, "high_till",
  default = NA_character_)]

tillage_hist[, tillage_doy := date_to_doy(date)]

# Join OLD events to OLD metadata only.
tillage_hist = merge(tillage_hist, old_meta,
                     by.x = c("parcel_id","source_year"), by.y = c("parcel_id","year"), all.x = TRUE)

event_hist = tillage_hist[
  till_state %chin% c("low_till","high_till") & !is.na(date) & is.finite(ndti_pct_change)]

if (!nrow(event_hist)) stop("No usable historical low/high-tillage events.")
message("Historical low/high tillage events: ", format(nrow(event_hist), big.mark = ","))

event_lookup_county_pft = event_hist[!is.na(county_safe) & !is.na(PFT),
                                     .(tillage_doy = mean_wrapped_doy(tillage_doy),
                                       ndti_pct_change = safe_mean(ndti_pct_change)),
                                     by = .(county_safe, PFT, till_state)]

event_lookup_pft = event_hist[!is.na(PFT),
                              .(fallback_pft_doy = mean_wrapped_doy(tillage_doy),
                                fallback_pft_ndti = safe_mean(ndti_pct_change)),
                              by = .(PFT, till_state)]

event_lookup_global = event_hist[,
                                 .(fallback_global_doy = mean_wrapped_doy(tillage_doy),
                                   fallback_global_ndti = safe_mean(ndti_pct_change)),
                                 by = till_state]

message("County/PFT/state lookup groups: ", nrow(event_lookup_county_pft),
        "; unique NDTI means: ", uniqueN(event_lookup_county_pft$ndti_pct_change))

# ---- future v4.1.2 crop projections ----
crop_files = file.path(config$crop_prediction_dir,
                       paste0("crop_identity_statewide_", config$prediction_years, ".parquet"))

missing = crop_files[!file.exists(crop_files)]
if (length(missing)) stop("Missing crop prediction files:\n", paste(missing, collapse = "\n"))

future = rbindlist(lapply(crop_files, function(f) {
  as.data.table(read_parquet(f,
                             col_select = c("parcel_id","COUNTY","year","season","CLASS","SUBCLASS")))
}), fill = TRUE)

future[, `:=`(
  parcel_id = bit64::as.integer64(as.character(parcel_id)),
  year = as.integer(year),
  CLASS = trimws(as.character(CLASS)),
  SUBCLASS = normalize_subclass(SUBCLASS))]

if ("season" %in% names(future) && future[!is.na(season) & season != 2L, .N])
  stop("Future crop predictions contain season other than 2.")

if (future[, .N, by = .(parcel_id, year)][N > 1L, .N])
  stop("Future crop predictions still contain duplicate parcel-year rows.")

future = merge(future, parcel_meta, by = "parcel_id", all.x = TRUE)

if (anyNA(future[, .(county_safe, county_geoid, ACRES)]))
  stop("Future crop rows missing fixed county/acreage metadata.")

future = merge(future, pft_code, by = c("CLASS","SUBCLASS"), all.x = TRUE)
future = merge(future, pft_class, by = "CLASS", all.x = TRUE)
future[is.na(PFT), PFT := fallback_PFT]
future[, fallback_PFT := NULL]

# ---- BAU/NBS tillage projections ----
for (scen in config$scenarios) {
  message("Processing ", scen)
  set.seed(config$seed)
  
  s = fread(scenario_files[[scen]])
  
  assert_cols(s, c("Crop","County","Year","Acres_Total","Tilled acres",
                   "Reduced till acres (CPS 345)","No till acres (CPS 329)"), scen)
  
  s[, `:=`(
    Crop = trimws(as.character(Crop)), County = trimws(as.character(County)),
    Year = as.integer(Year), Acres_Total = as.numeric(Acres_Total),
    `Tilled acres` = as.numeric(`Tilled acres`),
    `Reduced till acres (CPS 345)` = as.numeric(`Reduced till acres (CPS 345)`),
    `No till acres (CPS 329)` = as.numeric(`No till acres (CPS 329)`))]
  
  s[, county_safe := safe_county_name(County)]
  
  targets = rbindlist(lapply(
    intersect(unique(future$county_safe), unique(s$county_safe)),
    function(cty) {
      x = expand_targets(s, crop_data, cty, config$end_year,
                         config$start_year, crop_states)
      make_targets(x)
    }), fill = TRUE)
  
  if (!nrow(targets)) stop("No tillage targets created for ", scen)
  
  annual = build_annual_targets(baseline_till, targets,
                                config$start_year, config$end_year)
  
  pred = assign_tillage(future, annual)
  
  # Every active future crop row must receive a tillage state.
  missing_target = pred[is.na(till_state), .N]
  message(scen, " rows without tillage target: ",
          format(missing_target, big.mark = ","))
  
  if (missing_target)
    stop(scen, " has future crop rows without a tillage target.")
  
  # ---- realized vs target QC ----
  realized = pred[, .(realized_acres = sum(ACRES, na.rm = TRUE)),
                  by = .(county_safe, year, CLASS, till_state)]
  
  group_total = pred[, .(group_acres = sum(ACRES, na.rm = TRUE)),
                     by = .(county_safe, year, CLASS)]
  
  realized = merge(realized, group_total,
                   by = c("county_safe","year","CLASS"), all.x = TRUE)
  
  realized[, realized_share := realized_acres / group_acres]
  
  active_groups = unique(pred[, .(county_safe, year, CLASS)])
  
  qc_targets = annual[, .(
    county_safe, year, CLASS = crop_state,
    till_state, target_share = till_share)]
  
  qc_targets = merge(qc_targets, active_groups,
                     by = c("county_safe","year","CLASS"), all = FALSE)
  
  tillage_qc = merge(qc_targets, realized,
                     by = c("county_safe","year","CLASS","till_state"), all.x = TRUE)
  
  tillage_qc[is.na(realized_acres), realized_acres := 0]
  tillage_qc[is.na(group_acres), group_acres := 0]
  tillage_qc[is.na(realized_share), realized_share := 0]
  
  tillage_qc[, `:=`(
    target_acres = target_share * group_acres,
    difference_share = realized_share - target_share)]
  
  tillage_qc[, difference_acres := realized_acres - target_acres]
  
  setorder(tillage_qc, county_safe, year, CLASS, till_state)
  
  out_dir = file.path(config$output_root, scen)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  qc_path = file.path(out_dir, "tillage_projection_qc.parquet")
  write_parquet(tillage_qc, qc_path, compression = "zstd")
  
  message(scen, " maximum absolute tillage-share difference: ",
          round(max(abs(tillage_qc$difference_share), na.rm = TRUE), 4))
  message("Wrote: ", qc_path)
  
  # No-till is a condition, not an event.
  pred = pred[till_state %chin% c("low_till","high_till")]
  
  # Primary county + PFT lookup.
  pred = merge(pred, event_lookup_county_pft,
               by = c("county_safe","PFT","till_state"), all.x = TRUE)
  
  # Statewide PFT fallback.
  pred = merge(pred, event_lookup_pft,
               by = c("PFT","till_state"), all.x = TRUE)
  
  # Final statewide till-state fallback.
  pred = merge(pred, event_lookup_global,
               by = "till_state", all.x = TRUE)
  
  pred[, selected_doy := fcoalesce(
    tillage_doy, fallback_pft_doy, fallback_global_doy)]
  
  pred[, selected_ndti := fcoalesce(
    ndti_pct_change, fallback_pft_ndti, fallback_global_ndti)]
  
  pred[, date := doy_to_date(year, selected_doy)]
  
  bad = pred[is.na(date) | is.na(selected_ndti)]
  if (nrow(bad))
    stop(scen, " has ", format(nrow(bad), big.mark = ","),
         " projected tillage events without timing/NDTI values.")
  
  final = pred[, .(
    projection_year = year,
    event_type = "tillage",
    parcel_id = bit64::as.integer64(parcel_id),
    date,
    ndti_pct_drop = selected_ndti)]
  
  message(scen, " unique ndti_pct_drop values: ",
          uniqueN(final$ndti_pct_drop))
  
  message(scen, " ndti_pct_drop range: ",
          round(min(final$ndti_pct_drop), 2), " to ",
          round(max(final$ndti_pct_drop), 2))
  
  for (yy in config$prediction_years) {
    out = final[projection_year == yy,
                .(event_type, parcel_id, date, ndti_pct_drop)]
    
    setorder(out, parcel_id, date)
    
    if (out[!complete.cases(out), .N])
      stop(scen, " ", yy, " contains incomplete tillage events.")
    
    path = file.path(out_dir,
                     paste0("tillage_statewide_", yy, ".parquet"))
    
    write_parquet(out, path, compression = "zstd")
    
    message("Wrote: ", path, " (",
            format(nrow(out), big.mark = ","), " events; ",
            uniqueN(out$ndti_pct_drop), " unique NDTI values)")
  }
  
  message("Finished ", scen)
}

message("Tillage V1 complete: ", config$output_root)