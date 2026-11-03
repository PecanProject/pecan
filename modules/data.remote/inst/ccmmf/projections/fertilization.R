## Fertilization projections
## Synthetic N follows the dominant (season 2) crop only
## Compost follows CARB county x crop acreage targets
## Final output:
## event_type, parcel_id, date, nh4_n_kg_m2, no3_n_kg_m2, org_n_kg_m2, org_c_kg_m2

pacman::p_load(data.table, arrow, bit64, dplyr, PEcAn.utils)

# ---- setup ----
config = config::get(config = "scc", file = "config.yml")

config$hist_years = seq.int(config$historical_start_year, config$start_year)

config$pred_years = seq.int(config$start_year + 1L, config$end_year)

config$fert_path = config$fertilization_event_dir

config$ncc_path = config$ncc_event_dir

config$landiq_path = config$crops_path

config$crop_prediction_dir = file.path(config$work_root, config$prediction_dir)

config$planting_dir = file.path(config$work_root, config$planting_output_root)

config$phenology_dir = file.path(config$work_root, config$phenology_output_dir)

config$scenario_dir = file.path(config$work_root, config$scenario_dir)

config$output_root = file.path(config$work_root, config$fertilization_output_root)

config$ncc_output_root = file.path(config$work_root, config$ncc_output_root)

scenario_files = c(BAU_Targets = file.path(config$scenario_dir, "BAU_Targets.csv"),
                   NBS_Targets = file.path(config$scenario_dir, "NBS_Targets.csv"))

dir.create(config$output_root, recursive = TRUE, showWarnings = FALSE)

dir.create(config$ncc_output_root, recursive = TRUE,showWarnings = FALSE)

## ---- helpers ----
assert_cols = function(x, req, label) {
  miss = setdiff(req, names(x))
  if (length(miss)) stop(label, " missing: ", paste(miss, collapse = ", "))
}

safe_mean = function(x) {
  x = as.numeric(x); x = x[is.finite(x)]
  if (length(x)) mean(x) else NA_real_
}

safe_wmean = function(x, w) {
  x = as.numeric(x); w = as.numeric(w)
  ok = is.finite(x) & is.finite(w) & w > 0
  if (any(ok)) weighted.mean(x[ok], w[ok]) else NA_real_
}

pick_col = function(nms, choices, label, required = TRUE) {
  hit = intersect(choices, nms)
  if (length(hit)) return(hit[1])
  if (required) stop(label, " missing all of: ", paste(choices, collapse = ", "))
  NA_character_
}

norm_county = function(x) {
  x = trimws(tolower(as.character(x)))
  sub("\\s+county$", "", x)
}

norm_subclass = function(x) {
  x = trimws(as.character(x)); x = sub("\\.0+$", "", x)
  x[x %chin% c("", "NA", "NaN", "NULL")] = NA_character_
  x
}

make_code = function(class, subclass) {
  class = trimws(as.character(class)); subclass = norm_subclass(subclass)
  fifelse(is.na(subclass), class, paste0(class, subclass))
}

get_class = function(code) sub("[0-9*].*$", "", as.character(code))

# ---- parcel acreage ----
acre_candidates = c(
  file.path(config$work_root, config$crop_history_file),
  file.path(config$work_root, config$crop_data_file)
)


acre_path = acre_candidates[file.exists(acre_candidates)][1]
if (is.na(acre_path)) stop("Could not find crops_full_counties.csv or crop_year_states_cleaned.csv.")

acre_hist = fread(acre_path, integer64 = "character")
acre_col = pick_col(names(acre_hist), c("ACRES", "acres", "Acres"), "Crop history")
assert_cols(acre_hist, "parcel_id", "Crop history")

acre_hist[, `:=`(parcel_id = as.character(parcel_id), ACRES_TMP = as.numeric(get(acre_col)))]
parcel_acres = acre_hist[is.finite(ACRES_TMP) & ACRES_TMP > 0,
                         .(ACRES = median(ACRES_TMP, na.rm = TRUE)), by = parcel_id]

# ---- parcel county set up ----
landiq = as.data.table(arrow::read_parquet(
  config$landiq_path,
  col_select = c("parcel_id", "COUNTY", "year")))

landiq[, `:=`(parcel_id = as.character(parcel_id), year = as.integer(year))]

landiq = landiq[year <= config$start_year]

setorder(landiq, parcel_id, year)

parcel_county = unique(landiq[!is.na(COUNTY)], by = "parcel_id", fromLast = TRUE
)[, .(parcel_id, county = norm_county(COUNTY))]

if (parcel_county[, anyDuplicated(parcel_id)]) stop("Parcel county lookup contains duplicates.")

# ---- historical fertilization from v2.0 parquet chunks ----
fert_files = list.files(config$fert_path, pattern = "\\.parquet$", full.names = TRUE)

ncc_files = list.files(config$ncc_path, pattern = "\\.parquet$", full.names = TRUE)

if (!length(fert_files)) {
  stop("No fertilization parquet files found in: ", config$fert_path)
}

if (!length(ncc_files)) {
  stop("No NCC amendment parquet files found in: ", config$ncc_path)
}

message("Reading ", length(fert_files), " fertilization and ",
        length(ncc_files), " NCC parquet files.")

hist = rbindlist(lapply(c(fert_files, ncc_files), function(f) {
  as.data.table(arrow::read_parquet(f))
}),
fill = TRUE
)

# Standardize ensemble-member column name for existing downstream code
if ("ens_id" %in% names(hist) &&
    !"event_member_id" %in% names(hist)) {
  setnames(hist, "ens_id", "event_member_id")
}

assert_cols(hist, c("parcel_id", "event_member_id", "date", "crop_code", "nh4_n_kg_m2",
                    "no3_n_kg_m2", "org_n_kg_m2", "org_c_kg_m2"
),
"Historical fertilization v2.0")

hist[, `:=`(
  parcel_id = as.character(parcel_id),
  event_member_id = as.character(event_member_id),
  date = as.IDate(date),
  crop_code = trimws(as.character(crop_code)),
  
  nh4 = fifelse(is.finite(as.numeric(nh4_n_kg_m2)), as.numeric(nh4_n_kg_m2), 0
  ),
  
  no3 = fifelse(is.finite(as.numeric(no3_n_kg_m2)), as.numeric(no3_n_kg_m2), 0
  ),
  
  org_n = fifelse(is.finite(as.numeric(org_n_kg_m2)), as.numeric(org_n_kg_m2), 0
  ),
  
  org_c = fifelse(is.finite(as.numeric(org_c_kg_m2)), as.numeric(org_c_kg_m2), 0
  )
)]

hist[, `:=`(
  year = as.integer(format(date, "%Y")), CLASS = get_class(crop_code), event_kind = fifelse(
    org_n > 0 | org_c > 0, "organic", "synthetic"
  )
)]

hist = hist[
  year %in% config$hist_years &
    !is.na(date) &
    !is.na(crop_code)
]

hist = merge(hist, parcel_county, by = "parcel_id", all.x = TRUE)

message("Historical fertilization rows: ", format(nrow(hist), big.mark = ","))

message("Historical county match: ", round(100 * mean(!is.na(hist$county)), 1), "%")

# ---- synthetic N lookups ----
# Historical events carry ensemble members; averaging across them gives one
# representative rate per crop. This v1 projection is deterministic and does not
# regenerate an ensemble.
# Current projections represent all inorganic N as NH4. Preserve total historical
# inorganic N as NH4; projected NO3 is structurally zero.

syn = hist[event_kind == "synthetic"]
syn[, inorg_n := nh4 + no3]
if (!nrow(syn)) stop("No historical synthetic fertilizer events found.")

syn_county = syn[!is.na(county), .(syn_n = safe_mean(inorg_n),
                                   syn_nh4 = safe_mean(nh4),
                                   syn_no3 = safe_mean(no3)), by = .(county, crop_code)]
syn_crop = syn[, .(syn2_n = safe_mean(inorg_n),
                   syn2_nh4 = safe_mean(nh4),
                   syn2_no3 = safe_mean(no3)), by = crop_code]
syn_class = syn[, .(syn3_n = safe_mean(inorg_n),
                    syn3_nh4 = safe_mean(nh4),
                    syn3_no3 = safe_mean(no3)), by = CLASS]

syn_global = safe_mean(syn$inorg_n)
syn_global_nh4 = safe_mean(syn$nh4)
syn_global_no3 = safe_mean(syn$no3)

# ---- historical organic properties ----

org = hist[event_kind == "organic"]
if (!nrow(org)) stop("No historical organic amendment events found.")

org[, pan_frac := fifelse(nh4 + org_n > 0, nh4 / (nh4 + org_n), NA_real_)]
org[, pan_frac := pmin(1, pmax(0, pan_frac))]

pan_county = org[!is.na(county), .(pan1 = safe_mean(pan_frac)), by = .(county, crop_code)]
pan_crop = org[, .(pan2 = safe_mean(pan_frac)), by = crop_code]
pan_class = org[, .(pan3 = safe_mean(pan_frac)), by = CLASS]
pan_global = safe_mean(org$pan_frac)
if (!is.finite(pan_global)) pan_global = 0

syn_base = unique(syn[, .(parcel_id, year, event_member_id, county, crop_code, CLASS)])
org_keys = unique(org[, .(parcel_id, year, event_member_id)])
syn_base[, has_org := FALSE]
syn_base[org_keys, has_org := TRUE, on = .(parcel_id, year, event_member_id)]

prop_county = syn_base[!is.na(county), .(p1 = mean(has_org)), by = .(county, crop_code)]
prop_crop = syn_base[, .(p2 = mean(has_org)), by = crop_code]
prop_class = syn_base[, .(p3 = mean(has_org)), by = CLASS]
p_global = mean(syn_base$has_org)

# ---- scenario-specific future crops + planting anchors ----

read_future_crop = function(scen, yy) {
  path = file.path(config$crop_prediction_dir, scen,
                   paste0("crop_identity_statewide_", yy, ".parquet"))
  if (!file.exists(path)) stop("Missing: ", path)
  
  x = as.data.table(read_parquet(path))
  assert_cols(x, c("parcel_id", "COUNTY", "season", "CLASS", "SUBCLASS"), basename(path))
  
  x[, .(
    parcel_id = as.character(parcel_id),
    year = as.integer(yy),
    season = as.integer(season),
    county = norm_county(COUNTY),
    CLASS = trimws(as.character(CLASS)),
    SUBCLASS = norm_subclass(SUBCLASS)
  )]
}


read_future_plant = function(scen, yy) {
  path = file.path(config$planting_dir, scen,
                   paste0("planting_statewide_", yy, ".parquet"))
  if (!file.exists(path)) stop("Missing: ", path)
  
  x = as.data.table(read_parquet(path))
  
  id_col = pick_col(names(x), c("parcel_id", "site_id"), basename(path))
  code_col = pick_col(names(x), c("crop_code", "code", "CLASS_SUBCLASS"), basename(path))
  date_col = pick_col(names(x), c("date", "planting_date"), basename(path))
  
  data.table(parcel_id = as.character(x[[id_col]]),
             year = as.integer(yy), crop_code = trimws(as.character(x[[code_col]])), anchor = as.IDate(x[[date_col]]))
}

read_future_pheno = function(scen, yy) {
  path = file.path(config$phenology_dir, scen,
                   paste0("phenology_statewide_", yy, ".parquet"))
  if (!file.exists(path)) stop("Missing: ", path)
  
  x = as.data.table(read_parquet(path))
  assert_cols(x, c("parcel_id", "leafonday"), basename(path))
  
  data.table(
    parcel_id = as.character(x$parcel_id),
    year = as.integer(yy),
    leafonday = as.IDate(x$leafonday)
  )
}

build_future = function(scen) {
  future = rbindlist(lapply(config$pred_years, function(yy) read_future_crop(scen, yy)), fill = TRUE)
  future[, crop_code := make_code(CLASS, SUBCLASS)]
  future = future[!CLASS %chin% c("X", "I") & !is.na(CLASS)]
  
  dups = future[, .N, by = .(parcel_id, year, season)][N > 1L]
  if (nrow(dups))
    stop(scen, " crop predictions contain duplicate parcel-year-season rows.")
  
  # A cover crop that landed on its dominant crop's code cannot be told apart
  # from it downstream, so the cover cycle is dropped. Known upstream issue in
  # the cover template (F16); the fix belongs in the crop projection. This
  # script only uses season 2, so nothing it needs is lost.
  setorder(future, parcel_id, year, crop_code, season)
  future[, code_dup := rowid(parcel_id, year, crop_code)]
  future[, code_dup := max(code_dup) > 1L, by = .(parcel_id, year, crop_code)]
  
  n_drop = future[code_dup & season != 2L, .N]
  if (n_drop)
    message(scen, " cover cycles dropped for colliding crop_code: ",
            format(n_drop, big.mark = ","))
  
  future = future[!(code_dup & season != 2L)]
  future[, code_dup := NULL]
  
  # Planting output has no season column, so crop_code must distinguish cycles.
  code_dups = future[, .N, by = .(parcel_id, year, crop_code)][N > 1L]
  if (nrow(code_dups))
    stop(scen, " has multiple crop cycles with the same crop_code in one parcel-year.")
  
  future = merge(future, parcel_acres, by = "parcel_id", all.x = TRUE)
  if (future[is.na(ACRES) | ACRES <= 0, .N])
    stop(scen, " future crop parcels are missing valid acreage.")
  
  plant = rbindlist(lapply(config$pred_years, function(yy) read_future_plant(scen, yy)), fill = TRUE)
  plant = plant[!is.na(anchor), .(anchor = min(anchor)), by = .(parcel_id, year, crop_code)]
  
  future = merge(future, plant, by = c("parcel_id", "year", "crop_code"), all.x = TRUE)
  if (future[is.na(anchor), .N])
    stop(scen, " active future crop rows missing planting anchors.")
  
  pheno = rbindlist(lapply(config$pred_years,
                           function(yy) read_future_pheno(scen, yy)), fill = TRUE)
  
  # Phenology still has a row for every cycle the crop projection wrote,
  # including the cover cycles dropped above. Keep the earliest leaf-on dates
  # up to the number of cycles that survived.
  n_keep = future[, .(n_crop = .N), by = .(parcel_id, year)]
  
  setorder(pheno, parcel_id, year, leafonday)
  pheno[, rn := rowid(parcel_id, year)]
  pheno = merge(pheno, n_keep, by = c("parcel_id", "year"), all.x = TRUE)
  pheno = pheno[!is.na(n_crop) & rn <= n_crop]
  pheno[, c("rn", "n_crop") := NULL]
  
  # Both tables are sorted by their own date column, so a running row number
  # within each parcel-year is the cycle rank.
  setorder(future, parcel_id, year, anchor)
  future[, cycle_rank := rowid(parcel_id, year)]
  
  setorder(pheno, parcel_id, year, leafonday)
  pheno[, cycle_rank := rowid(parcel_id, year)]
  
  crop_counts = future[, .N, by = .(parcel_id, year)]
  pheno_counts = pheno[, .N, by = .(parcel_id, year)]
  count_check = merge(crop_counts, pheno_counts, by = c("parcel_id", "year"),
                      all = TRUE, suffixes = c("_crop", "_pheno"))
  
  if (count_check[is.na(N_crop) | is.na(N_pheno) | N_crop != N_pheno, .N])
    stop(scen, " crop-cycle and phenology counts do not match.")
  
  future = merge(future, pheno,
                 by = c("parcel_id", "year", "cycle_rank"), all.x = TRUE)
  
  future[, cycle_rank := NULL]
  future[]
}
# ---- PFT family for compost timing ----

crop_lookup = fread(config$crop_lookup_path)
pft_col = pick_col(names(crop_lookup), c("PFT", "landiq_PFT"), "Crop lookup")
assert_cols(crop_lookup, c("CLASS", "SUBCLASS"), "Crop lookup")

crop_lookup[, crop_code := make_code(CLASS, SUBCLASS)]
pft_lookup = unique(crop_lookup[, .(
  crop_code,
  pft_family = fifelse(tolower(as.character(get(pft_col))) == "woody", "perennial", "annual")
)], by = "crop_code")

normalize_crop_key = function(x) {
  x = tolower(trimws(as.character(x)))
  x = gsub("&", "and", x, fixed = TRUE)
  x = gsub("[[:punct:]]", " ", x)
  gsub("\\s+", " ", trimws(x))
}

ncc_lookup = copy(crop_lookup)
ncc_lookup[, `:=`(
  CLASS = trimws(as.character(CLASS)),
  SUBCLASS = norm_subclass(SUBCLASS)
)]

ncc_lookup[, scenario_crop := fcase(
  CLASS == "T" & SUBCLASS %chin% c("19", "28"), "All Other Berries",
  CLASS == "T" & SUBCLASS == "20", "Strawberries (Fresh Market)",
  CLASS == "D" & SUBCLASS == "12", "Almonds",
  CLASS == "D" & SUBCLASS %chin% c("13", "14", "17"), "All Other Nut Crops",
  CLASS == "D" & SUBCLASS %chin% c("1", "6"), "Pome Fruit",
  CLASS == "D" & SUBCLASS %chin% c("2", "3", "5", "7", "8", "16"), "Stone Fruit",
  CLASS == "D", "All Other Fruit Crops",
  CLASS == "C", "Citrus",
  CLASS == "V" & SUBCLASS == "1", "Grapes, Table",
  CLASS == "V" & SUBCLASS == "2", "Grapes, Wine",
  CLASS == "V" & SUBCLASS == "3", "Grapes Dried, Raisins",
  CLASS == "V", "Grapes, Wine",
  CLASS == "X", "Fallow",
  CLASS %chin% c("F", "P"), "All Other Field Crops (Incl. Pasture /Rangeland)",
  CLASS %chin% c("G", "R", "T"), "Annual Cropland",
  default = NA_character_
)]

ncc_lookup[, scenario_crop_key := normalize_crop_key(scenario_crop)]
ncc_lookup = unique(ncc_lookup[
  !is.na(scenario_crop_key),
  .(crop_code, scenario_crop_key)
])

# ---- county x crop scenario NCC targets ----

read_targets = function(path) {
  x = fread(path)
  
  assert_cols(x, c("Crop", "County", "Year", "Acres_Total",
                   "Compost acres (CPS 808)", "Compost N (lbs per acre)", "Compost C (lbs per acre)"
  ), basename(path))
  
  out = x[Year %in% config$pred_years, .(
    county = norm_county(County), year = as.integer(Year),
    scenario_crop_key = normalize_crop_key(Crop), 
    scenario_total_acres = as.numeric(Acres_Total),
    scenario_compost_acres = as.numeric(`Compost acres (CPS 808)`),
    compost_n_lb_ac = as.numeric(`Compost N (lbs per acre)`),
    compost_c_lb_ac = as.numeric(`Compost C (lbs per acre)`)
  )]
  
  if (out[, .N, by = .(county, year, scenario_crop_key)][N > 1L, .N])
    stop(basename(path), " contains duplicate county/crop/year NCC targets.")
  
  out[, compost_share := fifelse(
    is.finite(scenario_total_acres) & scenario_total_acres > 0 &
      is.finite(scenario_compost_acres),
    scenario_compost_acres / scenario_total_acres,
    0
  )]
  
  # The provided sheets carry compost acreage above the stated total acreage for
  # a large share of county-crop rows. Shares are clamped to 0-1 so the parcel
  # selection stays well defined; the clamped count is logged rather than fixed
  # here, since the discrepancy is in the source sheet.
  n_over = out[compost_share > 1, .N]
  
  if (n_over)
    message(basename(path), ": ", format(n_over, big.mark = ","),
            " county/crop rows have compost acres above total acres; clamped to 1.")
  
  out[, compost_share := pmin(1, pmax(0, compost_share))]
  
  if (out[!is.finite(compost_share), .N])
    stop(basename(path), " contains non-finite compost shares.")
  
  out[, `:=`(
    compost_n_kg_m2 = PEcAn.utils::ud_convert(
      compost_n_lb_ac, "lb/acre", "kg/m^2"),
    compost_c_kg_m2 = PEcAn.utils::ud_convert(
      compost_c_lb_ac, "lb/acre", "kg/m^2")
  )]
  
  out[]
}
# ---- scenario events + prediction outputs ----

for (scen in names(scenario_files)) {
  message("\nProcessing ", scen)
  
  fert_dir = file.path(config$output_root, scen)
  ncc_dir = file.path(config$ncc_output_root, scen)
  
  dir.create(fert_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(ncc_dir, recursive = TRUE, showWarnings = FALSE)
  
  future = build_future(scen)
  
  future = merge(future, pft_lookup, by = "crop_code", all.x = TRUE)
  future[is.na(pft_family), pft_family := "annual"]
  
  # Synthetic N follows the dominant crop only. Cover crops sit in non-dominant
  # seasons under G/P codes, so leaving them in would assign them the historical
  # grain and hay N rates, which is neither a real practice nor consistent with
  # the CDFA FREP guidelines these rates come from.
  synthetic = copy(future[season == 2L])
  
  message(scen, " cover/non-dominant cycles excluded from fertilization: ",
          format(future[season != 2L, .N], big.mark = ","))
  
  synthetic = merge(synthetic, syn_county, by = c("county", "crop_code"), all.x = TRUE)
  synthetic = merge(synthetic, syn_crop, by = "crop_code", all.x = TRUE)
  synthetic = merge(synthetic, syn_class, by = "CLASS", all.x = TRUE)
  synthetic[, nh4_n_kg_m2 := fcoalesce(syn_n, syn2_n, syn3_n, syn_global)]
  
  # Historical NH4:NO3 ratio, carried but not yet applied. All inorganic N is
  # emitted as NH4 for v1; SIPNET does not distinguish the forms, but other
  # models do, so the split is retained here for a future release.
  synthetic[, hist_nh4 := fcoalesce(syn_nh4, syn2_nh4, syn3_nh4, syn_global_nh4)]
  synthetic[, hist_no3 := fcoalesce(syn_no3, syn2_no3, syn3_no3, syn_global_no3)]
  synthetic[, no3_frac := fifelse(hist_nh4 + hist_no3 > 0,
                                  hist_no3 / (hist_nh4 + hist_no3), 0)]
  
  message(scen, " historical NO3 fraction of inorganic N: ",
          round(safe_mean(synthetic$no3_frac), 3))
  
  synthetic = synthetic[
    is.finite(nh4_n_kg_m2) & nh4_n_kg_m2 > 0,
    .(parcel_id, year,
      date = fifelse(pft_family == "perennial", leafonday, anchor),
      nh4_n_kg_m2, no3_frac, org_n_kg_m2 = 0, org_c_kg_m2 = 0)
  ]
  
  message(scen, " synthetic events/year average: ",
          round(nrow(synthetic) / length(config$pred_years)))
  
  # CARB compost acreage targets apply to the dominant crop acreage.
  # Do not count an added cover cycle as another parcel of land.
  organic_base = copy(future[season == 2L])
  
  if (organic_base[, .N, by = .(parcel_id, year)][N > 1L, .N])
    stop(scen, " dominant organic base contains duplicate parcel-year rows.")
  
  organic_base = merge(organic_base, ncc_lookup, by = "crop_code", all.x = TRUE)
  organic_base = merge(organic_base, prop_county, by = c("county", "crop_code"), all.x = TRUE)
  organic_base = merge(organic_base, prop_crop, by = "crop_code", all.x = TRUE)
  organic_base = merge(organic_base, prop_class, by = "CLASS", all.x = TRUE)
  organic_base = merge(organic_base, pan_county, by = c("county", "crop_code"), all.x = TRUE)
  organic_base = merge(organic_base, pan_crop, by = "crop_code", all.x = TRUE)
  organic_base = merge(organic_base, pan_class, by = "CLASS", all.x = TRUE)
  
  organic_base[, `:=`(
    p_org = pmax(fcoalesce(p1, p2, p3, p_global), 1e-6),
    pan_frac = pmin(1, pmax(0, fcoalesce(pan1, pan2, pan3, pan_global)))
  )]
  
  # Seeded by year only, so BAU and NBS rank parcels identically and the
  # scenarios differ solely through how far down the ranking the target reaches.
  organic_base[, rand := {
    set.seed(config$seed + .BY$year)
    runif(.N)
  }, by = year]
  
  organic_base[, rank_key := -log(pmax(rand, 1e-12)) / p_org]
  
  targets = read_targets(scenario_files[[scen]])
  
  organic_list = list()
  qc_list = list()
  
  for (yy in config$pred_years) {
    
    d = copy(organic_base[
      year == yy & !is.na(scenario_crop_key)
    ])
    
    t = targets[
      year == yy & compost_share > 0
    ]
    
    d = merge(d, t, by = c("county", "year", "scenario_crop_key"), all = FALSE)
    
    if (!nrow(d)) {
      next
    }
    
    # Preserve each scenario county/crop adoption proportion,
    # but apply it to the acreage represented by our mapped parcels.
    d[, mapped_crop_acres := sum(ACRES, na.rm = TRUE),
      by = .(county, year, scenario_crop_key)]
    
    d[, target_acres := mapped_crop_acres * compost_share]
    
    setorder(d, county, scenario_crop_key, rank_key)
    
    # Cumulative acreage midpoint, matching the cover crop and tillage
    # assignments: a parcel is selected if more than half of it fits under
    # the target.
    d[, acres_mid := cumsum(ACRES) - ACRES / 2,
      by = .(county, year, scenario_crop_key)]
    
    group_targets = unique(d[, .(county, year, scenario_crop_key,
                                 mapped_crop_acres, target_acres)])
    
    d = d[target_acres > 0 & acres_mid < target_acres]
    
    if (!nrow(d)) {
      next
    }
    
    set.seed(config$seed + yy + match(scen, names(scenario_files)) * 1000L)
    
    d[, offset_days := fifelse( pft_family == "perennial",
                                sample(30:210, .N, replace = TRUE), sample(14:180, .N, replace = TRUE)
    )]
    
    d[, `:=`(
      date = anchor - offset_days,
      nh4_n_kg_m2 = compost_n_kg_m2 * pan_frac,
      org_n_kg_m2 = compost_n_kg_m2 * (1 - pan_frac),
      org_c_kg_m2 = compost_c_kg_m2
    )]
    
    organic_list[[as.character(yy)]] = d[, .(
      parcel_id, year, date, nh4_n_kg_m2, org_n_kg_m2, org_c_kg_m2
    )]
    
    realized = d[, .(realized_acres = sum(ACRES, na.rm = TRUE), parcels = .N),
                 by = .(county, year, scenario_crop_key)]
    
    qc = merge(group_targets, realized,
               by = c("county", "year", "scenario_crop_key"), all.x = TRUE)
    
    qc[is.na(realized_acres), realized_acres := 0]
    qc[is.na(parcels), parcels := 0L]
    qc[, difference_acres := realized_acres - target_acres]
    
    qc_list[[as.character(yy)]] = qc
    
    message(yy, ": NCC groups=", nrow(qc), " | target acres=", round(sum(qc$target_acres)),
            " | realized acres=", round(sum(qc$realized_acres)))
  }
  
  organic = rbindlist(organic_list, fill = TRUE)
  
  # A scenario can legitimately have no compost anywhere; keep the schema so the
  # yearly writes below still produce empty files rather than failing.
  if (!nrow(organic)) {
    message(scen, " has no compost events in any projection year.")
    
    organic = data.table(
      parcel_id = character(), year = integer(), date = as.IDate(character()),
      nh4_n_kg_m2 = numeric(), org_n_kg_m2 = numeric(), org_c_kg_m2 = numeric())
  }
  
  if (length(qc_list)) {
    ncc_qc = rbindlist(qc_list, fill = TRUE)
    ncc_qc[, scenario := scen]
    setorder(ncc_qc, county, year, scenario_crop_key)
    
    qc_path = file.path(ncc_dir, "ncc_projection_qc.parquet")
    write_parquet(ncc_qc, qc_path, compression = "zstd")
    message("Wrote: ", qc_path)
  }
  
  # Pre-planting offsets can place a compost event in the previous calendar year.
  # This is allowed, matching the inventory amendment workflow, but is logged so
  # the count is visible in the run record.
  n_offyear = organic[!is.na(date) & year(date) < year, .N]
  
  if (n_offyear)
    message(scen, " compost events dated in the year before their crop: ",
            format(n_offyear, big.mark = ","))
  
  synthetic[, parcel_id := bit64::as.integer64(parcel_id)]
  organic[, parcel_id := bit64::as.integer64(parcel_id)]
  
  setorder(synthetic, year, parcel_id, date)
  setorder(organic, year, parcel_id, date)
  
  for (yy in config$pred_years) {
    
    fert_out = synthetic[year == yy, .(
      event_type = "fertilization", parcel_id = bit64::as.integer64(parcel_id),
      date = as.Date(date), nh4_n_kg_m2 = as.numeric(nh4_n_kg_m2),
      no3_n_kg_m2 = 0, org_n_kg_m2 = as.numeric(org_n_kg_m2), org_c_kg_m2 = as.numeric(org_c_kg_m2)
    )]
    
    ncc_out = organic[year == yy, .(
      event_type = "fertilization", parcel_id = bit64::as.integer64(parcel_id), date = as.Date(date),
      nh4_n_kg_m2 = as.numeric(nh4_n_kg_m2), no3_n_kg_m2 = 0, org_n_kg_m2 = as.numeric(org_n_kg_m2),
      org_c_kg_m2 = as.numeric(org_c_kg_m2)
    )]
    
    if (fert_out[!complete.cases(fert_out), .N])
      stop(scen, " ", yy, " synthetic fertilizer contains incomplete events.")
    
    if (ncc_out[!complete.cases(ncc_out), .N])
      stop(scen, " ", yy, " NCC contains incomplete events.")
    
    write_parquet(fert_out, file.path(fert_dir, paste0("fertilization_statewide_", yy, ".parquet")), compression = "zstd")
    
    write_parquet(ncc_out, file.path(ncc_dir, paste0("ncc_statewide_", yy, ".parquet")), compression = "zstd")
  }
  
  n_fert = length(list.files(fert_dir, pattern = "^fertilization_statewide_[0-9]{4}\\.parquet$"))
  n_ncc = length(list.files(ncc_dir, pattern = "^ncc_statewide_[0-9]{4}\\.parquet$"))
  
  if (n_fert != length(config$pred_years) || n_ncc != length(config$pred_years))
    stop(scen, " wrote ", n_fert, " fertilizer and ", n_ncc, " NCC files; expected ",
         length(config$pred_years), " of each.")
  
}

message("\nFertilization projection complete: ", config$output_root)