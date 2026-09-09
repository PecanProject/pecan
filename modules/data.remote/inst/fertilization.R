## Fertilization projections
## BAU/NBS targets control statewide annual compost adoption + compost N/C rates.
## Historical derived fertilization controls synthetic N, crop/county propensity, and organic N partitioning.
##
## Final output:
## event_type, parcel_id, date, nh4_n_kg_m2, org_n_kg_m2, org_c_kg_m2

pacman::p_load(data.table, arrow, bit64, dplyr, PEcAn.utils)

# ---- set up ----
#REQUIRED: Choose a folder to define work_root, where you want this framework to save intermediate and output files
#Uncomment the line below and replace the example path.
#work_root = "/path/to/your/folder"

#Shared Data: Shared project data, most users should not need to change this. 
ccmmf_root = "/projectnb/dietzelab/ccmmf"

config = list(seed = 42L, hist_years = 2018:2023, pred_years = 2024:2045,

  ##the historicla fertilization info the predictions will build off of             
  fert_path = file.path(ccmmf_root, "usr", "akash", "event_files", "combined", "v2.0", "_output", "fertilization.parquet"),
  
  ##crop, date, and target scenario information used for fertilization predictions
  landiq_path = file.path(ccmmf_root, "LandIQ-harmonized-v4.1.2", "crops_all_years.parq"),
  crop_lookup_path = file.path(ccmmf_root, "management", "LandIQ_cropCode_lookup_table.csv"),
  crop_prediction_dir = file.path(work_root, "crop_predictions"),
  planting_dir = file.path(work_root, "planting_projections"),
  scenario_dir = file.path(work_root, "MAGiC_scenarios_FINAL"),
  
  ##folder your prediction parquets are going to be stored 
  output_root = file.path(work_root, "fertilization_projections")
)

scenario_files = c(BAU_Targets = file.path(config$scenario_dir, "BAU_Targets.csv"),
  NBS_Targets = file.path(config$scenario_dir, "NBS_Targets.csv"))

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
acre_candidates = c(file.path(work_root, "crops_full_counties.csv"),
  file.path(work_root, "crop_year_states_cleaned.csv"))

acre_path = acre_candidates[file.exists(acre_candidates)][1]
if (is.na(acre_path)) stop("Could not find crops_full_counties.csv or crop_year_states_cleaned.csv.")

acre_hist = fread(acre_path, integer64 = "character")
acre_col = pick_col(names(acre_hist), c("ACRES", "acres", "Acres"), "Crop history")
assert_cols(acre_hist, "parcel_id", "Crop history")

acre_hist[, `:=`(parcel_id = as.character(parcel_id), ACRES_TMP = as.numeric(get(acre_col)))]
parcel_acres = acre_hist[is.finite(ACRES_TMP) & ACRES_TMP > 0,
                         .(ACRES = median(ACRES_TMP, na.rm = TRUE)), by = parcel_id]

# ---- parcel county set up ----
landiq = open_dataset(config$landiq_path) |>
  dplyr::select(parcel_id, COUNTY, year) |>
  dplyr::filter(year <= 2023) |>
  dplyr::collect() |>
  as.data.table()

landiq[, `:=`(parcel_id = as.character(parcel_id), year = as.integer(year))]
parcel_county = landiq[!is.na(COUNTY), .SD[which.max(year)], by = parcel_id][,
                                                                             .(parcel_id, county = norm_county(COUNTY))]

if (parcel_county[, anyDuplicated(parcel_id)]) stop("Parcel county lookup contains duplicates.")

# ---- historical combined fertilization ----
#note: fertilization.parquet is a partitioned dataset directory - Arrow detects event_member_id automatically. 
#Do not need to edit to specify partitioning.

hist_ds = open_dataset(config$fert_path)
hist_names = hist_ds$schema$names

id_col = pick_col(hist_names, c("parcel_id", "site_id"), "Historical fertilization")
crop_col = pick_col(hist_names, c("crop_code", "code"), "Historical fertilization")
member_col = pick_col(hist_names, c("event_member_id", "ens_id"), "Historical fertilization", FALSE)

wanted = unique(c(id_col, crop_col, member_col, "date", "nh4_n_kg_m2",
                  "no3_n_kg_m2", "org_n_kg_m2", "org_c_kg_m2"))
wanted = wanted[!is.na(wanted) & wanted %in% hist_names]

hist = hist_ds |>
  dplyr::select(dplyr::all_of(wanted)) |>
  dplyr::collect() |>
  as.data.table()

setnames(hist, id_col, "parcel_id")
setnames(hist, crop_col, "crop_code")
if (!is.na(member_col)) setnames(hist, member_col, "event_member_id")
if (!"event_member_id" %in% names(hist)) hist[, event_member_id := "ens_001"]
if (!"no3_n_kg_m2" %in% names(hist)) hist[, no3_n_kg_m2 := 0]

assert_cols(hist, c("parcel_id", "date", "crop_code", "nh4_n_kg_m2",
                    "org_n_kg_m2", "org_c_kg_m2"), "Historical fertilization")

hist[, `:=`(
  parcel_id = as.character(parcel_id), date = as.IDate(date),
  crop_code = trimws(as.character(crop_code)),
  nh4 = fifelse(is.finite(as.numeric(nh4_n_kg_m2)), as.numeric(nh4_n_kg_m2), 0),
  no3 = fifelse(is.finite(as.numeric(no3_n_kg_m2)), as.numeric(no3_n_kg_m2), 0),
  org_n = fifelse(is.finite(as.numeric(org_n_kg_m2)), as.numeric(org_n_kg_m2), 0),
  org_c = fifelse(is.finite(as.numeric(org_c_kg_m2)), as.numeric(org_c_kg_m2), 0)
)]

hist[, `:=`(
  year = as.integer(format(date, "%Y")), CLASS = get_class(crop_code),
  event_kind = fifelse(org_n > 0 | org_c > 0, "organic", "synthetic")
)]

hist = hist[year %in% config$hist_years & !is.na(date) & !is.na(crop_code)]
hist = merge(hist, parcel_county, by = "parcel_id", all.x = TRUE)

message("Historical fertilization rows: ", format(nrow(hist), big.mark = ","))
message("Historical county match: ", round(100 * mean(!is.na(hist$county)), 1), "%")

# ---- synthetic N lookups ----
# Final output drops NO3, so preserve total historical inorganic N as NH4.

syn = hist[event_kind == "synthetic"]
syn[, inorg_n := nh4 + no3]
if (!nrow(syn)) stop("No historical synthetic fertilizer events found.")

syn_county = syn[!is.na(county), .(syn_n = safe_mean(inorg_n)), by = .(county, crop_code)]
syn_crop = syn[, .(syn2_n = safe_mean(inorg_n)), by = crop_code]
syn_class = syn[, .(syn3_n = safe_mean(inorg_n)), by = CLASS]
syn_global = safe_mean(syn$inorg_n)

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

# ---- future crops ----

read_future_crop = function(yy) {
  path = file.path(config$crop_prediction_dir, paste0("crop_identity_statewide_", yy, ".parquet"))
  if (!file.exists(path)) stop("Missing: ", path)
  
  x = as.data.table(read_parquet(path))
  assert_cols(x, c("parcel_id", "COUNTY", "CLASS", "SUBCLASS"), basename(path))
  
  if ("season" %in% names(x)) {
    x[, season := as.integer(season)]
    if (x[!is.na(season) & season != 2L, .N]) stop(path, " contains non-season-2 rows.")
  }
  
  x[, .(
    parcel_id = as.character(parcel_id),
    year = as.integer(yy),
    county = norm_county(COUNTY),
    CLASS = trimws(as.character(CLASS)),
    SUBCLASS = norm_subclass(SUBCLASS)
  )]
}

future = rbindlist(lapply(config$pred_years, read_future_crop), fill = TRUE)
future[, crop_code := make_code(CLASS, SUBCLASS)]
future = future[!CLASS %chin% c("X", "I") & !is.na(CLASS)]

if (future[, .N, by = .(parcel_id, year)][N > 1, .N])
  stop("Future crop predictions contain duplicate parcel-year rows.")

future = merge(future, parcel_acres, by = "parcel_id", all.x = TRUE)
if (future[is.na(ACRES) | ACRES <= 0, .N])
  stop("Future crop parcels are missing valid acreage.")

# ---- future planting anchors ----

read_future_plant = function(yy) {
  path = file.path(config$planting_dir, paste0("planting_statewide_", yy, ".parquet"))
  if (!file.exists(path)) stop("Missing: ", path)
  
  x = as.data.table(read_parquet(path))
  
  id_col = pick_col(names(x), c("parcel_id", "site_id"), basename(path))
  code_col = pick_col(names(x), c("crop_code", "code", "CLASS_SUBCLASS"), basename(path))
  date_col = pick_col(names(x), c("date", "planting_date"), basename(path))
  
  data.table(
    parcel_id = as.character(x[[id_col]]),
    year = as.integer(yy),
    crop_code = trimws(as.character(x[[code_col]])),
    anchor = as.IDate(x[[date_col]])
  )
}

plant = rbindlist(lapply(config$pred_years, read_future_plant), fill = TRUE)
plant = plant[!is.na(anchor), .(anchor = min(anchor)), by = .(parcel_id, year, crop_code)]

future = merge(future, plant, by = c("parcel_id", "year", "crop_code"), all.x = TRUE)
if (future[is.na(anchor), .N]) stop("Active future crop rows missing planting anchors.")

# ---- PFT family for compost timing ----

crop_lookup = fread(config$crop_lookup_path)
pft_col = pick_col(names(crop_lookup), c("PFT", "landiq_PFT"), "Crop lookup")
assert_cols(crop_lookup, c("CLASS", "SUBCLASS"), "Crop lookup")

crop_lookup[, crop_code := make_code(CLASS, SUBCLASS)]
pft_lookup = unique(crop_lookup[, .(
  crop_code,
  pft_family = fifelse(tolower(as.character(get(pft_col))) == "woody", "perennial", "annual")
)], by = "crop_code")

future = merge(future, pft_lookup, by = "crop_code", all.x = TRUE)
future[is.na(pft_family), pft_family := "annual"]

# ---- synthetic future events ----

synthetic = copy(future)
synthetic = merge(synthetic, syn_county, by = c("county", "crop_code"), all.x = TRUE)
synthetic = merge(synthetic, syn_crop, by = "crop_code", all.x = TRUE)
synthetic = merge(synthetic, syn_class, by = "CLASS", all.x = TRUE)
synthetic[, nh4_n_kg_m2 := fcoalesce(syn_n, syn2_n, syn3_n, syn_global)]

synthetic = synthetic[is.finite(nh4_n_kg_m2) & nh4_n_kg_m2 > 0,
                      .(parcel_id, year, date = anchor, nh4_n_kg_m2, org_n_kg_m2 = 0, org_c_kg_m2 = 0)]

message("Shared synthetic events/year average: ",
        round(nrow(synthetic) / length(config$pred_years)))

# ---- organic parcel propensity ----

organic_base = copy(future)
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

organic_base[, rand := {
  set.seed(config$seed + .BY$year)
  runif(.N)
}, by = year]

organic_base[, rank_key := -log(pmax(rand, 1e-12)) / p_org]

# ---- statewide scenario targets ----

read_targets = function(path) {
  x = fread(path)
  
  assert_cols(x, c("Year", "Acres_Total", "Compost acres (CPS 808)",
                   "Compost N (lbs per acre)", "Compost C (lbs per acre)"), basename(path))
  
  x[, `:=`(
    Year = as.integer(Year),
    Acres_Total = as.numeric(Acres_Total),
    compost_acres = as.numeric(`Compost acres (CPS 808)`),
    compost_n = as.numeric(`Compost N (lbs per acre)`),
    compost_c = as.numeric(`Compost C (lbs per acre)`)
  )]
  
  out = x[Year %in% config$pred_years, .(
    total_acres = sum(Acres_Total, na.rm = TRUE),
    compost_acres = sum(compost_acres, na.rm = TRUE),
    compost_n_lb_ac = safe_wmean(compost_n, compost_acres),
    compost_c_lb_ac = safe_wmean(compost_c, compost_acres)
  ), by = .(year = Year)]
  
  out[, compost_share := compost_acres / total_acres]
  
  if (out[!is.finite(compost_share) | compost_share < 0 | compost_share > 1, .N])
    stop(basename(path), " has invalid STATEWIDE annual compost share.")
  
  out[, `:=`(
    compost_n_kg_m2 = PEcAn.utils::ud_convert(compost_n_lb_ac, "lb/acre", "kg/m^2"),
    compost_c_kg_m2 = PEcAn.utils::ud_convert(compost_c_lb_ac, "lb/acre", "kg/m^2")
  )]
  
  out[]
}

# ---- scenario events + prediction outputs ----

for (scen in names(scenario_files)) {
  message("\nProcessing ", scen)
  targets = read_targets(scenario_files[[scen]])
  organic_list = list()
  
  for (yy in config$pred_years) {
    d = copy(organic_base[year == yy])
    t = targets[year == yy]
    if (!nrow(t)) stop(scen, " missing target year ", yy)
    
    target_acres = t$compost_share * sum(d$ACRES, na.rm = TRUE)
    
    setorder(d, rank_key)
    d[, prev_acres := shift(cumsum(ACRES), fill = 0)]
    d = d[prev_acres < target_acres]
    
    set.seed(config$seed + yy + match(scen, names(scenario_files)) * 1000L)
    
    d[, offset_days := fifelse(
      pft_family == "perennial",
      sample(30:210, .N, replace = TRUE),
      sample(14:180, .N, replace = TRUE)
    )]
    
    d[, `:=`(
      date = anchor - offset_days,
      nh4_n_kg_m2 = t$compost_n_kg_m2 * pan_frac,
      org_n_kg_m2 = t$compost_n_kg_m2 * (1 - pan_frac),
      org_c_kg_m2 = t$compost_c_kg_m2
    )]
    
    organic_list[[as.character(yy)]] = d[, .(
      parcel_id, year, date,
      nh4_n_kg_m2, org_n_kg_m2, org_c_kg_m2
    )]
    
    realized = sum(d$ACRES, na.rm = TRUE) / sum(organic_base[year == yy]$ACRES, na.rm = TRUE)
    
    message(yy, ": target=", round(100 * t$compost_share, 2),
            "% | realized=", round(100 * realized, 2),
            "% | organic parcels=", format(nrow(d), big.mark = ","))
  }
  
  organic = rbindlist(organic_list, fill = TRUE)
  fert = rbindlist(list(synthetic, organic), fill = TRUE)
  fert[, parcel_id := bit64::as.integer64(parcel_id)]
  setorder(fert, year, parcel_id, date)
  
  out_dir = file.path(config$output_root, scen)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  for (yy in config$pred_years) {
    out = fert[year == yy, .(
      event_type = "fertilization",
      parcel_id = bit64::as.integer64(parcel_id),
      date = as.Date(date),
      nh4_n_kg_m2 = as.numeric(nh4_n_kg_m2),
      org_n_kg_m2 = as.numeric(org_n_kg_m2),
      org_c_kg_m2 = as.numeric(org_c_kg_m2)
    )]
    
    if (out[!complete.cases(out), .N]) stop(scen, " ", yy, " contains incomplete events.")
    
    write_parquet(out,
                  file.path(out_dir, paste0("fertilization_statewide_", yy, ".parquet")),
                  compression = "zstd")
  }
  
  message("Finished ", scen)
}

message("\nFertilization projection complete: ", config$output_root)