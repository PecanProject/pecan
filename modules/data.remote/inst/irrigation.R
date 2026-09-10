## Shared irrigation projection
## Final output schema: event_type, parcel_id, date, amount_mm

pacman::p_load(data.table, arrow, bit64, dplyr, readr, stringr, lubridate, sf,
               PEcAn.data.land, parallel, parallelly)

# ---------- setup ----------
#REQUIRED: Choose a folder to define work_root, where you want this framework to save intermediate and output files
#Uncomment the line below and replace the example path.
#work_root = "/path/to/your/folder"

#Shared Data: Shared project data, most users should not need to change this.
ccmmf_root = "/projectnb/dietzelab/ccmmf"

config = list(
  years = 2024:2045, hist_years = 2018:2023,
  crop_dir = file.path(work_root, "crop_predictions"),
  planting_dir = file.path(work_root, "planting_projections"),
  harvest_dir = file.path(work_root, "harvest_projections"),
  landiq_path = file.path(ccmmf_root, "LandIQ-harmonized-v4.1.2", "crops_all_years.parq"),
  matched_dir = file.path(ccmmf_root, "management", "phenology", "matched_landiq_mslsp_v4.1.2", "gapfill_dates"),
  climate_path = file.path(work_root, "caladapt_county_daily_climate.csv"),
  
  ##cache AWC values by county to reduce run time; first run creates these, later runs reuse them
  ssurgo_weights_path = file.path(ccmmf_root, "data_raw", "ssurgo", "ssurgo-weights.parquet"),
  ssurgo_gdb_path = file.path(ccmmf_root, "data_raw", "ssurgo", "gSSURGO_CA.gdb"),
  awc_cache_dir = file.path(work_root, "irrigation_awc_cache"),
  
  ##choosing one out of the three gcm/ssp combinations for simplicity
  gcm = "CESM2", ssp = "ssp245",
  
  rice_target = 125, rice_min = 62.5, rice_max = 175, rice_seepage = 2.5, irrigation_max = 150,
  
  ##will be used during the actual prediction section to improve run time
  workers = 6L,
  
  output_dir = file.path(work_root, "irrigation_projections")
)

dir.create(config$output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(config$awc_cache_dir, recursive = TRUE, showWarnings = FALSE)


# ---------- helpers ----------
assert_cols = function(x, req, label) {
  miss = setdiff(req, names(x))
  if (length(miss)) stop(label, " missing: ", paste(miss, collapse = ", "))
}

pick_col = function(nms, choices, label) {
  hit = intersect(choices, nms)
  if (!length(hit)) stop(label, " missing all of: ", paste(choices, collapse = ", "))
  hit[1]
}

safe_mean = function(x) {
  x = as.numeric(x)
  x = x[is.finite(x)]
  if (length(x)) mean(x) else NA_real_
}

norm_county = function(x) {
  x = stringr::str_squish(as.character(x))
  sub("\\s+County$", "", x, ignore.case = TRUE)
}

norm_subclass = function(x) {
  x = trimws(as.character(x))
  x = sub("\\.0+$", "", x)
  x[x %chin% c("", "NA", "NaN", "NULL")] = NA_character_
  x
}

make_code = function(class, subclass) {
  class = trimws(as.character(class))
  subclass = norm_subclass(subclass)
  fifelse(is.na(subclass), class, paste0(class, subclass))
}

calc_effective_awc = function(hzdept_r, hzdepb_r, awc_r, rooting_depth_cm) {
  top = pmin(hzdept_r, rooting_depth_cm)
  bottom = pmin(hzdepb_r, rooting_depth_cm)
  thickness = pmax(0, bottom - top)
  sum(awc_r * thickness, na.rm = TRUE) * 10
}


# ---------- future crops / planting / harvest ----------
read_crop = function(yy) {
  path = file.path(config$crop_dir, paste0("crop_identity_statewide_", yy, ".parquet"))
  if (!file.exists(path)) stop("Missing crop projection: ", path)
  
  x = as.data.table(read_parquet(path))
  assert_cols(x, c("parcel_id", "COUNTY", "CLASS", "SUBCLASS"), basename(path))
  
  if ("season" %in% names(x)) {
    x[, season := as.integer(season)]
    if (x[!is.na(season) & season != 2L, .N]) stop(basename(path), " contains non-season-2 rows.")
  }
  
  x[, .(
    parcel_id = bit64::as.integer64(as.character(parcel_id)), year = as.integer(yy),
    county = norm_county(COUNTY), CLASS = trimws(as.character(CLASS)),
    SUBCLASS = norm_subclass(SUBCLASS), crop_code = make_code(CLASS, SUBCLASS)
  )]
}

read_plant = function(yy) {
  path = file.path(config$planting_dir, paste0("planting_statewide_", yy, ".parquet"))
  if (!file.exists(path)) stop("Missing planting projection: ", path)
  
  x = as.data.table(read_parquet(path))
  id = pick_col(names(x), c("parcel_id", "site_id"), basename(path))
  dc = pick_col(names(x), c("date", "planting_date"), basename(path))
  
  data.table(parcel_id = bit64::as.integer64(as.character(x[[id]])),
             year = as.integer(yy), planting_date = as.IDate(x[[dc]]))
}

read_harvest = function(yy) {
  path = file.path(config$harvest_dir, paste0("harvest_statewide_", yy, ".parquet"))
  if (!file.exists(path)) stop("Missing harvest projection: ", path)
  
  x = as.data.table(read_parquet(path))
  id = pick_col(names(x), c("parcel_id", "site_id"), basename(path))
  dc = pick_col(names(x), c("date", "harvest_date"), basename(path))
  
  data.table(parcel_id = bit64::as.integer64(as.character(x[[id]])),
             year = as.integer(yy), harvest_date = as.IDate(x[[dc]]))
}

message("Loading future projections...")

future_crop = rbindlist(lapply(config$years, read_crop))
future_crop = future_crop[!CLASS %chin% c("X", "I") & !is.na(CLASS)]
planting = rbindlist(lapply(config$years, read_plant))
harvest = rbindlist(lapply(config$years, read_harvest))

for (nm in c("future_crop", "planting", "harvest")) {
  z = get(nm)
  if (z[, .N, by = .(parcel_id, year)][N > 1L, .N]) stop(nm, " contains duplicate parcel-year rows.")
}

future = merge(future_crop, planting, by = c("parcel_id", "year"), all.x = TRUE)
future = merge(future, harvest, by = c("parcel_id", "year"), all.x = TRUE)

if (future[is.na(planting_date) | is.na(harvest_date), .N]) stop("Active crops missing planting/harvest dates.")
if (future[harvest_date <= planting_date, .N]) stop("Harvest must occur after planting.")

rm(future_crop, planting, harvest); gc()

# ---------- historical county ----------
message("Loading historical county lookup...")

landiq = as.data.table(read_parquet(config$landiq_path, col_select = c("parcel_id", "COUNTY", "year")))
landiq[, `:=`(
  parcel_id = bit64::as.integer64(as.character(parcel_id)),
  COUNTY = norm_county(COUNTY),
  year = as.integer(year)
)]
landiq = landiq[year <= max(config$hist_years) & !is.na(COUNTY)]

parcel_county = landiq[, .SD[which.max(year)], by = parcel_id][, .(parcel_id, county = COUNTY)]
if (anyDuplicated(parcel_county$parcel_id)) stop("County lookup contains duplicate parcel IDs.")

rm(landiq); gc()

# ---------- historical peak fraction ----------
message("Calculating historical peak-position lookup...")

peak_hist = rbindlist(lapply(config$hist_years, function(yy) {
  path = file.path(config$matched_dir, paste0("assigned_year=", yy, "_gapfilled.parquet"))
  if (!file.exists(path)) stop("Missing phenology file: ", path)
  
  x = as.data.table(read_parquet(path))
  assert_cols(x, c("parcel_id", "landiq_CLASS", "landiq_SUBCLASS",
                   "mslsp_OGI", "mslsp_Peak", "mslsp_OGMn"), basename(path))
  
  if ("season" %in% names(x)) {
    x[, season := as.integer(season)]
    x = x[season == 2L]
  }
  
  x[, .(
    parcel_id = bit64::as.integer64(as.character(parcel_id)),
    CLASS = trimws(as.character(landiq_CLASS)), SUBCLASS = norm_subclass(landiq_SUBCLASS),
    hist_plant = as.IDate(mslsp_OGI), hist_peak = as.IDate(mslsp_Peak), hist_end = as.IDate(mslsp_OGMn)
  )]
}), fill = TRUE)

peak_hist = merge(peak_hist, parcel_county, by = "parcel_id", all.x = TRUE)
peak_hist[, crop_code := make_code(CLASS, SUBCLASS)]
peak_hist[, `:=`(season_days = as.numeric(hist_end - hist_plant),
                 peak_days = as.numeric(hist_peak - hist_plant))]

peak_hist = peak_hist[
  is.finite(season_days) & season_days > 0 &
    is.finite(peak_days) & peak_days >= 0 & peak_days <= season_days
]

peak_hist[, hist_peak_frac := peak_days / season_days]
if (!nrow(peak_hist)) stop("No valid historical peak fractions.")

lookup_cc = peak_hist[!is.na(county) & !is.na(crop_code),
                      .(peak_frac_cc = safe_mean(hist_peak_frac)), by = .(county, crop_code)]
lookup_cclass = peak_hist[!is.na(county) & !is.na(CLASS),
                          .(peak_frac_cclass = safe_mean(hist_peak_frac)), by = .(county, CLASS)]
lookup_code = peak_hist[!is.na(crop_code),
                        .(peak_frac_code = safe_mean(hist_peak_frac)), by = crop_code]
lookup_class = peak_hist[!is.na(CLASS),
                         .(peak_frac_class = safe_mean(hist_peak_frac)), by = CLASS]
peak_frac_global = safe_mean(peak_hist$hist_peak_frac)

future = merge(future, lookup_cc, by = c("county", "crop_code"), all.x = TRUE)
future = merge(future, lookup_cclass, by = c("county", "CLASS"), all.x = TRUE)
future = merge(future, lookup_code, by = "crop_code", all.x = TRUE)
future = merge(future, lookup_class, by = "CLASS", all.x = TRUE)

future[, peak_frac := as.numeric(peak_frac_cc)]
future[is.na(peak_frac), peak_frac := as.numeric(peak_frac_cclass)]
future[is.na(peak_frac), peak_frac := as.numeric(peak_frac_code)]
future[is.na(peak_frac), peak_frac := as.numeric(peak_frac_class)]
future[is.na(peak_frac), peak_frac := peak_frac_global]
future[, peak_frac := pmin(1, pmax(0, peak_frac))]

future[, season_days := as.numeric(harvest_date - planting_date)]
future[, peak_date := as.IDate(as.Date(planting_date) + as.integer(round(peak_frac * season_days)))]

if (future[is.na(peak_date), .N]) stop("Could not project peak date for all active crops.")
if (future[peak_date < planting_date | peak_date > harvest_date, .N]) stop("Impossible projected peak date.")

future[, c("peak_frac_cc", "peak_frac_cclass", "peak_frac_code",
           "peak_frac_class", "season_days") := NULL]

rm(peak_hist, lookup_cc, lookup_cclass, lookup_code, lookup_class, parcel_county); gc()

# ---------- crop-water parameters ----------
bism = PEcAn.data.land::bism_kc_by_crop |>
  distinct(landiq_class, landiq_subclass, crop_name) |>
  slice(1, .by = c("landiq_class", "landiq_subclass")) |>
  mutate(landiq_class = as.character(landiq_class),
         landiq_subclass = suppressWarnings(as.integer(landiq_subclass)),
         mapping_source = "exact")

crop_whc = PEcAn.data.land::crop_whc |>
  select(crop_name, whc_min_frac, rooting_depth_m) |>
  slice(1, .by = "crop_name")

proxy = tibble::tribble(
  ~landiq_class, ~landiq_subclass, ~crop_name,
  "D",3,"Stone fruits", "D",5,"Stone fruits", "D",7,"Stone fruits",
  "D",10,"Stone fruits", "D",14,"Almonds", "D",16,"Stone fruits",
  "C",7,"Avocado", "T",16,"Vegetables", "T",19,"Strawberries w/mulch",
  "T",31,"Potato", "P",4,"Improved Pasture", "P",7,"Turfgrass (cool-season)",
  "F",16,"Sorghum", "R",2,"Rice"
) |>
  mutate(mapping_source = "proxy")

bism = bind_rows(bism, proxy) |>
  distinct(landiq_class, landiq_subclass, .keep_all = TRUE)

modeled = as.data.frame(future) |>
  mutate(
    CLASS_lookup = as.character(CLASS),
    SUBCLASS_lookup = suppressWarnings(as.integer(SUBCLASS)),
    planting_date = as.Date(planting_date),
    peak_date = as.Date(peak_date),
    harvest_date = as.Date(harvest_date)
  ) |>
  left_join(bism, by = c("CLASS_lookup" = "landiq_class", "SUBCLASS_lookup" = "landiq_subclass")) |>
  left_join(crop_whc, by = "crop_name") |>
  mutate(irrigation_status = case_when(
    is.na(crop_name) ~ "missing_BISM_crop",
    crop_name != "Rice" & is.na(rooting_depth_m) ~ "missing_root_depth",
    crop_name != "Rice" & is.na(whc_min_frac) ~ "missing_MAD",
    is.na(peak_date) ~ "missing_peak",
    TRUE ~ "ready"
  ))

print(modeled |> count(irrigation_status))
modeled = modeled |> filter(irrigation_status == "ready")
if (!nrow(modeled)) stop("No crops eligible for irrigation.")

rm(future, bism, proxy, crop_whc); gc()

# ---------- soil AWC cache ----------
##first run calculates missing parcel/root-depth combinations from SSURGO; later runs reuse the county caches
message("Checking AWC cache...")

ssurgo_weights = read_parquet(config$ssurgo_weights_path)
assert_cols(ssurgo_weights, c("parcel_id", "mukey", "weight", "area_m2"), "SSURGO weights")
ssurgo_weights$parcel_id = bit64::as.integer64(as.character(ssurgo_weights$parcel_id))

##load SSURGO tables once instead of once per county
message("Loading SSURGO component/chorizon tables...")

component = sf::read_sf(config$ssurgo_gdb_path, layer = "component", as_tibble = TRUE)
if (inherits(component, "sf")) component = st_drop_geometry(component)
component = component |>
  select(mukey, cokey, comppct_r) |>
  semi_join(ssurgo_weights |> distinct(mukey), by = "mukey")

chorizon = sf::read_sf(config$ssurgo_gdb_path, layer = "chorizon", as_tibble = TRUE)
if (inherits(chorizon, "sf")) chorizon = st_drop_geometry(chorizon)
chorizon = chorizon |>
  select(cokey, hzdept_r, hzdepb_r, awc_r) |>
  semi_join(component |> distinct(cokey), by = "cokey")

nonrice = modeled |> filter(crop_name != "Rice")
rice = modeled |> filter(crop_name == "Rice") |> mutate(whc_mm = NA_real_)

county_groups = split(nonrice, nonrice$county)

nonrice_awc = lapply(names(county_groups), function(cty) {
  x = county_groups[[cty]]
  safe_cty = gsub("[^A-Za-z0-9_-]", "_", cty)
  cache_file = file.path(config$awc_cache_dir, paste0(safe_cty, "_awc.parquet"))
  
  needed = x |> distinct(parcel_id, rooting_depth_m)
  
  if (file.exists(cache_file)) {
    cache = read_parquet(cache_file) |> select(parcel_id, rooting_depth_m, whc_mm)
    cache$parcel_id = bit64::as.integer64(as.character(cache$parcel_id))
  } else {
    cache = tibble::tibble(parcel_id = bit64::integer64(), rooting_depth_m = numeric(), whc_mm = numeric())
  }
  
  missing = needed |> anti_join(cache, by = c("parcel_id", "rooting_depth_m"))
  
  if (nrow(missing)) {
    message("AWC ", cty, ": calculating ", format(nrow(missing), big.mark = ","), " new combinations.")
    
    new_awc = ssurgo_weights |>
      inner_join(missing, by = "parcel_id", relationship = "many-to-many") |>
      left_join(component, by = "mukey", relationship = "many-to-many") |>
      left_join(chorizon, by = "cokey", relationship = "many-to-many") |>
      filter(!is.na(awc_r), !is.na(hzdept_r), !is.na(hzdepb_r), !is.na(rooting_depth_m)) |>
      mutate(rooting_depth_cm = rooting_depth_m * 100) |>
      summarise(
        whc_mm_cmp = calc_effective_awc(hzdept_r, hzdepb_r, awc_r, rooting_depth_cm),
        .by = c(parcel_id, rooting_depth_m, mukey, cokey, area_m2, weight, comppct_r)
      ) |>
      summarise(
        whc_mm_mu = sum(whc_mm_cmp * comppct_r / sum(comppct_r)),
        .by = c(parcel_id, rooting_depth_m, mukey, area_m2, weight)
      ) |>
      summarise(whc_mm = sum(whc_mm_mu * weight), .by = c(parcel_id, rooting_depth_m))
    
    cache = bind_rows(cache, new_awc) |>
      distinct(parcel_id, rooting_depth_m, .keep_all = TRUE)
    
    write_parquet(cache, cache_file, compression = "zstd")
  }
  
  x |> left_join(cache, by = c("parcel_id", "rooting_depth_m"))
}) |>
  bind_rows()

missing_awc = nonrice_awc |> filter(!is.finite(whc_mm))
if (nrow(missing_awc)) message("Skipping non-rice rows without usable SSURGO AWC: ", nrow(missing_awc))

modeled = bind_rows(nonrice_awc |> filter(is.finite(whc_mm)), rice)

rm(ssurgo_weights, component, chorizon, county_groups, nonrice, nonrice_awc, rice, missing_awc); gc()
message("AWC attached. Continuing irrigation predictions.")


# ---------- climate ----------
message("Loading climate...")

climate = read_csv(config$climate_path, show_col_types = FALSE)
assert_cols(climate, c("County", "date", "GCM", "SSP", "ET0_mm", "precip_mm"), "Climate")

climate = climate |>
  mutate(
    County = norm_county(County),
    date = as.Date(date),
    ET0_mm = as.numeric(ET0_mm),
    precip_mm = as.numeric(precip_mm)
  ) |>
  filter(GCM == config$gcm, SSP == config$ssp)

if (!nrow(climate))
  stop("No climate rows for ", config$gcm, " / ", config$ssp)

if (climate |> count(County, date) |> filter(n > 1) |> nrow())
  stop("Climate contains duplicate County/date rows.")

real_min = min(year(climate$date), na.rm = TRUE)
real_max = max(year(climate$date), na.rm = TRUE)

##actual Cal-Adapt data should cover 2025-45
if (real_min > 2025 || real_max < 2045)
  stop("Climate file must contain the full 2025-2045 Cal-Adapt period.")

##use 2025 climate as the proxy for 2024
climate_2024 = climate |>
  filter(year(date) == 2025) |>
  mutate(date = suppressWarnings(make_date(2024, month(date), day(date)))) |>
  filter(!is.na(date))

climate = bind_rows(climate_2024, climate) |>
  arrange(County, date) |>
  distinct(County, date, .keep_all = TRUE)

rm(climate_2024); gc()
# ---------- water balance ----------
run_group = function(g, cclim) {
  crop = g$crop_name[[1]]
  plant = g$planting_date[[1]]
  peak = g$peak_date[[1]]
  harvest_date = g$harvest_date[[1]]
  dates = seq.Date(plant, harvest_date, by = "day")
  
  idx = match(dates, cclim$date)
  
  ##do not silently substitute another day's climate if a required date is missing
  if (anyNA(idx)) {
    missing_dates = dates[is.na(idx)]
    stop("Missing climate for ", crop, " from ", plant, " to ", harvest_date,
         ". First missing date: ", missing_dates[[1]])
  }
  
  clim = cclim[idx, , drop = FALSE]
  before_peak = max(1, as.numeric(peak - plant))
  after_peak = max(1, as.numeric(harvest_date - peak))
  
  canopy = ifelse(
    dates <= peak,
    0.15 + 0.85 * as.numeric(dates - plant) / before_peak,
    1 - 0.85 * as.numeric(dates - peak) / after_peak
  )
  canopy = pmin(1, pmax(0, canopy))
  
  etc = PEcAn.data.land::eto_to_etc_bism(eto = clim$ET0_mm, crop_name = crop, canopy_cover = canopy)
  
  if (crop == "Rice") {
    wb = PEcAn.data.land::calc_water_balance_rice(
      et = etc, precip = clim$precip_mm, flood_target = config$rice_target,
      flood_min = config$rice_min, flood_max = config$rice_max, seepage = config$rice_seepage
    )
    
    keep = which(is.finite(wb$irr) & wb$irr > 0)
    if (!length(keep)) return(tibble::tibble())
    
    return(bind_rows(lapply(seq_len(nrow(g)), function(i) {
      tibble::tibble(
        parcel_id = g$parcel_id[[i]], projection_year = g$year[[i]],
        date = dates[keep], amount_mm = as.numeric(wb$irr[keep]), method = "flood"
      )
    })))
  }
  
  bind_rows(lapply(seq_len(nrow(g)), function(i) {
    wb = PEcAn.data.land::calc_water_balance(
      et = etc, precip = clim$precip_mm, whc = g$whc_mm[[i]],
      whc_min_frac = g$whc_min_frac[[i]], irrigation_max = config$irrigation_max
    )
    
    keep = which(is.finite(wb$irr) & wb$irr > 0)
    if (!length(keep)) return(tibble::tibble())
    
    tibble::tibble(
      parcel_id = g$parcel_id[[i]], projection_year = g$year[[i]],
      date = dates[keep], amount_mm = as.numeric(wb$irr[keep]), method = "canopy"
    )
  }))
}

# ---------- parallel county predictions ----------
counties = sort(unique(modeled$county))
tmp_dir = file.path(config$output_dir, "_county_tmp")
dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)

county_path = function(cty) {
  safe = gsub("[^A-Za-z0-9_-]", "_", cty)
  file.path(tmp_dir, paste0("irrigation_", safe, ".parquet"))
}

done = vapply(counties, function(x) file.exists(county_path(x)), logical(1))
todo = counties[!done]

message(sum(done), " counties already complete; ", length(todo), " remaining.")

if (length(todo)) {
  # Build compact county jobs, then release giant statewide tables.
  jobs = lapply(todo, function(cty) {
    x = modeled |>
      filter(county == cty) |>
      select(parcel_id, year, crop_name, planting_date, peak_date, harvest_date, whc_mm, whc_min_frac)
    
    cc = climate |>
      filter(County == cty) |>
      select(date, ET0_mm, precip_mm) |>
      arrange(date)
    
    if (!nrow(cc)) stop("No climate rows found for county: ", cty)
    
    list(county = cty, data = x, climate = cc, path = county_path(cty))
  })
  
  # Large counties first + load balancing.
  sizes = vapply(jobs, function(x) nrow(x$data), integer(1))
  jobs = jobs[order(sizes, decreasing = TRUE)]
  
  rm(modeled, climate); gc()
  
  n_workers = min(config$workers, as.integer(parallelly::availableCores()), length(jobs))
  message("Starting ", n_workers, " PSOCK workers.")
  
  cl = parallel::makePSOCKcluster(n_workers, outfile = "")
  
  parallel::clusterEvalQ(cl, {
    library(dplyr)
    library(arrow)
    library(bit64)
    library(tibble)
    library(PEcAn.data.land)
    NULL
  })
  
  parallel::clusterExport(cl, c("run_group", "config"), envir = .GlobalEnv)
  
  results = tryCatch(
    parallel::parLapplyLB(cl, jobs, function(job) {
      message("RUN: ", job$county)
      
      groups = job$data |>
        group_by(crop_name, planting_date, peak_date, harvest_date) |>
        group_split(.keep = TRUE)
      
      ans = bind_rows(lapply(groups, run_group, cclim = job$climate))
      
      if (!nrow(ans)) {
        ans = tibble::tibble(
          parcel_id = bit64::as.integer64(character()), projection_year = integer(),
          date = as.Date(character()), amount_mm = numeric(), method = character()
        )
      } else {
        ans$parcel_id = bit64::as.integer64(as.character(ans$parcel_id))
        ans$date = as.Date(ans$date)
      }
      
      arrow::write_parquet(ans, job$path, compression = "zstd")
      message("DONE: ", job$county, " | ", format(nrow(ans), big.mark = ","), " events")
      
      c(county = job$county, events = as.character(nrow(ans)))
    }),
    finally = parallel::stopCluster(cl)
  )
  
  rm(jobs, results); gc()
}

message("All county predictions complete.")

# ---------- final yearly parquets ----------
parquet_files = list.files(tmp_dir, pattern = "\\.parquet$", full.names = TRUE)

if (!length(parquet_files))
  stop("No county parquet files found in: ", tmp_dir)

for (yy in config$years) {
  message("Building ", yy, "...")
  
  ##read each county file directly; avoids Arrow open_dataset filesystem issue
  pieces = lapply(parquet_files, function(f) {
    x = arrow::read_parquet(
      f,
      col_select = c("parcel_id", "projection_year", "date", "amount_mm")
    )
    
    x = x[x$projection_year == yy, , drop = FALSE]
    
    if (!nrow(x)) return(NULL)
    
    x[, c("parcel_id", "date", "amount_mm")]
  })
  
  out = bind_rows(pieces) |>
    mutate(
      event_type = "irrigation",
      parcel_id = bit64::as.integer64(as.character(parcel_id)),
      date = as.Date(date),
      amount_mm = as.numeric(amount_mm)
    ) |>
    select(event_type, parcel_id, date, amount_mm) |>
    arrange(parcel_id, date)
  
  if (nrow(out) && any(!complete.cases(out)))
    stop("Incomplete irrigation output for ", yy)
  
  if (nrow(out) && any(!is.finite(out$amount_mm) | out$amount_mm <= 0))
    stop("Invalid irrigation amounts for ", yy)
  
  path = file.path(config$output_dir, paste0("irrigation_statewide_", yy, ".parquet"))
  arrow::write_parquet(out, path, compression = "zstd")
  
  message("Wrote ", yy, ": ", format(nrow(out), big.mark = ","), " events")
  
  rm(out, pieces); gc()
}

message("DONE: ", config$output_dir)