setwd("/projectnb/dietzelab/ananyak")

library(data.table)
library(arrow)
library(sf)
library(tigris)

options(tigris_use_cache = TRUE)
options(arrow.unsafe_metadata = TRUE)

phenology_dir = "/projectnb/dietzelab/ccmmf/management/phenology/matched_landiq_mslsp_v4.1"

##-----loading files-----
#assigned parquets
phenology_files = list.files(phenology_dir, pattern = "^assigned_year=.*\\.parquet$",
                             full.names = TRUE)

#combining them all 
assigned_all = rbindlist(lapply(phenology_files, function(f) {
    dt = as.data.table(read_parquet(f))
    #pull year from filename if year column is not already there
    yr = as.integer(gsub(".*assigned_year=([0-9]{4})\\.parquet$", "\\1", f))
    dt[, source_year := yr]
    
    return(dt)}),
  fill = TRUE)

##-----inspect-----
names(assigned_all)

View(assigned_all[,
  .(
    parcel_id, source_year, season, mslsp_cycle, landiq_CLASS, landiq_SUBCLASS, landiq_ADOY, mslsp_OGI, 
    mslsp_OGMn, mslsp_OGD, qc_adoy_vs_cycle, qc_cycle_status)])

##-----merge phenology file info with crops&county info-----
crops_and_counties = fread("/projectnb/dietzelab/ananyak/crops_full_counties.csv")

if ("V1" %in% names(crops_and_counties)) {crops_and_counties[, V1 := NULL]}

parcel_county_lookup = unique(crops_and_counties[!is.na(county), .(parcel_id, county, county_geoid)])

assigned_all[, parcel_id := as.character(parcel_id)]
parcel_county_lookup[, parcel_id := as.character(parcel_id)]

assigned_county = merge(assigned_all, parcel_county_lookup, by = "parcel_id", all.x = TRUE)

##-----convert Date columns to day-of-year-----
assigned_county[, planting_doy := as.integer(format(mslsp_OGI, "%j"))]
assigned_county[, harvest_doy  := as.integer(format(mslsp_OGMn, "%j"))]
assigned_county[, peak_doy     := as.integer(format(mslsp_Peak, "%j"))]

#most likely already numeric but set just incase
assigned_county[, landiq_ADOY := as.numeric(landiq_ADOY)]

##-----helper function that doesnt wrap if the dates being used go through new years -----
#normal stats are skewed when dates cluster around New Year - ex) 365 and 5 are actually close but 
#numerically far apart

fix_wrap_doy = function(x, low_cutoff = 45, high_cutoff = 320) {
  x = x[!is.na(x)]
  
  if (length(x) == 0) return(x)
  
  wraps = quantile(x, 0.05, na.rm = TRUE) <= low_cutoff &&
    quantile(x, 0.95, na.rm = TRUE) >= high_cutoff
  
  if (wraps) {x = fifelse(x <= low_cutoff, x + 365, x)} #treat january dates as after december dates
  
  return(x)}

wrap_back_doy = function(x) {((round(x) - 1) %% 365) + 1}
 
##-----then can build table with means and sd's-----
assigned_county[, crop_type := fifelse(
  !is.na(landiq_SUBCLASS) & landiq_SUBCLASS != "",
  paste0(landiq_CLASS, "_", landiq_SUBCLASS),
  as.character(landiq_CLASS))]

mvp = assigned_county[
  !is.na(county) &
    !is.na(season) &
    !is.na(crop_type) &
    source_year >= 2018 &
    source_year <= 2023,
  {
    planting_adj = fix_wrap_doy(planting_doy)
    harvest_adj  = fix_wrap_doy(harvest_doy)
    peak_adj     = fix_wrap_doy(peak_doy)
    
    .(
      n_rows = .N, n_planting = sum(!is.na(planting_doy)), n_harvest  = sum(!is.na(harvest_doy)), n_peak = sum(!is.na(peak_doy)),
      
      planting_wraparound = length(planting_adj) > 0 && max(planting_adj, na.rm = TRUE) > 365,
      harvest_wraparound  = length(harvest_adj) > 0 && max(harvest_adj, na.rm = TRUE) > 365,
      
      mean_planting_doy = wrap_back_doy(mean(planting_adj, na.rm = TRUE)),
      sd_planting_doy   = sd(planting_adj, na.rm = TRUE),
      q05_planting_doy  = wrap_back_doy(quantile(planting_adj, 0.05, na.rm = TRUE)),
      q50_planting_doy  = wrap_back_doy(quantile(planting_adj, 0.50, na.rm = TRUE)),
      q95_planting_doy  = wrap_back_doy(quantile(planting_adj, 0.95, na.rm = TRUE)),
      
      mean_harvest_doy = wrap_back_doy(mean(harvest_adj, na.rm = TRUE)),
      sd_harvest_doy   = sd(harvest_adj, na.rm = TRUE),
      q05_harvest_doy  = wrap_back_doy(quantile(harvest_adj, 0.05, na.rm = TRUE)),
      q50_harvest_doy  = wrap_back_doy(quantile(harvest_adj, 0.50, na.rm = TRUE)),
      q95_harvest_doy  = wrap_back_doy(quantile(harvest_adj, 0.95, na.rm = TRUE)),
      
      mean_peak_doy = wrap_back_doy(mean(peak_adj, na.rm = TRUE)),
      sd_peak_doy   = sd(peak_adj, na.rm = TRUE))},
  by = .(county, county_geoid, season, crop_type, landiq_CLASS, landiq_SUBCLASS)]

setorder(mvp, county, season, crop_type)
#View(mvp)
#write.csv(mvp, 'mvp_draft1.csv')

pheno = assigned_all[
  year >= 2018 & year <= 2023 &
    assigned_by == "matched" &
    !is.na(mslsp_OGI) &
    !is.na(mslsp_OGMn) &
    !is.na(parcel_id),
  .(
    parcel_id = as.character(parcel_id), year = as.integer(year), season, planting_date = as.IDate(mslsp_OGI),
    harvest_date = as.IDate(mslsp_OGMn), peak_date = as.IDate(mslsp_Peak), landiq_CLASS,landiq_SUBCLASS)]


##-----assign all BAU and NBS predicted files planting and harvesting dates-----
#loop through the two subfolders in county_landiq_predictions and assign mean dates to each 
#countys original file 

#helper: modal value
mode_value = function(x) {
  x = x[!is.na(x)]
  if (length(x) == 0) return(NA)
  names(sort(table(x), decreasing = TRUE))[1]}

#build crop_type in prediction files the same way as mvp
make_crop_type = function(dt) {
  dt[, crop_type := fifelse(
    !is.na(SUBCLASS) & SUBCLASS != "",
    paste0(CLASS, "_", SUBCLASS),
    as.character(CLASS))]
  return(dt)}

#convert DOY to actual date in prediction year
doy_to_date = function(year, doy) {as.IDate(as.Date(paste0(year, "-01-01")) + as.integer(round(doy)) - 1L)}

normalize_geoid = function(x) {
  x = as.character(x)
  x = gsub("\\.0$", "", x)
  x = fifelse(is.na(x) | x == "" | x == "NA", NA_character_, x)
  x = fifelse(!is.na(x), sprintf("%05s", x), NA_character_)
  return(x)}

mvp[, county_geoid := normalize_geoid(county_geoid)]

#county + crop_type lookup, collapsing across season
pheno_lookup_county_crop = mvp[
  !is.na(county) &
    !is.na(crop_type) &
    !is.na(mean_planting_doy) &
    !is.na(mean_harvest_doy),
  .(
    assigned_season = mode_value(season),
    
    mean_planting_doy = round(weighted.mean(mean_planting_doy, w = n_planting, na.rm = TRUE)),
    mean_harvest_doy  = round(weighted.mean(mean_harvest_doy,  w = n_harvest,  na.rm = TRUE)),
    mean_peak_doy     = round(weighted.mean(mean_peak_doy,     w = n_peak,     na.rm = TRUE)),
    
    sd_planting_doy = weighted.mean(sd_planting_doy, w = n_planting, na.rm = TRUE),
    sd_harvest_doy  = weighted.mean(sd_harvest_doy,  w = n_harvest,  na.rm = TRUE),
    sd_peak_doy     = weighted.mean(sd_peak_doy,     w = n_peak,     na.rm = TRUE),
    
    n_pheno_rows = sum(n_rows, na.rm = TRUE),
    n_planting = sum(n_planting, na.rm = TRUE),
    n_harvest = sum(n_harvest, na.rm = TRUE),
    n_peak = sum(n_peak, na.rm = TRUE),
    
    planting_wraparound = any(planting_wraparound, na.rm = TRUE),
    harvest_wraparound = any(harvest_wraparound, na.rm = TRUE)
  ),
  by = .(county, county_geoid, crop_type, landiq_CLASS, landiq_SUBCLASS)]

#broader fallback: crop_type only, across all counties
pheno_lookup_crop = mvp[
  !is.na(crop_type) &
    !is.na(mean_planting_doy) &
    !is.na(mean_harvest_doy),
  .(
    fallback_season = mode_value(season),
    
    fallback_planting_doy = round(weighted.mean(mean_planting_doy, w = n_planting, na.rm = TRUE)),
    fallback_harvest_doy  = round(weighted.mean(mean_harvest_doy,  w = n_harvest,  na.rm = TRUE)),
    fallback_peak_doy     = round(weighted.mean(mean_peak_doy,     w = n_peak,     na.rm = TRUE)),
    
    fallback_sd_planting_doy = weighted.mean(sd_planting_doy, w = n_planting, na.rm = TRUE),
    fallback_sd_harvest_doy  = weighted.mean(sd_harvest_doy,  w = n_harvest,  na.rm = TRUE),
    fallback_sd_peak_doy     = weighted.mean(sd_peak_doy,     w = n_peak,     na.rm = TRUE),
    
    fallback_n_pheno_rows = sum(n_rows, na.rm = TRUE),
    fallback_planting_wraparound = any(planting_wraparound, na.rm = TRUE),
    fallback_harvest_wraparound = any(harvest_wraparound, na.rm = TRUE)
  ),
  by = .(crop_type, landiq_CLASS, landiq_SUBCLASS)]

scenario_dirs = c("BAU", "NBS_Targets")

for (scen in scenario_dirs) {
  
  input_dir = file.path("county_landiq_predictions", scen)
  output_dir = file.path("county_landiq_predictions_with_phenology", scen)
  
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  pred_files = list.files(input_dir, pattern = paste0("_predicted_2024_2045", "\\.csv$"), full.names = TRUE)
  
  message("Processing ", scen, ": ", length(pred_files), " files")
  
  for (f in pred_files) {
    
    message("  Reading: ", basename(f))
    
    pred = fread(f)
    
    pred[, `:=`(
      parcel_id = as.character(parcel_id), county = as.character(county), county_geoid = normalize_geoid(county_geoid),
      CLASS = as.character(CLASS), SUBCLASS = as.character(SUBCLASS), year = as.integer(year))]
    
    pred[SUBCLASS == "" | SUBCLASS == "NA", SUBCLASS := NA_character_]
    pred = make_crop_type(pred)
    
    #merge county-specific phenology first
    pred = merge(pred, pheno_lookup_county_crop, by.x = c("county", "county_geoid", "crop_type", "CLASS", "SUBCLASS"),
      by.y = c("county", "county_geoid", "crop_type", "landiq_CLASS", "landiq_SUBCLASS"), all.x = TRUE)
    
    #merge crop-only fallback
    pred = merge(pred, pheno_lookup_crop, by.x = c("crop_type", "CLASS", "SUBCLASS"),
                 by.y = c("crop_type", "landiq_CLASS", "landiq_SUBCLASS"), all.x = TRUE)
    
    #use county-specific first, fallback second
    pred[is.na(mean_planting_doy), mean_planting_doy := fallback_planting_doy]
    pred[is.na(mean_harvest_doy),  mean_harvest_doy  := fallback_harvest_doy]
    pred[is.na(mean_peak_doy),     mean_peak_doy     := fallback_peak_doy]
    
    pred[is.na(sd_planting_doy), sd_planting_doy := fallback_sd_planting_doy]
    pred[is.na(sd_harvest_doy),  sd_harvest_doy  := fallback_sd_harvest_doy]
    pred[is.na(sd_peak_doy),     sd_peak_doy     := fallback_sd_peak_doy]
    
    pred[is.na(n_pheno_rows), n_pheno_rows := fallback_n_pheno_rows]
    
    pred[is.na(planting_wraparound), planting_wraparound := fallback_planting_wraparound]
    pred[is.na(harvest_wraparound),  harvest_wraparound  := fallback_harvest_wraparound]
    
    #assign season if predicted season is currently NA
    if ("season" %in% names(pred)) {
      pred[is.na(season), season := assigned_season]
      pred[is.na(season), season := fallback_season]
    } else {
      pred[, season := assigned_season]
      pred[is.na(season), season := fallback_season]}
    
    #actual predicted dates
    pred[, planting_date := doy_to_date(year, mean_planting_doy)]
    pred[, peak_date     := doy_to_date(year, mean_peak_doy)]
    pred[, harvest_date  := doy_to_date(year, mean_harvest_doy)]
    
    #if harvest wraps after New Year, push harvest into next calendar year
    pred[
      !is.na(planting_date) &
        !is.na(harvest_date) &
        harvest_date < planting_date,
      harvest_date := doy_to_date(year + 1L, mean_harvest_doy)]
    
    #same logic for peak if needed
    pred[
      !is.na(planting_date) &
        !is.na(peak_date) &
        peak_date < planting_date &
        !is.na(harvest_date) &
        harvest_date > planting_date,
      peak_date := doy_to_date(year + 1L, mean_peak_doy)]
    
    pred[, phenology_source := fifelse(
      !is.na(assigned_season),
      "county_crop_subclass_mean_2018_2023",
      fifelse(!is.na(fallback_season), "crop_subclass_mean_2018_2023", NA_character_))]
    
    #remove fallback helper columns
    drop_cols = c("fallback_season", "fallback_planting_doy", "fallback_harvest_doy", "fallback_peak_doy", 
                  "fallback_sd_planting_doy", "fallback_sd_harvest_doy", "fallback_sd_peak_doy", "fallback_n_pheno_rows",
                  "fallback_planting_wraparound", "fallback_harvest_wraparound")
    
    drop_cols = intersect(drop_cols, names(pred))
    if (length(drop_cols) > 0) {pred[, (drop_cols) := NULL]}
    
    #optional: put key columns near front
    front_cols = c("parcel_id", "county", "county_geoid", "county_safe", "year", "CLASS", "SUBCLASS", 
                   "CLASS_desc", "SUBCLASS_desc", "PFT", "planting_date", "peak_date", "harvest_date",
                   "mean_planting_doy", "mean_peak_doy", "mean_harvest_doy", "till_state", "prob_crop_class", "prob_till_state",
                   "ACRES", "source", "scenario", "phenology_source")
    
    front_cols = intersect(front_cols, names(pred))
    other_cols = setdiff(names(pred), front_cols)
    setcolorder(pred, c(front_cols, other_cols))
    
    out_path = file.path(output_dir, basename(f))
    fwrite(pred, out_path)}
  
  message("Finished phenology assignment for: ", scen)}