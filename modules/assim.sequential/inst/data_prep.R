## Load packages
library(ncdf4)
library(data.table)
library(dplyr)
library(future.apply)
library(tidyr)
library(lubridate)
library(purrr)
library(kgc)
library(terra)
library(lutz)

##' @title read_annual_csv_series
##' @name read_annual_csv_series
##' @author Yang Gu
##'
##' @param years Integer vector of years.
##' @param path_template A character string template containing one `%d`
##'   placeholder for year, e.g. "/path/to/file_%d.csv".
##'
##' @description
##' Read a series of annual CSV files and return them as a list of data.tables.
##'
##' @return
##' A named list of data.tables, one per year.
read_annual_csv_series <- function(years, path_template) {
  out <- lapply(years, function(y) {
    fread(sprintf(path_template, y))
  })
  names(out) <- as.character(years)
  return(out)
}

##' @title bind_annual_tables
##' @name bind_annual_tables
##' @author Yang Gu
##'
##' @param dt_list A list of data.tables.
##' @param drop_year Logical; if TRUE and a `year` column exists, remove it.
##'
##' @description
##' Standardize and row-bind annual tables into one combined data.table.
##'
##' @return
##' A single combined data.table.
bind_annual_tables <- function(dt_list, drop_year = TRUE) {
  cleaned <- lapply(dt_list, function(df) {
    df <- as.data.table(df)
    if (drop_year && "year" %in% names(df)) {
      df[, year := NULL]
    }
    return(df)
  })
  out <- rbindlist(cleaned, use.names = TRUE, fill = TRUE)
  return(out)
}

##' @title extract_kgc_for_points
##' @name extract_kgc_for_points
##' @author Yang Gu
##'
##' @param coords_dt A data.table containing columns `lon`, `lat`, and `Y`.
##' @param kg_file Path to Köppen-Geiger raster.
##' @param kg_lookup Character vector mapping raster values to KGC classes.
##'
##' @description
##' Extract Köppen-Geiger climate classes for each coordinate point.
##'
##' @return
##' A data.table with columns `index`, `lon`, `lat`, and `KGC`.
extract_kgc_for_points <- function(coords_dt, kg_file, kg_lookup) {
  kg_raster <- rast(kg_file)
  coords_vect <- vect(coords_dt, geom = c("lon", "lat"), crs = "EPSG:4326")
  ex <- terra::extract(kg_raster, coords_vect, ID = FALSE)
  
  vals <- as.integer(ex[[1]])
  valid <- !is.na(vals) & vals >= 1 & vals <= length(kg_lookup)
  
  KGC <- rep(NA_character_, length(vals))
  KGC[valid] <- kg_lookup[vals[valid]]
  
  points_meta <- data.table(
    index = coords_dt$Y,
    lon   = coords_dt$lon,
    lat   = coords_dt$lat,
    KGC   = KGC
  )
  points_meta[, KGC := factor(KGC, levels = kg_lookup)]
  return(points_meta)
}

##' @title add_point_metadata_to_era
##' @name add_point_metadata_to_era
##' @author Yang Gu
##'
##' @param era_dt ERA5 data.table with `index`.
##' @param coords_file Path to coordinate CSV file.
##' @param kg_file Path to Köppen-Geiger raster.
##'
##' @description
##' Add point-level metadata, including lon/lat and KGC, to ERA5 records.
##'
##' @return
##' A list with:
##' \itemize{
##'   \item `era_all`: ERA data.table merged with metadata
##'   \item `points_meta`: point metadata table
##' }
add_point_metadata_to_era <- function(era_dt, coords_file, kg_file) {
  test_coords <- fread(coords_file)[, .(lon, lat, Y)]
  
  kg_lookup <- c(
    "Af","Am","Aw","BWh","BWk","BSh","BSk","Csa","Csb","Csc",
    "Cwa","Cwb","Cwc","Cfa","Cfb","Cfc","Dsa","Dsb","Dsc",
    "Dsd","Dwa","Dwb","Dwc","Dwd","Dfa","Dfb","Dfc","Dfd","ET","EF"
  )
  
  points_meta <- extract_kgc_for_points(
    coords_dt = test_coords,
    kg_file = kg_file,
    kg_lookup = kg_lookup
  )
  
  era_dt <- merge(
    era_dt,
    points_meta,
    by = "index",
    all.x = TRUE,
    allow.cartesian = TRUE
  )
  
  return(list(
    era_all = era_dt,
    points_meta = points_meta
  ))
}

##' @title expand_tiff_daily
##' @name expand_tiff_daily
##' @author Yang Gu
##'
##' @param tiff_file Path to TIFF-derived annual covariate CSV.
##'
##' @description
##' Expand annual/static TIFF-based covariates to daily resolution according
##' to the study year definition.
##'
##' @return
##' A daily-resolution data.table keyed by `index` and `Date`.
expand_tiff_daily <- function(tiff_file) {
  tiff <- fread(tiff_file)
  tiff <- as.data.table(tiff)
  
  tiff[, `:=`(
    start_date = fifelse(
      Year == 2012,
      as.Date("2012-01-01"),
      as.Date(paste0(Year - 1, "-07-16"))
    ),
    end_date = fifelse(
      Year == 2012,
      as.Date("2012-07-15"),
      as.Date(paste0(Year, "-07-15"))
    )
  )]
  
  tiff_daily <- tiff[, .(
    Date = seq(start_date[1], end_date[1], by = "day")
  ), by = .(
    Year, index, cell, cell_lon, cell_lat, diff_lon, diff_lat, distance,
    LC, twi, PH, Sand, agb, SOC, N, year_since_disturb
  )]
  
  return(tiff_daily)
}

##' @title read_soil_moisture_data
##' @name read_soil_moisture_data
##' @author Yang Gu
##'
##' @param soilm_file Path to soil moisture CSV.
##'
##' @description
##' Read and standardize soil moisture data.
##'
##' @return
##' A data.table with standardized columns `index` and `utc`.
read_soil_moisture_data <- function(soilm_file) {
  soilm <- fread(soilm_file)
  setnames(soilm, c("Y", "datetime"), c("index", "utc"))
  return(soilm)
}

##' @title add_daily_tiff_covariates
##' @name add_daily_tiff_covariates
##' @author Yang Gu
##'
##' @param envres Environmental data.table with `index` and `utc`.
##' @param tiff_daily Daily TIFF covariate data.table.
##'
##' @description
##' Merge daily TIFF covariates into the environmental table.
##'
##' @return
##' A merged data.table.
add_daily_tiff_covariates <- function(envres, tiff_daily) {
  envres[, Date := as.Date(utc)]
  setDT(envres)
  setDT(tiff_daily)
  setkey(envres, index, Date)
  setkey(tiff_daily, index, Date)
  
  out <- merge(
    envres,
    tiff_daily,
    by = c("index", "Date"),
    all.x = TRUE,
    sort = FALSE
  )
  return(out)
}

##' @title add_meteorological_derived_variables
##' @name add_meteorological_derived_variables
##' @author Yang Gu
##'
##' @param dt A data.table containing ERA5 meteorological variables.
##'
##' @description
##' Add derived meteorological variables such as PPFD, wind speed,
##' temperature in Celsius, relative humidity, VPD, and day/night indicator.
##'
##' @return
##' A data.table with derived variables appended.
add_meteorological_derived_variables <- function(dt) {
  dt[, `:=`(
    PPFD      = 0.45 * ssrd / 4.57 / (3 * 3600),
    WindSpeed = sqrt(u10^2 + v10^2),
    t_air_C   = t2m - 273.15,
    d_air_C   = d2m - 273.15
  )]
  
  dt[, `:=`(
    RH  = 100 * exp((17.625 * d_air_C) / (243.04 + d_air_C) -
                      (17.625 * t_air_C) / (243.04 + t_air_C)),
    e_s = 0.6108 * exp((17.27 * t_air_C) / (t_air_C + 237.3)),
    e_a = 0.6108 * exp((17.27 * d_air_C) / (d_air_C + 237.3)),
    is_day = as.integer(ssrd > 10)
  )]
  
  dt[, VPD := pmax(e_s - e_a, 0)]
  dt[, c("t_air_C", "d_air_C", "e_s", "e_a") := NULL]
  
  return(dt)
}

##' @title add_local_time_variables
##' @name add_local_time_variables
##' @author Yang Gu
##'
##' @param dt A data.table containing `index`, `lat.x`, `lon.x`, and `utc`.
##'
##' @description
##' Add local timezone, local datetime, and seasonal harmonic variables.
##'
##' @return
##' A data.table with local time variables appended.
add_local_time_variables <- function(dt) {
  site_coords <- unique(dt[, .(index, lat.x, lon.x)])
  site_coords[, tz := lutz::tz_lookup_coords(lat.x, lon.x, method = "accurate")]
  
  dt[site_coords, tz := i.tz, on = "index"]
  dt[, tlocal := with_tz(utc, tz = unique(tz)), by = tz]
  
  dt[, year_local := year(tlocal)]
  dt[, days_in_year := ifelse(leap_year(year_local), 366, 365)]
  dt[, doy := yday(tlocal) +
       hour(tlocal) / 24 +
       minute(tlocal) / 1440 +
       second(tlocal) / 86400]
  
  dt[, `:=`(
    sin_doy = sin(2 * pi * doy / days_in_year),
    cos_doy = cos(2 * pi * doy / days_in_year)
  )]
  
  cols_to_drop <- intersect(
    c("cell", "cell_lon", "cell_lat", "diff_lon", "diff_lat",
      "distance", "tz", "lat.y", "lon.y", "lon_index",
      "lat_index", "year_local", "days_in_year"),
    names(dt)
  )
  if (length(cols_to_drop) > 0) {
    dt[, (cols_to_drop) := NULL]
  }
  
  return(dt)
}

##' @title read_and_fill_modis_daily
##' @name read_and_fill_modis_daily
##' @author Yang Gu
##'
##' @param modis_files Character vector of MODIS CSV file paths.
##' @param start_date Start date.
##' @param end_date End date.
##'
##' @description
##' Read MODIS NDVI/EVI data and fill to daily resolution using nearest
##' available observations.
##'
##' @return
##' A data.table with columns `index`, `Date`, `EVI`, and `NDVI`.
read_and_fill_modis_daily <- function(modis_files,
                                      start_date = as.Date("2012-01-01"),
                                      end_date   = as.Date("2024-12-31")) {
  dat <- rbindlist(lapply(modis_files, fread), use.names = TRUE, fill = TRUE)
  
  setnames(
    dat,
    old = c("ID", "Latitude", "Longitude",
            "MYD13Q1_061__250m_16_days_EVI",
            "MYD13Q1_061__250m_16_days_NDVI"),
    new = c("index", "lat", "lon", "EVI", "NDVI")
  )
  
  dat <- dat[, .(index, Date, EVI, NDVI)]
  
  unique_idx <- unique(dat$index)
  all_days <- data.table(
    index = rep(unique_idx, each = as.integer(end_date - start_date + 1)),
    Date  = rep(seq(start_date, end_date, by = "day"), times = length(unique_idx)),
    lat   = NA_real_,
    lon   = NA_real_,
    EVI   = NA_real_,
    NDVI  = NA_real_
  )
  
  setkey(dat, index, Date)
  setkey(all_days, index, Date)
  
  all_days[, Date_cal := Date]
  filled <- dat[all_days, on = .(index, Date), roll = "nearest"]
  final_df <- filled[, .(index, Date = Date_cal, EVI, NDVI)]
  
  return(final_df)
}

##' @title add_modis_covariates
##' @name add_modis_covariates
##' @author Yang Gu
##'
##' @param envres Environmental result table.
##' @param modis_dt Daily MODIS table.
##'
##' @description
##' Merge daily MODIS EVI/NDVI data into the environmental result table.
##'
##' @return
##' A merged data.table.
add_modis_covariates <- function(envres, modis_dt) {
  out <- merge(
    envres,
    modis_dt,
    by = c("index", "Date"),
    all.x = TRUE
  )
  return(out)
}

##' @title read_sipnet_flux_outputs
##' @name read_sipnet_flux_outputs
##' @author Yang Gu
##'
##' @param years Integer vector of years.
##' @param path_template File path template for SIPNET annual CSV files.
##'
##' @description
##' Read and combine annual SIPNET flux output files.
##'
##' @return
##' A data.table with columns `utc`, `index`, `NEE_mean`, `Qle_mean`, `GPP_mean`.
read_sipnet_flux_outputs <- function(years, path_template) {
  sipnet_list <- read_annual_csv_series(years, path_template)
  sipnet_all <- bind_annual_tables(sipnet_list, drop_year = TRUE)
  
  setnames(sipnet_all, "time", "utc")
  output.df <- sipnet_all[, .(utc, index, NEE_mean, Qle_mean, GPP_mean)]
  
  return(output.df)
}

##' @title add_sipnet_outputs
##' @name add_sipnet_outputs
##' @author Yang Gu
##'
##' @param envres Environmental result table.
##' @param sipnet_dt SIPNET output table.
##'
##' @description
##' Merge SIPNET outputs into the environmental result table and convert
##' NEE units to observation-compatible units.
##'
##' @return
##' A merged data.table.
add_sipnet_outputs <- function(envres, sipnet_dt) {
  out <- merge(
    envres,
    sipnet_dt[, .(utc, index, NEE_mean, Qle_mean, GPP_mean)],
    by = c("utc", "index"),
    all.x = TRUE,
    sort = FALSE
  )
  
  out[, NEE_mean := NEE_mean * 1e8 / 1.157407]
  return(out)
}

##' @title read_and_match_ec_data
##' @name read_and_match_ec_data
##' @author Yang Gu
##'
##' @param ec_file Path to eddy covariance data CSV.
##' @param matched_file Path to matched point-site CSV.
##'
##' @description
##' Read eddy covariance data, fill missing flux variables using fallback
##' columns, and match site IDs to spatial indices.
##'
##' @return
##' A data.table with columns matched to `utc` and `index`.
read_and_match_ec_data <- function(ec_file, matched_file) {
  resimet.df <- fread(ec_file)
  close_points_df <- fread(matched_file)
  
  setDT(close_points_df)
  close_points_df <- close_points_df[order(min_dist_m), .SD[1], by = index]
  
  resimet.df[is.na(NEE_CUT_USTAR50) & !is.na(NEE_VUT_USTAR50),
             NEE_CUT_USTAR50 := NEE_VUT_USTAR50]
  resimet.df[is.na(GPP_NT_CUT_USTAR50) & !is.na(GPP_NT_VUT_USTAR50),
             GPP_NT_CUT_USTAR50 := GPP_NT_VUT_USTAR50]
  
  resimet.df <- resimet.df[, .(
    utc, NEE_CUT_USTAR50, LE_F_MDS, GPP_NT_CUT_USTAR50, Site_ID
  )]
  
  site_index_map <- close_points_df[, .(Site_ID, index)]
  
  resimet.df <- merge(
    resimet.df,
    site_index_map,
    by = "Site_ID",
    all.x = TRUE
  )
  
  resimet.df <- resimet.df[, .SD[1], by = .(utc, index)]
  return(resimet.df)
}

##' @title add_ec_observations_and_residuals
##' @name add_ec_observations_and_residuals
##' @author Yang Gu
##'
##' @param envres Environmental result table.
##' @param ec_dt Eddy covariance table.
##'
##' @description
##' Merge EC observations into the environmental result table and compute
##' residuals between SIPNET outputs and EC observations.
##'
##' @return
##' A data.table with residual columns added.
add_ec_observations_and_residuals <- function(envres, ec_dt) {
  out <- merge(
    envres,
    ec_dt,
    by = c("utc", "index"),
    all.x = TRUE,
    sort = FALSE
  )
  
  out[, `:=`(
    NEE_residual = NEE_mean - NEE_CUT_USTAR50,
    LE_residual  = Qle_mean - LE_F_MDS,
    GPP_residual = GPP_mean - GPP_NT_CUT_USTAR50
  )]
  
  out <- out[, .SD[1], by = .(utc, index)]
  return(out)
}

##' @title finalize_envres_columns
##' @name finalize_envres_columns
##' @author Yang Gu
##'
##' @param dt Final environmental result table.
##'
##' @description
##' Clean up duplicated coordinate columns and standardize final names.
##'
##' @return
##' A cleaned data.table ready for saving.
finalize_envres_columns <- function(dt) {
  drop_cols <- intersect(c("lat.y", "lon.y"), names(dt))
  if (length(drop_cols) > 0) {
    dt[, (drop_cols) := NULL]
  }
  
  rename_old <- intersect(c("lat.x", "lon.x"), names(dt))
  rename_new <- c("lat", "lon")[c("lat.x", "lon.x") %in% rename_old]
  
  if (length(rename_old) > 0) {
    setnames(dt, rename_old, rename_new)
  }
  
  return(dt)
}

##' @title build_envres_dataset
##' @name build_envres_dataset
##' @author Yang Gu
##'
##' @param years Integer vector of years to process.
##' @param era_template File path template for ERA annual CSV files.
##' @param coords_file Path to coordinate CSV.
##' @param kg_file Path to Köppen-Geiger raster.
##' @param tiff_file Path to TIFF annual covariate CSV.
##' @param soilm_file Path to soil moisture CSV.
##' @param modis_files Character vector of MODIS CSV files.
##' @param sipnet_template File path template for SIPNET annual CSV files.
##' @param ec_file Path to eddy covariance CSV.
##' @param matched_file Path to matched spatial index CSV.
##'
##' @description
##' Build the full environmental modeling dataset by combining ERA5,
##' soil moisture, TIFF-derived covariates, MODIS, SIPNET outputs, and
##' eddy covariance observations.
##'
##' @return
##' A final integrated data.table.
build_envres_dataset <- function(
    years,
    era_template,
    coords_file,
    kg_file,
    tiff_file,
    soilm_file,
    modis_files,
    sipnet_template,
    ec_file,
    matched_file
) {
  ## ERA5
  era_list <- read_annual_csv_series(years, era_template)
  era_all <- bind_annual_tables(era_list, drop_year = TRUE)
  rm(era_list)
  gc()
  
  era_meta <- add_point_metadata_to_era(
    era_dt = era_all,
    coords_file = coords_file,
    kg_file = kg_file
  )
  era_all <- era_meta$era_all
  points_meta <- era_meta$points_meta
  gc()
  
  ## TIFF
  tiff_daily <- expand_tiff_daily(tiff_file)
  gc()
  
  ## Soil moisture
  soilm <- read_soil_moisture_data(soilm_file)
  
  ## Merge ERA + soil moisture
  envres <- merge(era_all, soilm, by = c("index", "utc"), all.x = TRUE)
  rm(era_all, soilm)
  gc()
  
  ## Add daily TIFF covariates
  envres <- add_daily_tiff_covariates(envres, tiff_daily)
  rm(tiff_daily)
  gc()
  
  ## Derived meteorological variables
  envres <- add_meteorological_derived_variables(envres)
  gc()
  
  ## Local time and seasonal harmonics
  envres <- add_local_time_variables(envres)
  gc()
  
  ## MODIS
  modis_dt <- read_and_fill_modis_daily(modis_files)
  envres <- add_modis_covariates(envres, modis_dt)
  rm(modis_dt)
  gc()
  
  ## SIPNET
  sipnet_dt <- read_sipnet_flux_outputs(years, sipnet_template)
  envres <- add_sipnet_outputs(envres, sipnet_dt)
  rm(sipnet_dt)
  gc()
  
  ## Eddy covariance
  ec_dt <- read_and_match_ec_data(ec_file, matched_file)
  envres <- add_ec_observations_and_residuals(envres, ec_dt)
  rm(ec_dt, points_meta)
  gc()
  
  ## Final cleanup
  envres <- finalize_envres_columns(envres)
  gc()
  
  return(envres)
}

##' @title save_envres_dataset
##' @name save_envres_dataset
##' @author Yang Gu
##'
##' @param envres Final environmental result data.table.
##' @param output_file Output `.RData` file path.
##'
##' @description
##' Save the final dataset to an `.RData` file.
##'
##' @return
##' No return value. Saves `envres` to disk.
save_envres_dataset <- function(envres, output_file) {
  save(envres, file = output_file)
}