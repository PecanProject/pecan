#using the 198 design points that have Cal-Adapt 2025-45 data already downloaded, and collapsing them into
#county-level daily climate - this uses the spatial assumption that every parcel in a county experiences the same daily climate

pacman::p_load(ncdf4, sf, dplyr, fs, purrr, furrr, future,
               lubridate, tigris, FAO56, parallelly)

#REQUIRED: Choose a folder where intermediate products will be saved
#work_root = "/path/to/your/folder"

#Shared Data: Shared project data, most users should not need to change this.
ccmmf_root = "/projectnb/dietzelab/ccmmf"

##where you want the final output to be saved to
output_dir = work_root

##the Cal-Adapt 2025-45 info for irrigation estimates are already downloaded
base_dir = file.path(ccmmf_root, "ensemble", "CalAdapt_runs", "data_raw", "CalAdaptWRF")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# ---- helpers ----
specific_humidity_to_ea = function(q, pressure_pa) {
  P_kpa = pressure_pa / 1000
  (q * P_kpa) / (0.622 + 0.378 * q)
}

calculate_net_radiation = function(sw_rad, lw_rad, temp_c, albedo = 0.23, emissivity = 0.98) {
  sigma = 5.67e-8
  Rns = (1 - albedo) * sw_rad
  Rnl = lw_rad - emissivity * sigma * (temp_c + 273.15)^4
  (Rns + Rnl) * 0.0036
}

parse_nc_time = function(time_steps, time_units, year_val) {
  origin_string = sub("^.*since\\s+", "", time_units)
  origin_time = lubridate::ymd_hms(origin_string, tz = "UTC", quiet = TRUE)
  
  if (is.na(origin_time)) {
    origin_time = lubridate::ymd_hms(sprintf("%d-01-01 00:00:00", year_val), tz = "UTC")
  }
  
  if (grepl("^seconds", time_units, ignore.case = TRUE)) {
    origin_time + lubridate::seconds(time_steps)
  } else if (grepl("^hours", time_units, ignore.case = TRUE)) {
    origin_time + lubridate::hours(time_steps)
  } else if (grepl("^days", time_units, ignore.case = TRUE)) {
    origin_time + lubridate::days(time_steps)
  } else {
    stop("Unknown NetCDF time units: ", time_units)
  }
}

calculate_daily_et0 = function(df_hourly, wind_height_m = 10) {
  df_daily = df_hourly |>
    dplyr::mutate(
      ea_kpa = specific_humidity_to_ea(.data$spec_hum, .data$pressure),
      Rn_MJ = calculate_net_radiation(.data$sw_rad, .data$lw_rad, .data$temp_c),
      date = as.Date(.data$datetime)
    ) |>
    dplyr::group_by(.data$date) |>
    dplyr::summarise(
      T_min = min(.data$temp_c, na.rm = TRUE),
      T_max = max(.data$temp_c, na.rm = TRUE),
      mean_temp_c = mean(.data$temp_c, na.rm = TRUE),
      R_n = sum(.data$Rn_MJ, na.rm = TRUE),
      e_a = mean(.data$ea_kpa, na.rm = TRUE),
      P_kpa = mean(.data$pressure / 1000, na.rm = TRUE),
      wind = mean(.data$wind_speed, na.rm = TRUE),
      precip_mm = sum(.data$precip_mm, na.rm = TRUE),
      .groups = "drop"
    )
  
  df_daily |>
    dplyr::rowwise() |>
    dplyr::mutate(
      ET0_mm = {
        gamma = 0.000665 * .data$P_kpa
        if (wind_height_m == 2) {
          FAO56::ETo_FPM(
            R_n = .data$R_n, G = 0, gamma = gamma, u_2 = .data$wind,
            e_a = .data$e_a, T_min = .data$T_min, T_max = .data$T_max
          )
        } else {
          FAO56::ETo_FPM(
            R_n = .data$R_n, G = 0, gamma = gamma, u_z = .data$wind,
            z = wind_height_m, e_a = .data$e_a, T_min = .data$T_min, T_max = .data$T_max
          )
        }
      }
    ) |>
    dplyr::ungroup()
}

process_nc_file = function(nc_path, wind_height_m = 10) {
  nc = tryCatch(
    ncdf4::nc_open(nc_path),
    error = function(e) {
      warning("Could not open: ", nc_path)
      NULL
    }
  )
  
  if (is.null(nc)) return(NULL)
  on.exit(ncdf4::nc_close(nc))
  
  file_name = basename(nc_path)
  site_hash = basename(dirname(nc_path))
  parts = unlist(strsplit(gsub("\\.nc$", "", file_name), "\\."))
  
  if (length(parts) < 3) {
    warning("Unexpected filename: ", file_name)
    return(NULL)
  }
  
  model_name = parts[1]
  scenario_value = parts[2]
  year_val = as.integer(parts[3])
  
  time_steps = as.numeric(ncdf4::ncvar_get(nc, "time"))
  temp_k = as.numeric(ncdf4::ncvar_get(nc, "air_temperature"))
  wind_speed = as.numeric(ncdf4::ncvar_get(nc, "wind_speed"))
  precip = as.numeric(ncdf4::ncvar_get(nc, "precipitation_flux"))
  spec_hum = as.numeric(ncdf4::ncvar_get(nc, "specific_humidity"))
  lw_rad = as.numeric(ncdf4::ncvar_get(nc, "surface_downwelling_longwave_flux_in_air"))
  sw_rad = as.numeric(ncdf4::ncvar_get(nc, "surface_downwelling_shortwave_flux_in_air"))
  pressure = as.numeric(ncdf4::ncvar_get(nc, "air_pressure"))
  time_units = ncdf4::ncatt_get(nc, "time", "units")$value
  datetime = parse_nc_time(time_steps, time_units, year_val)
  
  df_hourly = tibble::tibble(
    datetime = datetime, temp_c = temp_k - 273.15, wind_speed = wind_speed,
    sw_rad = sw_rad, lw_rad = lw_rad, spec_hum = spec_hum,
    pressure = pressure, precip_mm = precip * 3600
  )
  
  calculate_daily_et0(df_hourly, wind_height_m) |>
    dplyr::mutate(
      site_hash = .env$site_hash,
      model = .env$model_name,
      scenario = .env$scenario_value,
      year = lubridate::year(.data$date),
      day_of_year = lubridate::yday(.data$date)
    ) |>
    dplyr::select(
      "date", "site_hash", "model", "scenario", "ET0_mm", "precip_mm",
      "mean_temp_c", "T_min", "T_max", dplyr::everything()
    )
}

# ---------- geo assignments ----------
#convert the Cal-Adapt lat/lon locations into county assignments, attach the county to the climate records,
#and create a lookup table for the design points

site_dirs = dir_ls(base_dir, type = "directory")

##site_index will store each site_hash and their lat/lon info
site_index = map_dfr(site_dirs, function(dir_path) {
  nc_files = dir_ls(dir_path, glob = "*.nc")
  if (!length(nc_files)) return(NULL)
  
  nc = nc_open(nc_files[1])
  lat = as.numeric(ncvar_get(nc, "latitude"))
  lon = as.numeric(ncvar_get(nc, "longitude"))
  nc_close(nc)
  
  tibble(site_hash = basename(dir_path), lat = lat, lon = lon)
})

##just for a check, should say 198
cat(sprintf("Indexed %d unique Cal-Adapt design-point sites.\n", nrow(site_index)))

##get CA county info
ca_counties = counties(state = "CA", cb = TRUE, class = "sf") |> select(County = NAME)

##sites_sf now holds each site_hash's lat/lon and point geometry
sites_sf = st_as_sf(site_index, coords = c("lon", "lat"), crs = 4326, remove = FALSE)

#double check both layers use same CRS
ca_counties = st_transform(ca_counties, st_crs(sites_sf))

##assign each site hash its county name
sites_with_county = st_join(sites_sf, ca_counties, join = st_intersects, left = TRUE) |> st_drop_geometry()

##can see the counties that are still not assigned to a site
missing_counties = ca_counties |> filter(!County %in% sites_with_county$County)
print(missing_counties$County)

##assign whats leftover using nearest neighbor
sites_projected = st_transform(sites_sf, 3310)
missing_counties_projected = st_transform(missing_counties, 3310)
missing_county_points = st_point_on_surface(missing_counties_projected)
nearest_site_index = st_nearest_feature(missing_county_points, sites_projected)

missing_county_lookup = tibble(
  County = missing_counties$County,
  site_hash = sites_projected$site_hash[nearest_site_index]
)

internal_site_lookup = sites_with_county |> filter(!is.na(County)) |> select(County, site_hash)
county_site_lookup = bind_rows(internal_site_lookup, missing_county_lookup) |> distinct()

##now have a lookup table with sites and their county info
write.csv(county_site_lookup, file.path(output_dir, "caladapt_county_site_lookup.csv"), row.names = FALSE)

#double check every county now has at least one climate site
unmatched_counties = setdiff(ca_counties$County, county_site_lookup$County)
print(unmatched_counties)
if (length(unmatched_counties)) stop("Some counties still have no Cal-Adapt site assignment.")


# ---------- process all NetCDF files ----------
all_nc_files = dir_ls(base_dir, recurse = TRUE, glob = "*.nc")
cat(sprintf("Found %d NetCDF files.\n", length(all_nc_files)))

plan(multisession, workers = max(1, parallelly::availableCores() - 1))

daily_climate_dataset = future_map_dfr(all_nc_files, process_nc_file,
  wind_height_m = 10, .progress = TRUE, .options = furrr_options(seed = TRUE))


# ---------- attach counties to daily climate records ----------
daily_climate_county = daily_climate_dataset |> inner_join(county_site_lookup, by = "site_hash")

county_daily_climate = daily_climate_county |>
  group_by(County, date, model, scenario) |>
  summarise(
    ET0_mm = mean(ET0_mm, na.rm = TRUE),
    precip_mm = mean(precip_mm, na.rm = TRUE),
    mean_temp_c = mean(mean_temp_c, na.rm = TRUE),
    min_temp_c = mean(T_min, na.rm = TRUE),
    max_temp_c = mean(T_max, na.rm = TRUE),
    n_climate_sites = n_distinct(site_hash),
    .groups = "drop"
  ) |>
  mutate(Year = year(date), Week = isoweek(date)) |>
  rename(GCM = model, SSP = scenario)

write.csv(county_daily_climate, file.path(output_dir, "caladapt_county_daily_climate.csv"), row.names = FALSE)

message("DONE: ", file.path(output_dir, "caladapt_county_daily_climate.csv"))