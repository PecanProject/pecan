#### Use for FIA configuration runs (otherwise use the assembler) #####

###############################################################################
# Obs_Prep.R
# Prepares obs.mean and obs.cov for all 7 SDA run configurations.
#
# Standard variables (LandTrendr AGB, MODIS LAI, SMAP SMP, SoilGrids SoilC)
# are extracted by calling PEcAn prep functions directly.
# FIA variables (FIA_AGB, GWBI) are handled by custom prep functions below.
#
# Units:
#   LandTrendr AGB -> Mg C/ha  (converted from Mg biomass/ha: x 0.5)
#   FIA AGB        -> Mg C/ha  (converted from Mg biomass/ha: x 0.5)
#   FIA GWBI       -> Mg C/ha/yr  (converted from Mg biomass/ha/yr: x 0.5)
# Carbon fraction 0.5 converts biomass to carbon to match SIPNET model state units.
#
# obs.mean format (required by PEcAn's new obs prep pipeline):
#   obs.mean[[t]][[site]] is a 1-ROW DATA.FRAME where:
#     - column names are state variable names (e.g. "AbvGrndWood", "LAI")
#     - each column has attr(col, "source") = XML <variables> tag name exactly
#       (e.g. attr(df$AbvGrndWood, "source") = "LandTrendr")
#     - for dual-constraint (+LT_FIA), the data.frame has TWO "AbvGrndWood"
#       columns with different source attributes — both constrain simultaneously
#
# obs.cov format:
#   obs.cov[[t]][[site]] is a square matrix (or scalar for 1 obs).
#   Diagonal for independent sources; full 2x2 block for FIA_AGB+GWBI in "+FIA_GWBI".
#   (obs.cov format is unchanged from the original PEcAn format)
#
# Source attribute values must match XML <variables> tag names exactly:
#   LandTrendr AGB -> "LandTrendr"
#   FIA AGB        -> "FIA_AGB"
#   MODIS LAI      -> "MODIS"
#   SMAP SMP       -> "SMAP"
#   SoilGrids SoilC-> "SoilGrids"
#   FIA GWBI       -> "FIA_GWBI"
#
# Covariance rules:
#   All obs: diagonal (independent), EXCEPT
#   +FIA_GWBI: full 2x2 covariance between AbvGrndWood and GWBI from ensemble
#   TotSoilCarb variance multiplied by number of time steps (SoilGrids is static)
#
# NOTE on SMAP:
#   SMAP_SMP_prep requires Inputs/Obs/SMAP_gee.csv from Google Earth Engine.
#   GEE script: https://code.earthengine.google.com/ecbeb770e576d8ef72f72f5f12da3496
#
# Run configurations:
#   base      : LAI + SMP + SoilC
#   +LT       : base + LandTrendr AGB
#   +FIA      : base + FIA AGB
#   +GWBI     : base + FIA GWBI
#   +LT_FIA   : base + LandTrendr AGB + FIA AGB (TRUE dual constraint)
#   +LT_GWBI  : base + LandTrendr AGB + FIA GWBI
#   +FIA_GWBI : base + FIA AGB + FIA GWBI (full AGB-GWBI covariance)
###############################################################################

library(terra)
library(future)
library(randomForest)
library(PEcAn.settings)
library(PEcAn.data.remote)
library(PEcAn.data.land)
library(PEcAnAssimSequential)
library(lubridate)
library(dplyr)
library(purrr)

# 1. Settings and paths

settings_path   <- "/projectnb/dietzelab/cwebb16/SDA/pecan.xml"
outdir          <- "/projectnb/dietzelab/cwebb16/SDA/Inputs/Obs"
FIA_AGB_path    <- "/projectnb/dietzelab/cwebb16/SDA/Inputs/FIA_Data/AGB_Mg_ha_all_plot_rcp45_full.rds"
FIA_GWBI_path   <- "/projectnb/dietzelab/cwebb16/SDA/Inputs/FIA_Data/GWBI_Mg_ha_all_plot_rcp45_full.rds"
corecoords_path <- "/projectnb/dietzelab/cwebb16/SDA/Inputs/corecoords.RData"
LT_AGB_indir    <- "/projectnb/dietzelab/dongchen/Multi-site/download_500_sites/AGB"

MgHa_to_kgm2 <- 0.5   # Mg biomass/ha -> Mg C/ha (carbon fraction 0.5)

start_date <- "2012-07-15"
end_date   <- "2024-07-15"
timestep   <- list(unit = "year", num = 1)

dir.create(file.path(outdir, "Rdata"), recursive = TRUE, showWarnings = FALSE)

# 2. Load settings and extract site_info

settings <- PEcAn.settings::read.settings(settings_path)

site_info <- settings$run %>%
  purrr::map('site') %>%
  purrr::map(function(s) {
    list(site_id   = s$id,
         lat       = as.numeric(s$lat),
         lon       = as.numeric(s$lon),
         site_name = s$name)
  }) %>%
  dplyr::bind_rows() %>%
  as.list()

time_points <- PEcAnAssimSequential::obs_timestep2timepoint(start_date, end_date, timestep)

# 3. FIA prep functions

#' Prepare FIA AGB data for SDA
#'
#' @param site_info  Bety-style site info list (site_id, lat, lon)
#' @param time_points Date vector of time points
#' @param FIA_AGB_path Path to AGB RDS array [n_cn x n_ens x n_years], Mg/ha
#' @param corecoords_path Path to corecoords.RData (FilteredPLOTCore: CN, LAT, LON)
#' @param unit_conv  Mg/ha -> kg/m^2 conversion (default 0.1)
#' @param outdir     Optional CSV export directory
#' @param export_csv Save CSV?
#' @return list(AGB_Output df, time_points, var = "AbvGrndWood")
#'   Columns: site_id, {tp}_AbvGrndWood, {tp}_SD
FIA_AGB_prep <- function(site_info, time_points, FIA_AGB_path, corecoords_path,
                         unit_conv = 0.1, outdir = NULL, export_csv = FALSE) {
  message("FIA AGB Prep: loading data...")
  AGB_array   <- readRDS(FIA_AGB_path)
  cn_ids      <- dimnames(AGB_array)[[1]]
  years_avail <- dimnames(AGB_array)[[3]]

  cc_env <- new.env()
  load(corecoords_path, envir = cc_env)
  corecoords <- cc_env$FilteredPLOTCore

  years     <- as.character(lubridate::year(time_points))
  col_names <- c("site_id",
                 paste0(time_points, "_AbvGrndWood"),
                 paste0(time_points, "_SD"))
  AGB_Output <- as.data.frame(matrix(NA, length(site_info$site_id), length(col_names)))
  colnames(AGB_Output) <- col_names
  AGB_Output$site_id <- site_info$site_id

  for (i in seq_along(site_info$site_id)) {
    cn_idx <- which(abs(corecoords$LAT - site_info$lat[i]) < 0.001 &
                    abs(corecoords$LON - site_info$lon[i]) < 0.001)
    if (length(cn_idx) == 0) next
    cn <- as.character(corecoords$CN[cn_idx[1]])
    if (!cn %in% cn_ids) next

    for (j in seq_along(time_points)) {
      yr <- years[j]
      if (!yr %in% years_avail) next
      vals <- AGB_array[cn, , yr] * unit_conv
      AGB_Output[i, paste0(time_points[j], "_AbvGrndWood")] <- mean(vals, na.rm = TRUE)
      AGB_Output[i, paste0(time_points[j], "_SD")]          <- sd(vals,   na.rm = TRUE)
    }
  }

  if (as.logical(export_csv) && !is.null(outdir))
    utils::write.csv(AGB_Output, file.path(outdir, "FIA_AGB.csv"), row.names = FALSE)

  message("FIA AGB Prep Completed!")
  list(AGB_Output = AGB_Output, time_points = time_points, var = "AbvGrndWood")
}


#' Prepare FIA GWBI data for SDA
#'
#' @param site_info  Bety-style site info list
#' @param time_points Date vector
#' @param FIA_GWBI_path Path to GWBI RDS array [n_cn x n_ens x n_years], Mg/ha
#' @param corecoords_path Path to corecoords.RData
#' @param unit_conv  Mg/ha -> kg/m^2/yr (default 0.1)
#' @param outdir     Optional CSV export directory
#' @param export_csv Save CSV?
#' @return list(GWBI_Output df, time_points, var = "GWBI")
#'   Columns: site_id, {tp}_GWBI, {tp}_SD
FIA_GWBI_prep <- function(site_info, time_points, FIA_GWBI_path, corecoords_path,
                          unit_conv = 0.1, outdir = NULL, export_csv = FALSE) {
  message("FIA GWBI Prep: loading data...")
  GWBI_array  <- readRDS(FIA_GWBI_path)
  cn_ids      <- dimnames(GWBI_array)[[1]]
  years_avail <- dimnames(GWBI_array)[[3]]

  cc_env <- new.env()
  load(corecoords_path, envir = cc_env)
  corecoords <- cc_env$FilteredPLOTCore

  years     <- as.character(lubridate::year(time_points))
  col_names <- c("site_id",
                 paste0(time_points, "_GWBI"),
                 paste0(time_points, "_SD"))
  GWBI_Output <- as.data.frame(matrix(NA, length(site_info$site_id), length(col_names)))
  colnames(GWBI_Output) <- col_names
  GWBI_Output$site_id <- site_info$site_id

  for (i in seq_along(site_info$site_id)) {
    cn_idx <- which(abs(corecoords$LAT - site_info$lat[i]) < 0.001 &
                    abs(corecoords$LON - site_info$lon[i]) < 0.001)
    if (length(cn_idx) == 0) next
    cn <- as.character(corecoords$CN[cn_idx[1]])
    if (!cn %in% cn_ids) next

    for (j in seq_along(time_points)) {
      yr <- years[j]
      if (!yr %in% years_avail) next
      vals <- GWBI_array[cn, , yr] * unit_conv
      GWBI_Output[i, paste0(time_points[j], "_GWBI")] <- mean(vals, na.rm = TRUE)
      GWBI_Output[i, paste0(time_points[j], "_SD")]   <- sd(vals,   na.rm = TRUE)
    }
  }

  if (as.logical(export_csv) && !is.null(outdir))
    utils::write.csv(GWBI_Output, file.path(outdir, "FIA_GWBI.csv"), row.names = FALSE)

  message("FIA GWBI Prep Completed!")
  list(GWBI_Output = GWBI_Output, time_points = time_points, var = "GWBI")
}


#' Full 2x2 covariance between FIA AGB and GWBI from the ensemble
#'
#' Used only for the "+FIA_GWBI" config to propagate the ensemble-derived
#' covariance between AbvGrndWood and GWBI rather than assuming independence.
#'
#' @return list(mean_vec = c(AbvGrndWood=..., GWBI=...), cov_mat = 2x2 matrix)
fia_joint_cov <- function(cn, yr, AGB_array, GWBI_array, unit_conv = 0.1) {
  agb_vals  <- AGB_array[cn,  , yr] * unit_conv
  gwbi_vals <- GWBI_array[cn, , yr] * unit_conv
  valid     <- !is.na(agb_vals) & !is.na(gwbi_vals)

  mean_vec <- c(AbvGrndWood = mean(agb_vals[valid]),
                GWBI        = mean(gwbi_vals[valid]))
  cov_mat  <- cov(cbind(agb_vals[valid], gwbi_vals[valid]))
  colnames(cov_mat) <- rownames(cov_mat) <- c("AbvGrndWood", "GWBI")

  list(mean_vec = mean_vec, cov_mat = cov_mat)
}

# 4. LandTrendr AGB extraction via terra
#
# sp::spTransform (used inside Landtrendr_AGB_prep -> extract.LandTrendr.AGB)
# requires sf, which cannot be compiled on this cluster. If AGB.csv doesn't
# exist yet, we generate it here using terra before calling the prep function.
# Landtrendr_AGB_prep detects the CSV and skips the sp/sf code path entirely.

lt_agb_csv <- file.path(outdir, "AGB.csv")

if (!file.exists(lt_agb_csv)) {
  message("Generating AGB.csv via terra (bypassing sp::spTransform)...")

  median_files <- sort(list.files(LT_AGB_indir, pattern = "_median\\.tif$", full.names = TRUE))
  stdv_files   <- sort(list.files(LT_AGB_indir, pattern = "_stdv\\.tif$",   full.names = TRUE))

  # Match Dongchen's approach: only use years with both median and stdv (< 2018)
  median_years <- as.integer(gsub(".*?(\\d{4})_median\\.tif$", "\\1", basename(median_files)))
  stdv_years   <- as.integer(gsub(".*?(\\d{4})_stdv\\.tif$",   "\\1", basename(stdv_files)))
  lt_years     <- sort(intersect(median_years, stdv_years))

  median_files <- median_files[median_years %in% lt_years]
  stdv_files   <- stdv_files[stdv_years   %in% lt_years]

  message("LandTrendr years with median+stdv: ", paste(lt_years, collapse = ", "))

  # Build site points in WGS84
  pts_wgs84 <- terra::vect(
    cbind(site_info$lon, site_info$lat),
    crs = "EPSG:4326"
  )

  agb_csv <- data.frame()
  for (k in seq_along(lt_years)) {
    yr       <- lt_years[k]
    med      <- terra::rast(median_files[k])
    sdv      <- terra::rast(stdv_files[k])
    pts_proj <- terra::project(pts_wgs84, terra::crs(med))
    med_vals <- terra::extract(med, pts_proj)[, 2]
    sdv_vals <- terra::extract(sdv, pts_proj)[, 2]

    agb_csv <- rbind(agb_csv, data.frame(
      date    = yr,
      site_id = site_info$site_id,
      lat     = site_info$lat,
      lon     = site_info$lon,
      agb     = med_vals,
      sd      = sdv_vals
    ))
  }

  utils::write.csv(agb_csv, lt_agb_csv, row.names = FALSE)
  message("AGB.csv written: ", nrow(agb_csv), " rows")
}

# 5. Run all prep functions

message("Standard obs prep functions")

LT_obs <- PEcAn.data.remote::Landtrendr_AGB_prep(
  site_info = site_info, start_date = start_date, end_date = end_date,
  time_points = time_points, AGB_indir = LT_AGB_indir,
  outdir = outdir, export_csv = TRUE, allow_download = FALSE
)

# Extend LandTrendr obs to 2018-2023 using RF-predicted SDs
# Landtrendr_AGB_prep hard-codes year < 2018; we extend here using the
# pre-trained random forest from Dongchen's workflow.
# Mean  = AGB median tif (Mg biomass/ha)
# SD    = RF prediction from covariates + normalized AGB (Mg biomass/ha)
# Both are in Mg biomass/ha here; × 0.5 carbon fraction is applied below.
message("Extending LandTrendr obs to 2018-2023 via RF-predicted SDs...")
rf_model   <- readRDS("/projectnb/dietzelab/dongchen/anchorSites/NA_runs/Obs/landtrendr_rf.rds")
lt_cov_dir <- "/projectnb/dietzelab/dongchen/anchorSites/NA_runs/covariates"
lc_dir     <- "/projectnb/dietzelab/dongchen/anchorSites/NA_runs/glance"

pts_wgs84      <- terra::vect(cbind(site_info$lon, site_info$lat), crs = "EPSG:4326")
lt_extra_years <- 2018:2023

for (yr in lt_extra_years) {
  tp <- as.Date(paste0(yr, "-07-15"))

  # Covariates
  cov     <- terra::rast(file.path(lt_cov_dir, paste0("covariates_", yr, ".tiff")))
  cov_pts <- terra::extract(cov, terra::project(pts_wgs84, terra::crs(cov)))

  # Land cover — glance only has 2012-2019; use 2019 for later years
  lc_yr  <- min(yr, 2019)
  lc     <- terra::rast(file.path(lc_dir, paste0(lc_yr, ".tif")))
  lc_pts <- terra::extract(lc, terra::project(pts_wgs84, terra::crs(lc)))
  cov_pts$LC <- factor(lc_pts[, 2], levels = rf_model$forest$xlevels$LC)

  # AGB median (Mg biomass/ha)
  agb_file <- list.files(LT_AGB_indir, pattern = paste0(yr, "_median"), full.names = TRUE)
  agb_rast <- terra::rast(agb_file)
  agb_pts  <- terra::extract(agb_rast, terra::project(pts_wgs84, terra::crs(agb_rast)))
  agb_vals <- agb_pts[, 2]

  # Use pre-normalized agb already in the covariates raster (built by Dongchen
  # on a consistent scale). Do NOT overwrite — avoids normalization range mismatch
  # when our 247 forested sites have a compressed AGB range vs the full RF domain.

  # Predict SD (Mg biomass/ha)
  sd_pred <- as.numeric(stats::predict(rf_model, cov_pts))

  # Append columns to LT_obs[[1]] (raw Mg biomass/ha — × 0.5 applied below)
  LT_obs[[1]][, paste0(tp, "_AbvGrndWood")] <- agb_vals
  LT_obs[[1]][, paste0(tp, "_SD")]          <- sd_pred

  message("  ", yr, ": AGB mean=", round(mean(agb_vals, na.rm = TRUE), 1),
          " SD mean=", round(mean(sd_pred, na.rm = TRUE), 1), " (Mg biomass/ha)")
}
LT_obs[[2]] <- sort(c(LT_obs[[2]], as.Date(paste0(lt_extra_years, "-07-15"))))
message("LandTrendr obs now covers: ",
        min(lubridate::year(LT_obs[[2]])), "-", max(lubridate::year(LT_obs[[2]])))

# Convert LandTrendr from Mg biomass/ha to Mg C/ha (carbon fraction 0.5)
# Applied to ALL years (2012-2023) at once via column grep.
lt_agb_cols <- grep("_AbvGrndWood$", names(LT_obs[[1]]))
lt_sd_cols  <- grep("_SD$",          names(LT_obs[[1]]))
LT_obs[[1]][, lt_agb_cols] <- LT_obs[[1]][, lt_agb_cols] * 0.5
LT_obs[[1]][, lt_sd_cols]  <- LT_obs[[1]][, lt_sd_cols]  * 0.5

LAI_obs <- PEcAn.data.remote::MODIS_LAI_prep(
  site_info = site_info, time_points = time_points,
  outdir = outdir, search_window = 30, export_csv = TRUE,
  sd_threshold = 20,
  boundary = NULL  # MODIS_LAI_ts_filter crashes when rm.inds is empty (c() -> -NULL is invalid); PEcAn bug
)

# Elevation filter: site 247 MODIS pixel is 356m below plot elevation (Sky Islands)
# — mask all LAI observations for that site
lai_elev_mask <- c("247")
lai_mask_cols <- grep("_LAI$|_SD$", names(LAI_obs$LAI_Output), value = TRUE)
LAI_obs$LAI_Output[LAI_obs$LAI_Output$site_id %in% lai_elev_mask, lai_mask_cols] <- NA
message("LAI elevation mask applied to sites: ", paste(lai_elev_mask, collapse = ", "))

# SMAP requires Inputs/Obs/SMAP_gee.csv from Google Earth Engine first
SMP_obs <- PEcAn.data.remote::SMAP_SMP_prep(
  site_info = site_info, start_date = start_date, end_date = end_date,
  time_points = time_points, outdir = outdir,
  search_window = 30, export_csv = TRUE, update_csv = FALSE
)

SoilC_obs <- PEcAn.data.land::Soilgrids_SoilC_prep(
  site_info = as.data.frame(site_info), start_date = start_date, end_date = end_date,
  time_points = time_points, outdir = outdir, export_csv = TRUE
)

message("FIA obs prep functions")

FIA_AGB_obs <- FIA_AGB_prep(
  site_info = site_info, time_points = time_points,
  FIA_AGB_path = FIA_AGB_path, corecoords_path = corecoords_path,
  unit_conv = MgHa_to_kgm2, outdir = outdir, export_csv = TRUE
)

FIA_GWBI_obs <- FIA_GWBI_prep(
  site_info = site_info, time_points = time_points,
  FIA_GWBI_path = FIA_GWBI_path, corecoords_path = corecoords_path,
  unit_conv = MgHa_to_kgm2, outdir = outdir, export_csv = TRUE
)


# 6. Source-name dictionary
#
# Maps each prep output's var name back to the XML product name that will be
# stored as the "source" attribute on each obs value. Must match exactly the
# section names inside <Obs_Prep><variables> in pecan.xml.
#
# For LT and FIA both mapping to "AbvGrndWood", we use the prep output list
# itself as the key (identified by which prep function produced it), not the
# variable name, so that the "+LT_FIA" dual constraint keeps both observations.


# Each entry: list(prep_output, source_name)
# The source_name must exactly match the XML <variables> tag name.
source_map <- list(
  LT_obs       = list(obs = LT_obs,       source = "LandTrendr"),
  LAI_obs      = list(obs = LAI_obs,      source = "MODIS"),
  SMP_obs      = list(obs = SMP_obs,      source = "SMAP"),
  SoilC_obs    = list(obs = SoilC_obs,    source = "SoilGrids"),
  FIA_AGB_obs  = list(obs = FIA_AGB_obs,  source = "FIA_AGB"),
  FIA_GWBI_obs = list(obs = FIA_GWBI_obs, source = "FIA_GWBI")
)

# Drop any prep outputs that failed (returned 0 or non-list) and warn
source_map <- Filter(function(entry) {
  ok <- is.list(entry$obs) && length(entry$obs) >= 2
  if (!ok) message("WARNING: dropping failed obs source '", entry$source,
                   "' (prep function returned 0 or invalid output)")
  ok
}, source_map)


# 7. build_obs: assemble obs.mean and obs.cov for one run config
#
# Each element of obs_sources is a key from source_map (e.g. "LT_obs").
#
# obs.mean[[t]][[site]] : 1-row data.frame where:
#   - column names are state variable names ("AbvGrndWood", "LAI", etc.)
#   - duplicate column names allowed for dual constraint (e.g. two "AbvGrndWood")
#   - each column has attr(col, "source") = XML product tag name exactly
#     (e.g. "LandTrendr", "FIA_AGB", "MODIS", "SMAP", "SoilGrids", "FIA_GWBI")
#
# obs.cov[[t]][[site]]  : square numeric matrix (or scalar for 1 obs).
#   Rows/cols ordered to match the obs.mean columns.
#   Diagonal for independent sources.  Full 2x2 block for FIA_AGB + GWBI.


build_obs <- function(obs_sources, site_info, source_map,
                      AGB_array = NULL, GWBI_array = NULL,
                      corecoords = NULL, unit_conv = 0.1) {

  # Soilgrids: inflate variance by n_timesteps (static variable)
  soilc_n_tp <- NULL
  if ("SoilC_obs" %in% obs_sources)
    soilc_n_tp <- length(source_map[["SoilC_obs"]]$obs[[2]])

  # "+FIA_GWBI" joint covariance flag
  joint_fia <- all(c("FIA_AGB_obs", "FIA_GWBI_obs") %in% obs_sources) &&
               !is.null(AGB_array) && !is.null(GWBI_array) && !is.null(corecoords)

  # Collect all time points across selected sources
  all_tp <- sort(unique(do.call(c, lapply(obs_sources, function(k) source_map[[k]]$obs[[2]]))))

  obs.mean <- obs.cov <- vector("list", length(all_tp))
  names(obs.mean) <- names(obs.cov) <- as.character(all_tp)

  for (i in seq_along(all_tp)) {
    t     <- all_tp[i]
    t_chr <- as.character(t)
    yr    <- as.character(lubridate::year(t))

    site_mean_list <- site_cov_list <- vector("list", length(site_info$site_id))
    names(site_mean_list) <- names(site_cov_list) <- site_info$site_id

    for (j in seq_along(site_info$site_id)) {
      sid <- site_info$site_id[j]

      # Accumulate observations as a list of scalars with source attributes.
      # obs_vals[[k]] = structure(value, source = "XML_product_name")
      # obs_vars[k]   = state variable name (column name in data.frame)
      # obs_var_n[k]  = variance
      obs_vals  <- list()
      obs_vars  <- character(0)
      obs_var_n <- numeric(0)

      for (k in seq_along(obs_sources)) {
        key    <- obs_sources[k]
        entry  <- source_map[[key]]
        prep   <- entry$obs         # list(df, time_points, var_name)
        src    <- entry$source
        var    <- prep[[3]]          # state variable name
        df     <- prep[[1]]
        tp_vec <- prep[[2]]

        if (!t %in% tp_vec) next

        row      <- which(df$site_id == sid)
        if (length(row) == 0) next

        mean_col <- paste0(t_chr, "_", var)
        sd_col   <- paste0(t_chr, "_SD")
        if (!mean_col %in% colnames(df)) next

        m <- df[row, mean_col]
        s <- df[row, sd_col]
        if (is.na(m) || is.na(s)) next

        # SMAP delivers SoilMoistFrac in percent (0-100); model state is fraction (0-1)
        if (var == "SoilMoistFrac") { m <- m / 100; s <- s / 100 }

        variance <- s^2
        if (var == "TotSoilCarb" && !is.null(soilc_n_tp))
          variance <- variance * soilc_n_tp

        obs_vals[[length(obs_vals) + 1]] <- structure(m, source = src)
        obs_vars  <- c(obs_vars,  var)
        obs_var_n <- c(obs_var_n, variance)
      }

      if (length(obs_vals) == 0) {
        site_mean_list[[j]] <- NULL
        site_cov_list[[j]]  <- NULL
        next
      }

      # Build covariance matrix
      n <- length(obs_vals)

      if (joint_fia &&
          "AbvGrndWood" %in% obs_vars &&
          "GWBI" %in% obs_vars) {
        # Find the FIA_AGB and FIA_GWBI entries specifically
        fia_agb_idx  <- which(obs_vars == "AbvGrndWood" &
                                sapply(obs_vals, function(x) attr(x, "source")) == "FIA_AGB")
        fia_gwbi_idx <- which(obs_vars == "GWBI" &
                                sapply(obs_vals, function(x) attr(x, "source")) == "FIA_GWBI")

        if (length(fia_agb_idx) > 0 && length(fia_gwbi_idx) > 0) {
          cn_idx <- which(abs(corecoords$LAT - site_info$lat[j]) < 0.001 &
                          abs(corecoords$LON - site_info$lon[j]) < 0.001)
          if (length(cn_idx) > 0) {
            cn <- as.character(corecoords$CN[cn_idx[1]])
            if (cn %in% dimnames(AGB_array)[[1]] && yr %in% dimnames(AGB_array)[[3]]) {
              jnt <- fia_joint_cov(cn, yr, AGB_array, GWBI_array, unit_conv)

              # Update means and variances with ensemble estimates
              obs_vals[[fia_agb_idx]]  <- structure(jnt$mean_vec["AbvGrndWood"],
                                                    source = "FIA_AGB")
              obs_vals[[fia_gwbi_idx]] <- structure(jnt$mean_vec["GWBI"],
                                                    source = "FIA_GWBI")
              obs_var_n[fia_agb_idx]  <- jnt$cov_mat["AbvGrndWood", "AbvGrndWood"]
              obs_var_n[fia_gwbi_idx] <- jnt$cov_mat["GWBI", "GWBI"]

              # Build full covariance: diagonal + off-diagonal AGB-GWBI block
              cov_mat <- diag(obs_var_n)
              cov_mat[fia_agb_idx,  fia_gwbi_idx] <- jnt$cov_mat["AbvGrndWood", "GWBI"]
              cov_mat[fia_gwbi_idx, fia_agb_idx]  <- jnt$cov_mat["GWBI", "AbvGrndWood"]

              site_mean_list[[j]] <- .build_obs_df(obs_vals, obs_vars)
              site_cov_list[[j]]  <- if (n == 1) cov_mat[1, 1] else cov_mat
              next
            }
          }
        }
      }

      # Default: diagonal covariance
      cov_mat <- if (n == 1) obs_var_n else diag(obs_var_n)
      site_mean_list[[j]] <- .build_obs_df(obs_vals, obs_vars)
      site_cov_list[[j]]  <- cov_mat
    }

    obs.mean[[i]] <- site_mean_list
    obs.cov[[i]]  <- site_cov_list
  }

  list(obs.mean = obs.mean, obs.cov = obs.cov)
}

# Helper: build a 1-row data.frame from a list of structured scalars.
# Column names are set to obs_vars (duplicate names allowed for dual constraint).
# Source attributes on each scalar are preserved as column attributes.
.build_obs_df <- function(obs_vals, obs_vars) {
  # Force all values to double to match Dongchen's format (terra extracts
  # LandTrendr raster values as integer; everything should be numeric/double)
  obs_vals <- lapply(obs_vals, function(x)
    structure(as.numeric(x), source = attr(x, "source")))
  # Use unique temp names to avoid data.frame() duplicate-arg error,
  # then rename columns after construction.
  tmp_names <- paste0("v", seq_along(obs_vals))
  df <- as.data.frame(
    setNames(obs_vals, tmp_names),
    check.names = FALSE
  )
  names(df) <- obs_vars
  rownames(df) <- NULL
  df
}

# 8. Load FIA arrays for joint covariance (+FIA_GWBI only)

message("Loading FIA arrays for joint covariance...")
AGB_array  <- readRDS(FIA_AGB_path)
GWBI_array <- readRDS(FIA_GWBI_path)
cc_env <- new.env()
load(corecoords_path, envir = cc_env)
corecoords <- cc_env$FilteredPLOTCore

# 9. Assemble and save obs for each run configuration

message("Assembling obs for each run config")

run_configs <- list(
  "base"      = c("LAI_obs", "SMP_obs", "SoilC_obs"),
  "+LT"       = c("LT_obs", "LAI_obs", "SMP_obs", "SoilC_obs"),
  "+FIA"      = c("FIA_AGB_obs", "LAI_obs", "SMP_obs", "SoilC_obs"),
  "+GWBI"     = c("FIA_GWBI_obs", "LAI_obs", "SMP_obs", "SoilC_obs"),
  "+LT_FIA"      = c("LT_obs", "FIA_AGB_obs", "LAI_obs", "SMP_obs", "SoilC_obs"),
  "+LT_GWBI"     = c("LT_obs", "FIA_GWBI_obs", "LAI_obs", "SMP_obs", "SoilC_obs"),
  "+FIA_GWBI"    = c("FIA_AGB_obs", "FIA_GWBI_obs", "LAI_obs", "SMP_obs", "SoilC_obs"),
  "+LT_FIA_GWBI" = c("LT_obs", "FIA_AGB_obs", "FIA_GWBI_obs", "LAI_obs", "SMP_obs", "SoilC_obs")
)

for (cfg_name in names(run_configs)) {
  message("  Building obs for: ", cfg_name)
  cfg_outdir <- file.path(outdir, "Rdata", cfg_name)
  dir.create(cfg_outdir, recursive = TRUE, showWarnings = FALSE)

  is_joint <- cfg_name == "+FIA_GWBI"

  result <- build_obs(
    obs_sources = run_configs[[cfg_name]],
    site_info   = site_info,
    source_map  = source_map,
    AGB_array   = if (is_joint) AGB_array  else NULL,
    GWBI_array  = if (is_joint) GWBI_array else NULL,
    corecoords  = if (is_joint) corecoords else NULL,
    unit_conv   = MgHa_to_kgm2
  )

  obs.mean <- result$obs.mean
  obs.cov  <- result$obs.cov

  save(obs.mean, file = file.path(cfg_outdir, "obs.mean.Rdata"))
  save(obs.cov,  file = file.path(cfg_outdir, "obs.cov.Rdata"))
  message("  Saved to: ", cfg_outdir)
}

message("Obs prep complete!")
message("Output: ", file.path(outdir, "Rdata"))
