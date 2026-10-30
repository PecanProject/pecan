# =====================================================================================
# 00_setup.R : shared settings and helper functions. Sourced by every other script.
#
# Climate-anchored prediction of crop planting and harvest timing (California).
# Run the scripts in order from this folder (setwd() here first):
#   01_historical_model.R     build the model from 2018-2023 data, validate it, calibrate it
#   02_future_projection.R    run it on 8 downscaled CMIP6 models, 2025-2051 (SSP3-7.0)
# All outputs are written as .rds files to <base_dir>/rebuild_outputs.
# =====================================================================================

suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(matrixStats) })
options(dplyr.summarise.inform = FALSE)

# ------------------------------------------------------------------ INPUT LOCATIONS
# Expected input files (not stored in the repo):
#   <base_dir>/daymet_cropped/{tmax,tmin}_<year>_cropped.tif   Daymet daily temperature (C), one stack per year
#   <base_dir>/daymet_precip_cropped/prcp_<year>_cropped.tif   Daymet daily precipitation (mm), one stack per year
#   <base_dir>/parcel_coords.xlsx                              columns parcel_id, lon, lat
#   <base_dir>/parcel_to_grid_d01.csv                          columns parcel_id, cell_id, cell_lon, cell_lat (45 km cell of each parcel)
#   <plant_dir>/assigned_year=<year>.parquet                   MSLSP phenology matched to LandIQ parcels
#   <future_dir>/<cell_id>/<model>.ssp370.<year>.nc            CalAdapt WRF 45 km daily future climate
# Defaults are the SCC paths; set the environment variables UROP_BASE, UROP_PHENO or
# UROP_FUTURE (or edit the three lines) to run somewhere else.
base_dir   <- Sys.getenv("UROP_BASE",   "/projectnb/dietzelab/mkim/UROP")
plant_dir  <- Sys.getenv("UROP_PHENO",  "/projectnb/dietzelab/ccmmf/management/phenology/matched_landiq_mslsp_v4.1.2")
future_dir <- Sys.getenv("UROP_FUTURE", "/projectnb/dietzelab/ccmmf/CalAdapt/wrf_45km")
out_dir    <- file.path(base_dir, "rebuild_outputs")
dir.create(out_dir, showWarnings = FALSE)

# ------------------------------------------------------------------ OPTIONAL INPUTS
# Commented out = the defaults below are used. Uncomment to change.
# Sys.setenv(MODELS = "CESM2")   # run only these climate models (comma separated); default is all 8
# Sys.setenv(NSLOTS = "8")       # CPU cores for the parallel steps (the SCC sets this in batch jobs)

# ------------------------------------------------------------------ SETTINGS
# Historical data and phenology
yrs_all        <- 2018:2023        # Daymet years on disk
val_years      <- 2019:2023        # years used for validation (2018 anchors need Sep-Dec 2017, so they are NA)
fwd_test_years <- 2021:2023        # forward-in-time test years (train on earlier years only)
SEASON         <- 2                # value of the phenology `season` column that is used
SUB_COL        <- "landiq_CLASS"   # column holding the LandIQ class letter (C D F G P R T V)

# Daily score = heat + dry-day flag + dry-soil flag
TBASE         <- 4      # C, base temperature for the heat term
PRECIP_THRESH <- 2.5    # mm, a day below this is "dry", at or above is "wet"
SM_MAX        <- 80     # mm, capacity of the soil bucket
DRY_FRAC      <- 0.3    # soil counts as dry when moisture <= DRY_FRAC * SM_MAX

# Anchor (start of the season), searched from Sep 1 (Y-1) to Feb 1 (Y)
ANCHOR_TMIN_C   <- 7     # "c7": 7-day mean of daily minimum temperature must be below this (C)
ANCHOR_REL_PCT  <- 0.25  # "rel": same, but the threshold is this quantile of the season's daily minimum
ANCHOR_WET_DAYS <- 3     # at least this many wet days in the trailing 14 days
ANCHOR_HOLD     <- 5     # conditions must hold this many days in a row

# Maximum accumulation window (days after the anchor) for each crop class
win_ogi <- tribble(~key, ~max_days,                                       # planting (onset of greenness increase)
  "hay_P", 300, "rice_R", 200, "row_F", 200, "row_G", 200, "row_T", 200,
  "woody_C", 300, "woody_D", 200, "woody_T", 200)
win_ogmn <- tribble(~key, ~max_days,                                      # harvest (onset of greenness minimum)
  "hay_P", 365, "rice_R", 400, "row_F", 330, "row_G", 330, "row_T", 330,
  "woody_C", 330, "woody_D", 330, "woody_T", 400)

CALIB_N <- 8000   # parcels sampled when calibrating Fcrit (per class x anchor x segment)

# Validation tiers (set in 01_historical_model.R)
#   "validated": beats the climatology baseline in >= 3 of 5 leave-one-year-out years AND >= 2 of 3 forward years
#   "strong"   : beats it in >= 4 of 5 leave-one-year-out years AND >= 2 of 3 forward years
LOO_MIN_WINS_VALIDATED <- 3
LOO_MIN_WINS_STRONG    <- 4
FWD_MIN_WINS           <- 2

# Future climate
scenario       <- "ssp370"
future_years   <- 2024:2051      # years of climate read for each cell
future_seasons <- 2025:2051      # season Y needs Sep (Y-1) to Feb (Y), so the first full season is 2025
HORIZON_DECADES <- (max(future_seasons) - min(future_seasons)) / 10   # converts days/decade to total shift (2.6)
all_models <- c("CESM2", "CNRM-ESM2-1", "EC-Earth3", "EC-Earth3-Veg",
                "FGOALS-g3", "MIROC6", "MPI-ESM1-2-HR", "TaiESM1")
models  <- strsplit(Sys.getenv("MODELS", paste(all_models, collapse = ",")), ",")[[1]]
n_cores <- max(1L, as.integer(Sys.getenv("NSLOTS", "1")))

# ------------------------------------------------------------------ HELPERS
# Future-climate cells that contain modelled parcels, and the number of parcels per (cell, class).
# Parcels that share a 45 km cell and class share the same future value, so they are weighted by count.
site_weights <- function() {
  pk   <- readRDS(file.path(out_dir, "parcel_classes.rds"))
  grid <- read.csv(file.path(base_dir, "parcel_to_grid_d01.csv")) %>% mutate(parcel_id = as.character(parcel_id))
  g    <- inner_join(grid, pk, by = "parcel_id")
  list(sites = distinct(g, cell_id, cell_lon, cell_lat),
       wts   = dplyr::count(g, cell_id, key, name = "n_parcels") %>% rename(site = cell_id),
       parcels = g)
}

# Reference evapotranspiration input: extraterrestrial radiation in mm/day (FAO-56)
extraterrestrial_mm <- function(lat_deg, doy) {
  phi <- lat_deg * pi / 180
  dr  <- 1 + 0.033 * cos(2 * pi * doy / 365)
  del <- 0.409 * sin(2 * pi * doy / 365 - 1.39)
  ws  <- acos(pmin(1, pmax(-1, -tan(phi) * tan(del))))
  Ra  <- 24 * 60 / pi * 0.082 * dr * (ws * sin(phi) * sin(del) + cos(phi) * cos(del) * sin(ws))
  0.408 * Ra
}

# The three parts of the daily score. Inputs are matrices (cells x days); `lat` has one value per row.
#   heat = mean temperature above TBASE
#   dry  = 1 on a dry day (precipitation < PRECIP_THRESH)
#   sm   = 1 when the soil bucket is dry (bucket fills with rain, drains by Hargreaves-Samani ET0)
daily_parts <- function(tmax, tmin, prcp, dates, lat) {
  doy   <- as.integer(format(dates, "%j"))
  tmean <- (tmax + tmin) / 2
  Ra    <- outer(lat, doy, function(l, d) extraterrestrial_mm(l, d))
  ET0   <- 0.0023 * Ra * (tmean + 17.8) * sqrt(pmax(tmax - tmin, 0))
  SM <- matrix(NA_real_, nrow(tmax), ncol(tmax)); prev <- rep(SM_MAX, nrow(tmax))
  for (t in seq_len(ncol(tmax))) {
    prev <- pmin(SM_MAX, pmax(0, prev + prcp[, t] - ET0[, t])); SM[, t] <- prev
  }
  list(heat = pmax(tmean - TBASE, 0), dry = (prcp < PRECIP_THRESH) * 1, sm = (SM <= SM_MAX * DRY_FRAC) * 1)
}

# Daily score matrix S = heat + dry + sm, padded with NA columns so windows never run off the end
build_S <- function(tmax, tmin, prcp, dates, lat) {
  p <- daily_parts(tmax, tmin, prcp, dates, lat)
  S <- p$heat + p$dry + p$sm
  cbind(S, matrix(NA_real_, nrow(S), 450))
}

# Rolling sum over the trailing k values (NA counts as 0; first k-1 entries are NA)
roll_sum <- function(x, k) {
  x[is.na(x)] <- 0; cs <- cumsum(c(0, x)); n <- length(x); out <- rep(NA_real_, n)
  if (n >= k) { i <- k:n; out[i] <- cs[i + 1] - cs[i + 1 - k] }
  out
}

# Anchor date for season `year`: first day of the first run of ANCHOR_HOLD days in which
#   the 7-day mean tmin is below the cold threshold AND >= ANCHOR_WET_DAYS of the last 14 days were wet.
# mode "c7": threshold = ANCHOR_TMIN_C.  mode "rel": threshold = ANCHOR_REL_PCT quantile of the season's tmin.
# Returns NA if the Sep-Feb window is incomplete (e.g. 2018), too sparse ("rel"), or no run is found.
find_onset <- function(d, tmin, prcp, year, mode = c("c7", "rel")) {
  mode <- match.arg(mode)
  s <- as.Date(sprintf("%d-09-01", year - 1)); e <- as.Date(sprintf("%d-02-01", year))
  if (min(d) > s || max(d) < e) return(as.Date(NA))
  i <- which(d >= s & d <= e)
  thr <- if (mode == "c7") ANCHOR_TMIN_C else {
    if (sum(!is.na(tmin[i])) < 150) return(as.Date(NA))
    unname(quantile(tmin[i], ANCHOR_REL_PCT, na.rm = TRUE))
  }
  cond <- (roll_sum(tmin, 7) / 7 < thr) & (roll_sum(as.numeric(prcp >= PRECIP_THRESH), 14) >= ANCHOR_WET_DAYS)
  cond[is.na(cond)] <- FALSE
  run  <- roll_sum(as.numeric(cond), ANCHOR_HOLD) == ANCHOR_HOLD
  j <- i[which(run[i])[1]]
  if (is.na(j)) as.Date(NA) else d[j - (ANCHOR_HOLD - 1)]
}

# Score in the `md` days after each start index: matrix (parcels x md); NA scores count as 0
get_W <- function(S, cr, st, md) {
  m <- length(cr)
  idx <- matrix(st, m, md) + matrix(seq_len(md), m, md, byrow = TRUE)
  W <- S[cbind(rep(cr, md), as.vector(idx))]; dim(W) <- c(m, md); W[is.na(W)] <- 0
  W
}

# Predicted days after the anchor = first day the cumulative score reaches Fcrit (NA if never within md days).
# cr = row of S (climate cell) for each parcel, st = column of S (anchor date) for each parcel.
pred_k <- function(S, cr, st, Fcrit, md, chunk = 20000) {
  n <- length(cr); out <- rep(NA_real_, n)
  for (a in seq(1, n, by = chunk)) {
    ii <- a:min(n, a + chunk - 1)
    cs <- rowCumsums(get_W(S, cr[ii], st[ii], md))
    k <- rowSums(cs < Fcrit) + 1; k[k > md] <- NA; out[ii] <- k
  }
  out
}

# Fit Fcrit by minimising RMSE between predicted and observed days (grid search, then 1-D refinement).
# Uses a random subsample of CALIB_N parcels (set.seed in 01_historical_model.R makes this repeatable).
calib_F <- function(S, cr, st, obs, md) {
  if (length(cr) > CALIB_N) { s <- sample.int(length(cr), CALIB_N); cr <- cr[s]; st <- st[s]; obs <- obs[s] }
  cs <- rowCumsums(get_W(S, cr, st, md))
  f <- function(F) {
    k <- rowSums(cs < F) + 1; k[k > md] <- NA
    if (mean(!is.na(k)) < 0.5) return(1e6)
    sqrt(mean((k - obs)^2, na.rm = TRUE))
  }
  grid <- exp(seq(log(1), log(10000), length.out = 80))
  v <- vapply(grid, f, 0); j <- which.min(v)
  o <- optimize(f, c(grid[max(1, j - 1)], grid[min(80, j + 1)]))
  c(Fcrit = o$minimum, rmse = o$objective)
}

# Linear trend in units per decade (needs at least 10 finite points)
slope10 <- function(y, x) {
  ok <- is.finite(y); if (sum(ok) < 10) return(NA_real_)
  unname(coef(lm(y[ok] ~ x[ok]))[2]) * 10
}

# One CalAdapt netCDF file -> daily tmax, tmin, precipitation (sub-daily values are collapsed)
read_daily <- function(f) {
  nc <- tryCatch(ncdf4::nc_open(f), error = function(e) NULL); if (is.null(nc)) return(NULL)
  on.exit(ncdf4::nc_close(nc))
  tm  <- ncdf4::ncvar_get(nc, "time"); tun <- ncdf4::ncatt_get(nc, "time", "units")$value
  mult   <- switch(tolower(sub("\\s.*", "", tun)), seconds = 1, minutes = 60, hours = 3600, days = 86400, 1)
  origin <- as.Date(substr(sub(".*since ", "", tun), 1, 10))
  T <- as.numeric(ncdf4::ncvar_get(nc, "air_temperature")); P <- as.numeric(ncdf4::ncvar_get(nc, "precipitation_flux"))
  ft <- ncdf4::ncatt_get(nc, "air_temperature", "_FillValue"); fp <- ncdf4::ncatt_get(nc, "precipitation_flux", "_FillValue")
  if (ft$hasatt) T[T == ft$value] <- NA
  if (fp$hasatt) P[P == fp$value] <- NA
  if (mean(T, na.rm = TRUE) > 200) T <- T - 273.15                                   # Kelvin -> C
  punits <- tryCatch(ncdf4::ncatt_get(nc, "precipitation_flux", "units")$value, error = function(e) "")
  if (grepl("s-1|/s", punits)) P <- P * median(diff(tm)) * mult                       # flux -> mm per step
  date <- origin + floor(tm * mult / 86400)
  data.frame(date, T, P) %>% group_by(date) %>%
    summarise(tmax = max(T, na.rm = TRUE), tmin = min(T, na.rm = TRUE), prcp = sum(P, na.rm = TRUE))
}
