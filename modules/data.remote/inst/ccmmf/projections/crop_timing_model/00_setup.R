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
out_dir    <- Sys.getenv("UROP_OUT", file.path(base_dir, "rebuild_outputs"))   # set UROP_OUT to write somewhere else
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

# Daily score = heat (degree days above TBASE); the dry day and dry soil flags are optional and off (see SCORE_WEIGHTS)
TBASE         <- 4      # C, base temperature for the heat term
PRECIP_THRESH <- 2.5    # mm, a day below this is "dry", at or above is "wet" (also used by the anchor rule)
SM_MAX        <- 80     # mm, capacity of the soil bucket (used only if the sm weight is above 0)
DRY_FRAC      <- 0.3    # soil counts as dry when moisture <= DRY_FRAC * SM_MAX (used only if the sm weight is above 0)

# Weight of each part in the daily score. Heat is in degrees (C); dry and sm are 0/1 flags that are available as
# options but switched off (weight 0) for the results, so the score is heat only (degree days above TBASE).
# A weight of 1 makes a dry day (or dry soil day) count like one extra degree of heat.
# Weights must not be negative: the running total has to keep rising for pred_k() to find the crossing day.
SCORE_WEIGHTS <- c(heat = 1, dry = 0, sm = 0)
# To compare scores without editing this file, set the environment variable SCORE_WEIGHTS to the three weights in the
# order heat, dry, sm separated by underscores, e.g. SCORE_WEIGHTS=1_1_1 for heat plus both flags.
if (nzchar(Sys.getenv("SCORE_WEIGHTS"))) {
  SCORE_WEIGHTS <- setNames(as.numeric(strsplit(Sys.getenv("SCORE_WEIGHTS"), "_")[[1]]), c("heat", "dry", "sm"))
}
stopifnot(all(SCORE_WEIGHTS >= 0), setequal(names(SCORE_WEIGHTS), c("heat", "dry", "sm")))

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

# Columns of NA added after the last day of the score matrix. A window can start on the last day and run up to
# max_days further, so the padding must be at least the longest window (400 days); NA scores count as 0.
PAD_DAYS <- max(win_ogi$max_days, win_ogmn$max_days)

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

# Extraterrestrial radiation Ra: the sunlight arriving at the top of the atmosphere on a given day and latitude,
# written as the depth of water it could evaporate. It is the radiation input of the Hargreaves-Samani
# evaporation formula used in daily_parts():  ET0 = 0.0023 * Ra * (Tmean + 17.8) * sqrt(Tmax - Tmin).
# These are the daily FAO-56 equations (Allen et al. 1998, chapter 3), the exact integral of the solar geometry over
# one day. PEcAn.data.atmosphere::cos_solar_zenith_angle() gives the instantaneous zenith angle, not this daily total.
#   lat_deg : latitude in degrees (one value per climate cell, or a single value)
#   doy     : day of year, 1 to 366
#   returns : Ra in mm/day (roughly 6 in January and 17 in June at 38 N)
extraterrestrial_mm <- function(lat_deg, doy) {
  phi <- lat_deg * pi / 180                                   # latitude in radians
  dr  <- 1 + 0.033 * cos(2 * pi * doy / 365)                  # Earth to Sun distance correction (unitless, 0.97 to 1.03)
  del <- 0.409 * sin(2 * pi * doy / 365 - 1.39)               # solar declination in radians (0.409 rad = 23.4 degrees)
  ws  <- acos(pmin(1, pmax(-1, -tan(phi) * tan(del))))        # sunset hour angle in radians; the clamp keeps acos defined
  # Ra in MJ per m2 per day: solar constant 0.082 MJ/m2/min x 1440 min/day / pi x the day length and angle terms
  Ra  <- 24 * 60 / pi * 0.082 * dr * (ws * sin(phi) * sin(del) + cos(phi) * cos(del) * sin(ws))
  0.408 * Ra                                                  # MJ/m2/day -> mm/day (0.408 = 1 / 2.45 MJ per kg of water)
}

# The three parts of the daily score. Inputs are matrices (cells x days); `lat` has one value per row.
# Units: temperatures in C, precipitation in mm/day, soil bucket and ET0 in mm.
#   heat = mean temperature above TBASE (C, i.e. degree days per day)
#   dry  = 1 on a dry day (precipitation < PRECIP_THRESH), otherwise 0
#   sm   = 1 when the soil bucket is dry, otherwise 0. The bucket (capacity SM_MAX mm) gains the day's
#          precipitation and loses Hargreaves-Samani reference ET0 (mm/day); it is a simple stand-in
#          for soil moisture, not observed or irrigation-model soil moisture.
# build_S() combines these using SCORE_WEIGHTS. The results use heat only (the dry and sm weights are 0). If the
# flags are switched on, the score mixes degrees and 0/1 flags, so it is an empirical index rather than a
# physical quantity. Fcrit (below) is fitted in the same index units, which absorbs the overall scale but not
# the relative weight of the parts.
daily_parts <- function(tmax, tmin, prcp, dates, lat) {
  doy   <- as.integer(format(dates, "%j"))
  tmean <- (tmax + tmin) / 2
  Ra    <- outer(lat, doy, function(l, d) extraterrestrial_mm(l, d))      # cells x days, mm/day
  ET0   <- 0.0023 * Ra * (tmean + 17.8) * sqrt(pmax(tmax - tmin, 0))     # Hargreaves-Samani, mm/day

  # Soil bucket: SM = water in the bucket (mm), cells x days. It starts full, then each day gains the
  # precipitation, loses ET0, and is kept between empty (0) and full (SM_MAX).
  SM   <- matrix(NA_real_, nrow(tmax), ncol(tmax))
  prev <- rep(SM_MAX, nrow(tmax))
  for (t in seq_len(ncol(tmax))) {
    prev    <- pmin(SM_MAX, pmax(0, prev + prcp[, t] - ET0[, t]))
    SM[, t] <- prev
  }

  # Logical -> 0/1. storage.mode() converts like as.numeric() but keeps the matrix shape
  # (as.numeric() would flatten the matrix into a vector).
  dry <- prcp < PRECIP_THRESH
  storage.mode(dry) <- "numeric"
  sm  <- SM <= SM_MAX * DRY_FRAC
  storage.mode(sm) <- "numeric"
  list(heat = pmax(tmean - TBASE, 0), dry = dry, sm = sm)
}

# Daily score matrix S (cells x days) = weighted sum of heat, dry and sm (weights in SCORE_WEIGHTS; heat only by
# default, since the dry and sm weights are 0), followed by PAD_DAYS columns of NA so that a window starting near
# the last day never runs off the end of S.
build_S <- function(tmax, tmin, prcp, dates, lat) {
  p <- daily_parts(tmax, tmin, prcp, dates, lat)
  S <- SCORE_WEIGHTS[["heat"]] * p$heat + SCORE_WEIGHTS[["dry"]] * p$dry + SCORE_WEIGHTS[["sm"]] * p$sm
  cbind(S, matrix(NA_real_, nrow(S), PAD_DAYS))
}

# Rolling sum over the trailing k values: out[i] = x[i-k+1] + ... + x[i], for each position i.
# NA counts as 0, and the first k - 1 positions are NA because they do not have k values yet.
# It is used on short vectors for the anchor rules (7 day mean of tmin, wet days in the last 14, 5 day hold).
#
# How it works: the running total cs of x is built once. A window sum is then the running total now minus the
# running total k days ago. This is not the original data: subtracting totals that are k steps apart gives the sum
# of k values, and only with k = 1 would it give x back. Example with x = c(2, 5, 1, 4) and k = 2:
#   cs = 0 2 7 8 12   ->   out = NA, 7 - 0, 8 - 2, 12 - 7 = NA 7 6 5   (that is NA, 2+5, 5+1, 1+4)
# stats::filter(x, rep(1, k), sides = 1) gives the same numbers; this form is kept so the anchors are unchanged.
roll_sum <- function(x, k) {
  n <- length(x)
  x[is.na(x)] <- 0                      # missing values add nothing
  cs  <- cumsum(c(0, x))                # cs[j + 1] = x[1] + ... + x[j]; the leading 0 makes cs[1] = 0
  out <- rep(NA_real_, n)               # positions without a full window stay NA
  if (n >= k) {
    i <- k:n                            # positions that have k values up to and including themselves
    out[i] <- cs[i + 1] - cs[i + 1 - k] # total up to i minus total up to i - k = sum of the k values ending at i
  }
  out
}

# Anchor date for season `year`: first day of the first run of ANCHOR_HOLD days in which
#   the 7-day mean tmin is below the cold threshold AND >= ANCHOR_WET_DAYS of the last 14 days were wet.
# mode "c7": threshold = ANCHOR_TMIN_C.  mode "rel": threshold = ANCHOR_REL_PCT quantile of the season's tmin.
# Returns NA if the Sep-Feb window is incomplete (e.g. 2018), too sparse ("rel"), or no run is found.
#   d    : dates of the daily series (Date vector)
#   tmin : daily minimum temperature (C), same length as d
#   prcp : daily precipitation (mm), same length as d
#   year : the season year Y; the search window is Sep 1 of Y-1 to Feb 1 of Y
find_onset <- function(d, tmin, prcp, year, mode = c("c7", "rel")) {
  mode <- match.arg(mode)
  s <- as.Date(sprintf("%d-09-01", year - 1))                 # start of the search window
  e <- as.Date(sprintf("%d-02-01", year))                     # end of the search window
  if (min(d) > s || max(d) < e) return(as.Date(NA))           # the series must cover the whole window
  i <- which(d >= s & d <= e)                                 # positions inside the window
  thr <- if (mode == "c7") ANCHOR_TMIN_C else {
    if (sum(!is.na(tmin[i])) < 150) return(as.Date(NA))       # "rel" needs enough days to estimate a quantile
    unname(quantile(tmin[i], ANCHOR_REL_PCT, na.rm = TRUE))
  }
  cold <- roll_sum(tmin, 7) / 7 < thr                                          # 7 day mean of tmin below the threshold
  wet  <- roll_sum(as.numeric(prcp >= PRECIP_THRESH), 14) >= ANCHOR_WET_DAYS   # enough wet days in the last 14
  cond <- cold & wet
  cond[is.na(cond)] <- FALSE
  run  <- roll_sum(as.numeric(cond), ANCHOR_HOLD) == ANCHOR_HOLD   # TRUE on the last day of a run of ANCHOR_HOLD days
  j <- i[which(run[i])[1]]                                         # last day of the first such run in the window
  if (is.na(j)) as.Date(NA) else d[j - (ANCHOR_HOLD - 1)]          # the anchor is the first day of that run
}

# get_W: the daily score in the `md` days after each parcel's anchor date.
#   S  : score matrix from build_S(); rows = climate cells, columns = days (already padded with NA at the end)
#   cr : for each parcel, the row of S that is its climate cell (integer vector of length m, one entry per parcel)
#   st : for each parcel, the column of S that is its anchor date (integer vector of length m)
#   md : number of days to collect after the anchor (the maximum window of the crop class)
# Returns an m x md matrix. Row i holds S[cr[i], st[i] + 1], S[cr[i], st[i] + 2], ..., S[cr[i], st[i] + md].
# NA (padding or missing days) becomes 0, so it adds nothing to the running total.
# Example: a parcel in cell 3 with its anchor in column 100 and md = 3 gets S[3, 101:103].
get_W <- function(S, cr, st, md) {
  m <- length(cr)
  # column to read for every parcel and day: anchor column + 1, + 2, ..., + md   (m x md)
  cols <- matrix(st, m, md) + matrix(seq_len(md), m, md, byrow = TRUE)
  # row to read: each parcel's cell row, repeated for each of its md days
  rows <- rep(cr, md)
  W <- S[cbind(rows, as.vector(cols))]    # one lookup of every (cell, day) pair
  dim(W) <- c(m, md)
  W[is.na(W)] <- 0
  W
}

# pred_k: predicted number of days after the anchor until the accumulated score reaches Fcrit.
#   S, cr, st, md : as in get_W()
#   Fcrit         : the accumulated score at which the event is predicted to happen (one number, fitted by calib_F)
#   chunk         : number of parcels processed at a time, to limit memory
# For each parcel: take the daily scores after its anchor (get_W), add them up as a running total (rowCumsums),
# and find the first day on which the total is at least Fcrit.
# Returns k per parcel: 1 means the day after the anchor; NA means Fcrit was not reached within md days.
# Counting the days with total < Fcrit finds that first day because the score is never negative, so the
# running total only rises and never drops back below Fcrit.
pred_k <- function(S, cr, st, Fcrit, md, chunk = 20000) {
  n   <- length(cr)
  out <- rep(NA_real_, n)
  for (a in seq(1, n, by = chunk)) {
    ii <- a:min(n, a + chunk - 1)                       # the parcels in this chunk
    cs <- rowCumsums(get_W(S, cr[ii], st[ii], md))      # running total of the score, parcels x days
    k  <- rowSums(cs < Fcrit) + 1                       # days still below Fcrit, plus 1 = the crossing day
    k[k > md] <- NA                                     # never crossed inside the window
    out[ii] <- k
  }
  out
}

# calib_F: fit Fcrit, the one fitted parameter of the model.
# Fcrit is the accumulated daily score (in the index units of build_S) that has to build up after the anchor
# before the event happens. For a candidate Fcrit, pred_k() gives each parcel's predicted days after the anchor;
# calib_F picks the Fcrit whose predictions are closest to the observed days:
#     Fcrit = the value that minimises sqrt(mean((predicted days - observed days)^2))
#   S, cr, st, md : as in get_W()
#   obs           : observed days from the anchor to the event for each parcel (days_t in 01_historical_model.R)
# Parcels whose total never reaches a candidate Fcrit are left out of its RMSE, and a candidate that is
# reached by fewer than half of the parcels is rejected (its error is set to 1e6).
# Why a search and not a least squares fit: the predicted day is a step function of Fcrit (it jumps from one
# whole day to the next), so there is no smooth gradient. The search tries 80 values spaced evenly on a log
# scale from 1 to 10000, then refines between the neighbours of the best one with optimize().
# The conventional thermal time estimate, calib_F_thermal() below, is the quick alternative; it fits in score
# units rather than in days. A random subsample of CALIB_N parcels is used for speed (set.seed in
# 01_historical_model.R makes it repeatable). Returns c(Fcrit, rmse in days).
calib_F <- function(S, cr, st, obs, md) {
  if (length(cr) > CALIB_N) {
    s <- sample.int(length(cr), CALIB_N)
    cr <- cr[s]; st <- st[s]; obs <- obs[s]
  }
  cs <- rowCumsums(get_W(S, cr, st, md))                # running totals, computed once for all candidates
  f <- function(F) {                                    # error in days for one candidate Fcrit
    k <- rowSums(cs < F) + 1
    k[k > md] <- NA
    if (mean(!is.na(k)) < 0.5) return(1e6)
    sqrt(mean((k - obs)^2, na.rm = TRUE))
  }
  grid <- exp(seq(log(1), log(10000), length.out = 80))
  v <- vapply(grid, f, 0)
  j <- which.min(v)
  o <- optimize(f, c(grid[max(1, j - 1)], grid[min(80, j + 1)]))
  c(Fcrit = o$minimum, rmse = o$objective)
}

# calib_F_thermal: the conventional thermal time estimate of Fcrit, for comparison (not used by the pipeline).
# For each parcel, add up the score from the day after the anchor to the observed day; Fcrit is the mean of
# those totals. It needs no search, but it minimises the error in score units rather than in days.
# Parcels with an observed day outside 1..md are skipped. Returns one number.
calib_F_thermal <- function(S, cr, st, obs, md) {
  ok <- is.finite(obs) & obs >= 1 & obs <= md
  cs <- rowCumsums(get_W(S, cr[ok], st[ok], md))
  mean(cs[cbind(seq_len(nrow(cs)), as.integer(round(obs[ok])))])
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
