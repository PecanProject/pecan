# --- STEP 1. Setup ---
# Core libraries
library(zoo)
librarian::shelf(
readr, dplyr, stringr, tidyr, purrr, ggplot2,
leaflet, sf, terra, progressr, lubridate
)

# parallel & progress
terraOptions(threads = 16)
handlers("txtprogressbar")



# --- STEP 2. Load Anchor CSV & Pick a Site ---
# Read and clean the CSV
path <- "/projectnb/dietzelab/XinyuanJi/anchor_sites_locations.csv"
raw  <- read_lines(path)
cleaned <- str_replace_all(raw,
                         ",c\\(([0-9\\.-]+), *([0-9\\.-]+)\\),",
                         ",\"c(\\1,\\2)\","
)
anchors <- read_csv(paste(cleaned, collapse = "\n"), show_col_types = FALSE) %>%
select(id, site_name,
       upper_left_lon, upper_left_lat,
       lower_right_lon, lower_right_lat) %>%
mutate(across(starts_with(c("upper_left","lower_right")), as.numeric),
       center_lon = (upper_left_lon + lower_right_lon)/2,
       center_lat = (upper_left_lat + lower_right_lat)/2)  %>%
filter(!is.na(center_lon), !is.na(center_lat))



# ── CHUNK 1 : read tillage sheet, keep Treatment column ─────────────────────
path <- "/projectnb/dietzelab/XinyuanJi/till_treatment_polygons.csv"
# path <- "/projectnb/dietzelab/XinyuanJi/anchor_sites_locations.csv"

raw  <- readr::read_lines(path)
cleaned <- stringr::str_replace_all(
raw,
",c\\(([0-9\\.-]+), *([0-9\\.-]+)\\),",
",\"c(\\1,\\2)\","               # quote the “c(x,y)” blobs (same fix as before)
)

anchors <- readr::read_csv(paste(cleaned, collapse = "\n"),
                         show_col_types = FALSE) %>%

# filter anchors to just 2018
dplyr::filter(Year == c(2018, 2019)) %>%



# ── 1 standardise column names so downstream code is unchanged
dplyr::rename(
  id        = SampleID,          # ← tillage file’s ID
  site_name = ProjectName.x      # ← adjust if your column is ProjectName
) %>%

# ── 2 include Treatment_Control
dplyr::select(
  id, site_name, Treatment_Control,
  upper_left_lon, upper_left_lat,
  lower_right_lon, lower_right_lat
) %>%

dplyr::mutate(
  dplyr::across(
    dplyr::starts_with(c("upper_left", "lower_right")),
    as.numeric
  ),
  center_lon = (upper_left_lon + lower_right_lon) / 2,
  center_lat = (upper_left_lat + lower_right_lat) / 2
) %>%
dplyr::filter(!is.na(center_lon), !is.na(center_lat))



# Make the whole code a function
process_ndti_for_site <- function(selected_anchor) {
  site_id    <- selected_anchor$id
  site_name  <- selected_anchor$site_name
  
  

  # --- STEP 3. Visualize All Anchor Sites ---
  # Please refer to Step 3 in:
  # //projectnb/dietzelab/XinyuanJi/ndti-evi-timeseries(Sarah)-L+S.qmd
  
  
  
  # --- STEP 4. Build Exact tile-polygon ROI ---  
  # A) Build a closed polygon in WGS84 from the CSV bbox
  corner_df <- data.frame(
    lon = c(
      selected_anchor$upper_left_lon,
      selected_anchor$upper_left_lon,
      selected_anchor$lower_right_lon,
      selected_anchor$lower_right_lon,
      selected_anchor$upper_left_lon  # close ring
    ),
    lat = c(
      selected_anchor$upper_left_lat,
      selected_anchor$lower_right_lat,
      selected_anchor$lower_right_lat,
      selected_anchor$upper_left_lat,
      selected_anchor$upper_left_lat  # close ring
    )
  )
  
  # B) Make an sf and then a terra SpatVector
  bbox_sf      <- st_sfc(st_polygon(list(as.matrix(corner_df))), crs = 4326)
  roi_vect_wgs <- terra::vect(bbox_sf)
  
  
  
  # --- STEP 5. Determine Which HLS Tile Covers This Site ---
  ## Determine which HLS tile covers this site  ────────────────────────────
  ## (robust: uses ROI polygon → largest-overlap → nearest fallback)
  
  # ── Load the MS-LSP grid (already in WGS-84 / EPSG:4326) ───────────────────
  grid <- st_read(
    "/projectnb/dietzelab/skanee/ccmmf-phenology/MSLSP_tileGrid.geojson",
    quiet = TRUE
  )
  
  # ── Build the anchor-site polygon in the grid CRS (4326) -------------------
  bbox_anchor <- st_transform(bbox_sf, st_crs(grid))   # bbox_sf from Step 5 later
  
  # ── 1) tiles whose polygons intersect the ROI -----------------------------
  hits <- st_intersects(bbox_anchor, grid) |> unlist()
  
  if (length(hits) == 1) {
    
    tileID <- grid$tileID[hits]                        # exactly one tile
    
  } else if (length(hits) > 1) {
    
    # 2) choose the one with the largest overlapping area
    overlap_areas <- st_area(st_intersection(grid[hits, ], bbox_anchor))
    tileID <- grid$tileID[hits[which.max(overlap_areas)]]
    
  } else {
    
    # 3) ROI outside every grid polygon – pick the nearest grid feature
    idx    <- st_nearest_feature(st_centroid(bbox_anchor), grid)
    tileID <- grid$tileID[idx]
  }
  
  site_id   <- selected_anchor$id
  site_name <- selected_anchor$site_name
  crop_type <- site_name     # keep your original variable
  
  cat("Selected site:", site_id, "-", site_name, "\n")
  cat("Using HLS tile:", tileID, "\n")
  
  
  
  # --- STEP 6. List & Filter HLS NetCDFs for That Tile ---
  hls_dir   <- "/projectnb/dietzelab/malmborg/CARB/HLS_data"
  hls_files <- list.files(hls_dir, pattern = "MSLSP.*\\.nc$", full.names = TRUE)
  
  # Filter by tileID in filename (fast, no raster I/O)
  hls_tile_files <- hls_files[str_detect(basename(hls_files), fixed(tileID))]
  
  cat("Found", length(hls_tile_files), "HLS files for tile", tileID, "\n")
  
  
  
  # --- STEP 7. Precompute Projected ROI Extent ---
  # Load one file to get its CRS
  metrics     <- c("50PCGI","50PCGD")#,"OGMn", "OGI_2", "Peak_2", "OGMn_2")
  template_r  <- rast(hls_tile_files[1], subds = metrics)
  roi_proj    <- project(roi_vect_wgs, crs(template_r))
  roi_ext     <- ext(roi_proj)
  
  
  
  # --- STEP 8. Extract phenology metrics ---
  extract_one <- function(nc) {
    yr <- as.integer(str_extract(basename(nc), "\\d{4}"))
    r  <- try(rast(nc, subds = metrics), silent = TRUE)
    if (inherits(r, "try-error")) {
      message("Skipping load: ", basename(nc))
      return(tibble(year = yr, OGI = NA, Peak = NA, OGMn = NA))
    }
    cr <- try(crop(r, roi_ext), silent = TRUE)
    if (inherits(cr, "try-error")) {
      message(" Crop failed for ", basename(nc))
      return(tibble(year = yr, OGI = NA, Peak = NA, OGMn = NA))
    }
    ms  <- mask(cr, roi_proj)
    vals <- global(ms, mean, na.rm = TRUE)
    out  <- as_tibble(t(vals)) %>% setNames(names(ms))
    out %>% mutate(year = yr, .before = 1)
  }
  
  # Sequential extraction with a text progress bar
  ts_list <- vector("list", length(hls_tile_files))
  pb      <- txtProgressBar(0, length(hls_tile_files), style = 3)
  for (i in seq_along(hls_tile_files)) {
    ts_list[[i]] <- extract_one(hls_tile_files[i])
    setTxtProgressBar(pb, i)
  }
  close(pb)
  
  ts_raw <- bind_rows(ts_list) %>% arrange(year)
  
  
  
  # --- STEP 9. Pivot long & plot phenology ---
  ts_long <- ts_raw %>%
    pivot_longer(
      cols      = all_of(metrics),
      names_to  = "metric",
      values_to = "doy"
    ) %>%
    # ensure doy is integer, then add to the first of each year
    mutate(
      doy = as.integer(doy),
      date = as.Date(paste0(year, "-01-01")) + (doy - 1)
    )
  
  
  
  ## --- 10. Extract Mean NDTI over same tile polygon ---
  
  # .tif directory
  tif_dir <- c("/projectnb/dietzelab/XinyuanJi/State_of_California_HLSL/2018",
               "/projectnb/dietzelab/XinyuanJi/State_of_California_HLSL/2019",
               "/projectnb/dietzelab/XinyuanJi/State_of_California_HLSS/2018",
               "/projectnb/dietzelab/XinyuanJi/State_of_California_HLSS/2019"
  )
  # tif_dir <- "/projectnb/dietzelab/XinyuanJi/anchor_sites/anchor_site_1"
  
  # .Fmask directory
  fmask_folder <- c("~/projectnb/XinyuanJi/State_of_California_HLSL/2018_Fmask",
                    "~/projectnb/XinyuanJi/State_of_California_HLSL/2019_Fmask",
                    "~/projectnb/XinyuanJi/State_of_California_HLSS/2018_Fmask",
                    "~/projectnb/XinyuanJi/State_of_California_HLSS/2019_Fmask"
  )
  
  # read both Landsat & Sentinel
  ref_file <- list.files(
    tif_dir,
    pattern = "B(06|07|11|12).*\\.tif$",
    full.names = TRUE
  )[1]
  
  # setup the ROI
  r0       <- rast(list.files(tif_dir, pattern="B11.*\\.tif$", full.names=TRUE)[1])
  roi_ndti <- project(roi_vect_wgs, crs(r0)) |> ext()
  
  # --------------------------------------------------------------------------
  # 1) KEEP ONLY THIS SITE’S TILE  (e.g. T10SGF), *not* every B06 in 2018
  # --------------------------------------------------------------------------
  # fine relevant files
  b11_all <- list.files(
    tif_dir,
    pattern    = paste0("T", tileID, ".*B11.*\\.tif$"),
    full.names = TRUE
  )
  # ...file cleaning, etc (copy your existing cleaning/overlap code here) ...
  
  # extracting dates from file name
  b11_dates <- as.Date(stringr::str_extract(b11_all, "\\d{7}"), "%Y%j")
  
  # Date filtering & NA-removal
  keep_idx <- !is.na(b11_dates) &
    b11_dates >= as.Date("2018-01-01") &
    b11_dates <= as.Date("2019-12-31")
  
  b11_sub   <- b11_all[keep_idx]
  b11_dates <- b11_dates[keep_idx]
  # ...date filtering etc as before...
  
  # Pre-allocating Results Vectors
  n_sen <- length(b11_sub)
  dates_sen  <- as.Date(rep(NA, n_sen))
  ndvals_sen <- numeric(n_sen)
  
  # for-loop processing Sentinel-2 data
  pb <- txtProgressBar(0, n_sen, style = 3)
  for (i in seq_along(b11_sub)) {
    b11 <- b11_sub[i]
    b12 <- sub("B11", "B12", b11)
    if (!file.exists(b12)) { setTxtProgressBar(pb, i); next }
    scene      <- basename(b11)
    scene_year <- stringr::str_extract(scene, "\\d{4}")
    fmask_dir  <- file.path("/projectnb/dietzelab/XinyuanJi/State_of_California_HLSS", paste0(scene_year, "_Fmask"))
    fmask_file <- file.path(fmask_dir, sub("B11.tif$", "Fmask.tif", scene))
    
    # Skip if missing
    if (!file.exists(b12) || !file.exists(fmask_file)) {
      setTxtProgressBar(pb, i)
      next
    }
    
    r11   <- crop(rast(b11), roi_ndti)
    r12   <- crop(rast(b12), roi_ndti)
    fmask <- crop(rast(fmask_file), roi_ndti)
    
    # calculate NDTI
    ndti <- (r11 - r12) / (r11 + r12)
    
    # cloud/shadow masking
    # 1 = cloud; 3 = cloud shadow; 4 = snow
    badmask <- (bitwAnd(values(fmask), bitwShiftL(1, 1)) != 0) |
      (bitwAnd(values(fmask), bitwShiftL(1, 3)) != 0) |
      (bitwAnd(values(fmask), bitwShiftL(1, 4)) != 0)
    ndti[badmask] <- NA
    
    # calculate the mean of all pixels in the raster
    nd_mean <- global(ndti, fun = mean, na.rm = TRUE)[1, 1]
    
    # storing the result
    if (is.na(nd_mean)) { setTxtProgressBar(pb, i); next }
    ndvals_sen[i] <- nd_mean
    dates_sen[i]  <- b11_dates[i]
    setTxtProgressBar(pb, i)
  }
  close(pb)
  
  ndti_sent <- tibble(
    date      = dates_sen,
    mean_ndti = ndvals_sen
  ) %>%
    filter(!is.na(date) & !is.na(mean_ndti)) %>% # remove invalid rows
    arrange(date) # sort the final result by date
  
  # --------------------------------------------------------------------------
  # LANDSAT: B06/B07; a copy of the code above but for Band 6 & 7
  # --------------------------------------------------------------------------
  b06_all <- list.files(
    tif_dir,
    pattern    = paste0("T", tileID, ".*B06.*\\.tif$"),
    full.names = TRUE
  )
  # ...copy file cleaning, overlap check code from above, but with B06 instead of B11 ...
  
  b06_dates <- as.Date(stringr::str_extract(b06_all, "\\d{7}"), "%Y%j")
  keep_idx <- !is.na(b06_dates) &
    b06_dates >= as.Date("2018-01-01") &
    b06_dates <= as.Date("2019-12-31")
  b06_sub   <- b06_all[keep_idx]
  b06_dates <- b06_dates[keep_idx]
  # ...date filtering...
  
  n_lan <- length(b06_sub)
  dates_lan  <- as.Date(rep(NA, n_lan))
  ndvals_lan <- numeric(n_lan)
  
  pb <- txtProgressBar(0, n_lan, style = 3)
  for (i in seq_along(b06_sub)) {
    b06 <- b06_sub[i]
    b07 <- sub("B06", "B07", b06)
    if (!file.exists(b07)) { setTxtProgressBar(pb, i); next }
    
    scene      <- basename(b06)
    scene_year <- stringr::str_extract(scene, "\\d{4}")
    fmask_dir  <- file.path("/projectnb/dietzelab/XinyuanJi/State_of_California_HLSL", paste0(scene_year, "_Fmask"))
    fmask_file <- file.path(fmask_dir, sub("B06.tif$", "Fmask.tif", scene))
    
    if (!file.exists(fmask_file)) {
      setTxtProgressBar(pb, i)
      next
    }
    
    r6    <- crop(rast(b06), roi_ndti)
    r7    <- crop(rast(b07), roi_ndti)
    fmask <- crop(rast(fmask_file), roi_ndti)
    
    # calculate NDTI
    ndti <- (r6 - r7) / (r6 + r7)
    
    badmask <- (bitwAnd(values(fmask), bitwShiftL(1, 1)) != 0) |
      (bitwAnd(values(fmask), bitwShiftL(1, 3)) != 0) |
      (bitwAnd(values(fmask), bitwShiftL(1, 4)) != 0)
    ndti[badmask] <- NA
    
    nd_mean <- global(ndti, fun = mean, na.rm = TRUE)[1, 1]
    
    
    if (is.na(nd_mean)) { setTxtProgressBar(pb, i); next }
    ndvals_lan[i] <- nd_mean
    dates_lan[i]  <- b06_dates[i]
    setTxtProgressBar(pb, i)
  }
  close(pb)
  
  ndti_land <- tibble(
    date      = dates_lan,
    mean_ndti = ndvals_lan
  ) %>%
    filter(!is.na(date) & !is.na(mean_ndti)) %>%
    arrange(date)
  
  # --------------------------------------------------------------------------
  # COMBINE AND (OPTIONALLY) INDICATE SENSOR
  # --------------------------------------------------------------------------
  ndti_df <- bind_rows(ndti_land, ndti_sent)
  
  
  
  # fill the gap
  ndti_df$mean_ndti_filled <- na.approx(ndti_df$mean_ndti, x = ndti_df$date, na.rm = FALSE)
  # smoothing with a Moving Average
  w <- 4
  k <- rep(1/w, w)
  ndti_df$smoothed <- as.numeric(stats::filter(ndti_df$mean_ndti_filled, k, sides = 2))
  
  # find annual minimum
  ndti_smooth <- ndti_df[!is.na(ndti_df$smoothed), ]
  min_row     <- ndti_smooth[which.min(ndti_smooth$smoothed), ]
  min_val     <- min_row$smoothed
  min_date    <- min_row$date
  
  # find the maximum before the minimum occured
  before_min  <- ndti_smooth[ndti_smooth$date < min_date, ]
  
  if (nrow(before_min) > 0 && !all(is.na(before_min$smoothed))) {
    max_before_min_row <- before_min[which.max(before_min$smoothed), ]
    max_before_min     <- max_before_min_row$smoothed
    max_before_min_date<- max_before_min_row$date
  } else {
    max_before_min     <- NA
    max_before_min_date<- NA
  }
  
  # calculate the Percentage Change (aka "drop")
  ratio <- if (!is.na(max_before_min) && max_before_min != 0) {
    (max_before_min - min_val) / max_before_min
  } else NA
  
  # Keep any extra info, e.g. treatment
  tibble(
    id                  = selected_anchor$id,
    site_name           = selected_anchor$site_name,
    Treatment_Control   = selected_anchor$Treatment_Control,
    min_ndti_smooth     = min_val,
    min_ndti_date       = min_date,
    max_before_min      = max_before_min,
    max_before_min_date = max_before_min_date,
    ndti_smooth_drop    = ratio
  )
}
  
  

# summary
summary_table <- anchors %>%
  split(.$id) %>%
  map_dfr(process_ndti_for_site)

# save the result
# write.csv(summary_table, "/projectnb/dietzelab/XinyuanJi/ndti_all_sites_summary.csv", row.names = FALSE)

  
  
# ndti_df$mean_ndti_filled <- na.approx(ndti_df$mean_ndti, x = ndti_df$date, na.rm = FALSE)
# w <- 4
# k <- rep(1/w, w)
# ndti_df$smoothed <- as.numeric(stats::filter(ndti_df$mean_ndti_filled, k, sides = 2))
# 
# ndti_smooth <- ndti_df[!is.na(ndti_df$smoothed), ]
# min_row     <- ndti_smooth[which.min(ndti_smooth$smoothed), ]
# min_val     <- min_row$smoothed
# min_date    <- min_row$date
# before_min  <- ndti_smooth[ndti_smooth$date < min_date, ]
# 
# if (nrow(before_min) > 0 && !all(is.na(before_min$smoothed))) {
#   max_before_min_row <- before_min[which.max(before_min$smoothed), ]
#   max_before_min     <- max_before_min_row$smoothed
#   max_before_min_date<- max_before_min_row$date
# } else {
#   max_before_min     <- NA
#   max_before_min_date<- NA
# }
# ratio <- if (!is.na(max_before_min) && max_before_min != 0) {
#   (max_before_min - min_val) / max_before_min
# } else NA

