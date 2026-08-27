#' Download harmonized landsat/sentinel imagery
#' 
#' @description This function loads HLS data from NASA EARTHDATA and calculates 
#' mean Enhanced Vegetation Index (EVI) for a set polygon at every timestep
#' 
#' Created using:
#' https://github.com/nasa/HLS-Data-Resources/blob/main/r/HLS_Tutorial.Rmd
#'
#' @param edl_username earth data login username
#' @param edl_password earth data login password
#' @param start_date imagery start date
#' @param end_date imagery end date
#' @param roi region of interest (spatial vector created using terra::vect)
#'
#' @returns data.frame of mean EVI for each date with available HLS data
#' @export
#' 
#' @author Abigail Lewis
#'
get_HLS_EVI <- function(edl_username,
                         edl_password,
                         start_date,
                         end_date,
                         roi
                         ) {
  
  # AUTHENTICATION
  earthdatalogin::edl_netrc(username = edl_username,
                            password = edl_password)

  # SET SPATIAL/TEMPORAL EXTENT
  start_date <- paste0(as.Date(start_date), "T00:00:00Z")
  end_date <- paste0(as.Date(end_date), "T23:59:59Z")
  s = rstac::stac("https://cmr.earthdata.nasa.gov/stac/LPCLOUD/")
  HLS_col <- list("HLSS30_2.0", "HLSL30_2.0")
  roi_extent <- terra::ext(roi)
  bbox <- c(roi_extent$xmin, roi_extent$ymin, roi_extent$xmax, roi_extent$ymax)
  roi_datetime <- paste(start_date, end_date, sep = "/")
  
  # LOAD IMAGE LIST
  items <- s |>
    rstac::stac_search(collections = HLS_col,
                       bbox = bbox,
                       datetime = roi_datetime) |>
    rstac::post_request()
  assets <- rstac::items_assets(items)
  sf_items <- rstac::items_as_sf(items)
  # Add Granule ID for each feature
  granule_id <- sapply(items$features, function(feature) feature$id)
  sf_items <- cbind(granule = granule_id, sf_items)
  # Retrieve Asset URLs for each feature using our extract_asset_urls function and transpose them to columns
  asset_urls <- t(sapply(items$features, extract_asset_urls))
  colnames(asset_urls) <- c('blue', 'nir', 'red', 'fmask')
  sf_items <- cbind(sf_items, asset_urls)
  
  # Filter based on cloud cover
  sf_items <- sf_items[sf_items$eo.cloud_cover < 100,]
  # Reset Row Indices
  row.names(sf_items) <- NULL
  
  terra::setGDALconfig("GDAL_HTTP_UNSAFESSL", value = "YES")
  terra::setGDALconfig("GDAL_HTTP_COOKIEFILE", value = ".rcookies")
  terra::setGDALconfig("GDAL_HTTP_COOKIEJAR", value = ".rcookies")
  terra::setGDALconfig("GDAL_DISABLE_READDIR_ON_OPEN", value = "EMPTY_DIR")
  terra::setGDALconfig("CPL_VSIL_CURL_ALLOWED_EXTENSIONS", value = "TIF")
  
  # Test opening and crop
  red_stack <- lapply(sf_items$red, open_hls, roi = roi)
  nir_stack <- lapply(sf_items$nir, open_hls, roi = roi)
  blue_stack <- lapply(sf_items$blue, open_hls, roi = roi)
  fmask_stack <- lapply(sf_items$fmask, open_hls, roi = roi)
  
  # Calculate EVI For all of our scenes
  evi_stack <- mapply(calculate_EVI, nir_stack, red_stack, blue_stack, SIMPLIFY = FALSE)
  
  # Rename the scenes in our list
  names(evi_stack) <- sf_items$datetime
  # Create a single Rast Object from our list
  evi_stacks <- terra::rast(evi_stack)
  
  selected_bit_nums <- c(1,2,3,4,5)
  
  # Create list of masks
  qmask_stack <- lapply(fmask_stack, build_mask, selected_bit_nums=selected_bit_nums)
  
  # Apply Mask to EVI using NA Values
  evi_masked <- mapply(function(x, y) {
    terra::mask(x, y, maskvalue = TRUE, updatevalue = NA)
  }, evi_stack, qmask_stack, SIMPLIFY = FALSE)
  
  evi_masked <- terra::rast(evi_masked)
  
  # Add Date Only Column
  sf_items$date <- sapply(sf_items$datetime, function(x) strsplit(x, "T")[[1]][1])
  
  evi_mean <- terra::global(evi_masked, 'mean', na.rm=TRUE)
  
  stats <- data.frame(
    mean = evi_mean
  )
  stats$Date <- lubridate::ymd_hms(sf_items$datetime) # convert string to date format (ISO 8601)
  
  df <- stats |>
    dplyr::select(Date, mean) |>
    dplyr::rename(evi = mean) |>
    dplyr::mutate(img_doy = lubridate::yday(Date))
  
  return(df)
}

#' Extract asset urls
#' 
#' @description
#' Define a function to extract asset urls for selected bands
#' This also includes a check to ensure the correct bands are extracted
#' depending on the collection (HLSL30 or HLSS30)
#'
#' @param feature feature from stac
#'
#' @returns URL for each asset
extract_asset_urls <- function(feature) {
  collection_id <- feature$collection
  if (collection_id == "HLSS30_2.0") {
    bands = c('B02', 'B8A','B04','Fmask')
  } else if (collection_id == "HLSL30_2.0") {
    bands = c('B02', 'B05','B04','Fmask')}
  sapply(bands, function(band) feature$assets[[band]]$href)
}

#' Open HLS
#' 
#' @description
#' This function reads an HLS scene from a URL, applies the scale factor if 
#' necessary, and optionally crops and
#' masks the scene based on a polygon. It requries the above GDAL configurations 
#' and a .netrc file. A .netrc can be created by running 
#' `earthdatalogin::edl_netrc()`.
#'
#' @param url URL of scene
#' @param roi region of interest
#'
#' @returns raster of the specified scene
#' 
open_hls <- function(url, roi = NULL) {
  if(is.null(url)){return(NA)}
  # Add VSICURL prefix
  url <- paste0('/vsicurl/', url)
  # Retrieve metadata
  meta <- terra::describe(url)
  # Check if dataset is Quality Layer (Fmask) - no scaling this asset (int8 datatype)
  is_fmask <- any(grep("Fmask", meta))
  # Check if Scale is present in band metadata
  will_autoscale <- any(grep("Scale:", meta))
  # Read the raster
  r <- terra::rast(url)
  # Apply Scale Factor if necessary
  if (!will_autoscale && !is_fmask){
    print(paste("No scale factor found in band metadata. Applying scale factor of 0.0001 to", basename(url)))
    r <- r * 0.0001
  }
  # Crop if roi specified
  if (!is.null(roi)){
    # Reproject roi to match crs of r
    roi_reproj <- terra::project(roi, terra::crs(r))
    r <- terra::mask(terra::crop(r, roi_reproj), roi_reproj)
  }
  return(r)
}

#' Calculate EVI
#'
#' @param nir NIR value
#' @param red Red value
#' @param blue Blue value
#'
#' @returns Calculated EVI
#'
calculate_EVI <- function(nir, red, blue){
  evi <- 2.5 * (nir - red) / (nir + 6 * red - 7.5 * blue + 1.0)
  return(evi)
}

# Filter based on quality
build_mask <- function(fmask, selected_bit_nums){
  # Create a mask of all zeros
  mask <- terra::rast(fmask, vals=0)
  for (b in selected_bit_nums){
    # Apply Bitwise AND to fmask values and selected bit numbers
    mask_temp <- terra::app(fmask, function(x) bitwAnd(x, bitwShiftL(1,b)) >0)
    # Update Mask to maintain only 1 layer with bitwise OR
    mask <- mask | mask_temp
  }
  return(mask)
}

start_date <- as.Date("2026-01-01")
end_date <- as.Date("2026-12-31")
spat_vect <- data.frame(Longitude = c(-76.54977013137942,
                                      -76.54990270236144,
                                      -76.5498590234411,
                                      -76.54971736079783,
                                      -76.54977013137942),
                        Latitude = c(38.87398361209654,
                                     38.87392850962871,
                                     38.873852498024824,
                                     38.87390242573706,
                                     38.87398361209654))
spat_vect <- as.matrix(spat_vect)

roi <- terra::vect(spat_vect, type = "polygons", crs = "EPSG:4326")
source("FitDoubleLogBeck.R")


fits_beck <- calc_curve(df = df, method = "Beck") |>
  dplyr::bind_rows()

pred_df_beck <- data.frame(doy = rep(1:365)) |>
  dplyr::cross_join(fits_beck |>
              tidyr::pivot_wider(names_from = param_name,
                          values_from = c(param_value, stdError))) |>
  dplyr::mutate(pred = param_value_mn + (param_value_mx - param_value_mn) *
           (1/(1 + exp(-param_value_rsp * (doy - param_value_sos))) +
              1/(1 + exp(param_value_rau * (doy - param_value_eos))))) |>
  dplyr::left_join(df |> dplyr::rename(doy = img_doy)) |>
  dplyr::mutate(method = "Beck")


pred_df_beck |>
  ggplot2::ggplot(ggplot2::aes(x = doy, y = evi, color = as.factor(lubridate::year(Date)))) +
  ggplot2::geom_point() +
  ggplot2::geom_line(ggplot2::aes(y = pred), color = "blue") +
  ggplot2::theme_minimal() +
  ggplot2::labs(title = "EVI and Double Log Fit (Beck Method)",
       x = "Day of Year",
       y = "EVI")

formatted <- pred_df_beck |>
  select(id, doy, evi, pred, method, Date) |>
  rename(evi_observed = evi,
         evi_predicted = pred) |>
  mutate(Date_mod = as.Date(doy - 1, origin = paste0(year, "-01-01")))

final <- formatted |>
  mutate(evi_observed = ifelse(year(Date) == year(Date_mod), evi_observed, NA)) |>
  select(-Date) |>
  rename(Date = Date_mod)