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
#' @returns data.frame of mean EVI for each date that has available HLS data
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
                       datetime = roi_datetime,
                       limit = 1000) |>
    rstac::post_request()
  
  if (length(items$features) == 0) {
    stop("No HLS scenes found for the specified ROI and date range.")
  }
  
  assets <- rstac::items_assets(items)
  sf_items <- rstac::items_as_sf(items)
  # Add Granule ID for each feature
  granule_id <- sapply(items$features, function(feature) feature$id)
  sf_items <- cbind(granule = granule_id, sf_items)
  # Retrieve Asset URLs for each feature using our extract_asset_urls function and transpose them to columns
  asset_urls <- t(sapply(items$features, extract_asset_urls))
  colnames(asset_urls) <- c('blue', 'nir', 'red', 'fmask')
  sf_items <- cbind(sf_items, asset_urls)
  sf_items <- sf_items |>
    dplyr::filter(
      !red == "NULL", #NULL is stored as character
      !nir == "NULL",
      !blue == "NULL",
      !fmask == "NULL"
    )
  
  # Filter based on cloud cover
  sf_items <- sf_items[sf_items$eo.cloud_cover < 100,]
  # Reset Row Indices
  row.names(sf_items) <- NULL
  
  terra::setGDALconfig("GDAL_HTTP_UNSAFESSL", value = "YES")
  terra::setGDALconfig("GDAL_HTTP_COOKIEFILE", value = ".rcookies")
  terra::setGDALconfig("GDAL_HTTP_COOKIEJAR", value = ".rcookies")
  terra::setGDALconfig("GDAL_DISABLE_READDIR_ON_OPEN", value = "EMPTY_DIR")
  terra::setGDALconfig("CPL_VSIL_CURL_ALLOWED_EXTENSIONS", value = "TIF")
  
  # Open and crop
  red_stack <- lapply(sf_items$red, open_hls, roi = roi)
  nir_stack <- lapply(sf_items$nir, open_hls, roi = roi)
  blue_stack <- lapply(sf_items$blue, open_hls, roi = roi)
  
  fmask_stack <- lapply(
    sf_items$fmask,
    open_hls,
    roi = roi,
    is_fmask = TRUE
  )
  
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
#' @param is_fmask Whether the raster is an fmask quality layer
#'
#' @returns raster of the specified scene
#' 
open_hls <- function(url, roi = NULL, is_fmask = FALSE) {
  
  # Return NULL if no URL is available
  if (is.null(url) || is.na(url)) {
    return(NULL)
  }
  
  # Add VSICURL prefix for remote access
  url <- paste0("/vsicurl/", url)
  
  # Open raster
  r <- terra::rast(url)
  
  # Apply scale factor to HLS reflectance bands
  # Fmask is an integer quality layer and should not be scaled
  meta <- terra::describe(url)
  will_autoscale <- any(grepl("Scale:", meta))
  
  if (!will_autoscale && !is_fmask) {
    r <- r * 0.0001
  }
  
  # Crop and mask to ROI
  if (!is.null(roi)) {
    roi_reproj <- terra::project(roi, terra::crs(r))
    r <- terra::crop(r, roi_reproj)
    r <- terra::mask(r, roi_reproj)
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

#' FMASK filter
#'
#' @param fmask fmask values
#' @param selected_bit_nums selected bit numbers
#'
#' @returns mask

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


#' Wrapper function for double log calculation
#'
#' @param df data frame
#'
#' @returns modeled evi on each date

calc_curve_beck <- function(df) {
  year_to_run <- unique(lubridate::year(df$Date))
  if(length(year_to_run) >1){
    stop("EVI has to be calculated one year at a time")
  }
  # Add explicit NAs
  x <- df |>
    dplyr::mutate(Date = as.Date(Date),
                  img_doy = lubridate::yday(Date)) |>
    dplyr::group_by(img_doy) |>
    dplyr::summarise(
      Date = min(Date),
      evi = mean(evi, na.rm = TRUE),
      .groups = "drop"
    ) |>
    tidyr::complete(img_doy = 1:365)
  
  # Run double log function
  fit <- FitDoubleLogBeck(x$evi, t = x$img_doy, hessian = T, ninit = 100)
  
  # Format output
  out <- data.frame(
    param_name = names(fit$params),
    param_value = fit$params,
    stdError = fit$stdError
  )
  rownames(out) <- NULL
  
  pred_df_beck <- data.frame(doy = rep(1:365)) |>
    dplyr::cross_join(out |>
                        tidyr::pivot_wider(names_from = param_name,
                                           values_from = c(param_value, stdError))) |>
    dplyr::mutate(pred = param_value_mn + (param_value_mx - param_value_mn) *
                    (1/(1 + exp(-param_value_rsp * (doy - param_value_sos))) +
                       1/(1 + exp(param_value_rau * (doy - param_value_eos))))) |>
    dplyr::left_join(df |> dplyr::rename(doy = img_doy)) |>
    dplyr::mutate(method = "Beck",
                  Date = as.Date(paste0(year_to_run,"-01-01"))+ lubridate::days(doy-1))
  
  return(pred_df_beck)
}
