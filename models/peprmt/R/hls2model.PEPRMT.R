# R Code to convert harmonized landsat/sentinel images into PEPRMT EVI files

##' hls2model for PEPRMT
##'
##' @title hls2model.PEPRMT
##' @export
##' @param in.path location on disk where inputs are stored
##' @param in.prefix prefix of input and output files
##' @param outfolder location on disk where outputs will be stored
##' @param start_date,end_date the start and end dates of the data to be downloaded (will only use the year part of the date)
##' @param lat,lon latitude and longitude in degrees
##' @param overwrite should existing files be overwritten
##' @param verbose should the function be very verbose
##' @param ... additional arguments, currently ignored
##' @author Abigail Lewis (add names)
hls2model.PEPRMT <- function(in.path, in.prefix, outfolder, start_date, end_date,
                             edl_username = Sys.getenv("edl_username"), 
                             edl_password = Sys.getenv("edl_password"),
                             lat, lon,overwrite = FALSE, verbose = FALSE, ...) {

  PEcAn.logger::logger.info("START hls2model.PEPRMT")

  # Error if any of the soft dependencies we use here are not installed
  PEcAn.utils::need_packages(c("terra", "earthdatalogin", "dygraphs", "imager", "rstac", "xts"))

  # Format dates for file names
  start_date <- as.POSIXlt(start_date, tz = "UTC")
  start_date_string <- as.character(strptime(start_date, "%Y-%m-%d"))
  end_date <- as.POSIXlt(end_date, tz = "UTC")

  out.file <- paste0(in.prefix,
                     ".csv")
  out.file.full <- file.path(outfolder, out.file)
  
  roi <- terra::vect(in.path)
  
  df <- get_HLS_EVI(edl_username,
                    edl_password,
                    start_date,
                    end_date,
                    roi)
  
  fits_beck <- df |>
    dplyr::mutate(year = lubridate::year(Date)) |>
    dplyr::group_by(year) |>
    calc_curve_beck() |>
    dplyr::bind_rows()
  
  out <- fits_beck |>
    dplyr::mutate(Year = lubridate::year(Date),
                  DOY_disc = doy)|>
    dplyr::rename(EVI = pred) |>
    dplyr::select(Year, DOY_disc, EVI) 
  
  utils::write.table(out, out.file.full, quote = FALSE, sep = " ", row.names = FALSE, col.names = TRUE)
  
  } # hls2model.PEPRMT


#' Wrapper function for double log calculation
#'
#' @param df data frame
#'
#' @returns modeled evi on each date
#' @export
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
