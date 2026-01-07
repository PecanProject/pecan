#' Download ERA5 Climate Data from the Copernicus CDS API
#'
#' @description
#' Download ERA5 climate data from the Copernicus Climate Data Store (CDS) API as NetCDF files, year by year, according to user-specified parameters.
#' The function saves one NetCDF file per year in the specified output directory.
#'
#' @details
#' This function requires a valid CDS API key and the \code{ecmwfr} package for accessing the Copernicus Climate Data Store.
#' To get a Copernicus CDS API key, register at \url{https://cds.climate.copernicus.eu/profile}.
#' You must provide both \code{user} (UID) and \code{key} parameters from your CDS profile.
#'
#' You can check the "CC-BY" license under the \href{https://cds.climate.copernicus.eu/profile?tab=licences}{'licences' tab of your profile page}.
#' @param outfolder Character. Directory where downloaded NetCDF files will be saved.
#' @param start_date character: the start date of the data to be downloaded. Format is YYYY-MM-DD (will only use the year part of the date)
#' @param end_date character: the end date of the data to be downloaded. Format is YYYY-MM-DD (will only use the year part of the date)
#' @param extent numeric: a vector of numbers contains the bounding box (formatted as xmin, xmax, ymin, ymax) (longitude and latitude in degrees).
#' @param variables character: a vector contains variables to be downloaded (e.g., c("2m_temperature","surface_pressure")).
#' @param time Character vector or NULL. Hours of the day to download (e.g., c("00:00", "12:00")). Default to NULL to download all hours.
#' @param dataset Character. Name of the CDS dataset to use (default: "reanalysis-era5-single-levels").
#' @param product_type Character. Product type to request from CDS (default: "ensemble_members").
#' @param user Character. CDS user ID (UID) from your CDS profile. Required for authentication.
#' @param key Character. CDS API key from your CDS profile. Required for authentication.
#' @param timeout numeric: the maximum time (in seconds) allowed to download the data. The default is 36000 seconds.
#'
#' @return
#' A list where each element is a list containing:
#'   \item{file}{File path to the downloaded NetCDF file.}
#'   \item{host}{Host name where the file was downloaded.}
#'   \item{startdate}{Start date and time of the data in the file.}
#'   \item{enddate}{End date and time of the data in the file.}
#'   \item{mimetype}{MIME type of the file ("application/x-netcdf").}
#'   \item{formatname}{Format name ("ERA5_year.nc").}
#'
#' @examples
#' \dontrun{
#' # Download ERA5 reanalysis data for 2020
#' output_dir <- withr::local_tempdir()
#' era5_files <- download.ERA5_cds(
#'   outfolder = output_dir,
#'   start_date = "2020-01-01",
#'   end_date = "2020-06-30",
#'   extent = c(-72.2215, -72.1215, 42.4878, 42.5878),
#'   variables = c("2m_temperature", "surface_pressure"),
#'   user = "your_cds_user_id",
#'   key = "your_cds_api_key",
#'   product_type = "reanalysis"
#' )
#' 
#' # Download ensemble data for specificed hours only
#' era5_files <- download.ERA5_cds(
#'   outfolder = output_dir,
#'   start_date = "2020-01-01",
#'   end_date = "2020-12-31",
#'   extent = c(-83.05, -82.95, 42.95, 43.05),
#'   variables = "surface_solar_radiation_downwards",
#'   user = "your_cds_user_id",
#'   key = "your_cds_api_key",
#'   time = c("00:00", "12:00")
#' )
#' }
#' @export
#' 
#' @author Dongchen Zhang, Akash

download.ERA5_cds <- function(outfolder, start_date, end_date,
                              extent, variables, user, key, time = NULL,
                              dataset = "reanalysis-era5-single-levels",
                              product_type = "ensemble_members",
                              timeout = 36000) {
  
  # check for required package
  if (!requireNamespace("ecmwfr", quietly = TRUE)) {
    PEcAn.logger::logger.severe(
      "Package 'ecmwfr' is required for ERA5 downloads. ",
      "Install with: install.packages('ecmwfr'). ",
      "Get CDS credentials from: https://cds.climate.copernicus.eu/profile"
    )
  }
  
  if (!dir.exists(outfolder)) dir.create(outfolder, recursive = TRUE)
  
  # setup timeout for download.
  options(timeout=timeout)
  # convert arguments to CDS API specific arguments.
  years <- sort(unique(lubridate::year(seq(lubridate::date(start_date), lubridate::date(end_date), "1 year"))))
  months <- sort(unique(lubridate::month(seq(lubridate::date(start_date), lubridate::date(end_date), "1 month")))) |> 
    purrr::map(function(d)sprintf("%02d", d))
  days <- sort(unique(lubridate::day(seq(lubridate::date(start_date), lubridate::date(end_date), "1 day")))) |> 
    purrr::map(function(d)sprintf("%02d", d))
  
  # handle time argument: all hours if Null
  if (is.null(time)) {
    times <- sprintf("%02d:00", 0:23)
  } else {
    times <- time
  }
  
  # Format area for CDS API (North, West, South, East)
  area <- round(c(extent[4], extent[1], extent[3], extent[2]), 2)
  variables <- as.list(variables)
  
  # Set CDS credentials
  if (is.null(user) || is.null(key)) {
    PEcAn.logger::logger.severe(
      "CDS 'user' and 'key' must be provided. ",
      "Get them from: https://cds.climate.copernicus.eu/profile"
    )
  }
  ecmwfr::wf_set_key(user = user, key = key)

  # loop over years.
  nc.paths <- c()
  for (y in years) {
    fname <- file.path(outfolder, paste0("ERA5_", y, ".nc"))

    request <- list(
      dataset_short_name = dataset,
      product_type = list(product_type),
      data_format = 'netcdf',
      download_format = "unarchived",
      day = days,
      time = times,
      month = months,
      year = list(as.character(y)),
      area = area,
      variable = variables,
      target = basename(fname)
    )
    
    # Submit request using ecmwfr
    tryCatch({
      ecmwfr::wf_request(
        request = request,
        user = user,
        path = outfolder,
        time_out = timeout
      )
      nc.paths <- c(nc.paths, fname)
    }, error = function(e) {
      PEcAn.logger::logger.error(
        "Failed to download data for year ", y, ": ",
        conditionMessage(e)
      )
    })
  }
  
  # construct results to meet the requirements of pecan.met workflow.
  results <- vector("list", length = length(years))
  for (i in seq_along(results)) {
    results[[i]] <- list(file = nc.paths[i],
                         host = PEcAn.remote::fqdn(),
                         startdate = paste0(paste(years[i], months[1], days[1], sep = "-"), " ", times[1], ":00"),
                         enddate = paste0(paste(years[i], months[length(months)], days[length(days)], sep = "-"), " ", times[length(times)], ":00"),
                         mimetype = "application/x-netcdf",
                         formatname = "ERA5_year.nc")
  }
  return(results)
}