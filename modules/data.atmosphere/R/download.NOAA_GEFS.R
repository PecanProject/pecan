##' @title Download NOAA GEFS Weather Data
##' 
##' @section Information on Units:
##' Information on NOAA weather units can be found below. Note that the temperature is measured in degrees C, 
##' but is converted at the station and downloaded in Kelvin.
##' @references https://www.ncdc.noaa.gov/crn/measurements.html
##' 
##' @section NOAA_GEFS General Information:
##' This function downloads NOAA GEFS weather data.  GEFS is an ensemble of 21 different weather forecast models.  
##' A 16 day forecast is avaliable every 6 hours.  Each forecast includes information on a total of 8 variables.  
##' These are transformed from the NOAA standard to the internal PEcAn
##' standard.
##' 
##' @section Data Avaliability:
##' NOAA GEFS weather data is avaliable on a rolling 12 day basis; dates provided in "start_date" must be within this range. The end date can be any point after
##' that, but if the end date is beyond 16 days, only 16 days worth of forecast are recorded.  Times are rounded down to the previous 6 hour forecast.  NOAA
##' GEFS weather data isn't always posted immediately, and to compensate, this function adjusts requests made in the last two hours
##' back two hours (approximately the amount of time it takes to post the data) to make sure the most current forecast is used.
##' 
##' @section Data Save Format:
##' Data is saved in the netcdf format to the specified directory.  File names reflect the precision of the data to the given range of days.
##' NOAA.GEFS.willow creek.3.2018-06-08T06:00.2018-06-24T06:00.nc specifies the forecast, using ensemble number 3 at willow creek on
##' June 6th, 2018 at 6:00 a.m. to June 24th, 2018 at 6:00 a.m.
##' 
##' @return A list of data frames is returned containing information about the data file that can be used to locate it later.  Each
##' data frame contains information about one file.
##'
##' @param outfolder Directory where results should be written
##' @param start_date, Range of dates/times to be downloaded (default assumed to be time that function is run) 
##' @param end_date, end date for range of dates to be downloaded (default 16 days from start_date)
##' @param lat.in site latitude in decimal degrees
##' @param lon.in site longitude in decimal degrees
##' @param site_id The unique ID given to each site. This is used as part of the file name.
##' @param sitename Site name 
##' @param username username from pecan workflow 
##' @param overwrite logical. Download a fresh version even if a local file with the same name already exists?
##' @param downscale logical, assumed True. Indicated whether data should be downscaled to hourly
##' @param ...  Additional optional parameters
##'
##' @export
##' 
##' @examples 
##' \dontrun{
##'  download.NOAA_GEFS(outfolder="~/Working/results", 
##'     lat.in= 45.805925, 
##'     lon.in = -90.07961, 
##'     site_id = 676)
##' }
##' 
##' @author Quinn Thomas, modified by K Zarada 
##' 
download.NOAA_GEFS <- function(site_id,
                               sitename = NULL,
                               username = 'pecan',
                               lat.in,
                               lon.in,
                               outfolder,
                               start_date= Sys.Date(),
                               end_date = start_date + lubridate::days(16),
                               downscale = TRUE,
                               overwrite = FALSE,
                               ...){
  
  forecast_date = as.Date(start_date)
  forecast_time = (lubridate::hour(start_date) %/% 6)*6
  
  end_hr = (as.numeric(difftime(end_date, start_date, units = 'hours')) %/% 6)*6
  
  model_name <- "NOAAGEFS_6hr"
  model_name_ds <-"NOAAGEFS_1hr" #Downscaled NOAA GEFS
  model_name_raw <- "NOAAGEFS_raw"
  
  PEcAn.logger::logger.info(paste0("Downloading GEFS for site ", site_id, " for ", start_date))
  
  PEcAn.logger::logger.info(paste0("Overwrite existing files: ", overwrite))
  
  
  noaa_grid_download(lat_list = lat.in,
                    lon_list = lon.in,
                    end_hr = end_hr,
                    forecast_time = forecast_time,
                    forecast_date = forecast_date,
                    model_name_raw = model_name_raw,
                    output_directory = outfolder)
  
  results <- process_gridded_noaa_download(lat_list = lat.in,
                                          lon_list = lon.in,
                                          site_id = site_id,
                                          downscale = downscale,
                                          overwrite = overwrite,
                                          forecast_date = forecast_date,
                                          forecast_time = forecast_time,
                                          model_name = model_name,
                                          model_name_ds = model_name_ds,
                                          model_name_raw = model_name_raw,
                                          output_directory = outfolder)
  return(results)
}
