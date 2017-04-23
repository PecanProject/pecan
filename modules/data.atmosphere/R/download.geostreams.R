#' Download Geostreams data from Clowder API
#'
#' @param outfolder directory in which to save json result. Will be created if necessary
#' @param sitename character. Should match a geostreams sensor_name
#' @param start_date,end_date datetime
#' @param url base url for Clowder host
#' @param ... other arguments passed as query parameters
#' @export
#' @importFrom PEcAn.utils logger.severe logger.info
#' @author Harsh Agrawal, Chris Black
#' @examples \dontrun{
#'  download.Geostreams(outfolder = "~/output/dbfiles/Clowder_EF",
#'                      sitename = "UIUC Energy Farm - CEN",
#'                      start_date = "2016-01-01", end_date="2016-12-31",
#'                      key="verysecret")
#' }
download.Geostreams <- function(outfolder, sitename, 
                                start_date, end_date,
                                url = "https://terraref.ncsa.illinois.edu/clowder/api/geostreams",
                                ...){
  # TODO Handle auth explicitly. Currently just passing key as part of ...
  
  start_date = lubridate::parse_date_time(start_date, orders = c("ymd", "ymdHMS", "ymdHMSz"), tz = "UTC")
  end_date = lubridate::parse_date_time(end_date, orders = c("ymd", "ymdHMS", "ymdHMSz"), tz = "UTC")

  sensor_result <- httr::GET(paste0(url, "/sensors"),
                           query = list(sensor_name = sitename, ...))
  httr::stop_for_status(sensor_result, "look up site info in Clowder")
  sensor_info <- jsonlite::fromJSON(httr::content(sensor_result, as = "text", encoding = "UTF-8"))
  sensor_mintime = lubridate::parse_date_time(sensor_info$min_start_time,
                                              orders = c("ymd", "ymdHMS", "ymdHMSz"), tz = "UTC")
  sensor_maxtime = lubridate::parse_date_time(sensor_info$max_end_time,
                                              orders = c("ymd", "ymdHMS", "ymdHMSz"), tz = "UTC")
  if (start_date < sensor_mintime) {
    logger.severe("Requested start date", start_date, "is before data begin", sensor_mintime)
  }
  if (end_date > sensor_maxtime) {
    logger.severe("Requested end date", end_date, "is after data end", sensor_maxtime)
  }

  sensor_id <- sensor_info$id
  query_args <- list(sensor_id = sensor_id, since = start_date, until = end_date, ...)
  met_result <- httr::GET(paste0(url, "/datapoints"), query = query_args)
  logger.info(met_result$url)
  httr::stop_for_status(met_result, "download met data from Clowder")

  dir.create(outfolder, showWarnings = FALSE, recursive = TRUE)
  result_file = file.path(outfolder, paste("Clowder", sitename, start_date, end_date, "json", sep="."))
  write(x = httr::content(met_result, as = "text", encoding = "UTF-8"),
        file=result_file)
  
  return(data.frame(file = result_file,
                    host = fqdn(),
                    mimetype = "text/json",
                    formatname = "Geostreams met",
                    startdate = start_date,
                    enddate = end_date,
                    dbfile.name = paste("Clowder", sitename, sep = "."),
                    stringsAsFactors = FALSE))
}
