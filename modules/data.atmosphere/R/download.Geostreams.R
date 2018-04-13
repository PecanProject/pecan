#' Download Geostreams data from Clowder API
#'
#' @param outfolder directory in which to save json result. Will be created if necessary
#' @param sitename character. Must match a Geostreams sensor_name
#' @param start_date,end_date datetime
#' @param url base url for Clowder host
#' @param key,user,pass authentication info for Clowder host.
#' @param ... other arguments passed as query parameters
#' @details Depending on the setup of your Clowder host, authentication may be by
#'   username/password, by API key, or skipped entirely. \code{download.Geostreams}
#'   looks first in its call arguments for an API key, then a username and password,
#'   then if these are NULL it looks in the user's home directory for a file named
#'   `~/.pecan.clowder.xml`, and finally if no keys or passwords are found there it
#'   attempts to connect unauthenticated.
#'
#' If using `~/.pecan.clowder.xml`, it must be a valid PEcAn-formatted XML settings
#'  file and must contain a \code{<clowder>} key that specifies hostname, user, and
#'  password for your Clowder server:
#'
#' \code{\preformatted{
#'   <?xml version="1.0"?>
#'   <pecan>
#'     <clowder>
#'       <hostname>terraref.ncsa.illinois.edu</hostname>
#'       <user>yourname</user>
#'       <password>superSecretPassw0rd</password>
#'     </clowder>
#'   </pecan>
#' }}
#'
#' @export
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
                                key = NULL,
                                user = NULL,
                                pass = NULL,
                                ...){

  start_date = lubridate::parse_date_time(start_date, orders = c("ymd", "ymdHMS", "ymdHMSz"), tz = "UTC")
  end_date = lubridate::parse_date_time(end_date, orders = c("ymd", "ymdHMS", "ymdHMSz"), tz = "UTC")

  auth <- get_clowderauth(key, user, pass, url)

  sensor_result <- httr::GET(url = paste0(url, "/sensors"),
                             query = list(sensor_name = sitename, key = auth$key, ...),
                             config = auth$userpass)
  httr::stop_for_status(sensor_result, "look up site info in Clowder")
  sensor_txt <- httr::content(sensor_result, as = "text", encoding = "UTF-8")
  sensor_info <- jsonlite::fromJSON(sensor_txt)
  sensor_id <- sensor_info$id

  # Request that Geostreams recalculate data availability for us
  # otherwise start and end dates may be null or stale
  update_result <- httr::GET(url = paste0(url, "/sensors/", sensor_id, "/update"),
                             query = list(key = auth$key, ...),
                             config = auth$userpass)
  update_status <- jsonlite::fromJSON(httr::content(update_result, as = "text", encoding = "UTF-8"))
  if (update_status$status != "updated") {
    PEcAn.logger::logger.error("Failed to update Clowder data availability.",
                               "Will attempt to get data anyway, but recently-added dates might not be found")
  }

  # Now get updated sensor info
  sensor_result <- httr::GET(url = paste0(url, "/sensors/", sensor_id),
                             query = list(key = auth$key, ...),
                             config = auth$userpass)
  httr::stop_for_status(sensor_result, "look up site info in Clowder")
  sensor_txt <- httr::content(sensor_result, as = "text", encoding = "UTF-8")
  sensor_info <- jsonlite::fromJSON(sensor_txt)
  sensor_mintime = lubridate::parse_date_time(sensor_info$min_start_time,
                                              orders = c("ymd", "ymdHMS", "ymdHMSz"), tz = "UTC")
  sensor_maxtime = lubridate::parse_date_time(sensor_info$max_end_time,
                                              orders = c("ymd", "ymdHMS", "ymdHMSz"), tz = "UTC")
  if (start_date < sensor_mintime) {
    PEcAn.logger::logger.severe("Requested start date", start_date, "is before data begin", sensor_mintime)
  }
  if (end_date > sensor_maxtime) {
    PEcAn.logger::logger.severe("Requested end date", end_date, "is after data end", sensor_maxtime)
  }

  result_files = c()
  for (year in lubridate::year(start_date):lubridate::year(end_date)) {
    query_args <- list(
      sensor_id = sensor_id,
      since = max(start_date, lubridate::ymd(paste0(year, "-01-01"), tz="UTC")),
      until = min(end_date, lubridate::ymd(paste0(year, "-12-31"), tz="UTC")),
      key = auth$key,
      ...)

    met_result <- httr::GET(url = paste0(url, "/datapoints"),
                            query = query_args,
                            config = auth$userpass)
    PEcAn.logger::logger.info(met_result$url)
    httr::stop_for_status(met_result, "download met data from Clowder")
    result_txt <- httr::content(met_result, as = "text", encoding = "UTF-8")
    combined_result <- paste0(
      '{"sensor_info":', sensor_txt, ',\n',
      '"data":', result_txt, '}')

    dir.create(outfolder, showWarnings = FALSE, recursive = TRUE)
    out_file <- file.path(
      outfolder,
      paste("Geostreams", sitename, start_date, end_date, year, "json", sep="."))
    write(x = combined_result, file=out_file)
    result_files = append(result_files, out_file)
  }

  return(data.frame(file = result_files,
                    host = PEcAn.remote::fqdn(),
                    mimetype = "application/json",
                    formatname = "Geostreams met",
                    startdate = start_date,
                    enddate = end_date,
                    dbfile.name = paste("Geostreams", sitename, start_date, end_date, sep = "."),
                    stringsAsFactors = FALSE))
}

#' Authentication lookup helper
#' 
#' @param key,user,pass passed unchanged from \code{\link{download.Geostreams}} call, possibly null
#' @param url matched against \code{<hostname>} in authfile, ignored if authfile contains no hostname.
#' @param authfile path to a PEcAn-formatted XML settings file; must contain a \code{<clowder>} key
#'
get_clowderauth <- function(key, user, pass, url, authfile="~/.pecan.clowder.xml") {
  if (!is.null(key)) {
    return(list(key = key))
  } else if (!is.null(user) && !is.null(pass)) {
    return(list(userpass = httr::authenticate(user = user, password = pass)))
  } else {
    if (file.exists(authfile)) {
      auth_file <- XML::xmlToList(XML::xmlParse(authfile))$clowder
      if (!is.null(auth_file$hostname) && auth_file$hostname != httr::parse_url(url)$host) {
        # auth in file isn't for this host; exit
        return(NULL)
      }
      if (!is.null(auth_file$key)) {
        return(list(key=key))
      } else {
        # allow for cases where one of user/pass given as argument and other is in file
        if (is.null(user)) { user <- auth_file$user }
        if (is.null(pass)) { pass <- auth_file$password }
        if (xor(is.null(user), is.null(pass))) { return(NULL) }
        return(list(userpass = httr::authenticate(user = user, password = pass)))
      }
    } else {
      return(NULL)
    }
  }
}
