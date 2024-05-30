#' Parallel download data from the NASA ORNL DAAC server given period, spatial bounding box, and data DOI.
#'
#' @param ul_lat Numeric: upper left latitude.
#' @param lr_lat Numeric: lower right latitude.
#' @param ul_lon Numeric: upper left longitude.
#' @param lr_lon Numeric: lower right longitude.
#' @param ncore Numeric: numbers of core to be used if the maximum core
#' @param from Character: date from which the data search starts. In the form
#'   "yyyy-mm-dd".
#' @param to Character: date on which the data search end. In the form
#'   "yyyy-mm-dd".
#' @param outdir Character: path of the directory in which to save the
#'   downloaded files.Default to the working directory. If it doesn't exist it
#'   will be created. Ignored if \code{just_path=TRUE}
#' @param doi Character: data DOI on the NASA DAAC server, it can be obtained directly from the NASA ORNL DAAC data portal (e.g., GEDI L4A through https://daac.ornl.gov/cgi-bin/dsviewer.pl?ds_id=2056).
#' @param netrc_file Character: path to the credential file, default is NULL.
#' @param just_path Boolean: if we just want the metadata and URL or proceed the actual download.
#'
#' @return A list containing meta data for each data downloaded.
#' @export
#' 
#' @examples
#' \dontrun{
#' ul_lat <- 35
#' ul_lon <- -121
#' lr_lat <- 33
#' lr_lon <- -117
#' from <- "2022-02-23"
#' to <- "2022-05-30"
#' doi <- "10.3334/ORNLDAAC/2183"
#' metadata <- NASA_DAAC_download(ul_lat = ul_lat, 
#'                                ul_lon = ul_lon, 
#'                                lr_lat = lr_lat, 
#'                                lr_lon = lr_lon, 
#'                                from = from, 
#'                                to = to, 
#'                                doi = doi,
#'                                just_path = T)
#' }
#' @author Dongchen Zhang
#' @importFrom foreach %dopar%
NASA_DAAC_download <- function(ul_lat,
                               ul_lon,
                               lr_lat,
                               lr_lon,
                               ncore = 1,
                               from,
                               to,
                               outdir,
                               doi,
                               netrc_file = NULL,
                               just_path = FALSE) {
  # if there is no credential file in the outdir.
  # we will create a new one.
  # this function is located within the GEDI_AGB_prep script.
  if (length(list.files(outdir, pattern = "netrc")) == 0) {
    netrc_file <- getnetrc(outdir)
  }
  # setup arguments for URL.
  daterange <- c(from, to)
  cmrurl <- "https://cmr.earthdata.nasa.gov/search/"
  doisearch <- paste0(cmrurl, "collections.json?doi=", doi)
  request <- httr::GET(doisearch)
  httr::stop_for_status(request)
  concept_id <- httr::content(request, "parsed")$feed$entry[[1]]$id
  page <- 1
  bbox <- paste(ul_lon, lr_lat, lr_lon, ul_lat, sep = ",")
  url_format <- paste0("https://cmr.earthdata.nasa.gov/search/granules.json?", 
                       "pretty=true&provider=ORNL_CLOUD&page_size=2000&concept_id=%s", 
                       "&bounding_box=%s")
  request_url <- sprintf(url_format, concept_id, bbox)
  if (!is.null(daterange)) {
    url_format <- paste0(request_url, "&temporal=%s,%s")
    request_url <- sprintf(url_format, daterange[1], daterange[2])
  }
  granules_href <- entry <- c()
  repeat {
    response <- curl::curl_fetch_memory(paste0(request_url, 
                                               "&pageNum=", page))
    content <- rawToChar(response$content)
    result <- jsonlite::parse_json(content)
    entry <- c(entry, result$feed$entry)
    if (response$status_code != 200) {
      stop(paste("\n", result$errors, collapse = "\n"))
    }
    granules <- result$feed$entry
    if (length(granules) == 0) 
      break
    granules_href <- c(granules_href, sapply(granules, function(x) x$links[[1]]$href))
    page <- page + 1
  }
  if (!just_path) {
    message("using ", ncore, " cores")
    # download
    if (ncore > 1) {
      cl <- parallel::makeCluster(ncore)
      doParallel::registerDoParallel(cl)
      message("start download")
      foreach::foreach(
        i = 1:length(granules_href),
        .packages = "httr"
      ) %dopar% {
        response <-
          httr::GET(
            granules_href[i],
            httr::write_disk(file.path(outdir, basename(granules_href)[i]), overwrite = T),
            httr::config(netrc = TRUE, netrc_file = netrc_file),
            httr::set_cookies("LC" = "cookies")
          )
      }
      parallel::stopCluster(cl)
      foreach::registerDoSEQ()
    } else {
      message("using ", ncore, " core")
      for (i in seq_along(granules_href)) {
        response <-
          httr::GET(
            granules_href[i],
            httr::write_disk(file.path(outdir, basename(granules_href)[i]), overwrite = T),
            httr::config(netrc = TRUE, netrc_file = netrc_file),
            httr::set_cookies("LC" = "cookies")
          )
      }
    }
  }
  return(list(metadata = entry, path = file.path(outdir, basename(granules_href))))
}