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
#'   downloaded files. Default is the current work directory(getwd()).
#' @param doi Character: data DOI on the NASA DAAC server, it can be obtained 
#' directly from the NASA ORNL DAAC data portal (e.g., GEDI L4A through 
#' https://daac.ornl.gov/cgi-bin/dsviewer.pl?ds_id=2056).
#' @param netrc_file Character: path to the credential file, default is NULL.
#' @param just_path Boolean: if we just want the metadata and URL or proceed the actual download.
#'
#' @return A list containing meta data and physical path for each data downloaded.
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
#' outdir <- "/projectnb/dietzelab/dongchen/SHIFT/test_download"
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
                               outdir = getwd(),
                               doi,
                               netrc_file = NULL,
                               just_path = FALSE) {
  # if there is no credential file in the outdir.
  # we will create a new one.
  # this function is located within the GEDI_AGB_prep script.
  if (is.null(outdir) & !just_path) {
    PEcAn.logger::logger.info("Please provide outdir if you want to download the file.")
    return(0)
  } else if (!is.null(outdir) & !just_path & is.null(netrc_file)) {
    if (length(list.files(outdir, pattern = "netrc")) == 0) {
      netrc_file <- getnetrc(outdir)
    }
  }
  # setup arguments for URL.
  daterange <- c(from, to)
  # grab provider and concept id from CMR based on DOI.
  provider_conceptID <- NASA_CMR_finder(doi = doi)
  # setup page number and bounding box.
  page <- 1
  bbox <- paste(ul_lon, lr_lat, lr_lon, ul_lat, sep = ",")
  # loop over page number.
  # initialize variable for storing data.
  granules_href <- entry <- c()
  repeat {
    request_url <- NASA_DAAC_URL(provider = provider_conceptID$provider[1],
                                 concept_id = provider_conceptID$concept_id[1],
                                 page = page, 
                                 bbox = bbox, 
                                 daterange = daterange)
    response <- curl::curl_fetch_memory(request_url)
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
  # if we need to download the data.
  if (length(granules_href) == 0) {
    return(NA)
  }
  if (!just_path) {
    # printing out parallel environment.
    message("using ", ncore, " core")
    # download
    # if we have (or assign) more than one core to be allocated.
    if (ncore > 1) {
      # setup the foreach parallel computation.
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
      # if we only assign one core.
      # download data through general for loop.
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
    # return paths of downloaded data and the associated metadata.
    return(list(metadata = entry, path = file.path(outdir, basename(granules_href))))
  } else {
    return(basename(granules_href))
  }
}
#' Create URL that can be used to request data from NASA DAAC server.
#'
#' @param base_url Character: base URL for the CMR search. 
#' default is "https://cmr.earthdata.nasa.gov/search/granules.json?pretty=true".
#' @param provider Character: ID of data provider from NASA DAAC. See `NASA_CMR_finder` for more details.
#' @param page_size Numeric: maximum requested length, default is 2000.
#' @param page Numeric: which page of the URL, default is 1.
#' @param concept_id Character: CMR Concept ID. See `NASA_CMR_finder` for more details.
#' @param bbox Numeric: vector of bounding box coordinates.
#' @param daterange Character: vectors of the requested start and end dates. In the form "yyyy-mm-dd".
#'
#' @return A character of URL that can be used to request data.
#' 
#' @examples
#' \dontrun{
#' provider <- "ORNL_CLOUD"
#' concept_id <- "C2770099044-ORNL_CLOUD"
#' bbox <- "-121,33,-117,35"
#' daterange <- c("2022-02-23", "2022-05-30")
#' URL <- NASA_DAAC_URL(provider = provider, 
#' concept_id = concept_id, 
#' bbox = bbox, 
#' daterange = daterange)
#' }
#' @author Dongchen Zhang
NASA_DAAC_URL <- function(base_url = "https://cmr.earthdata.nasa.gov/search/granules.json?pretty=true",
                          provider, page_size = 2000, page = 1, concept_id, bbox, daterange = NULL) {
  ## split url.
  provider_url <- paste0("&provider=", provider)
  page_size_url <- paste0("&page_size=", page_size)
  concept_id_url <- paste0("&concept_id=", concept_id)
  bounding_box_url <- paste0("&bounding_box=", bbox)
  URL <- paste0(base_url, provider_url, page_size_url, concept_id_url, bounding_box_url)
  if (!is.null(daterange)) {
    temporal_url <- sprintf("&temporal=%s,%s", daterange[1], daterange[2])
    URL <- paste0(URL, temporal_url)
  }
  page_url <- paste0("&pageNum=", page)
  URL <- paste0(URL, page_url)
  return(URL)
}
#' Create URL that can be used to request data from NASA DAAC server.
#'
#' @param doi Character: data DOI on the NASA DAAC server, it can be obtained 
#' directly from the NASA ORNL DAAC data portal (e.g., GEDI L4A through 
#' https://daac.ornl.gov/cgi-bin/dsviewer.pl?ds_id=2056).
#'
#' @return A list with each containing corresponding provider and concept ids 
#' given the data doi.
#' 
#' @examples
#' \dontrun{
#' provider_conceptID <- NASA_CMR_finder("10.3334/ORNLDAAC/2183")
#' }
#' @author Dongchen Zhang
#' @importFrom magrittr %>%
NASA_CMR_finder <- function(doi) {
  # base URL for searching CMR database.
  cmrurl <- "https://cmr.earthdata.nasa.gov/search/"
  # create new URL based on data doi.
  doisearch <- paste0(cmrurl, "collections.json?doi=", doi)
  # grab results.
  request <- httr::GET(doisearch)
  httr::stop_for_status(request)
  results <- httr::content(request, "parsed")
  # grab paried provider-conceptID records.
  provider <- results$feed$entry %>% purrr::map("data_center") %>% unlist
  concept_id <- results$feed$entry %>% purrr::map("id") %>% unlist
  # return results.
  return(as.list(data.frame(cbind(provider, concept_id))))
}