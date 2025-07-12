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
#' @param band Character: the band name (or vector of band names) of data to be requested.
#' @param credential_path Character: physical path to the credential file (.netrc file). The default NULL.
#' @param doi Character: data DOI on the NASA DAAC server, it can be obtained 
#' directly from the NASA ORNL DAAC data portal (e.g., GEDI L4A through 
#' https://daac.ornl.gov/cgi-bin/dsviewer.pl?ds_id=2056).
#' @param just_path Boolean: if we just want the metadata and URL or proceed the actual download.
#'
#' @return Physical paths of downloaded files (just_path = T) or URLs of the files (just_path = F).
#' @export
#' 
#' @examples
#' \dontrun{
#' # SHIFT Hyper-spectral data.
#' ul_lat <- 35
#' ul_lon <- -121
#' lr_lat <- 33
#' lr_lon <- -117
#' from <- "2022-02-23"
#' to <- "2022-05-30"
#' doi <- "10.3334/ORNLDAAC/2183"
#' paths <- NASA_DAAC_download(ul_lat = ul_lat, 
#'                             ul_lon = ul_lon, 
#'                             lr_lat = lr_lat, 
#'                             lr_lon = lr_lon, 
#'                             from = from, 
#'                             to = to, 
#'                             doi = doi,
#'                             just_path = T)
#' # GEDI level 4A data.
#' ul_lat <- 85
#' ul_lon <- -179
#' lr_lat <- 7
#' lr_lon <- -20
#' from <- "2020-01-01"
#' to <- "2020-12-31"
#' doi <- "10.3334/ORNLDAAC/2056"
#' paths <- NASA_DAAC_download(ul_lat = ul_lat, 
#'                             ul_lon = ul_lon, 
#'                             lr_lat = lr_lat, 
#'                             lr_lon = lr_lon, 
#'                             from = from, 
#'                             to = to, 
#'                             band = "V2_1",
#'                             doi = doi,
#'                             just_path = T)
#' # MODIS LAI data.
#' ul_lat <- 85
#' ul_lon <- -179
#' lr_lat <- 7
#' lr_lon <- -20
#' from <- "2020-01-01"
#' to <- "2020-01-31"
#' doi <- "10.5067/MODIS/MCD15A3H.061"
#' paths <- NASA_DAAC_download(ul_lat = ul_lat, 
#'                             ul_lon = ul_lon, 
#'                             lr_lat = lr_lat, 
#'                             lr_lon = lr_lon, 
#'                             from = from, 
#'                             to = to, 
#'                             doi = doi,
#'                             just_path = T)
#' # SMAP Soil Moisture data.
#' ul_lat <- 85
#' ul_lon <- -179
#' lr_lat <- 7
#' lr_lon <- -20
#' from <- "2020-01-01"
#' to <- "2020-01-31"
#' doi <- "10.5067/LWJ6TF5SZRG3"
#' paths <- NASA_DAAC_download(ul_lat = ul_lat, 
#'                             ul_lon = ul_lon, 
#'                             lr_lat = lr_lat, 
#'                             lr_lon = lr_lon, 
#'                             from = from, 
#'                             to = to, 
#'                             doi = doi,
#'                             just_path = T)
#' # GLANCE Phenology and LC data.
#' ul_lat <- 85
#' ul_lon <- -179
#' lr_lat <- 7
#' lr_lon <- -20
#' from <- "2019-01-01"
#' to <- "2019-12-31"
#' doi <- "10.5067/MEaSUREs/GLanCE/GLanCE30.001"
#' paths <- NASA_DAAC_download(ul_lat = ul_lat, 
#'                             ul_lon = ul_lon, 
#'                             lr_lat = lr_lat, 
#'                             lr_lon = lr_lon, 
#'                             from = from, 
#'                             to = to, 
#'                             doi = doi,
#'                             just_path = T)
#' # HLS reflectance data.
#' ul_lat <- 35
#' ul_lon <- -121
#' lr_lat <- 33
#' lr_lon <- -117
#' from <- "2022-02-23"
#' to <- "2022-05-30"
#' doi <- "10.5067/HLS/HLSS30.002"
#' paths <- NASA_DAAC_download(ul_lat = ul_lat, 
#'                             ul_lon = ul_lon, 
#'                             lr_lat = lr_lat, 
#'                             lr_lon = lr_lon, 
#'                             from = from, 
#'                             to = to, 
#'                             doi = doi,
#'                             just_path = T)
#'                             ul_lat <- 35
#' # HLS Phenology data.
#' ul_lon <- -121
#' lr_lat <- 33
#' lr_lon <- -117
#' from <- "2019-01-01"
#' to <- "2019-12-31"
#' doi <- "10.5067/Community/MuSLI/MSLSP30NA.011"
#' paths <- NASA_DAAC_download(ul_lat = ul_lat,
#'                             ul_lon = ul_lon,
#'                             lr_lat = lr_lat,
#'                             lr_lon = lr_lon,
#'                             from = from,
#'                             to = to,
#'                             doi = doi,
#'                             just_path = T)
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
                               band = NULL,
                               credential_path = NULL,
                               doi,
                               just_path = FALSE) {
  # Determine if we have enough inputs.
  if (is.null(outdir) & !just_path) {
    message("Please provide outdir if you want to download the file.")
    return(NA)
  }
  # setup DAAC Credentials.
  # detect if we need the credential or not.
  if (!just_path & is.null(credential_path)) {
    PEcAn.logger::logger.info("Please provide the physical path to the credential file!")
    return(NA)
  }
  if (!just_path) {
    netrc <- getnetrc(credential_path)
  }
  # setup arguments for URL.
  daterange <- c(from, to)
  # grab provider and concept id from CMR based on DOI.
  provider_conceptID <- NASA_CMR_finder(doi = doi)
  # setup page number and bounding box.
  page <- 1
  bbox <- paste(ul_lon, lr_lat, lr_lon, ul_lat, sep = ",")
  # initialize variable for storing data.
  granules_href <- c()
  # loop over providers.
  for (i in seq_along(provider_conceptID[[2]])) {
    # loop over page number.
    repeat {
      request_url <- NASA_DAAC_URL(provider = provider_conceptID$provider[i],
                                   concept_id = provider_conceptID$concept_id[i],
                                   page = page, 
                                   bbox = bbox, 
                                   daterange = daterange)
      response <- curl::curl_fetch_memory(request_url)
      content <- rawToChar(response$content)
      result <- jsonlite::parse_json(content)
      if (response$status_code != 200) {
        stop(paste("\n", result$errors, collapse = "\n"))
      }
      granules <- result$feed$entry
      if (length(granules) == 0) 
        break
      # grab raw URLs from the records.
      granules_href <- c(granules_href, sapply(granules, function(x) {sapply(x$links,function(y) y$href)}))
      # grab specific band.
      if (!is.null(band)) {
        granules_href <- granules_href[which(grepl(band, basename(granules_href), fixed = T))]
      }
      page <- page + 1
    }
  }
  # if no files are found.
  if (is.null(granules_href)) {
    PEcAn.logger::logger.info("No files found. Please check the spatial and temporal search window.")
    return(NA)
  }
  # remove non-target files (e.g. s3)
  granules_href <- granules_href[which(grepl("https*", granules_href))]
  # remove duplicated files.
  inds <- which(duplicated(basename(granules_href)))
  if (length(inds) > 0) {
    granules_href <- granules_href[-inds]
  }
  # remove non-image files.
  inds <- which(stringr::str_ends(basename(granules_href), ".h5") |
                  stringr::str_ends(basename(granules_href), ".tif") |
                  stringr::str_ends(basename(granules_href), ".hdf") |
                  stringr::str_ends(basename(granules_href), ".nc"))
  granules_href <- granules_href[inds]
  # detect existing files if we want to download the files.
  if (!just_path) {
    same.file.inds <- which(basename(granules_href) %in% list.files(outdir))
    if (length(same.file.inds) > 0) {
      granules_href <- granules_href[-same.file.inds]
    }
  }
  # if we need to download the data.
  if (length(granules_href) == 0) {
    return(NA)
  }
  if (!just_path) {
    # check if the doSNOW package is available.
    if ("try-error" %in% class(try(find.package("doSNOW")))) {
      message("The doSNOW package is not installed.")
      return(NA)
    }
    # printing out parallel environment.
    message("using ", ncore, " core")
    message("start downloading ", length(granules_href), " files.")
    # download
    # if we have (or assign) more than one core to be allocated.
    if (ncore > 1) {
      # setup the foreach parallel computation.
      cl <- parallel::makeCluster(ncore)
      doParallel::registerDoParallel(cl)
      # record progress.
      doSNOW::registerDoSNOW(cl)
      pb <- utils::txtProgressBar(min=1, max=length(granules_href), style=3)
      progress <- function(n) utils::setTxtProgressBar(pb, n)
      opts <- list(progress=progress)
      foreach::foreach(
        i = 1:length(granules_href),
        .packages=c("httr","Kendall"),
        .options.snow=opts
      ) %dopar% {
        # if there is a problem in downloading file.
        while ("try-error" %in% class(try(
          response <-
          httr::GET(
            granules_href[i],
            httr::write_disk(file.path(outdir, basename(granules_href)[i]), overwrite = T),
            httr::config(netrc = TRUE, netrc_file = netrc),
            httr::set_cookies("LC" = "cookies")
          )
        ))){
          response <-
            httr::GET(
              granules_href[i],
              httr::write_disk(file.path(outdir, basename(granules_href)[i]), overwrite = T),
              httr::config(netrc = TRUE, netrc_file = netrc),
              httr::set_cookies("LC" = "cookies")
            )
        }
        # Check if we can successfully open the downloaded file.
        # if it's H5 file.
        if (grepl(pattern = ".h5", x = basename(granules_href)[i], fixed = T)) {
          # check if the hdf5r package exists.
          if ("try-error" %in% class(try(find.package("hdf5r")))) {
            message("The hdf5r package is not installed.")
            return(NA)
          }
          while ("try-error" %in% class(try(hdf5r::H5File$new(file.path(outdir, basename(granules_href)[i]), mode = "r"), silent = T))) {
            response <-
              httr::GET(
                granules_href[i],
                httr::write_disk(file.path(outdir, basename(granules_href)[i]), overwrite = T),
                httr::config(netrc = TRUE, netrc_file = netrc),
                httr::set_cookies("LC" = "cookies")
              )
          }
          # if it's HDF4 or regular GeoTIFF file.
        } else if (grepl(pattern = ".tif", x = basename(granules_href)[i], fixed = T) |
                   grepl(pattern = ".tiff", x = basename(granules_href)[i], fixed = T) |
                   grepl(pattern = ".hdf", x = basename(granules_href)[i], fixed = T) |
                   grepl(pattern = ".nc", x = basename(granules_href)[i], fixed = T)) {
          while ("try-error" %in% class(try(terra::rast(file.path(outdir, basename(granules_href)[i])), silent = T))) {
            response <-
              httr::GET(
                granules_href[i],
                httr::write_disk(file.path(outdir, basename(granules_href)[i]), overwrite = T),
                httr::config(netrc = TRUE, netrc_file = netrc),
                httr::set_cookies("LC" = "cookies")
              )
          }
        }
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
            httr::config(netrc = TRUE, netrc_file = netrc),
            httr::set_cookies("LC" = "cookies")
          )
      }
    }
    # return paths of downloaded data and the associated metadata.
    return(file.path(outdir, basename(granules_href)))
  } else {
    return(granules_href)
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

#' Set NASA DAAC credentials to the .netrc file.
#'
#' @param dl_path Character: physical path to the .netrc credential file.
#'
#' @author Dongchen Zhang
getnetrc <- function (dl_path) {
  netrc <- path.expand(dl_path)
  if (file.exists(netrc) == FALSE ||
      any(grepl("urs.earthdata.nasa.gov",
                readLines(netrc))) == FALSE) {
    netrc_conn <- file(netrc, open = "at")
    writeLines(c(
      "machine urs.earthdata.nasa.gov",
      sprintf(
        "login %s",
        getPass::getPass(msg = "Enter NASA Earthdata Login Username \n (or create an account at urs.earthdata.nasa.gov) :")
      ),
      sprintf(
        "password %s",
        getPass::getPass(msg = "Enter NASA Earthdata Login Password:")
      )
    ),
    netrc_conn)
    close(netrc_conn)
    message(
      "A netrc file with your Earthdata Login credentials was stored in the output directory "
    )
  }
  return(netrc)
}