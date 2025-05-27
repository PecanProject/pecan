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
#' @param band Character: the band name of data to be requested.
#' @param credential.folder Character: physical path to the folder that contains 
#' the credential file. The default is NULL.
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
#' ul_lat <- 35
#' ul_lon <- -121
#' lr_lat <- 33
#' lr_lon <- -117
#' from <- "2022-02-23"
#' to <- "2022-05-30"
#' doi <- "10.3334/ORNLDAAC/2183"
#' outdir <- "/projectnb/dietzelab/dongchen/SHIFT/test_download"
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
                               credential.folder = NULL,
                               doi,
                               just_path = FALSE) {
  # Determine if we have enough inputs.
  if (is.null(outdir) & !just_path) {
    PEcAn.logger::logger.info("Please provide outdir if you want to download the file.")
    return(0)
  }
  # setup DAAC Credentials.
  DAAC_Set_Credential(folder.path = credential.folder)
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
      # if it's GLANCE product.
      # GLANCE product has special data archive.
      if (doi == "10.5067/MEaSUREs/GLanCE/GLanCE30.001") {
        granules_href <- c(granules_href, sapply(granules, function(x) {
          links <- c()
          for (j in seq_along(x$links)) {
            links <- c(links, x$links[[j]]$href)
          }
          return(links)
        }))
      } else {
        granules_href <- c(granules_href, sapply(granules, function(x) x$links[[1]]$href))
      }
      # grab specific band.
      if (!is.null(band)) {
        granules_href <- granules_href[which(grepl(band, granules_href, fixed = T))]
      }
      page <- page + 1
    }
  }
  # remove duplicated files.
  inds <- which(duplicated(basename(granules_href)))
  if (length(inds) > 0) {
    granules_href <- granules_href[-inds]
  }
  # remove non-image files.
  inds <- which(grepl(".h5", basename(granules_href)) |
                  grepl(".tif", basename(granules_href)) |
                  grepl(".hdf", basename(granules_href)))
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
      PEcAn.logger::logger.info("The doSNOW package is not installed.")
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
              httr::authenticate(user = Sys.getenv("ed_un"),
                                 password = Sys.getenv("ed_pw"))
            )
        ))){
          response <-
            httr::GET(
              granules_href[i],
              httr::write_disk(file.path(outdir, basename(granules_href)[i]), overwrite = T),
              httr::authenticate(user = Sys.getenv("ed_un"),
                                 password = Sys.getenv("ed_pw"))
            )
        }
        # Check if we can successfully open the downloaded file.
        # if it's H5 file.
        if (grepl(pattern = ".h5", x = basename(granules_href)[i], fixed = T)) {
          # check if the hdf5r package exists.
          if ("try-error" %in% class(try(find.package("hdf5r")))) {
            PEcAn.logger::logger.info("The hdf5r package is not installed.")
            return(NA)
          }
          while ("try-error" %in% class(try(hdf5r::H5File$new(file.path(outdir, basename(granules_href)[i]), mode = "r"), silent = T))) {
            response <-
              httr::GET(
                granules_href[i],
                httr::write_disk(file.path(outdir, basename(granules_href)[i]), overwrite = T),
                httr::authenticate(user = Sys.getenv("ed_un"),
                                   password = Sys.getenv("ed_pw"))
              )
          }
          # if it's HDF4 or regular GeoTIFF file.
        } else if (grepl(pattern = ".tif", x = basename(granules_href)[i], fixed = T) |
                   grepl(pattern = ".tiff", x = basename(granules_href)[i], fixed = T) |
                   grepl(pattern = ".hdf", x = basename(granules_href)[i], fixed = T)) {
          while ("try-error" %in% class(try(terra::rast(file.path(outdir, basename(granules_href)[i])), silent = T))) {
            response <-
              httr::GET(
                granules_href[i],
                httr::write_disk(file.path(outdir, basename(granules_href)[i]), overwrite = T),
                httr::authenticate(user = Sys.getenv("ed_un"),
                                   password = Sys.getenv("ed_pw"))
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
            httr::authenticate(user = Sys.getenv("ed_un"),
                               password = Sys.getenv("ed_pw"))
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

#' Set NASA DAAC credentials to the current environment.
#'
#' @param replace Boolean: determine if we want to replace the current credentials from the environment. The default is FALSE.
#' @param folder.path Character: physical path to the folder that contains the credential file. The default is NULL.
#'
#' @author Dongchen Zhang
DAAC_Set_Credential <- function(replace = FALSE, folder.path = NULL) {
  if (replace) {
    PEcAn.logger::logger.info("Replace previous stored NASA DAAC credentials.")
  }
  # if we have the credential file.
  if (!is.null(folder.path)) {
    if (file.exists(file.path(folder.path, ".nasadaacapirc"))) {
      key <- readLines(file.path(folder.path, ".nasadaacapirc"))
      Sys.setenv(ed_un = key[1], ed_pw = key[2])
    }
  }
  # otherwise we will type the credentials manually.
  if (replace | nchar(Sys.getenv("ed_un")) == 0 | nchar(Sys.getenv("ed_un")) == 0) {
    Sys.setenv(ed_un = sprintf(
      getPass::getPass(msg = "Enter NASA Earthdata Login Username \n (or create an account at urs.earthdata.nasa.gov) :")
    ), 
    ed_pw = sprintf(
      getPass::getPass(msg = "Enter NASA Earthdata Login Password:")
    ))
  }
}