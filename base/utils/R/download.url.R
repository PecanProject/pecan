#' Try and download a file.
#'
#' This will download a file, if retry is set and 404 is returned it will
#' wait until the file is available. If the file is still not available
#' after timeout tries, it will return NA. If the file is downloaded
#' it will return the name of the file
#'
#' @export
#' @param url the url of the file to download
#' @param file the filename
#' @param timeout number of seconds to wait for file (default 600)
#' @param .opts list of options for curl, for example to download from a
#'        protected site use list(userpwd=userpass, httpauth = 1L)
#' @param retry retry if url not found yet, this is used by Brown Dog
#' @return returns name of file if successful or NA if not.
#'
#' @examples
#' \dontrun{
#' download.url('http://localhost/', index.html)
#' }
download.url <- function(url, file, timeout = 600, .opts = list(), retry = TRUE) {
  count <- 0
  while (retry && !url_found(url) && count < timeout) {
    count <- count + 1
    Sys.sleep(1)
  }
  if (count >= timeout || (!retry && !url_found(url))) {
    return(NA)
  }
  dir.create(dirname(file), recursive = TRUE)
  res <- curl::curl_download(
    url = url,
    destfile = file,
    handle = curl::new_handle(.list = .opts))
  
  res
} # download.url


# An approximate replacement for RCurl::url.exists
# Treats any 200 status as success (NB does not follow redirects!)
url_found <- function(url) {
  h <- curl::new_handle(nobody = 1L) # "nobody" = header-only request
  res <- curl::curl_fetch_memory(url, handle = h)

  res$status_code %/% 200 == 1
}
