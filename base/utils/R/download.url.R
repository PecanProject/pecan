##' Try and download a file.
##' 
##' This will download a file, if retry404 and 404 is returned it will
##' wait until the file is available. If the file is still not available
##' after timeout tries, it will return NA. If the file is downloaded
##' it will return the name of the file
##' 
##' @name download.url
##' @title Download file from the url.
##' @export
##' @param url the url of the file to download
##' @param file the filename
##' @param timeout number of seconds to wait for file (default 600)
##' @param .opts list of options for curl, for example to download from a
##'        protected site use list(userpwd=userpass, httpauth = 1L)
##' @param retry404 retry on a 404, this is used by Brown Dog
##' @return returns name of file if successful or NA if not.
##' 
##' @examples
##' \dontrun{
##' download.url('http://localhost/', index.html)
##' }
download.url <- function(url, file, timeout = 600, .opts = list(), retry404 = TRUE) {
  dir.create(basename(file), recursive = TRUE)
  count <- 0
  while (!curl::curl_fetch_memory(url, handle = curl::new_handle(nobody = 1)) && count < timeout) {
    count <- count + 1
    Sys.sleep(1)
  }
  if (count >= timeout) {
    return(NA)
  }
  f <- open(file, mode = "wb")
  curl::curl_download(url, file, handle = curl::new_handle(.list = .opts))
  close(f)
  
  return(file)
} # download.url
