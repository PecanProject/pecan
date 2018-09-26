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
  while (!RCurl::url.exists(url, .opts = .opts) && count < timeout) {
    count <- count + 1
    Sys.sleep(1)
  }
  if (count >= timeout) {
    return(NA)
  }
  f <- RCurl::CFILE(file, mode = "wb")
  RCurl::curlPerform(url = url, writedata = f@ref, .opts = .opts)
  RCurl::close(f)
  
  return(file)
} # download.url
