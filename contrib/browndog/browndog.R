#!/usr/bin/Rscript

require(RCurl)

##' Try and download a file.
##'
##' This will download a file, if a 404 is returned it will wait until
##' the file is available. If the file is still not available after
##' timeout tries, it will return NA. If the file is downloaded it will
##' return the name of the file
##'
##' @name download.browndog
##' @title Download file from browndog.
##' @param the url of the file to download
##' @param the filename
##' @param timeout number of seconds to wait for file (default 60)
##' @param list of options for curl, for example to download from a
##'        protected site use list(userpwd=userpass, httpauth = 1L)
##' @return returns name of file if successfull or NA if not.
##'
##' @examples
##' \dontrun{
##' download.browndog('http://localhost/', index.html)
##' }
download.browndog = function(url,  file, timeout=60, .opts=list()) {
  count <- 0
  while (!url.exists(url, .opts=.opts) && count < timeout) {
    count <- count + 1
    Sys.sleep(1)
  }
  if (count >= timeout) {
    return(NA)
  }
  f = CFILE(file, mode="wb")
  curlPerform(url=url, writedata=f@ref, .opts=.opts)
  RCurl::close(f)

  return(file)
}


type      <- "NARR"
site      <- "US-NR1"
site_lat  <- "40.0329"
site_lon  <- "-105.546"
startDate <- "2001-01-01 00:00:00"
endDate   <- "2001-12-31 23:59:59"
browndog  <- "http://host/path";
userpass  <- "user:password"
output    <- "clim"

outputfile <- paste(site, output, sep=".")

xmldata <- paste0("<input>",
                  "<type>", type, "</type>",
                  "<site>", site, "</site>",
                  "<lat>", site_lat, "</lat>",
                  "<lon>", site_lon, "</lon>",
                  "<start_date>", startDate, "</start_date>",
                  "<end_date>", endDate, "</end_date>",
                  "</input>")

# post to browndog
curloptions <- list(userpwd=userpass, httpauth=1L, followlocation=TRUE)
result <- postForm(paste0(browndog, output, "/"),
                   "fileData"=fileUpload("pecan.xml", xmldata, "text/xml"),
                   .opts=curloptions)
url <- gsub('.*<a.*>(.*)</a>.*', '\\1', result)
download.browndog(url, outputfile, 120, curloptions)
