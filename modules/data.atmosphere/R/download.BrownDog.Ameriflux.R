##' @name download.BrownDog.Ameriflux
##' @title download.BrownDog.Ameriflux
##' @export
##' @param site the site to be downloaded, will be used as prefix as well
##' @param outfolder location on disk where outputs will be stored
##' @param start_date the start date of the data to be downloaded (will only use the year part of the date)
##' @param end_date the end date of the data to be downloaded (will only use the year part of the date)
##' @param bd.host base url to Browndog (currently "http://dap.ncsa.illinois.edu:8184/convert")
##' @param overwrite should existing files be overwritten
##' @param verbose should the function be very verbose
##' 
##' @author Betsy Cowdery

download.BrownDog.Ameriflux <- function(site, outfolder, start_date, end_date, bd.host, overwrite=FALSE, verbose=FALSE){
  
  require(lubridate)
  require(PEcAn.utils)
  require(data.table)
  
  
  # make sure output folder exists
  if(!file.exists(outfolder)){
    dir.create(outfolder, showWarnings=FALSE, recursive=TRUE)
  }
  
  start_date <- as.POSIXlt(start_date, tz = "GMT")
  end_date <- as.POSIXlt(end_date, tz = "GMT")
  
  # The results table is also created in download.Ameriflux
  # but may not be able to retrieve that output from Brown Dog call so recreating it here for now
  
  start_year <- year(start_date)
  end_year <- year(end_date)
  rows <- end_year - start_year + 1
  results <- data.frame(file=character(rows), host=character(rows),
                        mimetype=character(rows), formatname=character(rows),
                        startdate=character(rows), enddate=character(rows),
                        stringsAsFactors = FALSE)
  for(year in start_year:end_year) {
    outputfile <- file.path(outfolder, paste(site, year, "nc", sep="."))
    
    # create array with results
    row <- year - start_year + 1
    results$file[row] <- outputfile
    results$host[row] <- fqdn()
    results$startdate[row] <- paste0(year,"-01-01 00:00:00")
    results$enddate[row] <- paste0(year,"-12-31 23:59:59")
    results$mimetype[row] <- 'application/x-netcdf'
    results$formatname[row] <- 'Ameriflux'
  }
  
  ########################################################################
  # Send request to Brown Dog 
  
  require(RCurl)
#   require(httr)
  require(XML)
  
  # bd.host <- "http://dap.ncsa.illinois.edu:8184/convert"
  output.format <- "pecan.zip"
  url <- file.path(bd.host,output.format) 
  
  xml_text = newXMLNode("input")
  
newXMLNode("type", "ameriflux", parent = xml_text)
  newXMLNode("site", site, parent = xml_text)
  newXMLNode("start_date", paste(start_date), parent = xml_text)
  newXMLNode("end_date", paste(end_date), parent = xml_text)
  xml_text <- saveXML(xml_text)
  
  html <- postForm(uri = url, "pecan.xml" = fileUpload(filename = "pecan.xml", contents = xml_text))
  link <- getHTMLLinks(html)
  
  tf <- file.path(outfolder, paste("Ameriflux.zip"))  
  i = 1
  dl_file(link, tf, i)
  
  fname <- unzip(tf, list=TRUE)$Name
  unzip(tf, files=fname, exdir=outfolder, overwrite=TRUE) 
  file.remove(tf)
  invisible(results)
  
}


