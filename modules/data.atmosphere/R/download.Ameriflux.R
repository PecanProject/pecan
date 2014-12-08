# lookup the site based on the site_id
download.Ameriflux.site <- function(site_id) {
  sites <- c("622"="US-Syv", "676"="US-WCr", "678"="US-PFa", "679"="US-Los",
             "751"="US-ARM", "752"="US-Blo", "753"="US-Bo1", "754"="US-Brw", 
             "755"="US-Dk2", "756"="US-Dk3", "757"="US-FPe", "758"="US-Ha1", 
             "759"="US-Ho1", "760"="US-IB1", "761"="US-IB2", "762"="US-Ivo",
             "763"="US-Me2", "764"="US-Me3", "766"="US-Me5", "767"="US-MMS",
             "768"="US-MOz", "769"="US-Ne1", "770"="US-Ne2", "771"="US-Ne3",
             "772"="US-NR1", "773"="US-Shd", "774"="US-SO2", "775"="US-Ton",
             "776"="US-UMB", "777"="US-Var", "778"="US-Atq", "796"="US-Bar",
             "899"="US-WI8", "900"="US-WI1", "906"="US-WI7", "907"="US-WI0",
             "908"="US-WI9")
  sites[as.character(site_id)]
}

##' Download Ameriflux L2 netCDF files
##'
##' @name download.Ameriflux
##' @title download.Ameriflux
##' @export
##' @param site the site to be downloaded, will be used as prefix as well
##' @param outfolder location on disk where outputs will be stored
##' @param start_date the start date of the data to be downloaded (will only use the year part of the date)
##' @param end_date the end date of the data to be downloaded (will only use the year part of the date)
##' @param overwrite should existing files be overwritten
##' @param verbose should the function be very verbose
##' 
##' @author Josh Mantooth, Rob Kooper
download.Ameriflux <- function(site, outfolder, start_date, end_date, overwrite=FALSE, verbose=FALSE) {
  # get start/end year code works on whole years only
  start_year <- year(start_date)
  end_year <- year(end_date)
  
  # make sure output folder exists
  if(!file.exists(outfolder)){
    dir.create(outfolder, showWarnings=FALSE, recursive=TRUE)
  }
  
  # url where Ameriflux data is stored
  baseurl <- paste0("http://cdiac.ornl.gov/ftp/ameriflux/data/Level2/Sites_ByID/", site, "/with_gaps/")
  
  # fetch all links
  links <- xpathSApply(htmlParse(baseurl), "//a/@href")
  
  # find all links we need based on the years and download them
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
    
    # see if file exists
    if (file.exists(outputfile) && !overwrite) {
      logger.debug("File '", outputfile, "' already exists, skipping to next file.")
      next
    }
    
    file <- tail(as.character(links[grep(paste0('_', year, '_.*.nc'), links)]), n=1)
    download.file(paste0(baseurl, file), outputfile)
  }
  
  # return list of files downloaded
  invisible(results)
}

#site <- download.Ameriflux.site(622)
#print(download.Ameriflux(2001, 2005, site, "/tmp/met/ameriflux"))