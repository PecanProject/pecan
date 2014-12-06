# lookup the site based on the site_id
download.Ameriflux.site <- function(site_id) {
  sites <- read.csv(system.file("data/FLUXNET.sitemap.csv",package="PEcAn.data.atmosphere"),
                    stringsAsFactors=FALSE)
  sites$FLUX.id[which(sites$site.id == site_id)]
}

##' Download Ameriflux L2 netCDF files
##'
##' @name download.Ameriflux
##' @title download.Ameriflux
##' @export
##' @param start_year
##' @param end_year
##' @param site 
##' @param outfolder
##' @param con database connection
##' 
##' @author Josh Mantooth, Rob Kooper
download.Ameriflux <- function(start_year, end_year, site, outfolder, overwrite=FALSE) {
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