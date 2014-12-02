##' Download Ameriflux L2 netCDF files
##'
##' @name download.Ameriflux
##' @title download.Ameriflux
##' @export
##' @param start_year
##' @param end_year
##' @param site_id
##' @param in.prefix
##' @param outfolder
##' @param con database connection
##' 
##' @author Josh Mantooth, Rob Kooper
download.Ameriflux <- function(start_year, end_year, site_id, in.prefix, outfolder, con) {
  site <- download.Ameriflux.site(site_id)
  if (is.na(site)) {
    return(invisible(NA))
  }
  
  # make sure output folder exists
  if(!file.exists(outfolder)){
    dir.create(outfolder, showWarnings=FALSE, recursive=TRUE)
  }
  
  # url where Ameriflux data is stored
  baseurl <- paste0("http://cdiac.ornl.gov/ftp/ameriflux/data/Level2/Sites_ByID/", site, "/with_gaps/")
  
  # fetch all links
  links <- xpathSApply(htmlParse(baseurl), "//a/@href")
  
  # find all links we need based on the years and download them
  results <- list()
  for(year in start_year:end_year) {
    startdate <- paste0(year,"-01-01 00:00:00")
    enddate <- paste0(year,"-12-31 23:59:59")
    mimetype <- 'application/x-netcdf'
    formatname <- 'Ameriflux'
    
    id <- dbfile.input.check(site_id, startdate, enddate, mimetype, formatname, con=con)
    if (nrow(id) > 0) {
      outputfile <- file.path(id$file_path, id$file_name)
      if (file.exists(outputfile)) {
        results[as.character(tail(id$id, n=1))] <- outputfile
        next
      }
    }
    
    # file not found
    outputfile <- file.path(outfolder, paste0(in.prefix, ".", year, ".nc"))
    if (!file.exists(outputfile)) {
      file <- tail(as.character(links[grep(paste0('_', year, '_.*.nc'), links)]), n=1)
      download.file(paste0(baseurl, file), outputfile)
    }
    
    # save result to database
    id <- dbfile.input.insert(outputfile, site_id, startdate, enddate, mimetype, formatname, con=con)
    results[as.character(id$dfbile.id)] <- outputfile
  }
  
  # return list of files downloaded
  invisible(results)
}

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

